package app

import (
	"context"
	"errors"
	"fmt"
	"io"
	"net"
	"os"
	"path/filepath"
	"strings"
	"time"

	"runner.codes/internal/domain"
	"runner.codes/internal/ports"
)

// machineADB guarda, no diretório da máquina Android ligada, o endereço do adbd no host (127.0.0.1:<porta>): a porta
// é escolhida a cada start, e quem fala com o Android (rotas de tela e de entrada) a lê daqui.
const machineADB = "adb.addr"

// AndroidBootTimeout é o prazo padrão do boot do Android: a prova mediu 36 s num n2 com 4 vCPU (06/11/2026); o
// primeiro boot de uma máquina nova otimiza os apps (dexopt) e leva mais, e um worker carregado mais ainda. Era 4 min:
// num servidor de 1 vCPU (n1-standard-1 do autoscaler do superbot) o primeiro boot não fechou nesse prazo (06/11/2026).
const AndroidBootTimeout = 10 * time.Minute

// androidPath é o PATH do /init do Android (o dele mesmo; o init monta o resto do ambiente).
const androidPath = "PATH=/product/bin:/apex/com.android.runtime/bin:/system/bin:/system/xbin"

// AndroidSupport são as peças que um servidor precisa para ligar máquina Android. Zerado, o servidor recusa.
type AndroidSupport struct {
	// Rootfs prepara o rootfs da máquina (o Redroid extraído, com os ajustes) e devolve o diretório.
	Rootfs func(ctx context.Context, image, machineDir string) (string, error)
	// KrunfwDir é o diretório da libkrunfw com binder (o kernel do Android).
	KrunfwDir string
	// ADB fala com o adbd das máquinas.
	ADB ports.ADB
	// BootTimeout limita a espera do boot; 0 usa AndroidBootTimeout.
	BootTimeout time.Duration
	// FreePort escolhe a porta do ADB no host; nil pede uma ao sistema.
	FreePort func() (uint16, error)
}

// errNoAndroid é a recusa de um servidor sem o kernel do Android.
var errNoAndroid = domain.ValidationError{Err: domain.MachineError{Code: domain.CodeConfig,
	Msg: "android machines are not available on this server (it needs the android kernel, RUNNER_ANDROID_KRUNFW)"}}

// ready diz se o servidor tem tudo para ligar um Android.
func (a AndroidSupport) ready() bool { return a.Rootfs != nil && a.KrunfwDir != "" && a.ADB != nil }

// AndroidReady diz se este servidor liga máquina Android (vai no GET /api/v1/host para quem agenda).
func (m *Machines) AndroidReady() bool { return m.Android.ready() }

// freeLoopbackPort pede ao sistema uma porta livre no 127.0.0.1. Há uma janela entre fechar e o gateway abrir: outra
// coisa pode pegá-la nesse meio, e aí o start falha com "address already in use" — raro, e explícito.
func freeLoopbackPort() (uint16, error) {
	l, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		return 0, err
	}
	defer l.Close()
	return uint16(l.Addr().(*net.TCPAddr).Port), nil
}

// startAndroid liga uma máquina Android: o rootfs do Redroid por virtio-fs, o /init dele como PID 1 (sem o agente do
// rc), o kernel com binder, a rede atrás do gateway com a porta do ADB publicada no loopback, e o pronto é o
// sys.boot_completed=1 lido pelo ADB.
func (m *Machines) startAndroid(ctx context.Context, st MachineStatus, tied bool) (MachineStatus, error) {
	a := m.Android
	if !a.ready() {
		return st, errNoAndroid
	}
	name := st.Name
	dir := m.Store.Dir(name)
	root, err := a.Rootfs(ctx, st.Image, dir)
	if err != nil {
		return st, err
	}
	pick := a.FreePort
	if pick == nil {
		pick = freeLoopbackPort
	}
	port, err := pick()
	if err != nil {
		return st, fmt.Errorf("android: pick the adb port: %w", err)
	}
	addr := fmt.Sprintf("127.0.0.1:%d", port)
	if err := os.WriteFile(filepath.Join(dir, machineADB), []byte(addr), 0o600); err != nil {
		return st, fmt.Errorf("android: %w", err)
	}
	netw := st.Network
	netw.Ports = append(append([]domain.PortMap{}, netw.Ports...), domain.PortMap{Host: port, Guest: domain.ADBPort})
	spec := ports.VMSpec{
		TiedToCaller: tied,
		CPUs:         st.CPUs, MemoryMiB: st.MemoryMiB,
		RootDir: root, KrunfwDir: a.KrunfwDir,
		Console: filepath.Join(dir, machineConsole),
		Exec:    "/init", Args: st.Android.BootArgs(),
		// KRUN_INIT_PID1: o init da libkrun faz execvp em vez de fork, e o /init do Android vira o PID 1.
		Env: []string{"KRUN_INIT_PID1=1", androidPath},
		Net: m.vmNet(st.Machine, dir, netw),
	}
	vmmLog := filepath.Join(dir, machineVMMLog)
	logFrom := fileSize(vmmLog)
	pid, err := m.Launcher.StartDetached(spec, filepath.Join(dir, machineSpec), vmmLog)
	if err != nil {
		return st, err
	}
	if err := m.Store.SetPID(name, pid); err != nil {
		m.Procs.Kill(pid)
		return st, err
	}
	timeout := a.BootTimeout
	if timeout <= 0 {
		timeout = AndroidBootTimeout
	}
	bootCtx, cancel := context.WithTimeout(ctx, timeout)
	defer cancel()
	if err := a.ADB.WaitBoot(m.watchVMM(bootCtx, pid), addr, 2*time.Second); err != nil {
		alive := m.Procs.Alive(pid)
		m.Procs.Kill(pid)
		m.Store.SetPID(name, 0)
		if !alive {
			return st, fmt.Errorf("android %s failed to start: %s; console: %s", name,
				lastLine(readFrom(vmmLog, logFrom)), tailFile(filepath.Join(dir, machineConsole), 400))
		}
		return st, fmt.Errorf("android %s did not finish booting: %w; console: %s", name, err,
			tailFile(filepath.Join(dir, machineConsole), 400))
	}
	return m.Status(name)
}

// ErrNotAndroid é a recusa das operações de Android numa máquina comum.
var ErrNotAndroid = errors.New("not an android machine")

// androidAddr devolve o endereço do adbd de uma máquina Android ligada.
func (m *Machines) androidAddr(name string) (MachineStatus, string, error) {
	st, err := m.Status(name)
	if err != nil {
		return st, "", err
	}
	if !st.IsAndroid() {
		return st, "", domain.MachineError{Code: domain.CodeConfig, Msg: name + ": " + ErrNotAndroid.Error()}
	}
	if st.State != StateRunning {
		return st, "", domain.ErrMachineState(name, "is not running")
	}
	if m.Android.ADB == nil {
		return st, "", errNoAndroid
	}
	raw, err := os.ReadFile(filepath.Join(m.Store.Dir(name), machineADB))
	if err != nil {
		return st, "", fmt.Errorf("android %s: adb address: %w", name, err)
	}
	return st, strings.TrimSpace(string(raw)), nil
}

// AndroidUI devolve a árvore da tela do Android (o XML do uiautomator): o que o agente lê em vez de uma imagem. O
// dump vai para /data/local/tmp, não /sdcard: o /sdcard é armazenamento emulado, que o uiautomator e o shell veem
// por montagens diferentes — o dump dizia "dumped to /sdcard/ui.xml" e o arquivo não existia (medido em 06/11/2026).
func (m *Machines) AndroidUI(ctx context.Context, name string) ([]byte, error) {
	_, addr, err := m.androidAddr(name)
	if err != nil {
		return nil, err
	}
	const path = "/data/local/tmp/rc-ui.xml"
	out, err := m.Android.ADB.Shell(ctx, addr, "uiautomator dump "+path)
	if err != nil {
		return nil, err
	}
	if !strings.Contains(out, "dumped to") {
		return nil, fmt.Errorf("android %s: uiautomator: %s", name, strings.TrimSpace(out))
	}
	return m.Android.ADB.Exec(ctx, addr, "exec:cat "+path)
}

// AndroidInput executa uma ação na tela (toque, arrasto, texto, tecla), validada no domínio contra o tamanho da tela.
func (m *Machines) AndroidInput(ctx context.Context, name string, in domain.AndroidInput) error {
	st, addr, err := m.androidAddr(name)
	if err != nil {
		return err
	}
	cmd, err := in.Command(*st.Android)
	if err != nil {
		return domain.ValidationError{Err: domain.MachineError{Code: domain.CodeConfig, Msg: err.Error()}}
	}
	out, err := m.Android.ADB.Shell(ctx, addr, cmd)
	if err != nil {
		return err
	}
	// O `input` só escreve quando dá errado (uso, tecla desconhecida).
	if s := strings.TrimSpace(out); s != "" {
		return fmt.Errorf("android %s: %s: %s", name, cmd, s)
	}
	return nil
}

// AndroidShellMax é o teto de uma linha do terminal do console (4 KB): é o que se digita, não um script.
const AndroidShellMax = 4 << 10

// AndroidShell roda uma linha de comando no shell do Android (o terminal do console) e devolve stdout e stderr juntos.
// Sem pty: cada linha é um `sh -c` novo — quem quer `cd` persistente repete o diretório na linha (o console faz isso).
func (m *Machines) AndroidShell(ctx context.Context, name, cmd string) (string, error) {
	if strings.TrimSpace(cmd) == "" || len(cmd) > AndroidShellMax {
		return "", domain.ValidationError{Err: domain.MachineError{Code: domain.CodeConfig, Msg: "command is empty or too long"}}
	}
	_, addr, err := m.androidAddr(name)
	if err != nil {
		return "", err
	}
	return m.Android.ADB.Shell(ctx, addr, cmd)
}

// androidPTY é o terminal (PTY) de uma máquina Android: o shell v2 do adbd, já que ali não há o agente do rc. Sem argv
// é o shell de login; com argv, a linha vai ao sh do Android como veio (usuário, env e cwd não se aplicam: o adbd
// roda como shell e o TERM vai no próprio serviço).
func (m *Machines) androidPTY(ctx context.Context, name string, req ShellRequest, in io.Reader, out io.Writer, resize <-chan ports.Winsize) (int, error) {
	_, addr, err := m.androidAddr(name)
	if err != nil {
		return 0, err
	}
	code, err := m.Android.ADB.Interactive(ctx, addr, strings.Join(req.Argv, " "), req.Size, in, out, resize)
	if err != nil {
		return 0, fmt.Errorf("shell in machine %s: %w", name, err)
	}
	return code, nil
}

// AndroidScreenshot devolve a tela do Android em PNG (screencap pelo exec:, que não mexe nos bytes).
func (m *Machines) AndroidScreenshot(ctx context.Context, name string) ([]byte, error) {
	_, addr, err := m.androidAddr(name)
	if err != nil {
		return nil, err
	}
	png, err := m.Android.ADB.Exec(ctx, addr, "exec:screencap -p")
	if err != nil {
		return nil, err
	}
	if len(png) < 8 || string(png[1:4]) != "PNG" {
		return nil, fmt.Errorf("android %s: screencap did not return a PNG (%d bytes)", name, len(png))
	}
	return png, nil
}

// stopAndroid pede ao Android para desligar (reboot -p): o init dele para os serviços, o kernel encerra e o processo
// vmm sai. Erro não importa aqui — o que decide é o processo terminar, conferido por quem chama.
func (m *Machines) stopAndroid(ctx context.Context, name string) {
	_, addr, err := m.androidAddr(name)
	if err != nil {
		return
	}
	m.Android.ADB.Shell(ctx, addr, "reboot -p")
}
