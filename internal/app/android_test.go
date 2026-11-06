package app

import (
	"bytes"
	"context"
	"errors"
	"io"
	"net"
	"os"
	"path/filepath"
	"slices"
	"strconv"
	"strings"
	"sync"
	"testing"
	"time"

	"runner.codes/internal/domain"
	"runner.codes/internal/ports"
)

// TestAndroidPTY: o terminal de uma máquina Android vai ao shell v2 do adbd (não ao agente do rc, que ali não existe),
// com o tamanho pedido e o shell de login sem argv; o código de saída volta; a falha do adb sai com o nome da máquina;
// máquina desligada recusa sem chegar ao adb.
func TestAndroidPTY(t *testing.T) {
	m, _, _, _, adb := androidFixture(t)
	ctx := context.Background()
	if _, err := m.Shell(ctx, "phone", ShellRequest{Size: ports.Winsize{Rows: 30, Cols: 100}}, strings.NewReader("x"), io.Discard, nil); err == nil {
		t.Fatal("terminal de Android desligado passou")
	}
	if len(adb.ptys) != 0 {
		t.Fatalf("desligado chegou ao adb: %v", adb.ptys)
	}
	if _, err := m.Start(ctx, "phone"); err != nil {
		t.Fatal(err)
	}
	adb.ptyCode = 3
	var out bytes.Buffer
	code, err := m.Shell(ctx, "phone", ShellRequest{Size: ports.Winsize{Rows: 30, Cols: 100}}, strings.NewReader("ls\r"), &out, nil)
	if err != nil || code != 3 || out.String() != "ls\r" {
		t.Fatalf("pty: code=%d err=%v out=%q", code, err, out.String())
	}
	if want := "127.0.0.1:41555||30x100"; len(adb.ptys) != 1 || adb.ptys[0] != want {
		t.Fatalf("pty no adb: %v, quer %q", adb.ptys, want)
	}
	m.Shell(ctx, "phone", ShellRequest{Argv: []string{"top", "-n", "1"}}, strings.NewReader(""), io.Discard, nil)
	if got := adb.ptys[1]; !strings.HasPrefix(got, "127.0.0.1:41555|top -n 1|") {
		t.Fatalf("argv: %q", got)
	}
	adb.ptyErr = errors.New("adbd caiu")
	if _, err := m.Shell(ctx, "phone", ShellRequest{}, strings.NewReader(""), io.Discard, nil); err == nil || !strings.Contains(err.Error(), "phone") {
		t.Fatalf("falha: %v", err)
	}
}

// fakeADB é o adbd falso: anota cada comando e responde pelo roteiro.
type fakeADB struct {
	mu       sync.Mutex
	shells   []string
	execs    []string
	bootErr  error
	bootAddr string
	shellOut map[string]string // prefixo do comando -> saída
	execOut  map[string][]byte // serviço -> saída
	ptys     []string          // "endereço|comando|LxC" de cada terminal aberto
	ptyCode  int
	ptyErr   error
}

// Exec anota o serviço e devolve a saída do roteiro.
func (f *fakeADB) Exec(_ context.Context, addr, service string) ([]byte, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.execs = append(f.execs, service)
	if out, ok := f.execOut[service]; ok {
		return out, nil
	}
	return nil, errors.New("exec sem roteiro: " + service)
}

// Shell anota o comando e devolve a saída do primeiro prefixo que casar.
func (f *fakeADB) Shell(_ context.Context, addr, cmd string) (string, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.shells = append(f.shells, cmd)
	for p, out := range f.shellOut {
		if strings.HasPrefix(cmd, p) {
			return out, nil
		}
	}
	return "", nil
}

// WaitBoot anota o endereço e devolve o erro do roteiro.
func (f *fakeADB) WaitBoot(_ context.Context, addr string, _ time.Duration) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.bootAddr = addr
	return f.bootErr
}

// Interactive anota o comando e o tamanho, ecoa o teclado na saída e devolve o código (ou a falha) do roteiro.
func (f *fakeADB) Interactive(_ context.Context, addr, cmd string, size ports.Winsize, in io.Reader, out io.Writer, _ <-chan ports.Winsize) (int, error) {
	f.mu.Lock()
	f.ptys = append(f.ptys, addr+"|"+cmd+"|"+strconv.Itoa(int(size.Rows))+"x"+strconv.Itoa(int(size.Cols)))
	code, err := f.ptyCode, f.ptyErr
	f.mu.Unlock()
	io.Copy(out, in)
	return code, err
}

// androidFixture monta o serviço com suporte a Android e uma máquina Android registrada.
func androidFixture(t *testing.T) (*Machines, *memStore, *procs, *detached, *fakeADB) {
	m, st, ps, d, _ := machinesFixture(t)
	adb := &fakeADB{shellOut: map[string]string{}, execOut: map[string][]byte{}}
	m.Android = AndroidSupport{
		Rootfs: func(_ context.Context, image, dir string) (string, error) {
			root := filepath.Join(dir, "android-rootfs")
			return root, os.MkdirAll(root, 0o755)
		},
		KrunfwDir:   "/nix/libkrunfw-android/lib",
		ADB:         adb,
		BootTimeout: time.Second,
		FreePort:    func() (uint16, error) { return 41555, nil },
	}
	a := domain.DefaultAndroid
	if _, err := m.Create(context.Background(), domain.Machine{Name: "phone", Image: "redroid/redroid:14", Android: &a}); err != nil {
		t.Fatal(err)
	}
	return m, st, ps, d, adb
}

// TestAndroidCreateNoDisk: a máquina Android nasce sem o disco ext4 (modelo vazio) e sem construir imagem; num
// servidor sem o kernel do Android, o create recusa.
func TestAndroidCreateNoDisk(t *testing.T) {
	_, st, _, _, _ := androidFixture(t)
	if len(st.tpls) != 1 || st.tpls[0] != "" {
		t.Fatalf("modelos = %q; want um vazio", st.tpls)
	}
	m, _, _, _, _ := machinesFixture(t)
	a := domain.DefaultAndroid
	_, err := m.Create(context.Background(), domain.Machine{Name: "x", Image: "redroid/redroid:14", Android: &a})
	if err == nil || !strings.Contains(err.Error(), "android machines are not available") {
		t.Fatalf("sem suporte: %v", err)
	}
}

// TestAndroidStart trava a VM do Android: rootfs em diretório, o kernel próprio, /init como PID 1 com os parâmetros
// da tela, a porta do ADB publicada no loopback, e o pronto pelo boot_completed no endereço gravado.
func TestAndroidStart(t *testing.T) {
	m, st, _, d, adb := androidFixture(t)
	m.EgressProxy, m.EgressCA = "127.0.0.1:15080", "/etc/sbot/egress-ca.pem"
	got, err := m.Start(context.Background(), "phone")
	if err != nil || got.State != StateRunning {
		t.Fatalf("Start = %+v, %v", got.State, err)
	}
	s := d.specs[0]
	if s.RootDir != filepath.Join(st.Dir("phone"), "android-rootfs") || s.RootDisk != "" || s.DataDisk != "" {
		t.Errorf("raiz: dir=%q disk=%q data=%q", s.RootDir, s.RootDisk, s.DataDisk)
	}
	if s.KrunfwDir != "/nix/libkrunfw-android/lib" || s.Exec != "/init" || !slices.Equal(s.Args, domain.DefaultAndroid.BootArgs()) {
		t.Errorf("kernel/exec: %q %q %v", s.KrunfwDir, s.Exec, s.Args)
	}
	if !slices.Contains(s.Env, "KRUN_INIT_PID1=1") || s.VsockSocket != "" || s.SnapshotDir != "" {
		t.Errorf("env=%v vsock=%q snapshot=%q", s.Env, s.VsockSocket, s.SnapshotDir)
	}
	if s.Net == nil || !slices.Contains(s.Net.Network.Ports, domain.PortMap{Host: 41555, Guest: domain.ADBPort}) {
		t.Fatalf("porta do adb não publicada: %+v", s.Net)
	}
	// Host com proxy de saída: o Android sai direto mesmo assim (o proxy abre o TLS com uma CA que o Android não
	// tem, e todo HTTPS dele morria).
	if s.Net.EgressProxy != "" || s.Net.EgressCA != "" {
		t.Errorf("android com proxy de saída: %+v", s.Net)
	}
	if adb.bootAddr != "127.0.0.1:41555" {
		t.Errorf("boot esperado em %q", adb.bootAddr)
	}
	if raw, _ := os.ReadFile(filepath.Join(st.Dir("phone"), machineADB)); string(raw) != "127.0.0.1:41555" {
		t.Errorf("adb.addr = %q", raw)
	}
	// A porta do ADB não vaza para o registro da máquina (é escolhida a cada start).
	if mc, _ := st.Load("phone"); len(mc.Network.Ports) != 0 {
		t.Errorf("porta gravada no registro: %v", mc.Network.Ports)
	}
}

// TestAndroidStartFailures: boot que não fecha mata a VM e explica; vmm que morre no boot cita o log; rootfs e porta
// que falham param antes de lançar.
func TestAndroidStartFailures(t *testing.T) {
	m, st, ps, _, adb := androidFixture(t)
	adb.bootErr = errors.New(`sys.boot_completed="0"`)
	if _, err := m.Start(context.Background(), "phone"); err == nil || !strings.Contains(err.Error(), "did not finish booting") {
		t.Fatalf("boot: %v", err)
	}
	if st.PID("phone") != 0 || len(ps.killed) != 1 {
		t.Fatalf("VM não foi morta: pid=%d killed=%v", st.PID("phone"), ps.killed)
	}
	m2, _, _, d2, adb2 := androidFixture(t)
	d2.dieLog = "krun: android kernel refused"
	adb2.bootErr = context.Canceled
	if _, err := m2.Start(context.Background(), "phone"); err == nil || !strings.Contains(err.Error(), "android kernel refused") {
		t.Fatalf("vmm morto: %v", err)
	}
	m3, _, _, d3, _ := androidFixture(t)
	m3.Android.Rootfs = func(context.Context, string, string) (string, error) { return "", errors.New("registry 401") }
	if _, err := m3.Start(context.Background(), "phone"); err == nil || !strings.Contains(err.Error(), "registry 401") || len(d3.specs) != 0 {
		t.Fatalf("rootfs: %v (lançou %d)", err, len(d3.specs))
	}
	m3.Android.Rootfs = func(_ context.Context, _, dir string) (string, error) { return dir, nil }
	m3.Android.FreePort = func() (uint16, error) { return 0, errors.New("sem porta") }
	if _, err := m3.Start(context.Background(), "phone"); err == nil || !strings.Contains(err.Error(), "sem porta") {
		t.Fatalf("porta: %v", err)
	}
	m3.Android = AndroidSupport{}
	if _, err := m3.Start(context.Background(), "phone"); err == nil || !strings.Contains(err.Error(), "not available") {
		t.Fatalf("sem suporte no start: %v", err)
	}
}

// TestAndroidScreenOps: árvore da tela (dump em /data/local/tmp e o cat), entrada validada, screenshot que precisa
// ser PNG, e o stop pelo reboot -p.
func TestAndroidScreenOps(t *testing.T) {
	m, _, _, _, adb := androidFixture(t)
	ctx := context.Background()
	if _, err := m.AndroidUI(ctx, "phone"); err == nil || !strings.Contains(err.Error(), "not running") {
		t.Fatalf("parada: %v", err)
	}
	if _, err := m.Start(ctx, "phone"); err != nil {
		t.Fatal(err)
	}
	adb.shellOut["uiautomator dump"] = "UI hierchary dumped to: /data/local/tmp/rc-ui.xml\n"
	adb.execOut["exec:cat /data/local/tmp/rc-ui.xml"] = []byte("<hierarchy/>")
	if ui, err := m.AndroidUI(ctx, "phone"); err != nil || string(ui) != "<hierarchy/>" {
		t.Fatalf("UI = %q, %v", ui, err)
	}
	adb.shellOut["uiautomator dump"] = "ERROR: could not get idle state.\n"
	if _, err := m.AndroidUI(ctx, "phone"); err == nil || !strings.Contains(err.Error(), "could not get idle state") {
		t.Fatalf("dump falho: %v", err)
	}
	if err := m.AndroidInput(ctx, "phone", domain.AndroidInput{Kind: "tap", X: 10, Y: 20}); err != nil {
		t.Fatal(err)
	}
	if !slices.Contains(adb.shells, "input tap 10 20") {
		t.Fatalf("toque não chegou: %v", adb.shells)
	}
	if err := m.AndroidInput(ctx, "phone", domain.AndroidInput{Kind: "tap", X: 9999, Y: 1}); errCode(err) != domain.CodeConfig {
		t.Fatalf("fora da tela: %v", err)
	}
	adb.shellOut["input keyevent"] = "Error: unknown key"
	if err := m.AndroidInput(ctx, "phone", domain.AndroidInput{Kind: "key", Key: "HOME"}); err == nil || !strings.Contains(err.Error(), "unknown key") {
		t.Fatalf("saída do input: %v", err)
	}
	adb.shellOut["getprop ro.build.version.release"] = "14\n"
	if out, err := m.AndroidShell(ctx, "phone", "getprop ro.build.version.release"); err != nil || out != "14\n" {
		t.Fatalf("shell: %q %v", out, err)
	}
	for _, bad := range []string{"  ", strings.Repeat("a", AndroidShellMax+1)} {
		if _, err := m.AndroidShell(ctx, "phone", bad); errCode(err) != domain.CodeConfig {
			t.Fatalf("shell inválido (%d bytes): %v", len(bad), err)
		}
	}
	adb.execOut["exec:screencap -p"] = []byte("\x89PNG\r\n\x1a\n...")
	if png, err := m.AndroidScreenshot(ctx, "phone"); err != nil || len(png) < 8 {
		t.Fatalf("screenshot: %v", err)
	}
	adb.execOut["exec:screencap -p"] = []byte("error")
	if _, err := m.AndroidScreenshot(ctx, "phone"); err == nil || !strings.Contains(err.Error(), "not return a PNG") {
		t.Fatalf("screenshot ruim: %v", err)
	}
	if err := m.Stop(ctx, "phone", 50*time.Millisecond); err != nil {
		t.Fatal(err)
	}
	if !slices.Contains(adb.shells, "reboot -p") {
		t.Fatalf("stop sem reboot -p: %v", adb.shells)
	}
}

// TestAndroidOpsOnRegularMachine: as operações de Android numa máquina comum recusam com o motivo.
func TestAndroidOpsOnRegularMachine(t *testing.T) {
	m, _, _, _, _ := androidFixture(t)
	if _, err := m.Create(context.Background(), domain.Machine{Name: "py", Stack: "python"}); err != nil {
		t.Fatal(err)
	}
	if _, err := m.AndroidScreenshot(context.Background(), "py"); err == nil || !strings.Contains(err.Error(), "not an android machine") {
		t.Fatalf("máquina comum: %v", err)
	}
	if _, err := m.AndroidShell(context.Background(), "py", "id"); err == nil || !strings.Contains(err.Error(), "not an android machine") {
		t.Fatalf("shell em máquina comum: %v", err)
	}
	if _, err := m.AndroidUI(context.Background(), "nao-existe"); errCode(err) != domain.CodeNotFound {
		t.Fatalf("inexistente: %v", err)
	}
}

// TestFreeLoopbackPort: a porta devolvida está livre de verdade (dá para escutar nela logo depois).
func TestFreeLoopbackPort(t *testing.T) {
	p, err := freeLoopbackPort()
	if err != nil || p == 0 {
		t.Fatalf("porta = %d, %v", p, err)
	}
	l, err := net.Listen("tcp", net.JoinHostPort("127.0.0.1", strconv.Itoa(int(p))))
	if err != nil {
		t.Fatalf("porta %d não estava livre: %v", p, err)
	}
	l.Close()
}
