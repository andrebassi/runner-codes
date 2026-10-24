package k8sshim

import (
	"bytes"
	"context"
	"errors"
	"fmt"
	"io"
	"net"
	"os"
	"os/exec"
	"path"
	"path/filepath"
	"sync"
	"time"

	"github.com/containerd/containerd/v2/pkg/sys/reaper"

	"runner.codes/internal/adapters/guestproto"
	"runner.codes/internal/adapters/krun"
	"runner.codes/internal/ports"
	"runner.codes/internal/protocol"
)

// Config diz onde estão as peças do rc no nó. O instalador grava em /etc/rc/shim.json.
type Config struct {
	// RC é o binário rc (ele tem o subcomando vmm que vira o processo da VM).
	RC string `json:"rc"`
	// LibDir e LibPath: o diretório da libkrun/libkrunfw e a libkrun em si.
	LibDir  string `json:"lib_dir"`
	LibPath string `json:"lib_path"`
	// Image é o ext4 da VM do pod (Alpine com o guest do rc), read-only e compartilhado.
	Image string `json:"image"`
	// StateDir guarda o diretório de cada pod: spec, console, socket do vsock e o share.
	StateDir string `json:"state_dir"`
}

// bootTimeout é o teto do boot até o guest responder. Medido no droplet de teste (KVM aninhado):
// ~1,2 s; 60 s cobre nó carregado sem deixar o pod pendurado para sempre.
const bootTimeout = 60 * time.Second

// guestExec é a prova de vida e o executor de comandos de preparação no guest; trocado nos testes.
type guestExec interface {
	WaitReady(ctx context.Context, socket string) error
	Run(ctx context.Context, socket string, job protocol.Job) (protocol.Result, error)
}

// vmLauncher liga o processo vmm; o krun.Launcher satisfaz, e os testes trocam.
type vmLauncher interface {
	StartVM(s ports.VMSpec) (ports.VM, error)
}

// PodBooter liga VMs de pod com a libkrun, pelo mesmo vmm do rc.
type PodBooter struct {
	Cfg      Config
	launcher vmLauncher
	guest    guestExec
	// copyTree copia uma árvore do host preservando dono, modo e links (cp -a); trocado nos testes.
	copyTree func(src, dst string) error
	openProc func(ctx context.Context, socket string, job protocol.Job) (net.Conn, error)
}

// NewPodBooter monta o booter de verdade.
func NewPodBooter(cfg Config) *PodBooter {
	return &PodBooter{Cfg: cfg, guest: guestproto.Client{}, copyTree: cpTree, openProc: guestproto.OpenProc,
		launcher: &krun.Launcher{Exe: cfg.RC, LibDir: cfg.LibDir, LibPath: cfg.LibPath, WorkDir: filepath.Join(cfg.StateDir, "vmm"), Stderr: os.Stderr}}
}

// runChild roda um filho do shim e devolve o código de saída. O pkg/shim colhe todo filho que morre
// (wait4(-1) no SIGCHLD, para não sobrar zumbi de VM): um cmd.Wait comum chega depois e recebe
// "waitid: no child processes" mesmo com o filho tendo dado certo (medido no 1º pod com container, no
// k3s). Pelo reaper do containerd o código chega pela assinatura de saídas. Trocado nos testes, onde
// ninguém chama o reaper.Reap.
var runChild = func(cmd *exec.Cmd) (int, error) {
	ec, err := reaper.Default.Start(cmd)
	if err != nil {
		return -1, err
	}
	return reaper.Default.Wait(cmd, ec)
}

// cpTree é o cp -a do coreutils, com reflink quando o sistema de arquivos tiver (btrfs, xfs): copiar
// o rootfs é o que evita reexportar o overlay pelo virtio-fs, que quebra o copy-up dentro do guest.
func cpTree(src, dst string) error {
	if err := os.MkdirAll(filepath.Dir(dst), 0o755); err != nil {
		return err
	}
	var out bytes.Buffer
	cmd := exec.Command("cp", "-a", "--reflink=auto", src, dst)
	cmd.Stdout, cmd.Stderr = &out, &out
	code, err := runChild(cmd)
	if err != nil || code != 0 {
		return fmt.Errorf("cp %s: exit %d %v: %s", src, code, err, out.String())
	}
	return nil
}

// podVM é a VM viva de um pod.
type podVM struct {
	b      *PodBooter
	vm     ports.VM
	dir    string
	share  string
	socket string
	mu     sync.Mutex
	copies map[string]HostCopy
}

// Boot liga a VM: disco da imagem read-only, o share do pod por virtio-fs, a rede do pod (se houver
// netns) e o guest em /opt/runner/start.sh. Responde quando o guest está pronto e o share montado.
func (b *PodBooter) Boot(ctx context.Context, p PodSpec) (VM, error) {
	dir := filepath.Join(b.Cfg.StateDir, "pods", p.ID)
	share := filepath.Join(dir, "share")
	if err := os.MkdirAll(share, 0o711); err != nil {
		return nil, err
	}
	socket := filepath.Join(dir, "v.sock")
	spec := ports.VMSpec{CPUs: p.CPUs, MemoryMiB: p.MemoryMiB, RootDisk: b.Cfg.Image, VsockSocket: socket,
		VsockPort: protocol.DefaultPort, Console: filepath.Join(dir, "console.log"), PodNetns: p.Netns,
		Mounts: []ports.VMMount{{Tag: ShareTag, HostPath: share}}, Exec: "/opt/runner/start.sh"}
	vm, err := b.launcher.StartVM(spec)
	if err != nil {
		os.RemoveAll(dir)
		return nil, err
	}
	pv := &podVM{b: b, vm: vm, dir: dir, share: share, socket: socket, copies: map[string]HostCopy{}}
	ready, cancel := context.WithTimeout(ctx, bootTimeout)
	defer cancel()
	if err := b.guest.WaitReady(ready, socket); err != nil {
		pv.Close()
		return nil, fmt.Errorf("pod VM did not become ready: %w (console: %s)", err, filepath.Join(dir, "console.log"))
	}
	// A raiz da imagem é read-only: o /run vira tmpfs antes do ponto de montagem do share.
	setup := fmt.Sprintf("mount -t tmpfs -o mode=755 tmpfs /run && mkdir -p %s && mount -t virtiofs %s %s", GuestShare, ShareTag, GuestShare)
	if p.Hostname != "" {
		setup += " && hostname " + shellQuote(p.Hostname)
	}
	if err := pv.sh(ctx, setup); err != nil {
		pv.Close()
		return nil, err
	}
	return pv, nil
}

// sh roda um script no guest como root e falha com a saída quando o código não é zero.
func (v *podVM) sh(ctx context.Context, script string) error {
	res, err := v.b.guest.Run(ctx, v.socket, protocol.Job{TraceID: "pod", Op: protocol.OpExec, Argv: []string{"/bin/sh", "-c", script}, Timeout: 60})
	if err != nil {
		return err
	}
	if res.Status != "success" || res.ExitCode != 0 {
		return fmt.Errorf("guest script exit %d: %s %v", res.ExitCode, res.Stdout, res.Stderr)
	}
	return nil
}

// PrepareContainer copia o rootfs e as origens das montagens bind para o share e monta o resto no
// guest. A cópia é um retrato: o que o kubelet trocar depois no volume do host (token de
// serviceaccount, configmap) não chega ao container — limite declarado.
func (v *podVM) PrepareContainer(ctx context.Context, fs ContainerFS) (string, error) {
	if err := v.b.copyTree(fs.Rootfs+"/.", filepath.Join(v.share, fs.ID, "rootfs")); err != nil {
		return "", err
	}
	v.mu.Lock()
	copies := CopiesFor(fs.Mounts, v.copies)
	var fresh []HostCopy
	for src, c := range copies {
		if _, ok := v.copies[src]; !ok {
			v.copies[src] = c
			fresh = append(fresh, c)
		}
	}
	v.mu.Unlock()
	for _, c := range fresh {
		if err := v.b.copyTree(c.Source, filepath.Join(v.share, "mounts", fmt.Sprint(c.Index))); err != nil {
			return "", err
		}
	}
	root := path.Join(GuestShare, fs.ID, "rootfs")
	if err := v.sh(ctx, MountScript(root, fs.Mounts, copies, GuestShare)); err != nil {
		return "", err
	}
	return root, nil
}

// RemoveContainer desmonta no guest e apaga a cópia no host.
func (v *podVM) RemoveContainer(ctx context.Context, id string) error {
	err := v.sh(ctx, UnmountScript(path.Join(GuestShare, id, "rootfs")))
	os.RemoveAll(filepath.Join(v.share, id))
	return err
}

// Usage lê o uso do processo vmm no /proc do host; erro (VM já morta) vale zero.
func (v *podVM) Usage() (uint64, uint64) {
	c, m, _ := procUsage("/proc", v.vm.PID())
	return c, m
}

// Close desliga a VM (SIGKILL no vmm) e apaga o diretório do pod.
func (v *podVM) Close() error {
	v.vm.Kill()
	return os.RemoveAll(v.dir)
}

// Start abre o processo no guest e liga o stdio do containerd à conexão.
func (v *podVM) Start(ctx context.Context, root string, p ProcSpec, st Stdio) (Proc, error) {
	job := protocol.Job{TraceID: "proc", Root: root, Argv: p.Args, Env: p.Env, Cwd: p.Cwd, UID: p.UID, GID: p.GID,
		Groups: p.Groups, TTY: p.Terminal, Rows: p.Rows, Cols: p.Cols}
	conn, err := v.b.openProc(ctx, v.socket, job)
	if err != nil {
		return nil, err
	}
	return startGuestProc(conn, st), nil
}

// guestProc é um processo do guest visto pelo shim: a conexão do OpProc.
type guestProc struct {
	conn net.Conn
	wmu  sync.Mutex
	done chan struct{}
	code int
}

// startGuestProc liga o stdio às duas pontas da conexão e devolve o processo.
func startGuestProc(conn net.Conn, st Stdio) *guestProc {
	g := &guestProc{conn: conn, done: make(chan struct{}), code: 255}
	if st.Stdin != nil {
		go func() {
			buf := make([]byte, 32<<10)
			for {
				n, err := st.Stdin.Read(buf)
				if n > 0 && g.send(protocol.Event{Type: protocol.EventInput, Bytes: append([]byte(nil), buf[:n]...)}) != nil {
					return
				}
				if err != nil {
					// EOF do containerd (stdin fechado, `kubectl exec` sem -i terminou): repassa.
					if errors.Is(err, io.EOF) {
						g.send(protocol.Event{Type: protocol.EventEOF})
					}
					return
				}
			}
		}()
	}
	go func() {
		defer close(g.done)
		defer conn.Close()
		for {
			var ev protocol.Event
			if err := protocol.ReadFrame(conn, &ev); err != nil {
				// Conexão caiu sem exit: a VM morreu. 255, como o shim de referência, para o pod
				// aparecer como erro e não como sucesso.
				return
			}
			switch ev.Type {
			case protocol.EventStdout, protocol.EventOutput:
				if st.Stdout != nil {
					st.Stdout.Write(ev.Bytes)
				}
			case protocol.EventStderr:
				if st.Stderr != nil {
					st.Stderr.Write(ev.Bytes)
				}
			case protocol.EventExit:
				g.code = ev.Code
				return
			}
		}
	}()
	return g
}

// send escreve um frame (as escritas vêm de goroutines diferentes: stdin, Kill, Resize).
func (g *guestProc) send(ev protocol.Event) error {
	g.wmu.Lock()
	defer g.wmu.Unlock()
	return protocol.WriteFrame(g.conn, ev)
}

// Signal manda o sinal ao processo no guest.
func (g *guestProc) Signal(sig int) error {
	return g.send(protocol.Event{Type: protocol.EventSignal, Code: sig})
}

// Resize muda o tamanho do TTY.
func (g *guestProc) Resize(rows, cols uint16) error {
	return g.send(protocol.Event{Type: protocol.EventResize, Rows: rows, Cols: cols})
}

// CloseStdin fecha a entrada do processo.
func (g *guestProc) CloseStdin() error { return g.send(protocol.Event{Type: protocol.EventEOF}) }

// Wait espera o exit.
func (g *guestProc) Wait() int {
	<-g.done
	return g.code
}
