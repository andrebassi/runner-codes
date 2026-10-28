package k8sshim

import (
	"bytes"
	"context"
	"errors"
	"net"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"sync"
	"testing"
	"time"

	specs "github.com/opencontainers/runtime-spec/specs-go"

	"runner.codes/internal/guest"
	"runner.codes/internal/ports"
	"runner.codes/internal/protocol"
)

// vmProc é o processo vmm falso: só registra o Kill.
type vmProc struct{ killed bool }

// PID de um processo que não existe: o Usage lê zero.
func (p *vmProc) PID() int { return 1 << 30 }

// Done nunca fecha.
func (p *vmProc) Done() <-chan struct{} { return make(chan struct{}) }

// Wait não é usado.
func (p *vmProc) Wait() int { return 0 }

// Kill registra.
func (p *vmProc) Kill() { p.killed = true }

// fakeLauncher guarda a spec e devolve o vmm falso, ou erro.
type fakeLauncher struct {
	spec ports.VMSpec
	vm   *vmProc
	fail bool
}

// StartVM registra a spec.
func (l *fakeLauncher) StartVM(s ports.VMSpec) (ports.VM, error) {
	if l.fail {
		return nil, errors.New("launch boom")
	}
	l.spec, l.vm = s, &vmProc{}
	return l.vm, nil
}

// fakeGuest guarda os scripts e responde com o código pedido.
type fakeGuest struct {
	mu       sync.Mutex
	scripts  []string
	notReady bool
	code     int
	runErr   bool
}

// WaitReady falha quando o teste manda.
func (g *fakeGuest) WaitReady(context.Context, string) error {
	if g.notReady {
		return errors.New("never ready")
	}
	return nil
}

// Run registra o script do OpExec.
func (g *fakeGuest) Run(_ context.Context, _ string, job protocol.Job) (protocol.Result, error) {
	g.mu.Lock()
	defer g.mu.Unlock()
	if g.runErr {
		return protocol.Result{}, errors.New("conn boom")
	}
	g.scripts = append(g.scripts, job.Argv[2])
	return protocol.Result{Status: "success", ExitCode: g.code}, nil
}

// testBooter monta o booter com tudo falso, menos o guest dos processos (real, por pipe).
func testBooter(t *testing.T) (*PodBooter, *fakeLauncher, *fakeGuest, *[]string) {
	t.Helper()
	l, g := &fakeLauncher{}, &fakeGuest{}
	copied := &[]string{}
	b := &PodBooter{Cfg: Config{StateDir: t.TempDir(), Image: "/img.ext4"}, launcher: l, guest: g,
		copyTree: func(src, dst string) error { *copied = append(*copied, src+"->"+dst); return nil },
		openProc: func(_ context.Context, _ string, job protocol.Job) (net.Conn, error) {
			c, srv := net.Pipe()
			go guest.NewServer(guest.DefaultServerConfig()).ServeConn(srv)
			job.Op = protocol.OpProc
			return c, protocol.WriteFrame(c, job)
		}}
	return b, l, g, copied
}

// TestBoot: a spec da VM leva a imagem, o share por virtio-fs, o netns do pod e o tamanho; o guest
// monta o /run, o share e o hostname.
func TestBoot(t *testing.T) {
	b, l, g, _ := testBooter(t)
	v, err := b.Boot(context.Background(), PodSpec{ID: "p1", Netns: "/run/netns/p1", Hostname: "web-0", Resources: Resources{CPUs: 2, MemoryMiB: 512}})
	if err != nil {
		t.Fatal(err)
	}
	s := l.spec
	if s.RootDisk != "/img.ext4" || s.PodNetns != "/run/netns/p1" || s.CPUs != 2 || s.MemoryMiB != 512 || len(s.Mounts) != 1 || s.Mounts[0].Tag != ShareTag {
		t.Fatalf("spec %+v", s)
	}
	if !strings.Contains(g.scripts[0], "mount -t tmpfs -o mode=755 tmpfs /run") || !strings.Contains(g.scripts[0], "hostname 'web-0'") {
		t.Fatalf("setup %q", g.scripts[0])
	}
	if c, m := v.Usage(); c != 0 || m != 0 {
		t.Fatal("vmm morto vale zero")
	}
	v.Close()
	if !l.vm.killed {
		t.Fatal("close não matou o vmm")
	}
	if _, err := os.Stat(filepath.Join(b.Cfg.StateDir, "pods", "p1")); err == nil {
		t.Fatal("close não apagou o diretório do pod")
	}
}

// TestBootFailures: falha no launch, no boot (guest não responde) e no setup derrubam a VM e limpam.
func TestBootFailures(t *testing.T) {
	b, l, g, _ := testBooter(t)
	l.fail = true
	if _, err := b.Boot(context.Background(), PodSpec{ID: "a", Resources: Resources{CPUs: 1, MemoryMiB: 128}}); err == nil {
		t.Fatal("launch")
	}
	l.fail, g.notReady = false, true
	if _, err := b.Boot(context.Background(), PodSpec{ID: "b", Resources: Resources{CPUs: 1, MemoryMiB: 128}}); err == nil || !l.vm.killed {
		t.Fatalf("ready: %v", err)
	}
	g.notReady, g.code = false, 1
	if _, err := b.Boot(context.Background(), PodSpec{ID: "c", Resources: Resources{CPUs: 1, MemoryMiB: 128}}); err == nil || !l.vm.killed {
		t.Fatalf("setup: %v", err)
	}
	g.code, g.runErr = 0, true
	if _, err := b.Boot(context.Background(), PodSpec{ID: "d", Resources: Resources{CPUs: 1, MemoryMiB: 128}}); err == nil {
		t.Fatal("conexão do setup")
	}
}

// TestPrepareAndRemove: rootfs copiado para o share, cada origem de bind copiada uma vez só no pod
// (dois containers com o mesmo emptyDir), o script de montagem enviado e a remoção desmontando.
func TestPrepareAndRemove(t *testing.T) {
	b, _, g, copied := testBooter(t)
	v, _ := b.Boot(context.Background(), PodSpec{ID: "p", Resources: Resources{CPUs: 1, MemoryMiB: 128}})
	mounts := []specs.Mount{{Destination: "/data", Type: "bind", Source: "/kubelet/empty"}}
	root, err := v.PrepareContainer(context.Background(), ContainerFS{ID: "a", Rootfs: "/bundle/a/rootfs", Mounts: mounts})
	if err != nil || root != "/run/rc-pod/a/rootfs" {
		t.Fatal(root, err)
	}
	v.PrepareContainer(context.Background(), ContainerFS{ID: "b", Rootfs: "/bundle/b/rootfs", Mounts: mounts})
	n := 0
	for _, c := range *copied {
		if strings.HasPrefix(c, "/kubelet/empty->") {
			n++
		}
	}
	if n != 1 || len(*copied) != 3 {
		t.Fatalf("cópias %v", *copied)
	}
	if !strings.Contains(g.scripts[1], "mount --bind '/run/rc-pod/mounts/0' '/run/rc-pod/a/rootfs/data'") {
		t.Fatalf("script %q", g.scripts[1])
	}
	if err := v.RemoveContainer(context.Background(), "a"); err != nil || !strings.Contains(g.scripts[3], "umount -l") {
		t.Fatalf("remove %v %v", err, g.scripts)
	}
}

// TestPrepareFailures: falha na cópia do rootfs, na cópia de um volume e no script derrubam o prepare.
func TestPrepareFailures(t *testing.T) {
	b, _, g, _ := testBooter(t)
	v, _ := b.Boot(context.Background(), PodSpec{ID: "p", Resources: Resources{CPUs: 1, MemoryMiB: 128}})
	b.copyTree = func(src, _ string) error { return errors.New("cp boom") }
	if _, err := v.PrepareContainer(context.Background(), ContainerFS{ID: "a", Rootfs: "/r"}); err == nil {
		t.Fatal("cópia do rootfs")
	}
	b.copyTree = func(src, _ string) error {
		if src == "/vol" {
			return errors.New("cp boom")
		}
		return nil
	}
	if _, err := v.PrepareContainer(context.Background(), ContainerFS{ID: "a", Rootfs: "/r", Mounts: []specs.Mount{{Type: "bind", Source: "/vol"}}}); err == nil {
		t.Fatal("cópia do volume")
	}
	b.copyTree = func(string, string) error { return nil }
	g.code = 1
	if _, err := v.PrepareContainer(context.Background(), ContainerFS{ID: "c", Rootfs: "/r"}); err == nil {
		t.Fatal("script de montagem")
	}
}

// TestStartGuestProc: o processo roda no guest de verdade (OpProc por pipe): stdin chega e fecha,
// stdout e stderr separados, código de saída volta; e um sinal termina o processo com 128+sinal.
func TestStartGuestProc(t *testing.T) {
	b, _, _, _ := testBooter(t)
	v, _ := b.Boot(context.Background(), PodSpec{ID: "p", Resources: Resources{CPUs: 1, MemoryMiB: 128}})
	var out, errOut syncBuffer
	p, err := v.Start(context.Background(), "", ProcSpec{Args: []string{"sh", "-c", "cat; echo e >&2; exit 4"}, Env: []string{"PATH=/usr/bin:/bin"}},
		Stdio{Stdin: strings.NewReader("in\n"), Stdout: &out, Stderr: &errOut})
	if err != nil {
		t.Fatal(err)
	}
	if code := p.Wait(); code != 4 || out.String() != "in\n" || errOut.String() != "e\n" {
		t.Fatalf("code=%d out=%q err=%q", code, out.String(), errOut.String())
	}
	p2, _ := v.Start(context.Background(), "", ProcSpec{Args: []string{"sleep", "30"}, Env: []string{"PATH=/usr/bin:/bin"}}, Stdio{})
	time.Sleep(200 * time.Millisecond)
	p2.Resize(30, 100)
	p2.CloseStdin()
	p2.Signal(15)
	if code := p2.Wait(); code != 143 {
		t.Fatalf("code=%d", code)
	}
	b.openProc = func(context.Context, string, protocol.Job) (net.Conn, error) { return nil, errors.New("dial boom") }
	if _, err := v.Start(context.Background(), "", ProcSpec{Args: []string{"true"}}, Stdio{}); err == nil {
		t.Fatal("dial")
	}
}

// TestGuestProcConnDrop: a conexão cai sem exit (a VM morreu): o código é 255, nunca sucesso.
func TestGuestProcConnDrop(t *testing.T) {
	c, srv := net.Pipe()
	p := startGuestProc(c, Stdio{})
	srv.Close()
	if code := p.Wait(); code != 255 {
		t.Fatalf("code=%d", code)
	}
}

// syncBuffer é um bytes.Buffer seguro entre goroutines.
type syncBuffer struct {
	mu sync.Mutex
	b  bytes.Buffer
}

// Write grava com trava.
func (s *syncBuffer) Write(p []byte) (int, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.b.Write(p)
}

// String lê com trava.
func (s *syncBuffer) String() string { s.mu.Lock(); defer s.mu.Unlock(); return s.b.String() }

// TestCpTree: copia preservando a árvore; origem inexistente falha com a saída do cp.
func TestCpTree(t *testing.T) {
	if runtime.GOOS != "linux" {
		// O shim só roda em pod Linux, e o cpTree usa o cp do GNU (--reflink=auto); o cp do BSD do macOS recusa a
		// opção. No Mac o teste não prova nada sobre o alvo — vale no Linux (CI e workers).
		t.Skip("cpTree runs the GNU cp of the Linux pod")
	}
	orig := runChild
	runChild = func(cmd *exec.Cmd) (int, error) {
		err := cmd.Run()
		if ee, ok := err.(*exec.ExitError); ok {
			return ee.ExitCode(), nil
		}
		return 0, err
	}
	defer func() { runChild = orig }()
	src := t.TempDir()
	os.WriteFile(filepath.Join(src, "f"), []byte("x"), 0o640)
	dst := filepath.Join(t.TempDir(), "a", "b")
	if err := cpTree(src+"/.", dst); err != nil {
		t.Fatal(err)
	}
	if b, _ := os.ReadFile(filepath.Join(dst, "f")); string(b) != "x" {
		t.Fatal("cópia")
	}
	if err := cpTree("/does/not/exist", dst+"2"); err == nil {
		t.Fatal("origem inexistente")
	}
}

// TestNewPodBooter: o booter de verdade aponta o launcher para o rc e o diretório de estado.
func TestNewPodBooter(t *testing.T) {
	b := NewPodBooter(Config{RC: "/opt/rc/bin/rc", StateDir: "/var/lib/rc-shim"})
	if b.launcher == nil || b.guest == nil || b.copyTree == nil || b.openProc == nil {
		t.Fatal("peça faltando")
	}
}
