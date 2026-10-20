package k8sshim

import (
	"context"
	"encoding/json"
	"errors"
	"io"
	"os"
	"path/filepath"
	"sync"
	"testing"
	"time"

	eventstypes "github.com/containerd/containerd/api/events"
	taskAPI "github.com/containerd/containerd/api/runtime/task/v3"
	"github.com/containerd/containerd/api/types"
	"github.com/containerd/containerd/api/types/task"
	"github.com/containerd/containerd/v2/core/events"
	"github.com/containerd/errdefs"
	"github.com/containerd/errdefs/pkg/errgrpc"
	specs "github.com/opencontainers/runtime-spec/specs-go"
	"google.golang.org/protobuf/types/known/anypb"

	cg2 "github.com/containerd/cgroups/v3/cgroup2/stats"
	"github.com/containerd/typeurl/v2"
)

// fakeProc é um processo da VM controlado pelo teste: sai com o código que o canal receber.
type fakeProc struct {
	exit             chan int
	signals          []int
	resized, closedI bool
	mu               sync.Mutex
}

// Signal registra o sinal e, se for de morte, encerra o processo com 128+sinal.
func (p *fakeProc) Signal(sig int) error {
	p.mu.Lock()
	p.signals = append(p.signals, sig)
	p.mu.Unlock()
	if sig == 9 || sig == 15 {
		p.exit <- 128 + sig
	}
	return nil
}

// Resize registra a chamada.
func (p *fakeProc) Resize(uint16, uint16) error { p.resized = true; return nil }

// CloseStdin registra a chamada.
func (p *fakeProc) CloseStdin() error { p.closedI = true; return nil }

// Wait espera o código do canal.
func (p *fakeProc) Wait() int { return <-p.exit }

// fakeVM registra o que o serviço pediu à VM e falha onde o teste mandar.
type fakeVM struct {
	prepared, removed []string
	procs             []*fakeProc
	closed            bool
	failPrepare       bool
	failStart         bool
}

// PrepareContainer devolve o root do container dentro da VM.
func (v *fakeVM) PrepareContainer(_ context.Context, fs ContainerFS) (string, error) {
	if v.failPrepare {
		return "", errors.New("prepare boom")
	}
	v.prepared = append(v.prepared, fs.ID)
	return "/run/rc-pod/" + fs.ID + "/rootfs", nil
}

// RemoveContainer registra a remoção.
func (v *fakeVM) RemoveContainer(_ context.Context, id string) error {
	v.removed = append(v.removed, id)
	return nil
}

// Start cria um processo falso.
func (v *fakeVM) Start(context.Context, string, ProcSpec, Stdio) (Proc, error) {
	if v.failStart {
		return nil, errors.New("start boom")
	}
	p := &fakeProc{exit: make(chan int, 1)}
	v.procs = append(v.procs, p)
	return p, nil
}

// Usage devolve números fixos para o Stats conferir quem recebe.
func (v *fakeVM) Usage() (uint64, uint64) { return 11, 22 }

// Close registra o desligamento.
func (v *fakeVM) Close() error { v.closed = true; return nil }

// fakeBooter devolve a VM falsa (ou erro) e guarda o PodSpec pedido.
type fakeBooter struct {
	vm   *fakeVM
	spec PodSpec
	fail bool
}

// Boot liga a VM falsa.
func (b *fakeBooter) Boot(_ context.Context, p PodSpec) (VM, error) {
	if b.fail {
		return nil, errors.New("boot boom")
	}
	b.spec = p
	return b.vm, nil
}

// pubLog guarda os tópicos publicados.
type pubLog struct {
	mu     sync.Mutex
	topics []string
}

// Publish registra o tópico.
func (p *pubLog) Publish(_ context.Context, topic string, _ events.Event) error {
	p.mu.Lock()
	p.topics = append(p.topics, topic)
	p.mu.Unlock()
	return nil
}

// has espera o tópico aparecer (o publish é assíncrono).
func (p *pubLog) has(topic string) bool {
	for i := 0; i < 100; i++ {
		p.mu.Lock()
		for _, t := range p.topics {
			if t == topic {
				p.mu.Unlock()
				return true
			}
		}
		p.mu.Unlock()
		time.Sleep(5 * time.Millisecond)
	}
	return false
}

// nopCloser conta o fechamento do stdio.
type nopCloser struct{ closed *bool }

// Close marca o fechamento.
func (c nopCloser) Close() error { *c.closed = true; return nil }

// harness monta um serviço com as peças falsas.
type harness struct {
	s         *Service
	b         *fakeBooter
	pub       *pubLog
	ioClosed  bool
	ioFail    bool
	mountFail bool
	unmounted int
	shutdown  bool
}

// newHarness cria o serviço de teste.
func newHarness() *harness {
	h := &harness{b: &fakeBooter{vm: &fakeVM{}}, pub: &pubLog{}}
	openIO := func(context.Context, string, string, string, bool) (Stdio, io.Closer, error) {
		if h.ioFail {
			return Stdio{}, nil, errors.New("io boom")
		}
		return Stdio{}, nopCloser{&h.ioClosed}, nil
	}
	mount := func(bundle string, _ []*types.Mount, _ string) (string, func() error, error) {
		if h.mountFail {
			return "", nil, errors.New("mount boom")
		}
		return filepath.Join(bundle, "rootfs"), func() error { h.unmounted++; return nil }, nil
	}
	h.s = NewService(h.b, h.pub, openIO, mount, func() { h.shutdown = true })
	return h
}

// bundle grava o config.json de um container do pod p1 (ou do sandbox).
func bundle(t *testing.T, kind string) string {
	t.Helper()
	dir := t.TempDir()
	ann := map[string]string{annContainerType: kind, annSandboxID: "p1", annSandboxNetns: "/run/netns/p1", annCPUs: "3"}
	b, _ := json.Marshal(specs.Spec{Process: &specs.Process{Args: []string{"sh"}}, Hostname: "web-0", Root: &specs.Root{Path: "rootfs"}, Annotations: ann})
	os.WriteFile(filepath.Join(dir, "config.json"), b, 0o644)
	return dir
}

// isCode confere o código gRPC que o containerd vai ver.
func isCode(err error, want error) bool { return errors.Is(errgrpc.ToNative(err), want) }

// TestPodLifecycle percorre o ciclo de um pod: sandbox liga a VM com o netns, hostname e tamanho
// pedidos; container prepara, liga, recebe exec, sinais, resize e CloseIO; sai; é apagado; o sandbox
// apaga a VM; o Shutdown sem container encerra o shim. Os eventos saem nos tópicos certos.
func TestPodLifecycle(t *testing.T) {
	h := newHarness()
	ctx := context.Background()
	s := h.s
	if _, err := s.Create(ctx, &taskAPI.CreateTaskRequest{ID: "sb", Bundle: bundle(t, "sandbox")}); err != nil {
		t.Fatal(err)
	}
	if h.b.spec.Netns != "/run/netns/p1" || h.b.spec.Hostname != "web-0" || h.b.spec.CPUs != 3 {
		t.Fatalf("pod %+v", h.b.spec)
	}
	if _, err := s.Start(ctx, &taskAPI.StartRequest{ID: "sb"}); err != nil {
		t.Fatal(err)
	}
	cr, err := s.Create(ctx, &taskAPI.CreateTaskRequest{ID: "c1", Bundle: bundle(t, "container"), Stdout: "/fifo/out"})
	if err != nil || cr.Pid == 0 {
		t.Fatal(err)
	}
	if _, err := s.Start(ctx, &taskAPI.StartRequest{ID: "c1"}); err != nil {
		t.Fatal(err)
	}
	st, _ := s.State(ctx, &taskAPI.StateRequest{ID: "c1"})
	if st.Status != task.Status_RUNNING || st.Stdout != "/fifo/out" || st.ExitedAt != nil {
		t.Fatalf("state %+v", st)
	}
	procSpec, _ := json.Marshal(specs.Process{Args: []string{"id"}, Terminal: true})
	if _, err := s.Exec(ctx, &taskAPI.ExecProcessRequest{ID: "c1", ExecID: "e1", Spec: &anypb.Any{Value: procSpec}, Terminal: true}); err != nil {
		t.Fatal(err)
	}
	if _, err := s.Start(ctx, &taskAPI.StartRequest{ID: "c1", ExecID: "e1"}); err != nil {
		t.Fatal(err)
	}
	pids, _ := s.Pids(ctx, &taskAPI.PidsRequest{ID: "c1"})
	if len(pids.Processes) != 2 {
		t.Fatalf("pids %+v", pids.Processes)
	}
	s.ResizePty(ctx, &taskAPI.ResizePtyRequest{ID: "c1", ExecID: "e1", Width: 80, Height: 24})
	s.CloseIO(ctx, &taskAPI.CloseIORequest{ID: "c1", ExecID: "e1", Stdin: true})
	vm := h.b.vm
	if !vm.procs[1].resized || !vm.procs[1].closedI {
		t.Fatal("resize/closeio não chegaram ao exec")
	}
	// O exec sai sozinho com 0; o Wait dele devolve o código.
	vm.procs[1].exit <- 0
	if w, err := s.Wait(ctx, &taskAPI.WaitRequest{ID: "c1", ExecID: "e1"}); err != nil || w.ExitStatus != 0 {
		t.Fatalf("wait exec %v %v", w, err)
	}
	if _, err := s.Delete(ctx, &taskAPI.DeleteRequest{ID: "c1", ExecID: "e1"}); err != nil {
		t.Fatal(err)
	}
	// Stats: o uso da VM vai para o primeiro container da app; o sandbox recebe zero.
	usage := func(id string) (uint64, uint64) {
		r, err := s.Stats(ctx, &taskAPI.StatsRequest{ID: id})
		if err != nil {
			t.Fatal(err)
		}
		v, _ := typeurl.UnmarshalAny(r.Stats)
		m := v.(*cg2.Metrics)
		return m.CPU.UsageUsec, m.Memory.Usage
	}
	if c, m := usage("c1"); c != 11 || m != 22 {
		t.Fatalf("o primeiro container não recebeu o uso da VM: %d %d", c, m)
	}
	if c, m := usage("sb"); c != 0 || m != 0 {
		t.Fatalf("o sandbox recebeu uso: %d %d", c, m)
	}
	// Kill com All alcança o container (e os execs que restarem).
	if _, err := s.Kill(ctx, &taskAPI.KillRequest{ID: "c1", Signal: 15, All: true}); err != nil {
		t.Fatal(err)
	}
	w, _ := s.Wait(ctx, &taskAPI.WaitRequest{ID: "c1"})
	if w.ExitStatus != 143 {
		t.Fatalf("exit %d", w.ExitStatus)
	}
	st, _ = s.State(ctx, &taskAPI.StateRequest{ID: "c1"})
	if st.Status != task.Status_STOPPED || st.ExitedAt == nil {
		t.Fatalf("state depois %+v", st)
	}
	// Kill de quem já parou não faz nada.
	s.Kill(ctx, &taskAPI.KillRequest{ID: "c1", Signal: 9})
	if !h.ioClosed {
		t.Fatal("stdio não fechou no exit")
	}
	if _, err := s.Delete(ctx, &taskAPI.DeleteRequest{ID: "c1"}); err != nil {
		t.Fatal(err)
	}
	if len(vm.removed) != 1 || h.unmounted != 1 {
		t.Fatalf("delete não limpou: removed=%v unmounted=%d", vm.removed, h.unmounted)
	}
	// Shutdown com o sandbox ainda vivo não encerra.
	s.Shutdown(ctx, &taskAPI.ShutdownRequest{})
	if h.shutdown {
		t.Fatal("shutdown com container vivo")
	}
	s.Kill(ctx, &taskAPI.KillRequest{ID: "sb", Signal: 9})
	if w, _ := s.Wait(ctx, &taskAPI.WaitRequest{ID: "sb"}); w.ExitStatus != 137 {
		t.Fatalf("sandbox exit %d", w.ExitStatus)
	}
	conn, _ := s.Connect(ctx, &taskAPI.ConnectRequest{ID: "sb"})
	if conn.ShimPid == 0 || conn.TaskPid == 0 {
		t.Fatalf("connect %+v", conn)
	}
	if _, err := s.Delete(ctx, &taskAPI.DeleteRequest{ID: "sb"}); err != nil {
		t.Fatal(err)
	}
	if !vm.closed {
		t.Fatal("apagar o sandbox não desligou a VM")
	}
	s.Shutdown(ctx, &taskAPI.ShutdownRequest{})
	if !h.shutdown {
		t.Fatal("shutdown sem container não encerrou")
	}
	for _, topic := range []string{"/tasks/create", "/tasks/start", "/tasks/exec-added", "/tasks/exec-started", "/tasks/exit", "/tasks/delete"} {
		if !h.pub.has(topic) {
			t.Fatalf("evento %s não saiu", topic)
		}
	}
}

// TestCtrRunIsOwnSandbox: sem anotação do CRI (ctr run), o container liga a VM e roda nela.
func TestCtrRunIsOwnSandbox(t *testing.T) {
	h := newHarness()
	dir := t.TempDir()
	b, _ := json.Marshal(specs.Spec{Process: &specs.Process{Args: []string{"sh"}}})
	os.WriteFile(filepath.Join(dir, "config.json"), b, 0o644)
	ctx := context.Background()
	if _, err := h.s.Create(ctx, &taskAPI.CreateTaskRequest{ID: "solo", Bundle: dir}); err != nil {
		t.Fatal(err)
	}
	if len(h.b.vm.prepared) != 1 {
		t.Fatal("ctr run não preparou o container")
	}
	if _, err := h.s.Start(ctx, &taskAPI.StartRequest{ID: "solo"}); err != nil {
		t.Fatal(err)
	}
	h.b.vm.procs[0].exit <- 4
	if w, _ := h.s.Wait(ctx, &taskAPI.WaitRequest{ID: "solo"}); w.ExitStatus != 4 {
		t.Fatalf("exit %d", w.ExitStatus)
	}
	h.s.Delete(ctx, &taskAPI.DeleteRequest{ID: "solo"})
	if len(h.b.vm.removed) != 1 {
		t.Fatal("ctr run não limpou")
	}
}

// TestServiceErrors: cada recusa sai com o código que o containerd espera.
func TestServiceErrors(t *testing.T) {
	ctx := context.Background()
	h := newHarness()
	s := h.s
	if _, err := s.Create(ctx, &taskAPI.CreateTaskRequest{ID: "x", Bundle: t.TempDir()}); err == nil {
		t.Fatal("bundle sem config.json")
	}
	if _, err := s.Create(ctx, &taskAPI.CreateTaskRequest{ID: "c", Bundle: bundle(t, "container")}); !isCode(err, errdefs.ErrFailedPrecondition) {
		t.Fatalf("container antes do sandbox: %v", err)
	}
	h.b.fail = true
	if _, err := s.Create(ctx, &taskAPI.CreateTaskRequest{ID: "sb", Bundle: bundle(t, "sandbox")}); err == nil {
		t.Fatal("boot falhou e passou")
	}
	h.b.fail = false
	s.Create(ctx, &taskAPI.CreateTaskRequest{ID: "sb", Bundle: bundle(t, "sandbox")})
	if _, err := s.Create(ctx, &taskAPI.CreateTaskRequest{ID: "sb", Bundle: bundle(t, "sandbox")}); !isCode(err, errdefs.ErrAlreadyExists) {
		t.Fatalf("duplicado: %v", err)
	}
	h.mountFail = true
	if _, err := s.Create(ctx, &taskAPI.CreateTaskRequest{ID: "c", Bundle: bundle(t, "container")}); err == nil {
		t.Fatal("mount falhou e passou")
	}
	h.mountFail, h.b.vm.failPrepare = false, true
	if _, err := s.Create(ctx, &taskAPI.CreateTaskRequest{ID: "c", Bundle: bundle(t, "container")}); err == nil || h.unmounted != 1 {
		t.Fatalf("prepare falhou: %v unmounted=%d", err, h.unmounted)
	}
	h.b.vm.failPrepare = false
	s.Create(ctx, &taskAPI.CreateTaskRequest{ID: "c", Bundle: bundle(t, "container")})
	h.ioFail = true
	if _, err := s.Start(ctx, &taskAPI.StartRequest{ID: "c"}); err == nil {
		t.Fatal("stdio falhou e passou")
	}
	h.ioFail, h.b.vm.failStart = false, true
	if _, err := s.Start(ctx, &taskAPI.StartRequest{ID: "c"}); err == nil || !h.ioClosed {
		t.Fatalf("start falhou: %v io fechado=%v", err, h.ioClosed)
	}
	h.b.vm.failStart = false
	s.Start(ctx, &taskAPI.StartRequest{ID: "c"})
	if _, err := s.Start(ctx, &taskAPI.StartRequest{ID: "c"}); !isCode(err, errdefs.ErrFailedPrecondition) {
		t.Fatalf("start duas vezes: %v", err)
	}
	if _, err := s.Delete(ctx, &taskAPI.DeleteRequest{ID: "c"}); !isCode(err, errdefs.ErrFailedPrecondition) {
		t.Fatalf("delete rodando: %v", err)
	}
	for _, bad := range []*taskAPI.ExecProcessRequest{{ID: "c", ExecID: "e"}, {ID: "c", ExecID: "e", Spec: &anypb.Any{Value: []byte("{")}}} {
		if _, err := s.Exec(ctx, bad); !isCode(err, errdefs.ErrInvalidArgument) {
			t.Fatalf("exec inválido: %v", err)
		}
	}
	good, _ := json.Marshal(specs.Process{Args: []string{"id"}})
	if _, err := s.Exec(ctx, &taskAPI.ExecProcessRequest{ID: "nope", ExecID: "e", Spec: &anypb.Any{Value: good}}); !isCode(err, errdefs.ErrNotFound) {
		t.Fatalf("exec sem container: %v", err)
	}
	if _, err := s.Exec(ctx, &taskAPI.ExecProcessRequest{ID: "sb", ExecID: "e", Spec: &anypb.Any{Value: good}}); !isCode(err, errdefs.ErrNotImplemented) {
		t.Fatalf("exec no sandbox: %v", err)
	}
	s.Exec(ctx, &taskAPI.ExecProcessRequest{ID: "c", ExecID: "e", Spec: &anypb.Any{Value: good}})
	if _, err := s.Exec(ctx, &taskAPI.ExecProcessRequest{ID: "c", ExecID: "e", Spec: &anypb.Any{Value: good}}); !isCode(err, errdefs.ErrAlreadyExists) {
		t.Fatalf("exec duplicado: %v", err)
	}
	// Exec criado e nunca ligado: o Kill o encerra direto.
	s.Kill(ctx, &taskAPI.KillRequest{ID: "c", ExecID: "e", Signal: 9})
	if w, _ := s.Wait(ctx, &taskAPI.WaitRequest{ID: "c", ExecID: "e"}); w.ExitStatus != 137 {
		t.Fatal("exec não ligado não encerrou no kill")
	}
	notFound := []error{}
	_, e1 := s.Start(ctx, &taskAPI.StartRequest{ID: "c", ExecID: "missing"})
	_, e2 := s.Kill(ctx, &taskAPI.KillRequest{ID: "nope"})
	_, e3 := s.Wait(ctx, &taskAPI.WaitRequest{ID: "nope"})
	_, e4 := s.Delete(ctx, &taskAPI.DeleteRequest{ID: "nope"})
	_, e5 := s.State(ctx, &taskAPI.StateRequest{ID: "nope"})
	_, e6 := s.Pids(ctx, &taskAPI.PidsRequest{ID: "nope"})
	_, e7 := s.ResizePty(ctx, &taskAPI.ResizePtyRequest{ID: "nope"})
	_, e8 := s.CloseIO(ctx, &taskAPI.CloseIORequest{ID: "nope"})
	_, e9 := s.Stats(ctx, &taskAPI.StatsRequest{ID: "nope"})
	notFound = append(notFound, e1, e2, e3, e4, e5, e6, e7, e8, e9)
	for i, err := range notFound {
		if !isCode(err, errdefs.ErrNotFound) {
			t.Fatalf("chamada %d: %v", i+1, err)
		}
	}
	// Wait cancelado devolve o erro do ctx.
	cctx, cancel := context.WithCancel(ctx)
	cancel()
	if _, err := s.Wait(cctx, &taskAPI.WaitRequest{ID: "c"}); err == nil {
		t.Fatal("wait cancelado")
	}
	if _, err := s.Pause(ctx, nil); !isCode(err, errdefs.ErrNotImplemented) {
		t.Fatal("pause")
	}
	if _, err := s.Resume(ctx, nil); !isCode(err, errdefs.ErrNotImplemented) {
		t.Fatal("resume")
	}
	if _, err := s.Checkpoint(ctx, nil); !isCode(err, errdefs.ErrNotImplemented) {
		t.Fatal("checkpoint")
	}
	if _, err := s.Update(ctx, nil); err != nil {
		t.Fatal("update")
	}
	if c, _ := s.Connect(ctx, &taskAPI.ConnectRequest{ID: "nope"}); c.TaskPid != 0 {
		t.Fatal("connect sem container")
	}
}

// TestPublishNil: sem publisher (teste de unidade de outro pacote), publicar não quebra.
func TestPublishNil(t *testing.T) {
	(&Service{}).publish("/tasks/x", &eventstypes.TaskStart{})
}
