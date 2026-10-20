package k8sshim

import (
	"context"
	"encoding/json"
	"fmt"
	"io"
	"os"
	"sync"
	"time"

	eventstypes "github.com/containerd/containerd/api/events"
	taskAPI "github.com/containerd/containerd/api/runtime/task/v3"
	"github.com/containerd/containerd/api/types"
	"github.com/containerd/containerd/api/types/task"
	"github.com/containerd/containerd/v2/core/events"
	"github.com/containerd/containerd/v2/core/runtime"
	"github.com/containerd/containerd/v2/pkg/protobuf"
	"github.com/containerd/errdefs"
	"github.com/containerd/errdefs/pkg/errgrpc"
	specs "github.com/opencontainers/runtime-spec/specs-go"
	"google.golang.org/protobuf/types/known/emptypb"
)

// Publisher manda eventos ao containerd (o shim.Publisher do pkg/shim satisfaz).
type Publisher interface {
	Publish(ctx context.Context, topic string, event events.Event) error
}

// IOOpener abre as FIFOs (ou arquivos) de stdio que o containerd passou. Trocado nos testes.
type IOOpener func(ctx context.Context, stdin, stdout, stderr string, terminal bool) (Stdio, io.Closer, error)

// RootfsMounter monta o rootfs do container (as montagens do CreateTaskRequest) no bundle e devolve o
// diretório e o que desmonta. Trocado nos testes.
type RootfsMounter func(bundle string, mounts []*types.Mount, specRoot string) (string, func() error, error)

// process é um processo do containerd: o init de um container ou um exec.
type process struct {
	id, execID            string
	spec                  ProcSpec
	stdin, stdout, stderr string
	terminal              bool
	pid                   uint32
	status                task.Status
	exitStatus            uint32
	exitedAt              time.Time
	proc                  Proc
	stdio                 io.Closer
	done                  chan struct{}
}

// container é um container do pod (o sandbox também é um, sem processo de verdade).
type container struct {
	id, bundle string
	sandbox    bool
	root       string
	unmount    func() error
	init       *process
	execs      map[string]*process
}

// Service é o serviço de task do containerd (containerd.task.v3.Task) do rc: um por pod.
type Service struct {
	mu         sync.Mutex
	booter     Booter
	openIO     IOOpener
	mount      RootfsMounter
	publisher  Publisher
	shutdown   func()
	vm         VM
	containers map[string]*container
	nextPid    uint32
	// statsOwner é o container que recebe o uso da VM no Stats (o primeiro da app).
	statsOwner string
}

// NewService cria o serviço; shutdown é chamado quando o último container sai (o shim termina).
func NewService(b Booter, pub Publisher, openIO IOOpener, mount RootfsMounter, shutdown func()) *Service {
	return &Service{booter: b, publisher: pub, openIO: openIO, mount: mount, shutdown: shutdown,
		containers: map[string]*container{}, nextPid: firstPid}
}

// firstPid é o primeiro pid que o shim informa ao containerd. Os processos vivem dentro da VM, onde o
// pid do host não existe: o número só precisa ser único dentro do pod e diferente de 0.
const firstPid = 1000

// grpcErr converte o erro para o código que o containerd entende (NotFound, FailedPrecondition...).
func grpcErr(err error) error { return errgrpc.ToGRPC(err) }

// get acha o processo (init quando execID é vazio).
func (s *Service) get(id, execID string) (*container, *process, error) {
	c, ok := s.containers[id]
	if !ok {
		return nil, nil, fmt.Errorf("container %s: %w", id, errdefs.ErrNotFound)
	}
	if execID == "" {
		return c, c.init, nil
	}
	p, ok := c.execs[execID]
	if !ok {
		return nil, nil, fmt.Errorf("exec %s in %s: %w", execID, id, errdefs.ErrNotFound)
	}
	return c, p, nil
}

// publish manda o evento sem travar a chamada que o originou.
func (s *Service) publish(topic string, ev events.Event) {
	if s.publisher != nil {
		go s.publisher.Publish(context.Background(), topic, ev)
	}
}

// newProcess registra um processo com pid novo.
func (s *Service) newProcess(id, execID string, spec ProcSpec, stdin, stdout, stderr string, terminal bool) *process {
	s.nextPid++
	return &process{id: id, execID: execID, spec: spec, stdin: stdin, stdout: stdout, stderr: stderr, terminal: terminal,
		pid: s.nextPid, status: task.Status_CREATED, done: make(chan struct{})}
}

// Create cria o container: o sandbox liga a VM do pod; um container da app tem o rootfs montado,
// copiado e preparado dentro da VM. ctr run sem CRI vira o próprio sandbox e container.
func (s *Service) Create(ctx context.Context, r *taskAPI.CreateTaskRequest) (*taskAPI.CreateTaskResponse, error) {
	b, err := ReadBundle(r.Bundle)
	if err != nil {
		return nil, grpcErr(err)
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	if _, ok := s.containers[r.ID]; ok {
		return nil, grpcErr(fmt.Errorf("container %s: %w", r.ID, errdefs.ErrAlreadyExists))
	}
	c := &container{id: r.ID, bundle: r.Bundle, sandbox: b.Sandbox && b.SandboxID != "", execs: map[string]*process{}}
	if s.vm == nil {
		if !b.Sandbox {
			return nil, grpcErr(fmt.Errorf("container %s arrived before its pod sandbox: %w", r.ID, errdefs.ErrFailedPrecondition))
		}
		vm, err := s.booter.Boot(ctx, PodSpec{ID: r.ID, Netns: b.Netns, Hostname: b.Spec.Hostname, Resources: PodResources(b.Spec)})
		if err != nil {
			return nil, grpcErr(fmt.Errorf("boot pod VM: %w", err))
		}
		s.vm = vm
	}
	if !c.sandbox {
		var root string
		if b.Spec.Root != nil {
			root = b.Spec.Root.Path
		}
		dir, unmount, err := s.mount(r.Bundle, r.Rootfs, root)
		if err != nil {
			return nil, grpcErr(fmt.Errorf("mount rootfs: %w", err))
		}
		c.unmount = unmount
		if c.root, err = s.vm.PrepareContainer(ctx, ContainerFS{ID: r.ID, Rootfs: dir, Mounts: b.Spec.Mounts}); err != nil {
			unmount()
			return nil, grpcErr(fmt.Errorf("prepare container: %w", err))
		}
	}
	c.init = s.newProcess(r.ID, "", ProcFromOCI(b.Spec.Process), r.Stdin, r.Stdout, r.Stderr, r.Terminal)
	s.containers[r.ID] = c
	if !c.sandbox && s.statsOwner == "" {
		s.statsOwner = r.ID
	}
	s.publish(runtime.TaskCreateEventTopic, &eventstypes.TaskCreate{ContainerID: r.ID, Bundle: r.Bundle, Rootfs: r.Rootfs,
		IO: &eventstypes.TaskIO{Stdin: r.Stdin, Stdout: r.Stdout, Stderr: r.Stderr, Terminal: r.Terminal}, Pid: c.init.pid})
	return &taskAPI.CreateTaskResponse{Pid: c.init.pid}, nil
}

// Start liga o processo. O do sandbox não roda nada (a VM já é o pod): fica "rodando" até o Kill.
func (s *Service) Start(ctx context.Context, r *taskAPI.StartRequest) (*taskAPI.StartResponse, error) {
	s.mu.Lock()
	c, p, err := s.get(r.ID, r.ExecID)
	if err != nil {
		s.mu.Unlock()
		return nil, grpcErr(err)
	}
	if p.status != task.Status_CREATED {
		s.mu.Unlock()
		return nil, grpcErr(fmt.Errorf("%s already started: %w", r.ID, errdefs.ErrFailedPrecondition))
	}
	vm := s.vm
	s.mu.Unlock()
	if !c.sandbox {
		stdio, closer, err := s.openIO(ctx, p.stdin, p.stdout, p.stderr, p.terminal)
		if err != nil {
			return nil, grpcErr(fmt.Errorf("open stdio: %w", err))
		}
		proc, err := vm.Start(ctx, c.root, p.spec, stdio)
		if err != nil {
			closer.Close()
			return nil, grpcErr(fmt.Errorf("start process: %w", err))
		}
		p.proc, p.stdio = proc, closer
		go s.reap(p, func() int { return proc.Wait() })
	}
	s.mu.Lock()
	p.status = task.Status_RUNNING
	s.mu.Unlock()
	if r.ExecID != "" {
		s.publish(runtime.TaskExecStartedEventTopic, &eventstypes.TaskExecStarted{ContainerID: r.ID, ExecID: r.ExecID, Pid: p.pid})
	} else {
		s.publish(runtime.TaskStartEventTopic, &eventstypes.TaskStart{ContainerID: r.ID, Pid: p.pid})
	}
	return &taskAPI.StartResponse{Pid: p.pid}, nil
}

// reap espera o processo sair, registra o código e avisa o containerd.
func (s *Service) reap(p *process, wait func() int) {
	code := wait()
	s.exited(p, uint32(code))
}

// exited marca o processo como parado (uma vez só) e publica o TaskExit.
func (s *Service) exited(p *process, code uint32) {
	s.mu.Lock()
	if p.status == task.Status_STOPPED {
		s.mu.Unlock()
		return
	}
	p.status, p.exitStatus, p.exitedAt = task.Status_STOPPED, code, time.Now()
	if p.stdio != nil {
		p.stdio.Close()
	}
	close(p.done)
	s.mu.Unlock()
	s.publish(runtime.TaskExitEventTopic, &eventstypes.TaskExit{ContainerID: p.id, ID: idOf(p), Pid: p.pid,
		ExitStatus: code, ExitedAt: protobuf.ToTimestamp(p.exitedAt)})
}

// idOf é o id do processo no evento: o do exec, ou o do container para o init.
func idOf(p *process) string {
	if p.execID != "" {
		return p.execID
	}
	return p.id
}

// Exec registra um processo novo num container rodando; ele só liga no Start com o ExecID.
func (s *Service) Exec(ctx context.Context, r *taskAPI.ExecProcessRequest) (*emptypb.Empty, error) {
	var ps specs.Process
	if r.Spec == nil || json.Unmarshal(r.Spec.GetValue(), &ps) != nil || len(ps.Args) == 0 {
		return nil, grpcErr(fmt.Errorf("exec %s: invalid process spec: %w", r.ExecID, errdefs.ErrInvalidArgument))
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	c, _, err := s.get(r.ID, "")
	if err != nil {
		return nil, grpcErr(err)
	}
	if c.sandbox {
		return nil, grpcErr(fmt.Errorf("exec in the pod sandbox: %w", errdefs.ErrNotImplemented))
	}
	if _, ok := c.execs[r.ExecID]; ok {
		return nil, grpcErr(fmt.Errorf("exec %s: %w", r.ExecID, errdefs.ErrAlreadyExists))
	}
	c.execs[r.ExecID] = s.newProcess(r.ID, r.ExecID, ProcFromOCI(&ps), r.Stdin, r.Stdout, r.Stderr, r.Terminal)
	s.publish(runtime.TaskExecAddedEventTopic, &eventstypes.TaskExecAdded{ContainerID: r.ID, ExecID: r.ExecID})
	return &emptypb.Empty{}, nil
}

// Kill manda o sinal. No sandbox, encerra o "processo" dele com 128+sinal; com All, o sinal vai também
// aos execs do container.
func (s *Service) Kill(ctx context.Context, r *taskAPI.KillRequest) (*emptypb.Empty, error) {
	s.mu.Lock()
	c, p, err := s.get(r.ID, r.ExecID)
	if err != nil {
		s.mu.Unlock()
		return nil, grpcErr(err)
	}
	targets := []*process{p}
	if r.All && r.ExecID == "" {
		for _, e := range c.execs {
			targets = append(targets, e)
		}
	}
	s.mu.Unlock()
	for _, t := range targets {
		switch {
		case t.status == task.Status_STOPPED:
		case t.proc != nil:
			t.proc.Signal(int(r.Signal))
		default:
			// Sandbox, ou processo criado e nunca ligado: não há o que sinalizar dentro da VM.
			s.exited(t, 128+r.Signal)
		}
	}
	return &emptypb.Empty{}, nil
}

// Wait bloqueia até o processo sair.
func (s *Service) Wait(ctx context.Context, r *taskAPI.WaitRequest) (*taskAPI.WaitResponse, error) {
	s.mu.Lock()
	_, p, err := s.get(r.ID, r.ExecID)
	s.mu.Unlock()
	if err != nil {
		return nil, grpcErr(err)
	}
	select {
	case <-p.done:
	case <-ctx.Done():
		return nil, ctx.Err()
	}
	s.mu.Lock()
	defer s.mu.Unlock()
	return &taskAPI.WaitResponse{ExitStatus: p.exitStatus, ExitedAt: protobuf.ToTimestamp(p.exitedAt)}, nil
}

// Delete apaga o exec, ou o container: desmonta e tira a cópia de dentro da VM. Apagar o sandbox
// desliga a VM.
func (s *Service) Delete(ctx context.Context, r *taskAPI.DeleteRequest) (*taskAPI.DeleteResponse, error) {
	s.mu.Lock()
	c, p, err := s.get(r.ID, r.ExecID)
	if err != nil {
		s.mu.Unlock()
		return nil, grpcErr(err)
	}
	if p.status == task.Status_RUNNING {
		s.mu.Unlock()
		return nil, grpcErr(fmt.Errorf("%s is running: %w", r.ID, errdefs.ErrFailedPrecondition))
	}
	if r.ExecID != "" {
		delete(c.execs, r.ExecID)
	} else {
		delete(s.containers, r.ID)
		if s.statsOwner == r.ID {
			s.statsOwner = ""
		}
	}
	vm := s.vm
	if r.ExecID == "" && c.sandbox {
		s.vm = nil
	}
	exitedAt := p.exitedAt
	if exitedAt.IsZero() {
		exitedAt = time.Now()
	}
	s.mu.Unlock()
	if r.ExecID == "" {
		switch {
		case c.sandbox && vm != nil:
			vm.Close()
		case vm != nil:
			vm.RemoveContainer(ctx, r.ID)
		}
		if c.unmount != nil {
			c.unmount()
		}
		s.publish(runtime.TaskDeleteEventTopic, &eventstypes.TaskDelete{ContainerID: r.ID, Pid: p.pid,
			ExitStatus: p.exitStatus, ExitedAt: protobuf.ToTimestamp(exitedAt), ID: r.ID})
	}
	return &taskAPI.DeleteResponse{Pid: p.pid, ExitStatus: p.exitStatus, ExitedAt: protobuf.ToTimestamp(exitedAt)}, nil
}

// State devolve o estado do processo.
func (s *Service) State(ctx context.Context, r *taskAPI.StateRequest) (*taskAPI.StateResponse, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	c, p, err := s.get(r.ID, r.ExecID)
	if err != nil {
		return nil, grpcErr(err)
	}
	st := &taskAPI.StateResponse{ID: r.ID, Bundle: c.bundle, Pid: p.pid, Status: p.status, Stdin: p.stdin, Stdout: p.stdout,
		Stderr: p.stderr, Terminal: p.terminal, ExitStatus: p.exitStatus, ExecID: r.ExecID}
	if !p.exitedAt.IsZero() {
		st.ExitedAt = protobuf.ToTimestamp(p.exitedAt)
	}
	return st, nil
}

// Pids lista o init e os execs vivos do container.
func (s *Service) Pids(ctx context.Context, r *taskAPI.PidsRequest) (*taskAPI.PidsResponse, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	c, _, err := s.get(r.ID, "")
	if err != nil {
		return nil, grpcErr(err)
	}
	out := []*task.ProcessInfo{{Pid: c.init.pid}}
	for _, e := range c.execs {
		if e.status == task.Status_RUNNING {
			out = append(out, &task.ProcessInfo{Pid: e.pid})
		}
	}
	return &taskAPI.PidsResponse{Processes: out}, nil
}

// ResizePty muda o tamanho do TTY do processo.
func (s *Service) ResizePty(ctx context.Context, r *taskAPI.ResizePtyRequest) (*emptypb.Empty, error) {
	s.mu.Lock()
	_, p, err := s.get(r.ID, r.ExecID)
	s.mu.Unlock()
	if err != nil {
		return nil, grpcErr(err)
	}
	if p.proc != nil {
		p.proc.Resize(uint16(r.Height), uint16(r.Width))
	}
	return &emptypb.Empty{}, nil
}

// CloseIO fecha o stdin do processo.
func (s *Service) CloseIO(ctx context.Context, r *taskAPI.CloseIORequest) (*emptypb.Empty, error) {
	s.mu.Lock()
	_, p, err := s.get(r.ID, r.ExecID)
	s.mu.Unlock()
	if err != nil {
		return nil, grpcErr(err)
	}
	if p.proc != nil && r.Stdin {
		p.proc.CloseStdin()
	}
	return &emptypb.Empty{}, nil
}

// Connect informa o pid do shim e o do processo.
func (s *Service) Connect(ctx context.Context, r *taskAPI.ConnectRequest) (*taskAPI.ConnectResponse, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	resp := &taskAPI.ConnectResponse{ShimPid: uint32(os.Getpid())}
	if c, ok := s.containers[r.ID]; ok {
		resp.TaskPid = c.init.pid
	}
	return resp, nil
}

// Shutdown encerra o shim quando não sobrou container nenhum (o containerd chama a cada delete).
func (s *Service) Shutdown(ctx context.Context, r *taskAPI.ShutdownRequest) (*emptypb.Empty, error) {
	s.mu.Lock()
	left := len(s.containers)
	vm := s.vm
	s.mu.Unlock()
	if left > 0 {
		return &emptypb.Empty{}, nil
	}
	if vm != nil {
		vm.Close()
	}
	if s.shutdown != nil {
		s.shutdown()
	}
	return &emptypb.Empty{}, nil
}

// Stats devolve o uso no formato que o CRI espera. O custo é da VM inteira (não há cgroup por
// container dentro dela), então vai todo para o primeiro container da app do pod e os outros recebem
// zero: o kubectl top do pod soma certo, e nenhum número é contado duas vezes.
func (s *Service) Stats(ctx context.Context, r *taskAPI.StatsRequest) (*taskAPI.StatsResponse, error) {
	s.mu.Lock()
	c, ok := s.containers[r.ID]
	vm, owner := s.vm, s.statsOwner
	s.mu.Unlock()
	if !ok {
		return nil, grpcErr(fmt.Errorf("container %s: %w", r.ID, errdefs.ErrNotFound))
	}
	var cpu, mem uint64
	if vm != nil && !c.sandbox && owner == r.ID {
		cpu, mem = vm.Usage()
	}
	a, err := metricsAny(cpu, mem)
	if err != nil {
		return nil, grpcErr(err)
	}
	return &taskAPI.StatsResponse{Stats: a}, nil
}

// Update aceita e não faz nada: o tamanho da VM é fixado no boot.
func (s *Service) Update(ctx context.Context, r *taskAPI.UpdateTaskRequest) (*emptypb.Empty, error) {
	return &emptypb.Empty{}, nil
}

// Pause, Resume e Checkpoint não existem para VM de pod.
func (s *Service) Pause(context.Context, *taskAPI.PauseRequest) (*emptypb.Empty, error) {
	return nil, grpcErr(errdefs.ErrNotImplemented)
}

// Resume: veja Pause.
func (s *Service) Resume(context.Context, *taskAPI.ResumeRequest) (*emptypb.Empty, error) {
	return nil, grpcErr(errdefs.ErrNotImplemented)
}

// Checkpoint: veja Pause.
func (s *Service) Checkpoint(context.Context, *taskAPI.CheckpointTaskRequest) (*emptypb.Empty, error) {
	return nil, grpcErr(errdefs.ErrNotImplemented)
}
