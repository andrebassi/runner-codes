package app

import (
	"context"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"time"

	"runner.code/internal/domain"
	"runner.code/internal/ports"
	"runner.code/internal/protocol"
)

// Nomes dos arquivos de runtime no diretório de uma máquina (o store cuida do registro,
// do disco e do pid).
const (
	machineSocket  = "v.sock"
	machineConsole = "console.log"
	machineVMMLog  = "vmm.log"
	machineSpec    = "spec.json"
)

// DefaultStopTimeout é a espera do stop pelo desligamento limpo antes do SIGKILL: 10 s,
// o mesmo padrão. O sync do guest leva milissegundos; o prazo cobre disco lento.
const DefaultStopTimeout = 10 * time.Second

// DiskTemplates entrega o modelo vazio do disco de máquina (oci.Builder implementa).
type DiskTemplates interface {
	DiskTemplate(ctx context.Context, gib int) (string, error)
}

// Machines cuida das máquinas persistentes: registro, ciclo de vida da VM destacada,
// exec, cópia de arquivo e logs. Independente do Pool: máquina não é job.
type Machines struct {
	Store    ports.MachineStore
	Disks    DiskTemplates
	Images   ports.ImageStore
	Catalog  Catalog
	Launcher ports.DetachedLauncher
	Procs    ports.Processes
	Guest    ports.GuestClient
	// ReadyTimeout limita a espera do guest no start; 0 usa o ReadyTimeout do pool.
	ReadyTimeout time.Duration
}

// MachineStatus é a máquina com o estado de agora.
type MachineStatus struct {
	domain.Machine
	State string `json:"state"`
	PID   int    `json:"pid,omitempty"`
}

// Estados de uma máquina.
const (
	StateRunning = "running"
	StateStopped = "stopped"
)

// ExecRequest é um comando numa máquina ligada.
type ExecRequest struct {
	Argv    []string
	Env     []string
	Cwd     string
	Stdin   string
	Timeout time.Duration // 0 = sem prazo
}

// Create valida, confere a stack (imagem construída e GPU, se pedida) e registra a
// máquina com um disco novo. Não liga: o start é separado.
func (m *Machines) Create(ctx context.Context, mc domain.Machine) (domain.Machine, error) {
	mc = mc.WithDefaults()
	if err := mc.Validate(); err != nil {
		return mc, err
	}
	l, ok := m.Catalog.Get(mc.Stack)
	if !ok {
		return mc, domain.ValidationError{Err: domain.ErrUnsupportedLanguage{Name: mc.Stack}}
	}
	if _, ok := m.Images.ImagePath(mc.Stack); !ok {
		return mc, domain.ValidationError{Err: domain.ErrUnsupportedLanguage{Name: mc.Stack}}
	}
	if err := (domain.Job{GPU: mc.GPU}).CheckGPU(l); err != nil {
		return mc, err
	}
	if _, err := m.Store.Load(mc.Name); err == nil {
		return mc, domain.ErrMachineExists(mc.Name)
	}
	tpl, err := m.Disks.DiskTemplate(ctx, mc.DiskGiB)
	if err != nil {
		return mc, fmt.Errorf("disk template: %w", err)
	}
	if mc.CreatedAt.IsZero() {
		mc.CreatedAt = time.Now().UTC().Truncate(time.Second)
	}
	return mc, m.Store.Create(mc, tpl)
}

// Status lê o registro e diz se o vmm registrado está vivo.
func (m *Machines) Status(name string) (MachineStatus, error) {
	mc, err := m.Store.Load(name)
	if err != nil {
		return MachineStatus{}, err
	}
	st := MachineStatus{Machine: mc, State: StateStopped}
	if pid := m.Store.PID(name); m.Procs.Alive(pid) {
		st.State, st.PID = StateRunning, pid
	}
	return st, nil
}

// List devolve todas as máquinas com o estado de cada uma.
func (m *Machines) List() ([]MachineStatus, error) {
	ms, err := m.Store.List()
	if err != nil {
		return nil, err
	}
	out := make([]MachineStatus, 0, len(ms))
	for _, mc := range ms {
		st, err := m.Status(mc.Name)
		if err != nil {
			return nil, err
		}
		out = append(out, st)
	}
	return out, nil
}

// Start liga a VM da máquina destacada do processo atual e espera o guest responder.
// Se o guest não sobe no prazo, a VM é morta e o erro traz o fim do console.
func (m *Machines) Start(ctx context.Context, name string) (MachineStatus, error) {
	st, err := m.Status(name)
	if err != nil {
		return st, err
	}
	if st.State == StateRunning {
		return st, domain.ErrMachineState(name, "is already running")
	}
	image, ok := m.Images.ImagePath(st.Stack)
	if !ok {
		return st, fmt.Errorf("image for %s not built (run `rc image build --stack %s`)", st.Stack, st.Stack)
	}
	l, _ := m.Catalog.Get(st.Stack)
	dir := m.Store.Dir(name)
	sock := filepath.Join(dir, machineSocket)
	// Socket que ficou de uma VM morta faria a libkrun falhar ao criar o novo.
	os.Remove(sock)
	spec := ports.VMSpec{
		CPUs: st.CPUs, MemoryMiB: st.MemoryMiB, RootDisk: image,
		DataDisk:    filepath.Join(dir, "disk.ext4"),
		VsockSocket: sock, VsockPort: protocol.DefaultPort,
		Console: filepath.Join(dir, machineConsole), Exec: "/opt/runner/machine.sh",
	}
	if st.GPU {
		spec.GPU, spec.GPUVRAMMiB = true, l.GPUVRAMMiB
	}
	pid, err := m.Launcher.StartDetached(spec, filepath.Join(dir, machineSpec), filepath.Join(dir, machineVMMLog))
	if err != nil {
		return st, err
	}
	if err := m.Store.SetPID(name, pid); err != nil {
		m.Procs.Kill(pid)
		return st, err
	}
	timeout := m.ReadyTimeout
	if timeout <= 0 {
		timeout = ReadyTimeout
	}
	readyCtx, cancel := context.WithTimeout(ctx, timeout)
	defer cancel()
	if err := m.Guest.WaitReady(readyCtx, sock); err != nil {
		m.Procs.Kill(pid)
		m.Store.SetPID(name, 0)
		return st, fmt.Errorf("machine %s did not become ready: %w; console: %s", name, err,
			tailFile(filepath.Join(dir, machineConsole), 400))
	}
	return m.Status(name)
}

// Stop pede ao guest para gravar o disco e sair; se a VM não terminar no prazo, mata.
// Máquina já parada não é erro: o estado pedido já vale.
func (m *Machines) Stop(ctx context.Context, name string, timeout time.Duration) error {
	st, err := m.Status(name)
	if err != nil {
		return err
	}
	if st.State != StateRunning {
		return m.Store.SetPID(name, 0)
	}
	if timeout < 0 {
		timeout = DefaultStopTimeout
	}
	if timeout > 0 {
		sctx, cancel := context.WithTimeout(ctx, timeout)
		// Erro aqui (guest travado, conexão caiu quando o processo saiu) não importa: o que
		// decide é o processo terminar, conferido abaixo.
		m.Guest.Run(sctx, filepath.Join(m.Store.Dir(name), machineSocket), protocol.Job{TraceID: "stop", Op: protocol.OpShutdown})
		cancel()
		for end := time.Now().Add(timeout); time.Now().Before(end) && m.Procs.Alive(st.PID); {
			time.Sleep(20 * time.Millisecond)
		}
	}
	if m.Procs.Alive(st.PID) {
		m.Procs.Kill(st.PID)
	}
	return m.Store.SetPID(name, 0)
}

// Delete apaga a máquina e o disco dela. Ligada, só com force (que mata sem esperar).
func (m *Machines) Delete(ctx context.Context, name string, force bool) error {
	st, err := m.Status(name)
	if err != nil {
		return err
	}
	if st.State == StateRunning {
		if !force {
			return domain.ErrMachineState(name, "is running; stop it first or use force")
		}
		m.Procs.Kill(st.PID)
	}
	return m.Store.Delete(name)
}

// running devolve o socket do guest de uma máquina ligada, ou INVALID_STATE.
func (m *Machines) running(name string) (string, error) {
	st, err := m.Status(name)
	if err != nil {
		return "", err
	}
	if st.State != StateRunning {
		return "", domain.ErrMachineState(name, "is not running; start it first")
	}
	return filepath.Join(m.Store.Dir(name), machineSocket), nil
}

// Exec roda o argv na máquina. Com onOutput, a saída chega enquanto o programa roda.
func (m *Machines) Exec(ctx context.Context, name string, req ExecRequest, onOutput func(stream, data string)) (domain.Result, error) {
	if len(req.Argv) == 0 || req.Argv[0] == "" {
		return domain.Result{}, domain.ValidationError{Err: domain.MachineError{Code: domain.CodeConfig, Msg: "exec needs a command"}}
	}
	sock, err := m.running(name)
	if err != nil {
		return domain.Result{}, err
	}
	job := protocol.Job{TraceID: "exec-" + name, Op: protocol.OpExec, Argv: req.Argv, Env: req.Env, Cwd: req.Cwd}
	if req.Stdin != "" {
		job.Stdin = req.Stdin
	}
	runCtx := ctx
	if req.Timeout > 0 {
		job.Timeout = int((req.Timeout + time.Second - 1) / time.Second)
		var cancel context.CancelFunc
		runCtx, cancel = context.WithTimeout(ctx, req.Timeout+timeoutGrace)
		defer cancel()
	}
	start := time.Now()
	var res protocol.Result
	if onOutput != nil {
		res, err = m.Guest.RunStream(runCtx, sock, job, onOutput)
	} else {
		res, err = m.Guest.Run(runCtx, sock, job)
	}
	if err != nil {
		return domain.Result{}, fmt.Errorf("exec in machine %s: %w", name, err)
	}
	return fromProtocol(job.TraceID, res, time.Since(start)), nil
}

// CopyIn grava r em guestPath na máquina, em pedaços de 1 MiB. O guest só troca o
// arquivo no último pedaço, então uma cópia interrompida nunca deixa o destino pela metade.
func (m *Machines) CopyIn(ctx context.Context, name, guestPath string, r io.Reader, mode os.FileMode) (int64, error) {
	p, err := domain.GuestPath(guestPath)
	if err != nil {
		return 0, err
	}
	sock, err := m.running(name)
	if err != nil {
		return 0, err
	}
	// Lê um pedaço à frente: só assim se sabe que o pedaço atual é o último (Final).
	cur := make([]byte, protocol.ChunkBytes)
	n, rerr := io.ReadFull(r, cur)
	var off int64
	for {
		if rerr != nil && rerr != io.EOF && rerr != io.ErrUnexpectedEOF {
			return off, rerr
		}
		final := rerr != nil
		var next []byte
		var nn int
		if !final {
			next = make([]byte, protocol.ChunkBytes)
			nn, rerr = io.ReadFull(r, next)
			final = nn == 0 && (rerr == io.EOF || rerr == io.ErrUnexpectedEOF)
		}
		if off+int64(n) > protocol.MaxCopyBytes {
			return off, fmt.Errorf("copy: total_size exceeds maximum of %d bytes", int64(protocol.MaxCopyBytes))
		}
		res, err := m.Guest.Run(ctx, sock, protocol.Job{TraceID: "put", Op: protocol.OpPut, Path: p,
			Offset: off, Data: cur[:n], Final: final, Mode: uint32(mode.Perm())})
		if err != nil {
			return off, fmt.Errorf("copy to %s:%s: %w", name, p, err)
		}
		if res.Status != "success" {
			return off, errors.New(res.Error)
		}
		off += int64(n)
		if final {
			return off, nil
		}
		cur, n = next, nn
	}
}

// CopyOut escreve em w o arquivo guestPath da máquina, pedindo pedaços até o tamanho
// que o guest informa.
func (m *Machines) CopyOut(ctx context.Context, name, guestPath string, w io.Writer) (int64, error) {
	p, err := domain.GuestPath(guestPath)
	if err != nil {
		return 0, err
	}
	sock, err := m.running(name)
	if err != nil {
		return 0, err
	}
	var off int64
	for {
		res, err := m.Guest.Run(ctx, sock, protocol.Job{TraceID: "get", Op: protocol.OpGet, Path: p, Offset: off})
		if err != nil {
			return off, fmt.Errorf("copy from %s:%s: %w", name, p, err)
		}
		if res.Status != "success" {
			return off, errors.New(res.Error)
		}
		if _, err := w.Write(res.Data); err != nil {
			return off, err
		}
		off += int64(len(res.Data))
		// Pedaço vazio antes do fim = arquivo encolheu no meio da cópia; parar evita laço.
		if off >= res.Size || len(res.Data) == 0 {
			return off, nil
		}
	}
}

// Logs devolve as últimas linhas do console da máquina (o guest e o que o machine.sh
// escreve), legível com a máquina parada. tail 0 devolve tudo.
func (m *Machines) Logs(name string, tail int) (string, error) {
	if _, err := m.Store.Load(name); err != nil {
		return "", err
	}
	raw, err := os.ReadFile(filepath.Join(m.Store.Dir(name), machineConsole))
	if errors.Is(err, os.ErrNotExist) {
		return "", nil
	}
	if err != nil {
		return "", err
	}
	if tail <= 0 {
		return string(raw), nil
	}
	lines := strings.SplitAfter(string(raw), "\n")
	if lines[len(lines)-1] == "" {
		lines = lines[:len(lines)-1]
	}
	if len(lines) > tail {
		lines = lines[len(lines)-tail:]
	}
	return strings.Join(lines, ""), nil
}
