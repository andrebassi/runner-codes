package app

import (
	"context"
	"crypto/sha256"
	"encoding/hex"
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
// o mesmo padrão do smolvm. O sync do guest leva milissegundos; o prazo cobre disco lento.
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
	// BuildImage constrói o ext4 de uma imagem OCI qualquer (`image` no runner.code); nil
	// recusa máquina por imagem. Recebe a linguagem sintética com o nome do ext4.
	BuildImage func(ctx context.Context, l domain.Language) error
}

// initMarker marca, no disco da máquina, que o init já rodou: fica na raiz persistente, então
// sobrevive ao stop/start e some com o delete (que leva o disco).
const initMarker = "/var/lib/runner/init-done"

// stopGrace é a folga do host sobre o stop_timeout do workload: depois do sinal e da espera,
// o guest ainda faz sync antes de sair.
const stopGrace = 5 * time.Second

// ImageStackName dá o nome do ext4 de uma máquina criada por imagem: "img-" e 12 hex do
// sha256 da referência. Mesmo ref, mesmo nome: a segunda máquina reaproveita o build.
func ImageStackName(ref string) string {
	sum := sha256.Sum256([]byte(ref))
	return "img-" + hex.EncodeToString(sum[:6])
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
// máquina com um disco novo. Não liga: o start é separado, como no smolvm.
func (m *Machines) Create(ctx context.Context, mc domain.Machine) (domain.Machine, error) {
	if mc.Image != "" {
		if mc.Stack != "" {
			return mc, domain.ValidationError{Err: domain.MachineError{Code: domain.CodeConfig, Msg: "set stack or image, not both"}}
		}
		mc.Stack = ImageStackName(mc.Image)
	}
	mc = mc.WithDefaults()
	if err := mc.Validate(); err != nil {
		return mc, err
	}
	if _, err := m.Store.Load(mc.Name); err == nil {
		return mc, domain.ErrMachineExists(mc.Name)
	}
	if mc.Image != "" {
		// Imagem qualquer: a GPU é responsabilidade de quem escolheu a imagem (precisa do
		// Mesa com Venus); o ext4 sai do mesmo build das stacks, sem warmup.
		if m.BuildImage == nil {
			return mc, domain.ValidationError{Err: domain.MachineError{Code: domain.CodeConfig, Msg: "machines from an image are not available here"}}
		}
		if err := m.BuildImage(ctx, domain.Language{Name: mc.Stack, Image: mc.Image, CPUs: mc.CPUs, MemoryMiB: mc.MemoryMiB}); err != nil {
			return mc, fmt.Errorf("build image %s: %w", mc.Image, err)
		}
	} else {
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
		if st.GPUVRAMMiB > 0 {
			spec.GPUVRAMMiB = st.GPUVRAMMiB
		}
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
	if err := m.provision(ctx, st.Machine, sock); err != nil {
		// A VM fica ligada: dá para entrar com `rc machine shell` e ver o que falhou. O init
		// não marcado roda de novo no próximo start.
		return st, err
	}
	return m.Status(name)
}

// provision roda o init (uma vez por disco) e liga o workload, os dois com o env e o
// workdir da máquina.
func (m *Machines) provision(ctx context.Context, mc domain.Machine, sock string) error {
	// O workdir é o cwd do init e do workload: no disco novo ele ainda não existe, e o exec
	// com cwd ausente falharia antes do primeiro comando.
	if mc.Workdir != "" && (len(mc.Init) > 0 || len(mc.Workload) > 0) {
		res, err := m.Guest.Run(ctx, sock, protocol.Job{TraceID: "workdir", Op: protocol.OpExec, Argv: []string{"mkdir", "-p", mc.Workdir}})
		if err != nil {
			return fmt.Errorf("machine %s: create workdir: %w", mc.Name, err)
		}
		if res.ExitCode != 0 {
			return fmt.Errorf("machine %s: create workdir %s: %s", mc.Name, mc.Workdir, asString(res.Stderr))
		}
	}
	if len(mc.Init) > 0 {
		res, err := m.Guest.Run(ctx, sock, protocol.Job{TraceID: "init-check", Op: protocol.OpExec, Argv: []string{"test", "-f", initMarker}})
		if err != nil {
			return fmt.Errorf("machine %s: check init: %w", mc.Name, err)
		}
		if res.ExitCode != 0 {
			for i, cmd := range mc.Init {
				res, err := m.Guest.Run(ctx, sock, protocol.Job{TraceID: fmt.Sprintf("init-%d", i), Op: protocol.OpExec,
					Argv: []string{"sh", "-c", cmd}, Env: mc.Env, Cwd: mc.Workdir})
				if err != nil {
					return fmt.Errorf("machine %s: init %q: %w", mc.Name, cmd, err)
				}
				if res.ExitCode != 0 {
					return fmt.Errorf("machine %s: init %q exited with %d: %s", mc.Name, cmd, res.ExitCode, asString(res.Stderr))
				}
			}
			mark := "mkdir -p " + filepath.Dir(initMarker) + " && touch " + initMarker
			if _, err := m.Guest.Run(ctx, sock, protocol.Job{TraceID: "init-done", Op: protocol.OpExec, Argv: []string{"sh", "-c", mark}}); err != nil {
				return fmt.Errorf("machine %s: mark init done: %w", mc.Name, err)
			}
		}
	}
	if len(mc.Workload) > 0 {
		res, err := m.Guest.Run(ctx, sock, protocol.Job{TraceID: "workload", Op: protocol.OpWorkload,
			Argv: mc.Workload, Env: mc.Env, Cwd: mc.Workdir, StopOnExit: mc.StopOnExit})
		if err != nil {
			return fmt.Errorf("machine %s: start workload: %w", mc.Name, err)
		}
		if res.Status != "success" {
			return fmt.Errorf("machine %s: start workload: %s", mc.Name, res.Error)
		}
	}
	return nil
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
	// Negativo = o stop_timeout da máquina (10 s se ela não declara); explícito vale só
	// para este stop, como o `smolvm machine stop --timeout`.
	if timeout < 0 {
		timeout = DefaultStopTimeout
		if st.StopTimeout > 0 {
			timeout = time.Duration(st.StopTimeout) * time.Second
		}
	}
	if timeout > 0 {
		sig, _ := domain.SignalNumber(st.StopSignal) // validado no create
		wait := timeout
		if len(st.Workload) > 0 {
			// Com workload, o prazo é dele; o sync do guest vem depois e ganha a folga. Sem
			// workload o prazo inteiro já é para o sync, e um --timeout curto continua curto.
			wait += stopGrace
		}
		sctx, cancel := context.WithTimeout(ctx, wait)
		// Erro aqui (guest travado, conexão caiu quando o processo saiu) não importa: o que
		// decide é o processo terminar, conferido abaixo.
		m.Guest.Run(sctx, filepath.Join(m.Store.Dir(name), machineSocket), protocol.Job{TraceID: "stop", Op: protocol.OpShutdown,
			Signal: sig, Timeout: int(timeout / time.Second)})
		cancel()
		for end := time.Now().Add(wait); time.Now().Before(end) && m.Procs.Alive(st.PID); {
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
	mc, _ := m.Store.Load(name)
	job := protocol.Job{TraceID: "exec-" + name, Op: protocol.OpExec, Argv: req.Argv, Env: append(append([]string{}, mc.Env...), req.Env...), Cwd: req.Cwd}
	if job.Cwd == "" {
		job.Cwd = mc.Workdir
	}
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

// ShellRequest é uma sessão de terminal numa máquina ligada. Argv vazio abre o shell
// padrão da imagem (bash se houver, senão sh).
type ShellRequest struct {
	Argv []string
	Env  []string
	Cwd  string
	Size ports.Winsize
}

// Shell liga um terminal (PTY no guest) a in/out até o programa sair, repassando cada
// resize; devolve o código de saída do programa.
func (m *Machines) Shell(ctx context.Context, name string, req ShellRequest, in io.Reader, out io.Writer, resize <-chan ports.Winsize) (int, error) {
	sock, err := m.running(name)
	if err != nil {
		return 0, err
	}
	mc, _ := m.Store.Load(name)
	job := protocol.Job{TraceID: "shell-" + name, Argv: req.Argv, Env: append(append([]string{}, mc.Env...), req.Env...), Cwd: req.Cwd,
		Rows: req.Size.Rows, Cols: req.Size.Cols}
	if job.Cwd == "" {
		job.Cwd = mc.Workdir
	}
	code, err := m.Guest.Interactive(ctx, sock, job, in, out, resize)
	if err != nil {
		return 0, fmt.Errorf("shell in machine %s: %w", name, err)
	}
	return code, nil
}

// Fontes do console aceitas pelo Logs: tudo, ou só o workload do runner.code.
const (
	LogSourceAll      = "all"
	LogSourceWorkload = "workload"
)

// workloadPrefix é a marca que o guest põe em cada linha do workload (internal/guest).
const workloadPrefix = "[workload] "

// Logs devolve as últimas linhas do console da máquina (o guest e o que o machine.sh
// escreve), legível com a máquina parada. tail 0 devolve tudo. source "workload" deixa só
// as linhas do workload, sem a marca; o tail conta depois do filtro.
func (m *Machines) Logs(name string, tail int, source string) (string, error) {
	if source != "" && source != LogSourceAll && source != LogSourceWorkload {
		return "", domain.ValidationError{Err: domain.MachineError{Code: domain.CodeConfig,
			Msg: fmt.Sprintf("unknown log source %q (use all or workload)", source)}}
	}
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
	lines := strings.SplitAfter(string(raw), "\n")
	if lines[len(lines)-1] == "" {
		lines = lines[:len(lines)-1]
	}
	if source == LogSourceWorkload {
		kept := lines[:0]
		for _, l := range lines {
			if strings.HasPrefix(l, workloadPrefix) {
				// O console da VM é um terminal: a quebra chega como \r\n. Quem pede só o
				// workload quer as linhas como o programa as escreveu.
				l = strings.TrimPrefix(l, workloadPrefix)
				if strings.HasSuffix(l, "\r\n") {
					l = strings.TrimSuffix(l, "\r\n") + "\n"
				}
				kept = append(kept, l)
			}
		}
		lines = kept
	}
	if tail <= 0 {
		return strings.Join(lines, ""), nil
	}
	if len(lines) > tail {
		lines = lines[len(lines)-tail:]
	}
	return strings.Join(lines, ""), nil
}
