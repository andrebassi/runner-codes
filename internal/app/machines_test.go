package app

import (
	"bytes"
	"context"
	"crypto/rand"
	"errors"
	"fmt"
	"io"
	"net"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	"runner.codes/internal/domain"
	"runner.codes/internal/ports"
	"runner.codes/internal/protocol"
)

// memStore é um MachineStore em memória, com diretório de verdade para os arquivos.
type memStore struct {
	root string
	mu   sync.Mutex
	ms   map[string]domain.Machine
	pids map[string]int
	tpls []string
}

// newMemStore cria o store falso num diretório temporário.
func newMemStore(t *testing.T) *memStore {
	return &memStore{root: t.TempDir(), ms: map[string]domain.Machine{}, pids: map[string]int{}}
}

// Create registra a máquina e o modelo usado.
func (s *memStore) Create(m domain.Machine, tpl string) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	if _, ok := s.ms[m.Name]; ok {
		return domain.ErrMachineExists(m.Name)
	}
	s.ms[m.Name], s.tpls = m, append(s.tpls, tpl)
	return os.MkdirAll(s.Dir(m.Name), 0o700)
}

// Load devolve a máquina ou NOT_FOUND.
func (s *memStore) Load(n string) (domain.Machine, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	m, ok := s.ms[n]
	if !ok {
		return m, domain.ErrMachineNotFound(n)
	}
	return m, nil
}

// Save regrava o registro de uma máquina existente.
func (s *memStore) Save(m domain.Machine) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	if _, ok := s.ms[m.Name]; !ok {
		return domain.ErrMachineNotFound(m.Name)
	}
	s.ms[m.Name] = m
	return nil
}

// List devolve as máquinas (ordem não importa nos testes que usam).
func (s *memStore) List() ([]domain.Machine, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	var out []domain.Machine
	for _, m := range s.ms {
		out = append(out, m)
	}
	return out, nil
}

// Delete apaga o registro.
func (s *memStore) Delete(n string) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	delete(s.ms, n)
	return nil
}

// Dir é o diretório da máquina dentro do temporário.
func (s *memStore) Dir(n string) string { return filepath.Join(s.root, n) }

// PID devolve o pid registrado.
func (s *memStore) PID(n string) int { s.mu.Lock(); defer s.mu.Unlock(); return s.pids[n] }

// SetPID grava o pid.
func (s *memStore) SetPID(n string, pid int) error {
	s.mu.Lock()
	defer s.mu.Unlock()
	s.pids[n] = pid
	return nil
}

// procs simula processos: vivo é quem está no mapa; Kill tira.
type procs struct {
	mu      sync.Mutex
	alive   map[int]bool
	killed  []int
	paused  []int
	resumed []int
}

// Alive confere o mapa.
func (p *procs) Alive(pid int) bool { p.mu.Lock(); defer p.mu.Unlock(); return p.alive[pid] }

// Pause anota; pid morto é erro, como o kill de verdade.
func (p *procs) Pause(pid int) error {
	p.mu.Lock()
	defer p.mu.Unlock()
	if !p.alive[pid] {
		return errors.New("no such process")
	}
	p.paused = append(p.paused, pid)
	return nil
}

// Resume anota.
func (p *procs) Resume(pid int) error {
	p.mu.Lock()
	defer p.mu.Unlock()
	if !p.alive[pid] {
		return errors.New("no such process")
	}
	p.resumed = append(p.resumed, pid)
	return nil
}

// Kill anota e mata.
func (p *procs) Kill(pid int) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.killed = append(p.killed, pid)
	delete(p.alive, pid)
}

// detached simula o lançamento destacado: o pid nasce vivo.
type detached struct {
	procs *procs
	specs []ports.VMSpec
	next  int
	fail  error
	// dieLog faz o vmm "morrer" logo ao nascer, deixando esta linha no log dele.
	dieLog string
}

// StartDetached registra a spec e devolve um pid vivo.
func (d *detached) StartDetached(s ports.VMSpec, specPath, logPath string) (int, error) {
	if d.fail != nil {
		return 0, d.fail
	}
	d.next++
	pid := 1000 + d.next
	d.specs = append(d.specs, s)
	if d.dieLog != "" {
		os.WriteFile(logPath, []byte("boot\n"+d.dieLog+"\n"), 0o600)
		return pid, nil
	}
	d.procs.mu.Lock()
	d.procs.alive[pid] = true
	d.procs.mu.Unlock()
	return pid, nil
}

// disks devolve um modelo fictício por tamanho.
type disks struct{ fail error }

// DiskTemplate devolve o caminho do modelo ou a falha configurada.
func (d disks) DiskTemplate(_ context.Context, gib int) (string, error) {
	if d.fail != nil {
		return "", d.fail
	}
	return filepath.Join("/tpl", strings.Repeat("g", gib)), nil
}

// fileGuest é um guest falso com sistema de arquivos em memória: exec ecoa o argv,
// put/get guardam e devolvem bytes, shutdown "desliga" a VM.
type fileGuest struct {
	// hang faz o WaitReady esperar o contexto acabar.
	hang        bool
	mu          sync.Mutex
	files       map[string][]byte
	partial     map[string][]byte
	shutdown    func()
	jobs        []protocol.Job
	readyErr    error
	execErr     error
	workloadErr string
	// dialed guarda as portas pedidas ao DialPort; dialErr simula porta fechada no guest.
	dialed  []uint16
	dialErr error
}

// Run atende as operações das máquinas.
func (g *fileGuest) Run(_ context.Context, _ string, j protocol.Job) (protocol.Result, error) {
	g.mu.Lock()
	defer g.mu.Unlock()
	g.jobs = append(g.jobs, j)
	ok := protocol.Result{Status: "success"}
	switch j.Op {
	case protocol.OpExec:
		if g.execErr != nil {
			return protocol.Result{}, g.execErr
		}
		// "exit7" sai com 7; "slow" espera o contexto (Ctrl+C); "late" estoura o timeout.
		if len(j.Argv) > 0 && j.Argv[0] == "exit7" {
			return protocol.Result{Status: "success", ExitCode: 7}, nil
		}
		if len(j.Argv) > 0 && j.Argv[0] == "late" {
			return protocol.Result{Status: "success", Exception: "Timeout"}, nil
		}
		// O marcador do init é um arquivo de verdade no "disco" falso.
		if len(j.Argv) == 3 && j.Argv[0] == "test" && j.Argv[1] == "-f" {
			if _, found := g.files[j.Argv[2]]; found {
				return ok, nil
			}
			return protocol.Result{Status: "success", ExitCode: 1}, nil
		}
		if len(j.Argv) == 3 && j.Argv[0] == "sh" && strings.Contains(j.Argv[2], "touch "+initMarker) {
			g.files[initMarker] = nil
			return ok, nil
		}
		if len(j.Argv) == 7 && j.Argv[3] == "mount" && j.Argv[4] == "/busy" {
			return protocol.Result{Status: "success", ExitCode: 32, Stderr: "mount: resource busy"}, nil
		}
		if len(j.Argv) == 3 && j.Argv[0] == "mkdir" && j.Argv[2] == "/denied" {
			return protocol.Result{Status: "success", ExitCode: 1, Stderr: "permission denied"}, nil
		}
		// Volume staged: o sync de /broken e o stage de /badstage falham, como um disco cheio.
		if len(j.Argv) >= 6 && j.Argv[3] == "sync" && j.Argv[4] == "/workspace/.disk/broken" {
			return protocol.Result{Status: "success", ExitCode: 1, Stderr: "sync broke"}, nil
		}
		if len(j.Argv) >= 8 && j.Argv[3] == "stage" && j.Argv[6] == "/badstage" {
			return protocol.Result{Status: "success", ExitCode: 1, Stderr: "stage broke"}, nil
		}
		// /proc/net/tcp de mentira: 0.0.0.0:8000 (1F40) em LISTEN, para o ListeningPorts.
		if len(j.Argv) == 3 && j.Argv[0] == "sh" && strings.Contains(j.Argv[2], "/proc/net/tcp") {
			ok.Stdout = "  sl  local_address rem_address st\n   0: 00000000:1F40 00000000:0000 0A 0 0 0 0 0 0 1\n"
			return ok, nil
		}
		if len(j.Argv) == 3 && j.Argv[0] == "sh" && strings.HasPrefix(j.Argv[2], "fail") {
			return protocol.Result{Status: "success", ExitCode: 2, Stderr: "init broke"}, nil
		}
		ok.Stdout = strings.Join(j.Argv, " ") + "\n"
		ok.ExitCode = 0
		if j.Argv[0] == "false" {
			ok.ExitCode = 1
		}
		return ok, nil
	case protocol.OpPut:
		if j.Path == "/denied" {
			return protocol.Result{Status: "failed", Error: "put: permission denied"}, nil
		}
		if j.Offset == 0 {
			g.partial[j.Path] = nil
		}
		g.partial[j.Path] = append(g.partial[j.Path], j.Data...)
		if j.Final {
			g.files[j.Path] = g.partial[j.Path]
			delete(g.partial, j.Path)
		}
		return ok, nil
	case protocol.OpGet:
		data, found := g.files[j.Path]
		if !found {
			return protocol.Result{Status: "failed", Error: "get: no such file"}, nil
		}
		end := min(int(j.Offset)+protocol.ChunkBytes, len(data))
		ok.Data, ok.Size = data[j.Offset:end], int64(len(data))
		return ok, nil
	case protocol.OpWorkload:
		if g.workloadErr != "" {
			return protocol.Result{Status: "failed", Error: g.workloadErr}, nil
		}
		return ok, nil
	case protocol.OpMountS3:
		// "denied" é o bucket que o S3 recusa (o guest responde com o motivo).
		if j.S3.Bucket == "denied" {
			return protocol.Result{Status: "failed", Error: "mount-s3: s3://denied: s3: 403 AccessDenied"}, nil
		}
		return ok, nil
	case protocol.OpSetEnv, protocol.OpSSHAgent:
		return ok, nil
	case protocol.OpShutdown:
		if g.shutdown != nil {
			g.shutdown()
		}
		return ok, nil
	}
	return protocol.Result{}, errors.New("unexpected op " + j.Op)
}

// RunStream entrega o stdout em dois pedaços antes do resultado.
func (g *fileGuest) RunStream(ctx context.Context, s string, j protocol.Job, out func(string, string)) (protocol.Result, error) {
	out(protocol.EventStdout, "part1 ")
	out(protocol.EventStdout, "part2\n")
	if len(j.Argv) > 0 && j.Argv[0] == "slow" {
		out(protocol.EventStderr, "waiting\n")
		<-ctx.Done()
		return protocol.Result{}, ctx.Err()
	}
	return g.Run(ctx, s, j)
}

// Interactive guarda o job e ecoa a entrada na saída, saindo com 6; shellErr simula a
// conexão que cai.
func (g *fileGuest) Interactive(_ context.Context, _ string, j protocol.Job, in io.Reader, out io.Writer, resize <-chan ports.Winsize) (int, error) {
	g.mu.Lock()
	g.jobs = append(g.jobs, j)
	g.mu.Unlock()
	if g.execErr != nil {
		return 0, g.execErr
	}
	if resize != nil {
		ws := <-resize
		fmt.Fprintf(out, "size=%dx%d;", ws.Rows, ws.Cols)
	}
	io.Copy(out, in)
	return 6, nil
}

// DialPort devolve uma ponta de net.Pipe que responde "porta N" e fecha; dialErr simula a porta fechada no guest.
func (g *fileGuest) DialPort(_ context.Context, socket string, port uint16) (net.Conn, error) {
	g.mu.Lock()
	g.dialed = append(g.dialed, port)
	g.mu.Unlock()
	if g.dialErr != nil {
		return nil, g.dialErr
	}
	a, b := net.Pipe()
	go func() { fmt.Fprintf(b, "porta %d", port); b.Close() }()
	return a, nil
}

// WaitReady responde conforme readyErr; com hang, espera o contexto acabar, como um guest
// que nunca responde.
func (g *fileGuest) WaitReady(ctx context.Context, _ string) error {
	if g.hang {
		<-ctx.Done()
		return ctx.Err()
	}
	return g.readyErr
}

// machinesFixture monta o serviço com dublês; python tem imagem, llama tem GPU.
func machinesFixture(t *testing.T) (*Machines, *memStore, *procs, *detached, *fileGuest) {
	st, ps := newMemStore(t), &procs{alive: map[int]bool{}}
	d := &detached{procs: ps}
	g := &fileGuest{files: map[string][]byte{}, partial: map[string][]byte{}}
	m := &Machines{
		Store: st, Disks: disks{}, Procs: ps, Launcher: d, Guest: g, ReadyTimeout: time.Second,
		Images: images{"python": "/img/python.ext4", "llama": "/img/llama.ext4"},
		Catalog: catalog{
			"python": {Name: "python", CPUs: 1, MemoryMiB: 512},
			"llama":  {Name: "llama", CPUs: 4, MemoryMiB: 4096, GPU: true, GPUVRAMMiB: 2048},
			"ruby":   {Name: "ruby", CPUs: 1, MemoryMiB: 512},
		},
	}
	return m, st, ps, d, g
}

// errCode devolve o código estável do erro ("" se não for de máquina).
func errCode(err error) string {
	var me domain.MachineError
	if errors.As(err, &me) {
		return me.Code
	}
	return ""
}

// TestMachineCreate: padrões aplicados, disco do tamanho pedido, e cada recusa: nome
// inválido, stack sem imagem, GPU em stack sem GPU, nome repetido, falha do modelo.
func TestMachineCreate(t *testing.T) {
	m, st, _, _, _ := machinesFixture(t)
	mc, err := m.Create(context.Background(), domain.Machine{Name: "dev", Stack: "python", DiskGiB: 3})
	if err != nil || mc.CPUs != domain.DefaultMachineCPUs || mc.CreatedAt.IsZero() || st.tpls[0] != "/tpl/ggg" {
		t.Fatalf("create: %+v %v %v", mc, err, st.tpls)
	}
	if _, err := m.Create(context.Background(), domain.Machine{Name: "dev", Stack: "python"}); errCode(err) != domain.CodeConflict {
		t.Fatalf("duplicate: %v", err)
	}
	var ve domain.ValidationError
	for name, bad := range map[string]domain.Machine{
		"bad name":       {Name: "Bad Name", Stack: "python"},
		"no image":       {Name: "r", Stack: "ruby"},
		"not in catalog": {Name: "c", Stack: "cobol"},
		"gpu on cpu":     {Name: "g", Stack: "python", GPU: true},
	} {
		if _, err := m.Create(context.Background(), bad); !errors.As(err, &ve) {
			t.Fatalf("%s: expected a validation error, got %v", name, err)
		}
	}
	if _, err := m.Create(context.Background(), domain.Machine{Name: "gpu", Stack: "llama", GPU: true}); err != nil {
		t.Fatalf("GPU machine on a GPU stack: %v", err)
	}
	m.Disks = disks{fail: errors.New("no build VM")}
	if _, err := m.Create(context.Background(), domain.Machine{Name: "x", Stack: "python"}); err == nil {
		t.Fatal("disk template failure must fail")
	}
}

// TestMachineLifecycle: start destacado com a spec certa (disco gravável, machine.sh,
// socket no diretório da máquina), exec, stop limpo (shutdown e o processo some sem
// SIGKILL), start de novo e delete.
func TestMachineLifecycle(t *testing.T) {
	m, st, ps, d, g := machinesFixture(t)
	ctx := context.Background()
	if _, err := m.Create(ctx, domain.Machine{Name: "dev", Stack: "llama", GPU: true}); err != nil {
		t.Fatal(err)
	}
	if s, _ := m.Status("dev"); s.State != StateStopped {
		t.Fatalf("new machine must be stopped: %+v", s)
	}
	s, err := m.Start(ctx, "dev")
	if err != nil || s.State != StateRunning || s.PID != 1001 {
		t.Fatalf("start: %+v %v", s, err)
	}
	spec := d.specs[0]
	if spec.RootDisk != "/img/llama.ext4" || spec.DataDisk != filepath.Join(st.Dir("dev"), "disk.ext4") ||
		spec.Exec != "/opt/runner/machine.sh" || spec.VsockSocket != filepath.Join(st.Dir("dev"), "v.sock") ||
		!spec.GPU || spec.GPUVRAMMiB != 2048 {
		t.Fatalf("machine spec: %+v", spec)
	}
	if _, err := m.Start(ctx, "dev"); errCode(err) != domain.CodeInvalidState {
		t.Fatalf("double start: %v", err)
	}
	res, err := m.Exec(ctx, "dev", ExecRequest{Argv: []string{"uname", "-a"}, Env: []string{"A=1"}, Cwd: "/workspace", Stdin: "in", Timeout: 1500 * time.Millisecond}, nil)
	if err != nil || res.Stdout != "uname -a\n" {
		t.Fatalf("exec: %+v %v", res, err)
	}
	last := g.jobs[len(g.jobs)-1]
	if last.Op != protocol.OpExec || last.Cwd != "/workspace" || last.Env[0] != "A=1" || last.Stdin != "in" || last.Timeout != 2 {
		t.Fatalf("exec job: %+v", last)
	}
	var streamed strings.Builder
	if _, err := m.Exec(ctx, "dev", ExecRequest{Argv: []string{"ls"}}, func(_, d string) { streamed.WriteString(d) }); err != nil || streamed.String() != "part1 part2\n" {
		t.Fatalf("streamed exec: %q %v", streamed.String(), err)
	}
	if ms, _ := m.List(); len(ms) != 1 || ms[0].State != StateRunning {
		t.Fatalf("list: %+v", ms)
	}

	g.shutdown = func() { ps.mu.Lock(); delete(ps.alive, 1001); ps.mu.Unlock() }
	if err := m.Stop(ctx, "dev", time.Second); err != nil {
		t.Fatal(err)
	}
	if len(ps.killed) != 0 || st.PID("dev") != 0 {
		t.Fatalf("clean stop must not SIGKILL: killed=%v pid=%d", ps.killed, st.PID("dev"))
	}
	if err := m.Stop(ctx, "dev", time.Second); err != nil {
		t.Fatal("stopping a stopped machine is not an error")
	}
	if _, err := m.Exec(ctx, "dev", ExecRequest{Argv: []string{"ls"}}, nil); errCode(err) != domain.CodeInvalidState {
		t.Fatalf("exec on a stopped machine: %v", err)
	}
	if _, err := m.Start(ctx, "dev"); err != nil {
		t.Fatalf("start again: %v", err)
	}
	if err := m.Delete(ctx, "dev", false); errCode(err) != domain.CodeInvalidState {
		t.Fatalf("delete running without force: %v", err)
	}
	if err := m.Delete(ctx, "dev", true); err != nil || len(ps.killed) != 1 {
		t.Fatalf("forced delete: %v killed=%v", err, ps.killed)
	}
	if _, err := m.Status("dev"); errCode(err) != domain.CodeNotFound {
		t.Fatalf("deleted machine: %v", err)
	}
}

// TestMachineStopHung: guest que não desliga é morto no prazo; timeout 0 mata na hora.
func TestMachineStopHung(t *testing.T) {
	m, _, ps, _, _ := machinesFixture(t)
	ctx := context.Background()
	m.Create(ctx, domain.Machine{Name: "a", Stack: "python"})
	m.Start(ctx, "a")
	start := time.Now()
	if err := m.Stop(ctx, "a", 100*time.Millisecond); err != nil || len(ps.killed) != 1 || time.Since(start) > time.Second {
		t.Fatalf("hung stop: %v killed=%v after %v", err, ps.killed, time.Since(start))
	}
	m.Start(ctx, "a")
	if err := m.Stop(ctx, "a", 0); err != nil || len(ps.killed) != 2 {
		t.Fatalf("immediate stop: %v %v", err, ps.killed)
	}
	m.Start(ctx, "a")
	m.Stop(ctx, "a", -1) // negativo = padrão de 10 s; o guest falso não desliga, então mata
	if len(ps.killed) != 3 {
		t.Fatal("default timeout must still end in a kill")
	}
}

// TestMachineStartFailures: VM que não fica pronta é morta e o pid limpo; falha do
// lançamento; stack sem imagem no start; máquina inexistente.
func TestMachineStartFailures(t *testing.T) {
	m, st, ps, d, g := machinesFixture(t)
	ctx := context.Background()
	m.Create(ctx, domain.Machine{Name: "a", Stack: "python"})
	os.WriteFile(filepath.Join(st.Dir("a"), "console.log"), []byte("kernel panic: no init\n"), 0o600)
	g.readyErr = errors.New("guest never answered")
	if _, err := m.Start(ctx, "a"); err == nil || !strings.Contains(err.Error(), "kernel panic") || len(ps.killed) != 1 || st.PID("a") != 0 {
		t.Fatalf("not ready: %v killed=%v pid=%d", err, ps.killed, st.PID("a"))
	}
	g.readyErr = nil
	d.fail = errors.New("no hypervisor")
	if _, err := m.Start(ctx, "a"); err == nil {
		t.Fatal("launch failure must fail")
	}
	d.fail = nil
	m.Images = images{}
	if _, err := m.Start(ctx, "a"); err == nil || !strings.Contains(err.Error(), "not built") {
		t.Fatalf("missing image: %v", err)
	}
	for _, f := range []func() error{
		func() error { _, err := m.Start(ctx, "nope"); return err },
		func() error { return m.Stop(ctx, "nope", 0) },
		func() error { return m.Delete(ctx, "nope", true) },
		func() error { _, err := m.Logs("nope", 0, ""); return err },
		func() error { _, err := m.Exec(ctx, "nope", ExecRequest{Argv: []string{"ls"}}, nil); return err },
	} {
		if err := f(); errCode(err) != domain.CodeNotFound {
			t.Fatalf("missing machine must be NOT_FOUND: %v", err)
		}
	}
}

// TestMachineCopy: ida e volta de 2,5 MiB em pedaços (só o último marca Final), arquivo
// vazio, caminho relativo recusado, erro do guest repassado, e máquina parada recusada.
func TestMachineCopy(t *testing.T) {
	m, _, _, _, g := machinesFixture(t)
	ctx := context.Background()
	m.Create(ctx, domain.Machine{Name: "a", Stack: "python"})
	m.Start(ctx, "a")
	data := make([]byte, 2*protocol.ChunkBytes+protocol.ChunkBytes/2)
	rand.Read(data)
	n, err := m.CopyIn(ctx, "a", "/workspace/../workspace/f.bin", bytes.NewReader(data), 0o600)
	if err != nil || n != int64(len(data)) {
		t.Fatalf("copy in: %d %v", n, err)
	}
	var puts, finals int
	for _, j := range g.jobs {
		if j.Op == protocol.OpPut {
			puts++
			if j.Final {
				finals++
			}
			if j.Path != "/workspace/f.bin" || j.Mode != 0o600 {
				t.Fatalf("put job: path=%s mode=%o", j.Path, j.Mode)
			}
		}
	}
	if puts != 3 || finals != 1 {
		t.Fatalf("chunks: %d puts, %d final", puts, finals)
	}
	var out bytes.Buffer
	if n, err := m.CopyOut(ctx, "a", "/workspace/f.bin", &out); err != nil || n != int64(len(data)) || !bytes.Equal(out.Bytes(), data) {
		t.Fatalf("copy out: %d %v equal=%v", n, err, bytes.Equal(out.Bytes(), data))
	}
	exact := make([]byte, protocol.ChunkBytes)
	if n, err := m.CopyIn(ctx, "a", "/exact", bytes.NewReader(exact), 0); err != nil || n != int64(len(exact)) || len(g.files["/exact"]) != len(exact) {
		t.Fatalf("exactly one chunk: %d %v", n, err)
	}
	if _, err := m.CopyIn(ctx, "a", "/empty", bytes.NewReader(nil), 0); err != nil || g.files["/empty"] == nil && len(g.files["/empty"]) != 0 {
		t.Fatalf("empty file: %v", err)
	}
	if _, ok := g.files["/empty"]; !ok {
		t.Fatal("empty file was not created")
	}
	var ve domain.ValidationError
	if _, err := m.CopyIn(ctx, "a", "rel", bytes.NewReader(data), 0); !errors.As(err, &ve) {
		t.Fatalf("relative path: %v", err)
	}
	if _, err := m.CopyOut(ctx, "a", "rel", &out); !errors.As(err, &ve) {
		t.Fatalf("relative path out: %v", err)
	}
	if _, err := m.CopyIn(ctx, "a", "/denied", bytes.NewReader(data), 0); err == nil || !strings.Contains(err.Error(), "permission denied") {
		t.Fatalf("guest error: %v", err)
	}
	if _, err := m.CopyOut(ctx, "a", "/missing", &out); err == nil || !strings.Contains(err.Error(), "no such file") {
		t.Fatalf("missing file: %v", err)
	}
	if _, err := m.CopyIn(ctx, "a", "/x", errReader{}, 0); err == nil {
		t.Fatal("reader error must fail")
	}
	m.Stop(ctx, "a", 0)
	if _, err := m.CopyIn(ctx, "a", "/x", bytes.NewReader(data), 0); errCode(err) != domain.CodeInvalidState {
		t.Fatalf("stopped machine: %v", err)
	}
	if _, err := m.CopyOut(ctx, "a", "/x", &out); errCode(err) != domain.CodeInvalidState {
		t.Fatalf("stopped machine out: %v", err)
	}
}

// errReader falha na leitura, como um arquivo do host que some no meio da cópia.
type errReader struct{}

// Read devolve erro sempre.
func (errReader) Read([]byte) (int, error) { return 0, errors.New("disk read error") }

// TestMachineExecAndLogs: argv vazio recusado, erro do guest repassado, e o tail do log.
func TestMachineExecAndLogs(t *testing.T) {
	m, st, _, _, g := machinesFixture(t)
	ctx := context.Background()
	m.Create(ctx, domain.Machine{Name: "a", Stack: "python"})
	if out, err := m.Logs("a", 0, ""); err != nil || out != "" {
		t.Fatalf("no console yet: %q %v", out, err)
	}
	os.WriteFile(filepath.Join(st.Dir("a"), "console.log"), []byte("l1\nl2\nl3\n"), 0o600)
	if out, _ := m.Logs("a", 2, ""); out != "l2\nl3\n" {
		t.Fatalf("tail: %q", out)
	}
	if out, _ := m.Logs("a", 0, ""); out != "l1\nl2\nl3\n" {
		t.Fatalf("all: %q", out)
	}
	os.WriteFile(filepath.Join(st.Dir("a"), "console.log"), []byte("boot\n[workload] w1\nguest\r\n[workload] w2\r\n[workload] w3"), 0o600)
	if out, _ := m.Logs("a", 2, LogSourceWorkload); out != "w2\nw3" {
		t.Fatalf("workload tail counts after the filter: %q", out)
	}
	if out, _ := m.Logs("a", 0, LogSourceAll); !strings.HasPrefix(out, "boot\n[workload] w1") {
		t.Fatalf("all keeps the prefix: %q", out)
	}
	var bad domain.ValidationError
	if _, err := m.Logs("a", 0, "kernel"); !errors.As(err, &bad) {
		t.Fatalf("unknown source: %v", err)
	}
	m.Start(ctx, "a")
	var ve domain.ValidationError
	if _, err := m.Exec(ctx, "a", ExecRequest{}, nil); !errors.As(err, &ve) {
		t.Fatalf("empty argv: %v", err)
	}
	g.execErr = errors.New("connection reset")
	if _, err := m.Exec(ctx, "a", ExecRequest{Argv: []string{"ls"}}, nil); err == nil || !strings.Contains(err.Error(), "connection reset") {
		t.Fatalf("guest error: %v", err)
	}
}

// TestMachineShell: o shell sai como OpPTY com argv, env, cwd e tamanho; a entrada chega,
// o resize chega, o código volta; máquina parada é INVALID_STATE e erro do guest repassa.
func TestMachineShell(t *testing.T) {
	m, _, _, _, g := machinesFixture(t)
	ctx := context.Background()
	m.Create(ctx, domain.Machine{Name: "a", Stack: "python"})
	if _, err := m.Shell(ctx, "a", ShellRequest{}, strings.NewReader(""), io.Discard, nil); errCode(err) != domain.CodeInvalidState {
		t.Fatalf("stopped machine: %v", err)
	}
	m.Start(ctx, "a")
	resize := make(chan ports.Winsize, 1)
	resize <- ports.Winsize{Rows: 50, Cols: 160}
	var out bytes.Buffer
	code, err := m.Shell(ctx, "a", ShellRequest{Argv: []string{"bash"}, Env: []string{"TERM=xterm"}, Cwd: "/workspace", Size: ports.Winsize{Rows: 30, Cols: 100}},
		strings.NewReader("typed"), &out, resize)
	if err != nil || code != 6 || out.String() != "size=50x160;typed" {
		t.Fatalf("shell: %d %v %q", code, err, out.String())
	}
	j := g.jobs[len(g.jobs)-1]
	if j.Argv[0] != "bash" || j.Env[0] != "TERM=xterm" || j.Cwd != "/workspace" || j.Rows != 30 || j.Cols != 100 {
		t.Fatalf("pty job: %+v", j)
	}
	g.execErr = errors.New("connection reset")
	if _, err := m.Shell(ctx, "a", ShellRequest{}, strings.NewReader(""), io.Discard, nil); err == nil || !strings.Contains(err.Error(), "connection reset") {
		t.Fatalf("guest error: %v", err)
	}
}

// jobsOf filtra os jobs de uma operação.
func jobsOf(g *fileGuest, op string) []protocol.Job {
	g.mu.Lock()
	defer g.mu.Unlock()
	var out []protocol.Job
	for _, j := range g.jobs {
		if j.Op == op {
			out = append(out, j)
		}
	}
	return out
}

// TestMachineProvision: o init roda uma vez por disco (com env e workdir da máquina) e é
// marcado; o workload sobe a cada start com o stop_on_exit; init que falha deixa a VM ligada
// e não marca, para rodar de novo; exec herda env e workdir da máquina.
func TestMachineProvision(t *testing.T) {
	m, _, ps, _, g := machinesFixture(t)
	ctx := context.Background()
	mc := domain.Machine{Name: "app", Stack: "python", Env: []string{"MODE=dev"}, Workdir: "/app",
		Init: []string{"echo one", "echo two"}, Workload: []string{"python3", "-m", "http.server"}, StopOnExit: true}
	if _, err := m.Create(ctx, mc); err != nil {
		t.Fatal(err)
	}
	if _, err := m.Start(ctx, "app"); err != nil {
		t.Fatal(err)
	}
	var inits []string
	for _, j := range jobsOf(g, protocol.OpExec) {
		if strings.HasPrefix(j.TraceID, "init-") && j.TraceID != "init-done" && j.TraceID != "init-check" {
			inits = append(inits, j.Argv[2])
			if j.Env[0] != "MODE=dev" || j.Cwd != "/app" {
				t.Fatalf("init job: %+v", j)
			}
		}
	}
	if strings.Join(inits, ",") != "echo one,echo two" {
		t.Fatalf("init commands: %v", inits)
	}
	wl := jobsOf(g, protocol.OpWorkload)
	if len(wl) != 1 || wl[0].Argv[2] != "http.server" || !wl[0].StopOnExit || wl[0].Cwd != "/app" {
		t.Fatalf("workload: %+v", wl)
	}
	m.Stop(ctx, "app", 0)
	m.Start(ctx, "app")
	n := 0
	for _, j := range jobsOf(g, protocol.OpExec) {
		if j.TraceID == "init-0" {
			n++
		}
	}
	if n != 1 || len(jobsOf(g, protocol.OpWorkload)) != 2 {
		t.Fatalf("init must run once per disk (%d), workload every start (%d)", n, len(jobsOf(g, protocol.OpWorkload)))
	}
	m.Exec(ctx, "app", ExecRequest{Argv: []string{"env"}, Env: []string{"X=1"}}, nil)
	last := jobsOf(g, protocol.OpExec)
	if j := last[len(last)-1]; strings.Join(j.Env, ",") != "MODE=dev,X=1" || j.Cwd != "/app" {
		t.Fatalf("exec must inherit the machine env/workdir: %+v", j)
	}
	m.Shell(ctx, "app", ShellRequest{}, strings.NewReader(""), io.Discard, nil)
	pty := g.jobs[len(g.jobs)-1]
	if pty.Cwd != "/app" || pty.Env[0] != "MODE=dev" {
		t.Fatalf("shell must inherit the machine env/workdir: %+v", pty)
	}

	// O "disco" do guest falso é um só para todas as máquinas: apagar o marcador simula o
	// disco novo da próxima.
	g.mu.Lock()
	delete(g.files, initMarker)
	g.mu.Unlock()
	if _, err := m.Create(ctx, domain.Machine{Name: "broken", Stack: "python", Init: []string{"fail now"}}); err != nil {
		t.Fatal(err)
	}
	killedBefore := len(ps.killed)
	if _, err := m.Start(ctx, "broken"); err == nil || !strings.Contains(err.Error(), "init broke") {
		t.Fatalf("failing init: %v", err)
	}
	if st, _ := m.Status("broken"); st.State != StateRunning || len(ps.killed) != killedBefore {
		t.Fatal("a failed init leaves the VM running for inspection")
	}
	g.workloadErr = "workload: already running"
	if _, err := m.Create(ctx, domain.Machine{Name: "wl", Stack: "python", Workload: []string{"x"}}); err != nil {
		t.Fatal(err)
	}
	if _, err := m.Start(ctx, "wl"); err == nil || !strings.Contains(err.Error(), "already running") {
		t.Fatalf("workload failure: %v", err)
	}
	g.workloadErr = ""
	m.Create(ctx, domain.Machine{Name: "wd", Stack: "python", Workdir: "/denied", Workload: []string{"x"}})
	if _, err := m.Start(ctx, "wd"); err == nil || !strings.Contains(err.Error(), "permission denied") {
		t.Fatalf("workdir failure: %v", err)
	}
	if wd := jobsOf(g, protocol.OpExec); wd[len(wd)-1].Argv[0] != "mkdir" {
		t.Fatalf("workdir must be created before init/workload: %+v", wd[len(wd)-1])
	}
	g.execErr = errors.New("guest gone")
	m.Create(ctx, domain.Machine{Name: "gone-wd", Stack: "python", Workdir: "/x", Init: []string{"x"}})
	if _, err := m.Start(ctx, "gone-wd"); err == nil || !strings.Contains(err.Error(), "create workdir") {
		t.Fatalf("guest error on workdir: %v", err)
	}
	m.Create(ctx, domain.Machine{Name: "gone", Stack: "python", Init: []string{"x"}})
	if _, err := m.Start(ctx, "gone"); err == nil || !strings.Contains(err.Error(), "check init") {
		t.Fatalf("guest error during provision: %v", err)
	}
}

// TestMachineStopPolicy: o stop manda o stop_signal e o stop_timeout da máquina ao guest;
// timeout explícito vale só para aquele stop.
func TestMachineStopPolicy(t *testing.T) {
	m, _, ps, _, g := machinesFixture(t)
	ctx := context.Background()
	m.Create(ctx, domain.Machine{Name: "s", Stack: "python", Workload: []string{"srv"}, StopSignal: "SIGINT", StopTimeout: 1})
	m.Start(ctx, "s")
	g.shutdown = func() { ps.mu.Lock(); ps.alive = map[int]bool{}; ps.mu.Unlock() }
	if err := m.Stop(ctx, "s", -1); err != nil {
		t.Fatal(err)
	}
	sd := jobsOf(g, protocol.OpShutdown)
	if len(sd) != 1 || sd[0].Signal != 2 || sd[0].Timeout != 1 {
		t.Fatalf("shutdown job: %+v", sd)
	}
	m.Start(ctx, "s")
	m.Stop(ctx, "s", 3*time.Second)
	if sd := jobsOf(g, protocol.OpShutdown); sd[1].Timeout != 3 {
		t.Fatalf("explicit timeout must win: %+v", sd[1])
	}
}

// TestMachineFromImage: `image` vira um ext4 construído na hora com nome estável
// (img-<sha>), sem passar pelo catálogo; stack e image juntos é erro; sem builder é erro.
func TestMachineFromImage(t *testing.T) {
	m, st, _, _, _ := machinesFixture(t)
	ctx := context.Background()
	var built []domain.Language
	m.BuildImage = func(_ context.Context, l domain.Language) error { built = append(built, l); return nil }
	mc, err := m.Create(ctx, domain.Machine{Name: "redis", Image: "redis:7-alpine", GPU: true})
	if err != nil || mc.Stack != ImageStackName("redis:7-alpine") || len(built) != 1 || built[0].Image != "redis:7-alpine" || built[0].Name != mc.Stack {
		t.Fatalf("image machine: %+v %v %+v", mc, err, built)
	}
	if !strings.HasPrefix(mc.Stack, "img-") || len(mc.Stack) != 16 || ImageStackName("redis:7-alpine") != mc.Stack {
		t.Fatalf("stable name: %s", mc.Stack)
	}
	if saved, _ := st.Load("redis"); saved.Image != "redis:7-alpine" {
		t.Fatalf("record: %+v", saved)
	}
	var ve domain.ValidationError
	if _, err := m.Create(ctx, domain.Machine{Name: "x", Image: "a", Stack: "python"}); !errors.As(err, &ve) {
		t.Fatalf("stack + image: %v", err)
	}
	m.BuildImage = func(context.Context, domain.Language) error { return errors.New("pull failed") }
	if _, err := m.Create(ctx, domain.Machine{Name: "y", Image: "nope:latest"}); err == nil || !strings.Contains(err.Error(), "pull failed") {
		t.Fatalf("build failure: %v", err)
	}
	m.BuildImage = nil
	if _, err := m.Create(ctx, domain.Machine{Name: "z", Image: "alpine"}); !errors.As(err, &ve) {
		t.Fatalf("no builder: %v", err)
	}
}

// TestMachineMounts: o create guarda o caminho real (sem link) e recusa o que não é
// diretório; o start passa cada volume como virtio-fs com tag própria e monta no guest, a
// cada start, antes do workdir; volume que sumiu ou montagem que falha viram erro claro.
func TestMachineMounts(t *testing.T) {
	m, _, _, d, g := machinesFixture(t)
	ctx := context.Background()
	dir := t.TempDir()
	real, _ := filepath.EvalSymlinks(dir)
	link := filepath.Join(t.TempDir(), "link")
	os.Symlink(dir, link)
	mc, err := m.Create(ctx, domain.Machine{Name: "v", Stack: "python", Workdir: "/www", Workload: []string{"x"},
		Mounts: []domain.Mount{{Host: link, Guest: "/www"}, {Host: dir, Guest: "/models", ReadOnly: true}}})
	if err != nil || mc.Mounts[0].Host != real {
		t.Fatalf("create: %+v %v", mc.Mounts, err)
	}
	if _, err := m.Start(ctx, "v"); err != nil {
		t.Fatal(err)
	}
	sp := d.specs[len(d.specs)-1].Mounts
	if len(sp) != 2 || sp[0] != (ports.VMMount{Tag: "vol0", HostPath: real}) || !sp[1].ReadOnly || sp[1].Tag != "vol1" {
		t.Fatalf("spec mounts: %+v", sp)
	}
	var order []string
	for _, j := range jobsOf(g, protocol.OpExec) {
		order = append(order, j.TraceID)
		if j.TraceID == "mount-vol1" && (j.Argv[4] != "/models" || j.Argv[5] != "vol1" || j.Argv[6] != "ro") {
			t.Fatalf("mount job: %v", j.Argv)
		}
	}
	if got := strings.Join(order, ","); !strings.Contains(got, "mount-vol0,mount-vol1,workdir") {
		t.Fatalf("mounts must come before the workdir: %s", got)
	}

	var ve domain.ValidationError
	file := filepath.Join(dir, "f")
	os.WriteFile(file, nil, 0o600)
	for _, host := range []string{file, filepath.Join(dir, "missing")} {
		if _, err := m.Create(ctx, domain.Machine{Name: "bad", Stack: "python", Mounts: []domain.Mount{{Host: host, Guest: "/x"}}}); !errors.As(err, &ve) {
			t.Fatalf("%s must be refused: %v", host, err)
		}
	}
	gone := t.TempDir()
	m.Create(ctx, domain.Machine{Name: "gone", Stack: "python", Mounts: []domain.Mount{{Host: gone, Guest: "/x"}}})
	os.RemoveAll(gone)
	if _, err := m.Start(ctx, "gone"); !errors.As(err, &ve) || !strings.Contains(err.Error(), "not available") {
		t.Fatalf("vanished host dir: %v", err)
	}
	m.Create(ctx, domain.Machine{Name: "busy", Stack: "python", Mounts: []domain.Mount{{Host: dir, Guest: "/busy"}}})
	if _, err := m.Start(ctx, "busy"); err == nil || !strings.Contains(err.Error(), "resource busy") {
		t.Fatalf("mount failure: %v", err)
	}
	g.execErr = errors.New("guest gone")
	m.Create(ctx, domain.Machine{Name: "lost", Stack: "python", Mounts: []domain.Mount{{Host: dir, Guest: "/y"}}})
	if _, err := m.Start(ctx, "lost"); err == nil || !strings.Contains(err.Error(), "mount /y") {
		t.Fatalf("guest error on mount: %v", err)
	}
}

// TestMachineNetwork: máquina com rede leva a virtio-net na spec (log e socket de controle
// no diretório dela); sem rede, nenhuma placa; o update grava com a máquina parada, entrega
// ao gateway com ela ligada, e recusa o que não tem como aplicar.
func TestMachineNetwork(t *testing.T) {
	m, st, _, d, _ := machinesFixture(t)
	ctx := context.Background()
	m.Create(ctx, domain.Machine{Name: "off", Stack: "python"})
	m.Start(ctx, "off")
	if d.specs[len(d.specs)-1].Net != nil {
		t.Fatal("no network by default")
	}
	net := domain.Network{Enabled: true, Ports: []domain.PortMap{{Host: 8080, Guest: 80}}}
	m.Create(ctx, domain.Machine{Name: "on", Stack: "python", Network: net})
	os.WriteFile(filepath.Join(st.Dir("on"), machineEgressSocket), nil, 0o600)
	m.Start(ctx, "on")
	sp := d.specs[len(d.specs)-1].Net
	if sp == nil || sp.Network.Ports[0].Host != 8080 || sp.ControlSocket != filepath.Join(st.Dir("on"), machineEgressSocket) ||
		sp.DenialsLog != filepath.Join(st.Dir("on"), machineDenials) {
		t.Fatalf("spec net: %+v", sp)
	}
	if _, err := os.Stat(sp.ControlSocket); !os.IsNotExist(err) {
		t.Fatal("a stale control socket must be removed before start")
	}

	var sent []string
	m.UpdateEgress = func(sock string, e domain.Egress) error {
		sent = append(sent, sock+" "+strings.Join(e.AllowHosts, ","))
		if len(e.AllowHosts) > 0 && e.AllowHosts[0] == "down.example" {
			return errors.New("machine network is not reachable")
		}
		return nil
	}
	add := func(h string) func(*domain.Egress) {
		return func(e *domain.Egress) { e.AllowHosts = append(e.AllowHosts, h) }
	}
	if _, err := m.Update("on", add("example.com")); err != nil || len(sent) != 1 || !strings.HasSuffix(sent[0], "egress.sock example.com") {
		t.Fatalf("live update: %v %v", err, sent)
	}
	if saved, _ := st.Load("on"); saved.Network.Egress.AllowHosts[0] != "example.com" {
		t.Fatal("live update must also persist")
	}
	if _, err := m.Update("on", func(e *domain.Egress) { e.AllowHosts = []string{"down.example"} }); err == nil {
		t.Fatal("gateway error must surface")
	}
	if saved, _ := st.Load("on"); saved.Network.Egress.AllowHosts[0] != "example.com" {
		t.Fatal("a failed live update must not persist")
	}
	if _, err := m.Update("off", add("x.com")); errCode(err) != domain.CodeInvalidState {
		t.Fatalf("running without NIC: %v", err)
	}
	m.Stop(ctx, "off", 0)
	if mc, err := m.Update("off", add("x.com")); err != nil || mc.Network.Egress.AllowHosts[0] != "x.com" || len(sent) != 2 {
		t.Fatalf("stopped machine: just save: %v %+v", err, mc.Network)
	}
	var ve domain.ValidationError
	if _, err := m.Update("off", func(e *domain.Egress) { e.AllowCIDRs = []string{"bad"} }); !errors.As(err, &ve) {
		t.Fatalf("invalid change: %v", err)
	}
	if _, err := m.Update("nope", add("x")); errCode(err) != domain.CodeNotFound {
		t.Fatalf("missing: %v", err)
	}
	m.UpdateEgress = nil
	if _, err := m.Update("on", add("y.com")); errCode(err) != domain.CodeInvalidState {
		t.Fatalf("no live update support: %v", err)
	}
}
