package app

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"sync/atomic"
	"testing"
	"time"

	"runner.code/internal/domain"
	"runner.code/internal/ports"
	"runner.code/internal/protocol"
)

// vm é uma VM falsa que vive até Kill (ou até die, simulando crash).
type vm struct {
	id     int
	done   chan struct{}
	once   sync.Once
	killed atomic.Bool
}

// PID devolve o id falso.
func (v *vm) PID() int { return v.id }

// Done fecha quando a VM morre.
func (v *vm) Done() <-chan struct{} { return v.done }

// Wait espera a morte.
func (v *vm) Wait() int { <-v.done; return 0 }

// Kill marca e encerra.
func (v *vm) Kill() { v.killed.Store(true); v.die() }

// die encerra sem ser por Kill (crash).
func (v *vm) die() { v.once.Do(func() { close(v.done) }) }

// launcher falso: guarda as specs e as VMs criadas.
type launcher struct {
	mu    sync.Mutex
	specs []ports.VMSpec
	vms   []*vm
	fail  error
}

// StartVM cria uma VM falsa viva, ou falha se configurado.
func (l *launcher) StartVM(s ports.VMSpec) (ports.VM, error) {
	if l.fail != nil {
		return nil, l.fail
	}
	l.mu.Lock()
	defer l.mu.Unlock()
	v := &vm{id: len(l.vms) + 1, done: make(chan struct{})}
	l.specs = append(l.specs, s)
	l.vms = append(l.vms, v)
	return v, nil
}

// vm devolve a i-ésima VM lançada, com o lock (a reposição escreve em paralelo).
func (l *launcher) vm(i int) *vm { l.mu.Lock(); defer l.mu.Unlock(); return l.vms[i] }

// spec devolve a i-ésima spec lançada, com o lock.
func (l *launcher) spec(i int) ports.VMSpec { l.mu.Lock(); defer l.mu.Unlock(); return l.specs[i] }

// count devolve quantas VMs foram lançadas.
func (l *launcher) count() int { l.mu.Lock(); defer l.mu.Unlock(); return len(l.vms) }

// guest falso: readyErr decide a prontidão; run decide a execução.
type guest struct {
	readyErr func(socket string) error
	run      func(ctx context.Context, socket string, j protocol.Job) (protocol.Result, error)
	sockets  sync.Map
}

// Run delega para a função configurada.
func (g *guest) Run(ctx context.Context, socket string, j protocol.Job) (protocol.Result, error) {
	g.sockets.Store(socket, true)
	return g.run(ctx, socket, j)
}

// WaitReady responde na hora, ou espera o ctx se readyErr devolver erro de bloqueio.
func (g *guest) WaitReady(ctx context.Context, socket string) error {
	if g.readyErr == nil {
		return nil
	}
	if err := g.readyErr(socket); err != nil {
		if errors.Is(err, errBlock) {
			<-ctx.Done()
			return ctx.Err()
		}
		return err
	}
	return nil
}

// errBlock faz o WaitReady falso esperar o ctx, como um guest que nunca sobe.
var errBlock = errors.New("bloqueia")

// images falso: python sempre construído; o resto conforme o mapa.
type images map[string]string

// ImagePath consulta o mapa.
func (i images) ImagePath(lang string) (string, bool) { p, ok := i[lang]; return p, ok }

// catalog falso com python e ruby.
type catalog map[string]domain.Language

// Get consulta o mapa.
func (c catalog) Get(n string) (domain.Language, bool) { l, ok := c[n]; return l, ok }

// fixtures monta o pool com as dependências falsas.
func fixtures(t *testing.T, size int) (*Pool, *launcher, *guest) {
	l := &launcher{}
	g := &guest{run: func(context.Context, string, protocol.Job) (protocol.Result, error) {
		return protocol.Result{Status: "success", Stdout: "ok"}, nil
	}}
	p := &Pool{
		Launcher: l, Guest: g, WorkDir: t.TempDir(), Size: size, Log: &strings.Builder{},
		Images:  images{"python": "/img/python.ext4"},
		Catalog: catalog{"python": {Name: "python", CPUs: 1, MemoryMiB: 512}, "ruby": {Name: "ruby", CPUs: 1, MemoryMiB: 512}},
	}
	retryBackoff = 10 * time.Millisecond
	return p, l, g
}

// eventually espera a condição por até 2 s.
func eventually(t *testing.T, what string, cond func() bool) {
	t.Helper()
	for end := time.Now().Add(2 * time.Second); time.Now().Before(end); time.Sleep(5 * time.Millisecond) {
		if cond() {
			return
		}
	}
	t.Fatalf("não aconteceu: %s", what)
}

// TestPoolFillsAndRefills: enche até Size, nunca passa, e repõe o que foi entregue.
func TestPoolFillsAndRefills(t *testing.T) {
	p, l, _ := fixtures(t, 2)
	p.Start([]string{"python"})
	defer p.Close()
	eventually(t, "pool cheio", func() bool { return p.Ready("python") == 2 })
	time.Sleep(30 * time.Millisecond)
	if l.count() != 2 {
		t.Fatalf("pool de 2 lançou %d VMs", l.count())
	}
	spec := l.spec(0)
	if spec.RootDisk != "/img/python.ext4" || spec.VsockPort != 5000 || spec.Exec != "/opt/runner/start.sh" || spec.Network {
		t.Fatalf("spec de job errada: %+v", spec)
	}
	a, err := p.Take(context.Background(), "python")
	if err != nil {
		t.Fatal(err)
	}
	b, _ := p.Take(context.Background(), "python")
	if a.VM == b.VM {
		t.Fatal("a mesma VM foi entregue duas vezes")
	}
	eventually(t, "reposição", func() bool { return p.Ready("python") == 2 && l.count() == 4 })
}

// TestTakeColdBootWhenEmpty: sem pool, o job faz boot na hora (nunca é recusado).
func TestTakeColdBootWhenEmpty(t *testing.T) {
	p, l, _ := fixtures(t, 0)
	p.Start([]string{"python"})
	r, err := p.Take(context.Background(), "python")
	if err != nil || l.count() != 1 {
		t.Fatalf("boot frio: err=%v vms=%d", err, l.count())
	}
	p.Destroy(r)
	if !l.vm(0).killed.Load() {
		t.Fatal("Destroy deveria matar a VM")
	}
	p.Close()
}

// TestTakeSkipsDeadVM: VM que morreu parada na fila não é entregue.
func TestTakeSkipsDeadVM(t *testing.T) {
	p, l, _ := fixtures(t, 1)
	p.Start([]string{"python"})
	defer p.Close()
	eventually(t, "pool cheio", func() bool { return p.Ready("python") == 1 })
	l.vm(0).die()
	r, err := p.Take(context.Background(), "python")
	if err != nil {
		t.Fatal(err)
	}
	if r.VM == ports.VM(l.vm(0)) {
		t.Fatal("entregou VM morta")
	}
}

// TestBootErrors cobre linguagem fora do catálogo, imagem ausente, launcher que falha,
// guest que nunca fica pronto e VM que morre durante o boot.
func TestBootErrors(t *testing.T) {
	ctx := context.Background()
	p, _, _ := fixtures(t, 0)
	if _, err := p.Boot(ctx, "cobol"); err == nil {
		t.Fatal("fora do catálogo")
	}
	if _, err := p.Boot(ctx, "ruby"); err == nil || !strings.Contains(err.Error(), "image build") {
		t.Fatalf("imagem ausente deveria orientar o build: %v", err)
	}
	p, l, _ := fixtures(t, 0)
	l.fail = errors.New("sem hypervisor")
	if _, err := p.Boot(ctx, "python"); err == nil {
		t.Fatal("launcher que falha")
	}
	p, l, g := fixtures(t, 0)
	g.readyErr = func(string) error { return errors.New("recusado") }
	if _, err := p.Boot(ctx, "python"); err == nil || !l.vm(0).killed.Load() {
		t.Fatal("guest que não responde: erro e VM morta")
	}
	p, l, g = fixtures(t, 0)
	g.readyErr = func(string) error { return errBlock }
	go func() {
		eventually(t, "vm lançada", func() bool { return l.count() == 1 })
		os.WriteFile(l.spec(0).Console, []byte("kernel panic"), 0o644)
		l.vm(0).die()
	}()
	start := time.Now()
	_, err := p.Boot(ctx, "python")
	if err == nil || !strings.Contains(err.Error(), "kernel panic") {
		t.Fatalf("VM morta no boot deveria trazer o console: %v", err)
	}
	if time.Since(start) > 5*time.Second {
		t.Fatal("esperou o ReadyTimeout em vez de reagir à morte da VM")
	}
	p, _, _ = fixtures(t, 0)
	p.WorkDir = "/dev/null/x"
	if _, err := p.Boot(ctx, "python"); err == nil {
		t.Fatal("workdir impossível")
	}
}

// TestRefillRetriesAfterFailure: boot falho não derruba a reposição; ela tenta de novo.
func TestRefillRetriesAfterFailure(t *testing.T) {
	p, _, g := fixtures(t, 1)
	var n atomic.Int32
	g.readyErr = func(string) error {
		if n.Add(1) == 1 {
			return errors.New("primeiro boot falhou")
		}
		return nil
	}
	p.Start([]string{"python"})
	defer p.Close()
	eventually(t, "pool recuperado", func() bool { return p.Ready("python") == 1 })
}

// TestCloseKillsParkedVMs: fechar o serviço não deixa VM viva.
func TestCloseKillsParkedVMs(t *testing.T) {
	p, l, _ := fixtures(t, 2)
	p.Start([]string{"python"})
	eventually(t, "pool cheio", func() bool { return p.Ready("python") == 2 })
	p.Close()
	l.mu.Lock()
	all := append([]*vm(nil), l.vms...)
	l.mu.Unlock()
	for _, v := range all {
		if !v.killed.Load() {
			t.Fatalf("VM %d sobreviveu ao Close", v.id)
		}
	}
	(&Pool{}).Close() // Close sem Start não pode entrar em pânico
}

// TestCloseDuringBootRetry: fechar enquanto a reposição espera o backoff encerra limpo.
func TestCloseDuringBootRetry(t *testing.T) {
	p, _, g := fixtures(t, 1)
	retryBackoff = time.Hour
	g.readyErr = func(string) error { return errors.New("sempre falha") }
	p.Start([]string{"python"})
	time.Sleep(30 * time.Millisecond)
	done := make(chan struct{})
	go func() { p.Close(); close(done) }()
	select {
	case <-done:
	case <-time.After(2 * time.Second):
		t.Fatal("Close travou esperando backoff")
	}
}

// TestCloseDuringBoot: Close com boot em andamento cancela o boot e sai.
func TestCloseDuringBoot(t *testing.T) {
	p, _, g := fixtures(t, 1)
	g.readyErr = func(string) error { return errBlock }
	p.Start([]string{"python"})
	time.Sleep(30 * time.Millisecond)
	done := make(chan struct{})
	go func() { p.Close(); close(done) }()
	select {
	case <-done:
	case <-time.After(2 * time.Second):
		t.Fatal("Close travou com boot em andamento")
	}
}

// TestTailFile cobre arquivo ausente e corte do fim.
func TestTailFile(t *testing.T) {
	if tailFile("/nao/existe", 10) != "(sem console)" {
		t.Fatal("ausente")
	}
	f := filepath.Join(t.TempDir(), "c")
	os.WriteFile(f, []byte("0123456789abcdef"), 0o644)
	if got := tailFile(f, 4); got != "cdef" {
		t.Fatalf("tail %q", got)
	}
}

// executor monta o Executor sobre o pool falso.
func executor(t *testing.T) (*Executor, *Pool, *launcher, *guest) {
	p, l, g := fixtures(t, 0)
	p.Start(nil)
	return &Executor{VMs: p, Guest: g, Images: p.Images, Catalog: p.Catalog}, p, l, g
}

// job é um job Python válido.
func job() domain.Job {
	return domain.Job{TraceID: "t", Language: "python", Code: "print(1)", Timeout: time.Second}
}

// TestExecuteSuccessDestroysVM: resultado mapeado e a VM morta depois do job.
func TestExecuteSuccessDestroysVM(t *testing.T) {
	e, _, l, g := executor(t)
	var got protocol.Job
	g.run = func(_ context.Context, _ string, j protocol.Job) (protocol.Result, error) {
		got = j
		return protocol.Result{Status: "success", Stdout: "1\n", Stderr: nil, Exception: nil, ExecutionTime: 12}, nil
	}
	j := job()
	j.Files = []domain.File{{Name: "main.py", Content: "x"}}
	j.Stdin = "in"
	res, err := e.Run(context.Background(), j)
	if err != nil || res.Stdout != "1\n" || res.Stderr != "" || res.Duration != 12*time.Millisecond {
		t.Fatalf("res=%+v err=%v", res, err)
	}
	if got.Lang != "python" || got.Timeout != 1 || got.Stdin != "in" || len(got.Files) != 1 {
		t.Fatalf("job convertido errado: %+v", got)
	}
	if !l.vm(0).killed.Load() {
		t.Fatal("VM não foi destruída depois do job")
	}
}

// TestExecuteTimeout: guest que não responde vira timeout E001 e a VM morre.
func TestExecuteTimeout(t *testing.T) {
	e, _, l, g := executor(t)
	g.run = func(ctx context.Context, _ string, _ protocol.Job) (protocol.Result, error) {
		<-ctx.Done()
		return protocol.Result{}, ctx.Err()
	}
	j := job()
	j.Timeout = 50 * time.Millisecond
	res, err := e.Run(context.Background(), j)
	if err != nil || !res.TimedOut || !strings.HasPrefix(res.Error, "E001") {
		t.Fatalf("esperava timeout E001: res=%+v err=%v", res, err)
	}
	if !l.vm(0).killed.Load() {
		t.Fatal("VM sobreviveu ao timeout")
	}
}

// TestExecuteGuestTimeoutCode: o guest matou o programa e respondeu E001.
func TestExecuteGuestTimeoutCode(t *testing.T) {
	e, _, _, g := executor(t)
	g.run = func(context.Context, string, protocol.Job) (protocol.Result, error) {
		return protocol.Result{Status: "failed", Error: domain.ErrTimeout.Error(), Stderr: 42}, nil
	}
	res, err := e.Run(context.Background(), job())
	if err != nil || !res.TimedOut || res.Stderr != "42" {
		t.Fatalf("res=%+v err=%v", res, err)
	}
}

// TestExecuteGuestTimeoutException trava o caso medido na VM real: o guest matou o laço
// infinito e respondeu status "success" com exception "Timeout"; isso é timeout.
func TestExecuteGuestTimeoutException(t *testing.T) {
	e, _, _, g := executor(t)
	g.run = func(context.Context, string, protocol.Job) (protocol.Result, error) {
		return protocol.Result{Status: "success", Exception: "Timeout", ExitCode: -1}, nil
	}
	res, err := e.Run(context.Background(), job())
	if err != nil || !res.TimedOut || res.Status != "success" {
		t.Fatalf("res=%+v err=%v", res, err)
	}
}

// TestExecuteErrors cobre validação, falta de VM, erro do guest e cancelamento do chamador.
func TestExecuteErrors(t *testing.T) {
	e, _, l, g := executor(t)
	bad := job()
	bad.Language = "ruby" // no catálogo, sem imagem
	var ul domain.ErrUnsupportedLanguage
	if _, err := e.Run(context.Background(), bad); !errors.As(err, &ul) {
		t.Fatalf("sem imagem deveria ser E006: %v", err)
	}
	if l.count() != 0 {
		t.Fatal("job inválido não pode subir VM")
	}
	l.fail = errors.New("sem hypervisor")
	if _, err := e.Run(context.Background(), job()); err == nil {
		t.Fatal("sem VM deveria falhar")
	}
	l.fail = nil
	g.run = func(context.Context, string, protocol.Job) (protocol.Result, error) {
		return protocol.Result{}, errors.New("conexão resetada")
	}
	if _, err := e.Run(context.Background(), job()); err == nil {
		t.Fatal("erro do guest deveria subir")
	}
	g.run = func(ctx context.Context, _ string, _ protocol.Job) (protocol.Result, error) {
		<-ctx.Done()
		return protocol.Result{}, ctx.Err()
	}
	ctx, cancel := context.WithTimeout(context.Background(), 30*time.Millisecond)
	defer cancel()
	res, err := e.Run(ctx, job())
	if err == nil || res.TimedOut {
		t.Fatalf("cancelamento do chamador não é timeout do job: res=%+v err=%v", res, err)
	}
}
