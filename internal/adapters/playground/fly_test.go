package playground

import (
	"context"
	"errors"
	"fmt"
	"io"
	"net"
	"net/http"
	"net/http/httptest"
	"strings"
	"sync"
	"testing"
	"time"

	"runner.codes/internal/adapters/flymachines"
	"runner.codes/internal/domain"
)

// fakeFly é a API de Machines em memória: guarda as máquinas e as chamadas, e falha na operação pedida.
type fakeFly struct {
	mu      sync.Mutex
	ms      []flymachines.Machine
	calls   []string
	fail    map[string]error
	lastEnv map[string]string
	n       int
}

// err devolve a falha programada para a operação.
func (f *fakeFly) err(op string) error {
	f.calls = append(f.calls, op)
	return f.fail[op]
}

// List devolve uma cópia das máquinas.
func (f *fakeFly) List(context.Context) ([]flymachines.Machine, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if err := f.err("list"); err != nil {
		return nil, err
	}
	return append([]flymachines.Machine(nil), f.ms...), nil
}

// Create acrescenta a máquina já ligada.
func (f *fakeFly) Create(_ context.Context, name string, env, _ map[string]string) (flymachines.Machine, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if err := f.err("create " + name); err != nil {
		return flymachines.Machine{}, err
	}
	f.n++
	m := flymachines.Machine{ID: fmt.Sprintf("m%d", f.n), Name: name, State: "started", PrivateIP: "fdaa::" + fmt.Sprint(f.n)}
	f.ms = append(f.ms, m)
	f.lastEnv = env
	return m, nil
}

// setState troca o estado da máquina pelo id.
func (f *fakeFly) setState(id, state string) {
	for i := range f.ms {
		if f.ms[i].ID == id {
			f.ms[i].State = state
		}
	}
}

// Start liga.
func (f *fakeFly) Start(_ context.Context, id string) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	if err := f.err("start " + id); err != nil {
		return err
	}
	f.setState(id, "started")
	return nil
}

// Stop desliga.
func (f *fakeFly) Stop(_ context.Context, id string) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	if err := f.err("stop " + id); err != nil {
		return err
	}
	f.setState(id, "stopped")
	return nil
}

// Wait só registra (ou falha).
func (f *fakeFly) Wait(_ context.Context, id, state string, _ int) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.err("wait " + id + " " + state)
}

// Delete tira a máquina.
func (f *fakeFly) Delete(_ context.Context, m flymachines.Machine) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	if err := f.err("delete " + m.ID); err != nil {
		return err
	}
	f.ms = remove(f.ms, m.ID)
	return nil
}

// flyRig monta o backend com o relógio controlado.
func flyRig(ms ...flymachines.Machine) (*FlyBackend, *fakeFly, *time.Time) {
	f := &fakeFly{ms: ms, fail: map[string]error{}, n: 100}
	now := time.Date(2026, 10, 20, 12, 0, 0, 0, time.UTC)
	b := &FlyBackend{API: f, Secret: []byte("s"), Max: 3, IdleStop: 30 * time.Minute, MaxAge: 24 * time.Hour,
		Host: "play.example.com", PortDomain: "example.com", Now: func() time.Time { return now },
		Dial: func(ctx context.Context, addr string) (net.Conn, error) {
			return (&net.Dialer{}).DialContext(ctx, "tcp", addr)
		}}
	return b, f, &now
}

// vm é uma máquina de visitante no estado dado, mexida pela última vez em at.
func vm(id, visitor, state string, at time.Time) flymachines.Machine {
	name, _ := domain.VisitorMachineName(visitor)
	return flymachines.Machine{ID: id, Name: name, State: state, PrivateIP: "fdaa::" + id, UpdatedAt: at}
}

// TestFlyEnsureCreates: visitante novo ganha máquina com o token, o host público e o endereço das portas na env.
func TestFlyEnsureCreates(t *testing.T) {
	b, f, _ := flyRig()
	name, err := b.Ensure(context.Background(), "visitorAAAAAAAAAAAAA")
	if err != nil {
		t.Fatal(err)
	}
	id := strings.TrimPrefix(name, domain.VisitorPrefix)
	if f.lastEnv["RC_TOKEN"] != Token(b.Secret, name) || f.lastEnv["RC_PUBLIC_HOST"] != "play.example.com" ||
		f.lastEnv["RC_PORT_URL"] != "https://%d-"+id+".example.com/" || f.lastEnv["RC_PORTS_TOKEN"] == f.lastEnv["RC_TOKEN"] {
		t.Fatalf("env %v", f.lastEnv)
	}
	if b.ip(name) != "fdaa::101" || f.calls[len(f.calls)-1] != "wait m101 started" {
		t.Fatalf("ip %q chamadas %v", b.ip(name), f.calls)
	}
	// Segunda vez: a mesma máquina, sem criar outra.
	b.Ensure(context.Background(), "visitorAAAAAAAAAAAAA")
	if len(f.ms) != 1 {
		t.Fatalf("máquinas %v", f.ms)
	}
}

// TestFlyEnsureStartsStopped: máquina parada é ligada; a que já está subindo só é esperada; a quebrada é recriada.
func TestFlyEnsureStartsStopped(t *testing.T) {
	at := time.Date(2026, 10, 20, 11, 0, 0, 0, time.UTC)
	b, f, _ := flyRig(vm("a", "visitorAAAAAAAAAAAAA", "stopped", at), vm("b", "visitorBBBBBBBBBBBBB", "starting", at),
		vm("c", "visitorCCCCCCCCCCCCC", "failed", at))
	ctx := context.Background()
	b.Ensure(ctx, "visitorAAAAAAAAAAAAA")
	b.Ensure(ctx, "visitorBBBBBBBBBBBBB")
	b.Ensure(ctx, "visitorCCCCCCCCCCCCC")
	got := strings.Join(f.calls, "|")
	for _, want := range []string{"start a|wait a started", "list|wait b started", "delete c|create pg-"} {
		if !strings.Contains(got, want) {
			t.Fatalf("faltou %q em %s", want, got)
		}
	}
	if strings.Contains(got, "start b") {
		t.Fatal("religou máquina que já subia")
	}
}

// TestFlyEnsureFull: no teto, apaga a parada usada há mais tempo; com todas ligadas, recusa com ErrFull.
func TestFlyEnsureFull(t *testing.T) {
	old := time.Date(2026, 10, 20, 9, 0, 0, 0, time.UTC)
	newer := old.Add(time.Hour)
	b, f, _ := flyRig(vm("a", "visitorAAAAAAAAAAAAA", "stopped", newer), vm("b", "visitorBBBBBBBBBBBBB", "stopped", old),
		vm("c", "visitorCCCCCCCCCCCCC", "started", old), flymachines.Machine{ID: "x", Name: "other", State: "started"})
	if _, err := b.Ensure(context.Background(), "visitorDDDDDDDDDDDDD"); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(strings.Join(f.calls, "|"), "delete b|create") {
		t.Fatalf("não apagou a mais antiga: %v", f.calls)
	}
	b2, _, _ := flyRig(vm("a", "visitorAAAAAAAAAAAAA", "started", old), vm("b", "visitorBBBBBBBBBBBBB", "starting", old),
		vm("c", "visitorCCCCCCCCCCCCC", "started", old))
	if _, err := b2.Ensure(context.Background(), "visitorDDDDDDDDDDDDD"); !errors.Is(err, ErrFull) {
		t.Fatalf("lotado: %v", err)
	}
}

// TestFlyEnsureErrors: visitante inválido e falha em cada chamada voltam como erro.
func TestFlyEnsureErrors(t *testing.T) {
	ctx := context.Background()
	b, _, _ := flyRig()
	var ve domain.ValidationError
	if _, err := b.Ensure(ctx, "../x"); !errors.As(err, &ve) {
		t.Fatalf("visitante inválido: %v", err)
	}
	at := time.Date(2026, 10, 20, 9, 0, 0, 0, time.UTC)
	boom := errors.New("boom")
	cases := []struct {
		op string
		ms []flymachines.Machine
	}{
		{"list", nil},
		{"start a", []flymachines.Machine{vm("a", "visitorAAAAAAAAAAAAA", "stopped", at)}},
		{"wait a started", []flymachines.Machine{vm("a", "visitorAAAAAAAAAAAAA", "started", at)}},
		{"delete a", []flymachines.Machine{vm("a", "visitorAAAAAAAAAAAAA", "failed", at)}},
		{"delete b", []flymachines.Machine{vm("b", "visitorBBBBBBBBBBBBB", "stopped", at), vm("c", "visitorCCCCCCCCCCCCC", "stopped", at), vm("d", "visitorDDDDDDDDDDDDD", "stopped", at)}},
	}
	for _, c := range cases {
		b, f, _ := flyRig(c.ms...)
		f.fail[c.op] = boom
		if _, err := b.Ensure(ctx, "visitorAAAAAAAAAAAAA"); !errors.Is(err, boom) {
			t.Errorf("%s: %v", c.op, err)
		}
	}
	b, f, _ := flyRig()
	name, _ := domain.VisitorMachineName("visitorAAAAAAAAAAAAA")
	f.fail["create "+name] = boom
	if _, err := b.Ensure(ctx, "visitorAAAAAAAAAAAAA"); !errors.Is(err, boom) {
		t.Fatalf("create: %v", err)
	}
}

// TestFlyReap: desliga a ligada ociosa, apaga a velha, deixa a usada há pouco; Touch adia o desligamento.
func TestFlyReap(t *testing.T) {
	b, f, now := flyRig()
	ctx := context.Background()
	nameA, _ := b.Ensure(ctx, "visitorAAAAAAAAAAAAA")
	nameB, _ := b.Ensure(ctx, "visitorBBBBBBBBBBBBB")
	f.ms = append(f.ms, vm("old", "visitorCCCCCCCCCCCCC", "stopped", now.Add(-48*time.Hour)))
	*now = now.Add(20 * time.Minute)
	b.Touch(nameB)
	b.Touch("pg-desconhecid")
	*now = now.Add(15 * time.Minute)
	done := strings.Join(b.Reap(ctx), "|")
	if done != "stopped "+nameA+"|deleted pg-"+strings.TrimPrefix(vm("", "visitorCCCCCCCCCCCCC", "", *now).Name, "pg-") {
		t.Fatalf("reap: %s", done)
	}
	if b.ip(nameA) != "" || b.ip(nameB) == "" || b.Terminal(nameA) != nil {
		t.Fatal("estado depois do reap")
	}
	f.fail["list"] = errors.New("boom")
	if got := b.Reap(ctx); len(got) != 1 || !strings.Contains(got[0], "boom") {
		t.Fatalf("reap com erro: %v", got)
	}
}

// TestFlyTerminalAndPort: o terminal e as portas vão ao rc-visitor com o Host público; a porta leva número e token
// próprios e o que o cliente mandou nesses cabeçalhos é descartado.
func TestFlyTerminalAndPort(t *testing.T) {
	visitor := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "%s %s port=%s tok=%s", r.Host, r.URL.Path, r.Header.Get(HeaderPort), r.Header.Get(HeaderToken))
	}))
	defer visitor.Close()
	b, _, _ := flyRig()
	var dialed []string
	b.Dial = func(ctx context.Context, addr string) (net.Conn, error) {
		dialed = append(dialed, addr)
		return (&net.Dialer{}).DialContext(ctx, "tcp", visitor.Listener.Addr().String())
	}
	name, _ := b.Ensure(context.Background(), "visitorAAAAAAAAAAAAA")
	call := func(h http.Handler, host, path string) (int, string) {
		req := httptest.NewRequest("GET", "http://"+host+path, nil)
		req.Header.Set(HeaderToken, "forjado")
		rec := httptest.NewRecorder()
		h.ServeHTTP(rec, req)
		return rec.Code, rec.Body.String()
	}
	if code, body := call(b.Terminal(name), "play.example.com", "/state"); code != 200 || body != "play.example.com /state port= tok=" {
		t.Fatalf("terminal: %d %q", code, body)
	}
	code, body := call(b.Port(name, 3000), "3000-x.example.com", "/docs")
	if code != 200 || body != "3000-x.example.com /docs port=3000 tok="+portsToken(b.Secret, name) {
		t.Fatalf("porta: %d %q", code, body)
	}
	if dialed[0] != "[fdaa::101]:7681" || dialed[1] != "[fdaa::101]:7682" {
		t.Fatalf("discou %v", dialed)
	}
	if code, _ := call(b.Port("pg-0000000000", 3000), "x", "/"); code != http.StatusBadGateway {
		t.Fatalf("máquina desligada: %d", code)
	}
}

// TestFlyDialReady: o terminal espera o rc-visitor escutar até o prazo e, passado o prazo, responde 502.
func TestFlyDialReady(t *testing.T) {
	b, _, _ := flyRig()
	b.ReadyTimeout = 600 * time.Millisecond
	tries := 0
	ln, _ := net.Listen("tcp", "127.0.0.1:0")
	defer ln.Close()
	go http.Serve(ln, http.HandlerFunc(func(w http.ResponseWriter, _ *http.Request) { io.WriteString(w, "ok") }))
	b.Dial = func(ctx context.Context, _ string) (net.Conn, error) {
		tries++
		if tries < 3 {
			return nil, errors.New("connection refused")
		}
		return (&net.Dialer{}).DialContext(ctx, "tcp", ln.Addr().String())
	}
	name, _ := b.Ensure(context.Background(), "visitorAAAAAAAAAAAAA")
	rec := httptest.NewRecorder()
	b.Terminal(name).ServeHTTP(rec, httptest.NewRequest("GET", "/", nil))
	if rec.Code != 200 || tries != 3 {
		t.Fatalf("pronto: %d depois de %d tentativas", rec.Code, tries)
	}
	b.Dial = func(context.Context, string) (net.Conn, error) { return nil, errors.New("connection refused") }
	b.ReadyTimeout = 300 * time.Millisecond
	rec = httptest.NewRecorder()
	b.Terminal(name).ServeHTTP(rec, httptest.NewRequest("GET", "/", nil))
	if rec.Code != http.StatusBadGateway || !strings.Contains(rec.Body.String(), "starting") {
		t.Fatalf("prazo: %d %q", rec.Code, rec.Body.String())
	}
}
