package httpapi

import (
	"context"
	"errors"
	"fmt"
	"net/http/httptest"
	"strings"
	"testing"
	"time"

	"runner.codes/internal/app"
	"runner.codes/internal/domain"
)

// persistMachines é o fakeMachines com disco próprio por máquina: soltar registra, e o estado da preempção é o
// plantado.
type persistMachines struct {
	*fakeMachines
	enabled    bool
	released   []string
	relErr     error
	preempting bool
	drained    []string
}

// Release registra a máquina solta (ou devolve o erro plantado).
func (p *persistMachines) Release(_ context.Context, name string) error {
	p.released = append(p.released, name)
	return p.relErr
}

// Preemption devolve o estado plantado.
func (p *persistMachines) Preemption() (bool, []string) { return p.preempting, p.drained }

// Adopt devolve a máquina adotada (ou o erro plantado).
func (p *persistMachines) Adopt(_ context.Context, name string) (app.MachineStatus, error) {
	if p.relErr != nil {
		return app.MachineStatus{}, p.relErr
	}
	return app.MachineStatus{Machine: domain.Machine{Name: name}, State: app.StateStopped}, nil
}

// HasPersistentDisks diz se o recurso está ligado.
func (p *persistMachines) HasPersistentDisks() bool { return p.enabled }

// TestReleaseRoute: 204 solto; 501 no host sem o recurso (fake sem a interface, ou com ela desligada); o erro de
// máquina sai com o status do código estável (sem disco próprio → 400, desconhecida → 404).
func TestReleaseRoute(t *testing.T) {
	p := &persistMachines{fakeMachines: &fakeMachines{files: map[string][]byte{}}, enabled: true}
	s := &Server{Machines: p}
	if code, body := do(t, s, "POST", "/api/v1/machines/dev/release", ""); code != 204 || len(p.released) != 1 || p.released[0] != "dev" {
		t.Fatalf("soltar: %d %s %v", code, body, p.released)
	}
	p.relErr = app.ErrNoPersistentDisk
	if code, _ := do(t, s, "POST", "/api/v1/machines/dev/release", ""); code != 400 {
		t.Fatalf("sem disco próprio: %d", code)
	}
	p.relErr = domain.ErrMachineNotFound("x")
	if code, _ := do(t, s, "POST", "/api/v1/machines/x/release", ""); code != 404 {
		t.Fatalf("desconhecida: %d", code)
	}
	p.enabled = false
	if code, body := do(t, s, "POST", "/api/v1/machines/dev/release", ""); code != 501 || !strings.Contains(body, "not enabled") {
		t.Fatalf("desligado: %d %s", code, body)
	}
	if code, _ := do(t, machineServer(&fakeMachines{files: map[string][]byte{}}), "POST", "/api/v1/machines/dev/release", ""); code != 501 {
		t.Fatalf("sem a interface: %d", code)
	}
}

// TestHostPreemption: o host diz se tem disco próprio por máquina, se está em preempção e quem estava ligada no aviso
// — é por aqui que o orquestrador sabe o que religar em outro host.
func TestHostPreemption(t *testing.T) {
	measured := func() (domain.Host, error) { return domain.Host{KVM: true}, nil }
	p := &persistMachines{fakeMachines: &fakeMachines{files: map[string][]byte{}}, enabled: true, preempting: true, drained: []string{"a", "b"}}
	code, body := do(t, &Server{Host: measured, Machines: p}, "GET", "/api/v1/host", "")
	if code != 200 || !strings.Contains(body, `"persistent_disks":true`) || !strings.Contains(body, `"preempting":true`) ||
		!strings.Contains(body, `"drained":["a","b"]`) {
		t.Fatalf("host: %d %s", code, body)
	}
	code, body = do(t, &Server{Host: measured, Machines: &fakeMachines{files: map[string][]byte{}}}, "GET", "/api/v1/host", "")
	if code != 200 || !strings.Contains(body, `"persistent_disks":false`) || !strings.Contains(body, `"preempting":false`) {
		t.Fatalf("sem o recurso: %d %s", code, body)
	}
}

// TestAdoptRoute: 200 com a máquina adotada; 404 o disco não existe; 501 sem o recurso.
func TestAdoptRoute(t *testing.T) {
	p := &persistMachines{fakeMachines: &fakeMachines{files: map[string][]byte{}}, enabled: true}
	s := &Server{Machines: p}
	if code, body := do(t, s, "POST", "/api/v1/machines/dev/adopt", ""); code != 200 || !strings.Contains(body, `"dev"`) {
		t.Fatalf("adotar: %d %s", code, body)
	}
	p.relErr = domain.ErrMachineNotFound("dev")
	if code, _ := do(t, s, "POST", "/api/v1/machines/dev/adopt", ""); code != 404 {
		t.Fatalf("inexistente: %d", code)
	}
	if code, _ := do(t, machineServer(&fakeMachines{files: map[string][]byte{}}), "POST", "/api/v1/machines/dev/adopt", ""); code != 501 {
		t.Fatalf("sem a interface: %d", code)
	}
}

// suspendMachines é o fakeMachines que suspende: anota o pedido com o prazo e devolve o erro plantado.
type suspendMachines struct {
	*fakeMachines
	asked   []string
	timeout time.Duration
	err     error
}

// Suspend anota e devolve o erro plantado.
func (s *suspendMachines) Suspend(_ context.Context, name string, timeout time.Duration) error {
	s.asked, s.timeout = append(s.asked, name), timeout
	return s.err
}

// TestSuspendRoute: 204 suspensa, com o ?timeout repassado; timeout fora de [1, 120] é 400 sem chegar ao serviço;
// host cuja libkrun não sabe (ErrSuspendUnsupported) ou fake sem a interface é 501; máquina parada é 409.
func TestSuspendRoute(t *testing.T) {
	sm := &suspendMachines{fakeMachines: &fakeMachines{files: map[string][]byte{}}}
	s := &Server{Machines: sm}
	if code, body := do(t, s, "POST", "/api/v1/machines/desk/suspend?timeout=15", ""); code != 204 || len(sm.asked) != 1 || sm.timeout != 15*time.Second {
		t.Fatalf("suspender: %d %s %v %v", code, body, sm.asked, sm.timeout)
	}
	if code, _ := do(t, s, "POST", "/api/v1/machines/desk/suspend", ""); code != 204 || sm.timeout != 0 {
		t.Fatalf("sem timeout: %d %v", code, sm.timeout)
	}
	for _, bad := range []string{"0", "121", "x"} {
		if code, _ := do(t, s, "POST", "/api/v1/machines/desk/suspend?timeout="+bad, ""); code != 400 || len(sm.asked) != 2 {
			t.Fatalf("timeout %s: %d", bad, code)
		}
	}
	sm.err = app.ErrSuspendUnsupported
	if code, body := do(t, s, "POST", "/api/v1/machines/desk/suspend", ""); code != 501 || !strings.Contains(body, "cannot suspend") {
		t.Fatalf("sem suporte: %d %s", code, body)
	}
	sm.err = domain.ErrMachineState("desk", "is not running")
	if code, _ := do(t, s, "POST", "/api/v1/machines/desk/suspend", ""); code != 409 {
		t.Fatalf("parada: %d", code)
	}
	if code, _ := do(t, machineServer(&fakeMachines{files: map[string][]byte{}}), "POST", "/api/v1/machines/desk/suspend", ""); code != 501 {
		t.Fatalf("sem a interface: %d", code)
	}
}

// goldenMachines é o fakeMachines que sabe de dourado: cria "restaurada" do snapshot plantado (ou devolve o erro
// plantado) e anota as publicações.
type goldenMachines struct {
	*fakeMachines
	fromErr   error
	fromGold  []string
	published []string
	pubErr    error
}

// CreateFromGolden devolve a máquina pedida ou o erro plantado, e anota o snapshot.
func (g *goldenMachines) CreateFromGolden(_ context.Context, mc domain.Machine, snapshot string) (domain.Machine, error) {
	g.fromGold = append(g.fromGold, mc.Name+"<-"+snapshot)
	return mc, g.fromErr
}

// PublishGolden anota e devolve o erro plantado.
func (g *goldenMachines) PublishGolden(_ context.Context, name, snapshot string) error {
	g.published = append(g.published, name+"->"+snapshot)
	return g.pubErr
}

// doHeader é o do com o cabeçalho X-RC-Golden da resposta.
func doHeader(t *testing.T, s *Server, method, path, body string) (int, string) {
	t.Helper()
	req := httptest.NewRequest(method, path, strings.NewReader(body))
	req.Header.Set("Content-Type", "application/json")
	w := httptest.NewRecorder()
	s.Handler().ServeHTTP(w, req)
	return w.Code, w.Header().Get(goldenHeader)
}

// TestCreateFromGoldenRoute: com ?golden= a máquina nasce do dourado ("restored"); dourado ausente ou que não serve cai
// no create normal na mesma chamada ("cold"); outro erro do dourado sobe sem criar; nome de snapshot inválido é 400;
// serviço sem o recurso cria do jeito normal; sem ?golden=, nada muda.
func TestCreateFromGoldenRoute(t *testing.T) {
	gm := &goldenMachines{fakeMachines: &fakeMachines{files: map[string][]byte{}}}
	s := &Server{Machines: gm}
	body := `{"name":"desk","stack":"python"}`
	if code, h := doHeader(t, s, "POST", "/api/v1/machines?golden=sbot-golden-trial-abc", body); code != 201 || h != "restored" ||
		len(gm.fromGold) != 1 || gm.fromGold[0] != "desk<-sbot-golden-trial-abc" || gm.created.Name != "" {
		t.Fatalf("do dourado: %d %q %v criada=%q", code, h, gm.fromGold, gm.created.Name)
	}
	gm.fromErr = fmt.Errorf("%w: não serve", app.ErrNoGolden)
	if code, h := doHeader(t, s, "POST", "/api/v1/machines?golden=sbot-golden-trial-abc", body); code != 201 || h != "cold" || gm.created.Name != "desk" {
		t.Fatalf("recuo: %d %q criada=%q", code, h, gm.created.Name)
	}
	gm.created, gm.fromErr = domain.Machine{}, errors.New("attach")
	if code, _ := doHeader(t, s, "POST", "/api/v1/machines?golden=sbot-golden-trial-abc", body); code != 500 || gm.created.Name != "" {
		t.Fatalf("erro do dourado: %d criada=%q", code, gm.created.Name)
	}
	if code, _ := doHeader(t, s, "POST", "/api/v1/machines?golden=Bad_Name", body); code != 400 {
		t.Fatalf("nome inválido: %d", code)
	}
	plain := machineServer(&fakeMachines{files: map[string][]byte{}})
	if code, h := doHeader(t, plain, "POST", "/api/v1/machines?golden=sbot-golden-trial-abc", body); code != 201 || h != "cold" {
		t.Fatalf("sem o recurso: %d %q", code, h)
	}
	if code, h := doHeader(t, plain, "POST", "/api/v1/machines", body); code != 201 || h != "" {
		t.Fatalf("sem ?golden=: %d %q", code, h)
	}
}

// TestPublishGoldenRoute: 204 publicado; corpo sem snapshot, com campo a mais ou com nome inválido é 400; máquina não
// suspensa é 409; host sem disco que saiba de snapshot é 501, pela interface ou pelo ErrNoGolden.
func TestPublishGoldenRoute(t *testing.T) {
	gm := &goldenMachines{fakeMachines: &fakeMachines{files: map[string][]byte{}}}
	s := &Server{Machines: gm}
	if code, body := do(t, s, "POST", "/api/v1/machines/mold/golden", `{"snapshot":"sbot-golden-desk-abc"}`); code != 204 || gm.published[0] != "mold->sbot-golden-desk-abc" {
		t.Fatalf("publicar: %d %s %v", code, body, gm.published)
	}
	for _, bad := range []string{`{}`, `{"snapshot":"X"}`, `{"snapshot":"ok","x":1}`, `nada`} {
		if code, _ := do(t, s, "POST", "/api/v1/machines/mold/golden", bad); code != 400 || len(gm.published) != 1 {
			t.Fatalf("corpo %s: %d", bad, code)
		}
	}
	gm.pubErr = domain.ErrMachineState("mold", "is not suspended")
	if code, _ := do(t, s, "POST", "/api/v1/machines/mold/golden", `{"snapshot":"sbot-golden-desk-abc"}`); code != 409 {
		t.Fatalf("não suspensa: %d", code)
	}
	gm.pubErr = app.ErrNoGolden
	if code, _ := do(t, s, "POST", "/api/v1/machines/mold/golden", `{"snapshot":"sbot-golden-desk-abc"}`); code != 501 {
		t.Fatalf("sem o recurso: %d", code)
	}
	if code, _ := do(t, machineServer(&fakeMachines{files: map[string][]byte{}}), "POST", "/api/v1/machines/mold/golden", `{"snapshot":"a"}`); code != 501 {
		t.Fatalf("sem a interface: %d", code)
	}
}
