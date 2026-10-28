package httpapi

import (
	"context"
	"errors"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"runner.codes/internal/app"
	"runner.codes/internal/domain"
)

// exportFail é o erro que o Export falso devolve depois de começar a escrever (conexão cortada).
var exportFail = errors.New("disco sumiu no meio")

// Export escreve "RCMX" (ou falha no meio, para a máquina "off-quebrada").
func (f *fakeMachines) Export(_ context.Context, name string, w io.Writer) error {
	if f.err != nil {
		return f.err
	}
	_, _ = w.Write([]byte("RCMX"))
	if name == "off-quebrada" {
		return exportFail
	}
	return nil
}

// Import lê o corpo inteiro e devolve a máquina "off-" com o corpo no nome do stack.
func (f *fakeMachines) Import(_ context.Context, r io.Reader) (domain.Machine, error) {
	b, _ := io.ReadAll(r)
	if f.err != nil {
		return domain.Machine{}, f.err
	}
	return domain.Machine{Name: "off-importada", Stack: string(b)}, nil
}

// TestToken: com Token, /api/v1 exige o Bearer exato (sem, errado ou com prefixo 401); /health fica
// aberto; sem Token, tudo aberto como antes.
func TestToken(t *testing.T) {
	s := machineServer(&fakeMachines{files: map[string][]byte{}})
	s.Token = "segredo"
	h := s.Handler()
	call := func(path, auth string) int {
		r := httptest.NewRequest("GET", path, nil)
		if auth != "" {
			r.Header.Set("Authorization", auth)
		}
		w := httptest.NewRecorder()
		h.ServeHTTP(w, r)
		return w.Code
	}
	for name, c := range map[string]struct {
		auth string
		code int
	}{
		"sem": {"", 401}, "errado": {"Bearer outro", 401}, "prefixo": {"Bearer segred", 401}, "sem Bearer": {"segredo", 401},
		"certo": {"Bearer segredo", 200},
	} {
		if got := call("/api/v1/machines", c.auth); got != c.code {
			t.Errorf("%s: %d", name, got)
		}
	}
	if got := call("/health", ""); got != 200 {
		t.Fatalf("health: %d", got)
	}
	open := machineServer(&fakeMachines{files: map[string][]byte{}})
	if code, _ := do(t, open, "GET", "/api/v1/machines", ""); code != 200 {
		t.Fatalf("sem token: %d", code)
	}
}

// TestExportImportRoutes: export só da desligada (ligada 409, inexistente 404), em streaming com o
// tipo próprio; falha no meio corta a conexão; import devolve 201 com a máquina, e erro com o código.
func TestExportImportRoutes(t *testing.T) {
	f := &fakeMachines{files: map[string][]byte{}}
	s := machineServer(f)
	h := s.Handler()
	r := httptest.NewRequest("GET", "/api/v1/machines/off-dev/export", nil)
	w := httptest.NewRecorder()
	h.ServeHTTP(w, r)
	if w.Code != 200 || w.Body.String() != "RCMX" || w.Header().Get("Content-Type") != "application/x-runner-machine" ||
		!strings.Contains(w.Header().Get("Content-Disposition"), "off-dev.rcmx") {
		t.Fatalf("export: %d %q %v", w.Code, w.Body, w.Header())
	}
	if code, body := do(t, s, "GET", "/api/v1/machines/dev/export", ""); code != 409 || errorCode(t, body) != "INVALID_STATE" {
		t.Fatalf("export ligada: %d %s", code, body)
	}
	// Falha depois do 200: o corpo para no que já tinha saído (truncado), sem JSON de erro no meio.
	w = httptest.NewRecorder()
	h.ServeHTTP(w, httptest.NewRequest("GET", "/api/v1/machines/off-quebrada/export", nil))
	if w.Body.String() != "RCMX" {
		t.Fatalf("falha no meio: %q", w.Body)
	}
	if code, body := do(t, s, "POST", "/api/v1/machines/import", "corpo"); code != 201 || !strings.Contains(body, `"name":"off-importada"`) ||
		!strings.Contains(body, `"state":"stopped"`) {
		t.Fatalf("import: %d %s", code, body)
	}
	f.err = domain.ErrMachineExists("off-importada")
	if code, body := do(t, s, "POST", "/api/v1/machines/import", "x"); code != 409 || errorCode(t, body) != "CONFLICT" {
		t.Fatalf("import repetida: %d %s", code, body)
	}
	f.err = domain.ErrMachineNotFound("off-x")
	if code, _ := do(t, s, "GET", "/api/v1/machines/off-x/export", ""); code != 404 {
		t.Fatalf("export inexistente: %d", code)
	}
}

// TestImportStatusFails: máquina importada que some antes do Status responde o erro dele.
func TestImportStatusFails(t *testing.T) {
	s := newServer(&runner{respond: echo})
	s.Machines = &statusFails{fakeMachines: &fakeMachines{files: map[string][]byte{}}}
	if code, _ := do(t, s, "POST", "/api/v1/machines/import", "x"); code != 404 {
		t.Fatalf("%d", code)
	}
}

// statusFails importa bem e não acha a máquina depois.
type statusFails struct{ *fakeMachines }

// Status devolve NOT_FOUND sempre.
func (s *statusFails) Status(n string) (app.MachineStatus, error) {
	return app.MachineStatus{}, domain.ErrMachineNotFound(n)
}

// TestDisplayProxy: repassa método, caminho e corpo ao visualizador da porta da máquina, sem o
// token do rc; visualizador fora é 502; máquina desligada responde o erro do DisplayPort.
func TestDisplayProxy(t *testing.T) {
	var gotPath, gotAuth, gotMethod string
	viewer := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		gotPath, gotAuth, gotMethod = r.URL.Path+"?"+r.URL.RawQuery, r.Header.Get("Authorization"), r.Method
		_, _ = io.WriteString(w, "quadro")
	}))
	defer viewer.Close()
	f := &fakeMachines{files: map[string][]byte{}}
	s := machineServer(f)
	s.Token = "t"
	s.DisplayBase = func(port uint16) string {
		if port != 6080 {
			t.Errorf("porta %d", port)
		}
		return viewer.URL
	}
	h := s.Handler()
	r := httptest.NewRequest("POST", "/api/v1/machines/desk/display/frame?seq=3", strings.NewReader("{}"))
	r.Header.Set("Authorization", "Bearer t")
	w := httptest.NewRecorder()
	h.ServeHTTP(w, r)
	if w.Code != 200 || w.Body.String() != "quadro" || gotPath != "/frame?seq=3" || gotAuth != "" || gotMethod != "POST" || f.displayName != "desk" {
		t.Fatalf("proxy: %d %q path=%q auth=%q", w.Code, w.Body, gotPath, gotAuth)
	}
	s.DisplayBase = func(uint16) string { return "http://127.0.0.1:1" }
	r = httptest.NewRequest("GET", "/api/v1/machines/desk/display/", nil)
	r.Header.Set("Authorization", "Bearer t")
	w = httptest.NewRecorder()
	s.Handler().ServeHTTP(w, r)
	if w.Code != 502 {
		t.Fatalf("visualizador fora: %d", w.Code)
	}
	f.err = domain.ErrMachineState("desk", "is not running")
	r = httptest.NewRequest("GET", "/api/v1/machines/desk/display/", nil)
	r.Header.Set("Authorization", "Bearer t")
	w = httptest.NewRecorder()
	s.Handler().ServeHTTP(w, r)
	if w.Code != 409 {
		t.Fatalf("desligada: %d", w.Code)
	}
	if (&Server{}).displayBase(6100) != "http://127.0.0.1:6100" {
		t.Fatal("padrão do visualizador")
	}
}
