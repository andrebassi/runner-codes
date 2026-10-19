package httpapi

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"io"
	"net/http/httptest"
	"os"
	"strings"
	"testing"
	"time"

	"runner.code/internal/app"
	"runner.code/internal/domain"
)

// fakeMachines responde conforme err (se houver) e grava o que recebeu.
type fakeMachines struct {
	err      error
	files    map[string][]byte
	lastExec app.ExecRequest
	stopped  time.Duration
	forced   bool
	tail     int
}

// Create devolve a máquina pedida.
func (f *fakeMachines) Create(_ context.Context, m domain.Machine) (domain.Machine, error) {
	return m.WithDefaults(), f.err
}

// List devolve uma máquina ligada, ou nil (para conferir a lista vazia em JSON).
func (f *fakeMachines) List() ([]app.MachineStatus, error) {
	if f.err != nil {
		return nil, f.err
	}
	if f.files == nil {
		return nil, nil
	}
	return []app.MachineStatus{{Machine: domain.Machine{Name: "dev"}, State: app.StateRunning}}, nil
}

// Status devolve a máquina pedida ligada.
func (f *fakeMachines) Status(n string) (app.MachineStatus, error) {
	return app.MachineStatus{Machine: domain.Machine{Name: n}, State: app.StateRunning, PID: 7}, f.err
}

// Start devolve a máquina ligada.
func (f *fakeMachines) Start(_ context.Context, n string) (app.MachineStatus, error) {
	return f.Status(n)
}

// Stop grava o prazo recebido.
func (f *fakeMachines) Stop(_ context.Context, _ string, d time.Duration) error {
	f.stopped = d
	return f.err
}

// Delete grava o force.
func (f *fakeMachines) Delete(_ context.Context, _ string, force bool) error {
	f.forced = force
	return f.err
}

// Exec ecoa o argv em dois pedaços de stdout e sai com 3.
func (f *fakeMachines) Exec(_ context.Context, _ string, req app.ExecRequest, out func(string, string)) (domain.Result, error) {
	f.lastExec = req
	if f.err != nil {
		return domain.Result{}, f.err
	}
	if out != nil {
		out("stdout", "a\n")
		out("stderr", "e\n")
	}
	return domain.Result{ExitCode: 3, Stdout: strings.Join(req.Argv, " "), Duration: 9 * time.Millisecond}, nil
}

// CopyIn guarda o corpo.
func (f *fakeMachines) CopyIn(_ context.Context, _, p string, r io.Reader, _ os.FileMode) (int64, error) {
	if f.err != nil {
		return 0, f.err
	}
	b, _ := io.ReadAll(r)
	f.files[p] = b
	return int64(len(b)), nil
}

// CopyOut devolve o que foi guardado; ausente é NOT_FOUND.
func (f *fakeMachines) CopyOut(_ context.Context, _, p string, w io.Writer) (int64, error) {
	b, ok := f.files[p]
	if !ok {
		return 0, domain.MachineError{Code: domain.CodeNotFound, Msg: "no such file " + p}
	}
	n, err := w.Write(b)
	return int64(n), err
}

// Logs devolve um console fixo e grava o tail.
func (f *fakeMachines) Logs(_ string, tail int) (string, error) {
	f.tail = tail
	return "boot ok\n", f.err
}

// machineServer monta o servidor com o serviço falso.
func machineServer(f *fakeMachines) *Server {
	s := newServer(&runner{respond: echo})
	s.Machines = f
	return s
}

// errorCode lê o código do corpo de erro.
func errorCode(t *testing.T, body string) string {
	t.Helper()
	var e apiError
	if err := json.Unmarshal([]byte(body), &e); err != nil {
		t.Fatalf("error body is not JSON: %s", body)
	}
	return e.Error.Code
}

// TestMachineRoutesHappyPath percorre as rotas com o serviço respondendo bem.
func TestMachineRoutesHappyPath(t *testing.T) {
	f := &fakeMachines{files: map[string][]byte{}}
	s := machineServer(f)
	if code, body := do(t, s, "POST", "/api/v1/machines", `{"name":"dev","stack":"python","disk_gib":3}`); code != 201 ||
		!strings.Contains(body, `"state":"stopped"`) || !strings.Contains(body, `"disk_gib":3`) {
		t.Fatalf("create: %d %s", code, body)
	}
	if code, body := do(t, s, "GET", "/api/v1/machines", ""); code != 200 || !strings.Contains(body, `"name":"dev"`) {
		t.Fatalf("list: %d %s", code, body)
	}
	if code, body := do(t, s, "GET", "/api/v1/machines/dev", ""); code != 200 || !strings.Contains(body, `"pid":7`) {
		t.Fatalf("status: %d %s", code, body)
	}
	if code, _ := do(t, s, "POST", "/api/v1/machines/dev/start", ""); code != 200 {
		t.Fatalf("start: %d", code)
	}
	if code, _ := do(t, s, "POST", "/api/v1/machines/dev/stop?timeout=3", ""); code != 204 || f.stopped != 3*time.Second {
		t.Fatalf("stop: %d %v", code, f.stopped)
	}
	if code, _ := do(t, s, "POST", "/api/v1/machines/dev/stop", ""); code != 204 || f.stopped != app.DefaultStopTimeout {
		t.Fatalf("stop default: %v", f.stopped)
	}
	code, body := do(t, s, "POST", "/api/v1/machines/dev/exec", `{"command":["ls","-l"],"env":["A=1"],"workdir":"/w","stdin":"x","timeout":5}`)
	if code != 200 || !strings.Contains(body, `"exitCode":3`) || !strings.Contains(body, `"stdout":"ls -l"`) {
		t.Fatalf("exec: %d %s", code, body)
	}
	if f.lastExec.Cwd != "/w" || f.lastExec.Env[0] != "A=1" || f.lastExec.Stdin != "x" || f.lastExec.Timeout != 5*time.Second {
		t.Fatalf("exec request: %+v", f.lastExec)
	}
	code, body = do(t, s, "POST", "/api/v1/machines/dev/exec/stream", `{"command":["ls"]}`)
	evs := parseSSE(t, body)
	if code != 200 || len(evs) != 3 || evs[0] != (sseEvent{"stdout", `"a\n"`}) || evs[1].name != "stderr" ||
		evs[2].name != "exit" || !strings.Contains(evs[2].data, `"exitCode":3`) {
		t.Fatalf("stream: %d %+v", code, evs)
	}
	req := httptest.NewRequest("PUT", "/api/v1/machines/dev/files?path=/w/a.bin&mode=600", bytes.NewReader([]byte("payload")))
	w := httptest.NewRecorder()
	s.Handler().ServeHTTP(w, req)
	if w.Code != 200 || string(f.files["/w/a.bin"]) != "payload" || !strings.Contains(w.Body.String(), `"bytes":7`) {
		t.Fatalf("put: %d %s", w.Code, w.Body.String())
	}
	if code, body := do(t, s, "GET", "/api/v1/machines/dev/files?path=/w/a.bin", ""); code != 200 || body != "payload" {
		t.Fatalf("get: %d %q", code, body)
	}
	f.files["/empty"] = nil
	if code, body := do(t, s, "GET", "/api/v1/machines/dev/files?path=/empty", ""); code != 200 || body != "" {
		t.Fatalf("empty file: %d %q", code, body)
	}
	if code, body := do(t, s, "GET", "/api/v1/machines/dev/logs?tail=5", ""); code != 200 || body != "boot ok\n" || f.tail != 5 {
		t.Fatalf("logs: %d %q %d", code, body, f.tail)
	}
	if code, _ := do(t, s, "DELETE", "/api/v1/machines/dev?force=true", ""); code != 204 || !f.forced {
		t.Fatalf("delete: %d forced=%v", code, f.forced)
	}
	f.files = nil
	if _, body := do(t, s, "GET", "/api/v1/machines", ""); strings.TrimSpace(body) != "[]" {
		t.Fatalf("empty list must be []: %s", body)
	}
}

// TestMachineRoutesErrors: cada código estável vira o status HTTP certo, e pedidos
// malformados são CONFIG_ERROR (400).
func TestMachineRoutesErrors(t *testing.T) {
	cases := []struct {
		err    error
		status int
		code   string
	}{
		{domain.ErrMachineNotFound("dev"), 404, domain.CodeNotFound},
		{domain.ErrMachineExists("dev"), 409, domain.CodeConflict},
		{domain.ErrMachineState("dev", "is running"), 409, domain.CodeInvalidState},
		{domain.MachineError{Code: domain.CodeConfig, Msg: "bad"}, 400, domain.CodeConfig},
		{domain.ValidationError{Err: domain.ErrUnsupportedLanguage{Name: "cobol"}}, 400, domain.CodeConfig},
		{errors.New("no hypervisor"), 500, "RUNNER_ERROR"},
	}
	for _, c := range cases {
		s := machineServer(&fakeMachines{err: c.err, files: map[string][]byte{}})
		for _, r := range [][3]string{
			{"POST", "/api/v1/machines", `{"name":"dev","stack":"python"}`},
			{"GET", "/api/v1/machines", ""},
			{"GET", "/api/v1/machines/dev", ""},
			{"DELETE", "/api/v1/machines/dev", ""},
			{"POST", "/api/v1/machines/dev/start", ""},
			{"POST", "/api/v1/machines/dev/stop", ""},
			{"POST", "/api/v1/machines/dev/exec", `{"command":["ls"]}`},
			{"POST", "/api/v1/machines/dev/exec/stream", `{"command":["ls"]}`},
			{"PUT", "/api/v1/machines/dev/files?path=/x", "data"},
			{"GET", "/api/v1/machines/dev/logs", ""},
		} {
			code, body := do(t, s, r[0], r[1], r[2])
			if code != c.status || errorCode(t, body) != c.code {
				t.Fatalf("%s %s with %v: %d %s", r[0], r[1], c.err, code, body)
			}
		}
	}
	s := machineServer(&fakeMachines{files: map[string][]byte{}})
	for _, r := range [][3]string{
		{"POST", "/api/v1/machines", `{"stack":"python"}`},
		{"POST", "/api/v1/machines/dev/exec", `{"command":[]}`},
		{"POST", "/api/v1/machines/dev/exec/stream", `{`},
		{"POST", "/api/v1/machines/dev/stop?timeout=999", ""},
		{"POST", "/api/v1/machines/dev/stop?timeout=x", ""},
		{"PUT", "/api/v1/machines/dev/files?path=/x&mode=999", "d"},
	} {
		if code, body := do(t, s, r[0], r[1], r[2]); code != 400 || errorCode(t, body) != domain.CodeConfig {
			t.Fatalf("%s %s: %d %s", r[0], r[1], code, body)
		}
	}
	if code, body := do(t, s, "GET", "/api/v1/machines/dev/files?path=/missing", ""); code != 404 || errorCode(t, body) != domain.CodeNotFound {
		t.Fatalf("missing file: %d %s", code, body)
	}
	if code, _ := do(t, newServer(&runner{respond: echo}), "GET", "/api/v1/machines", ""); code != 404 {
		t.Fatal("without a machine service the routes must not exist")
	}
}

// failAfterFirst entrega um pedaço de saída e depois falha: erro no meio do streaming.
type failAfterFirst struct{ fakeMachines }

// Exec manda um pedaço e devolve erro.
func (f *failAfterFirst) Exec(_ context.Context, _ string, _ app.ExecRequest, out func(string, string)) (domain.Result, error) {
	out("stdout", "partial")
	return domain.Result{}, errors.New("guest connection reset")
}

// TestMachineStreamErrorAfterStart: depois do primeiro evento o erro vira `event: error`.
func TestMachineStreamErrorAfterStart(t *testing.T) {
	s := newServer(&runner{respond: echo})
	s.Machines = &failAfterFirst{fakeMachines{files: map[string][]byte{}}}
	code, body := do(t, s, "POST", "/api/v1/machines/dev/exec/stream", `{"command":["ls"]}`)
	evs := parseSSE(t, body)
	if code != 200 || len(evs) != 2 || evs[1].name != "error" || !strings.Contains(evs[1].data, "connection reset") {
		t.Fatalf("%d %+v", code, evs)
	}
}
