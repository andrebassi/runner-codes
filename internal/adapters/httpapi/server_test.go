package httpapi

import (
	"context"
	"encoding/json"
	"errors"
	"net/http/httptest"
	"strings"
	"sync"
	"testing"
	"time"

	"runner.code/internal/domain"
)

// runner falso: guarda os jobs e devolve o que respond decidir.
type runner struct {
	mu      sync.Mutex
	jobs    []domain.Job
	respond func(domain.Job) (domain.Result, error)
}

// Run anota o job e delega.
func (r *runner) Run(_ context.Context, j domain.Job) (domain.Result, error) {
	r.mu.Lock()
	r.jobs = append(r.jobs, j)
	r.mu.Unlock()
	return r.respond(j)
}

// Known aceita só python.
func (r *runner) Known(l string) bool { return l == "python" }

// echo responde stdout = stdin, como `cat`.
func echo(j domain.Job) (domain.Result, error) {
	return domain.Result{Status: "success", Stdout: j.Stdin, Duration: 15 * time.Millisecond}, nil
}

// do faz a requisição no handler e devolve status e corpo.
func do(t *testing.T, s *Server, method, path, body string) (int, string) {
	t.Helper()
	req := httptest.NewRequest(method, path, strings.NewReader(body))
	req.Header.Set("Content-Type", "application/json")
	w := httptest.NewRecorder()
	s.Handler().ServeHTTP(w, req)
	return w.Code, w.Body.String()
}

// newServer monta o servidor com o runner falso.
func newServer(r *runner) *Server {
	return &Server{Runner: r, Version: "test", Languages: func() []LanguageInfo {
		return []LanguageInfo{{Name: "python", Image: "python:3.12-alpine", Ready: true}}
	}}
}

// TestHealthAndLanguages trava as rotas de leitura.
func TestHealthAndLanguages(t *testing.T) {
	s := newServer(&runner{respond: echo})
	if code, body := do(t, s, "GET", "/health", ""); code != 200 || !strings.Contains(body, `"version":"test"`) {
		t.Fatalf("health: %d %s", code, body)
	}
	if code, body := do(t, s, "GET", "/api/v1/languages", ""); code != 200 || !strings.Contains(body, `"ready":true`) {
		t.Fatalf("languages: %d %s", code, body)
	}
	if _, body := do(t, s, "GET", "/health", ""); strings.Contains(body, `"pool"`) {
		t.Fatalf("without Pool the field must not appear: %s", body)
	}
	s.Pool = func() map[string]int { return map[string]int{"python": 2} }
	if _, body := do(t, s, "GET", "/health", ""); !strings.Contains(body, `"pool":{"python":2}`) {
		t.Fatalf("health without the pool: %s", body)
	}
}

// TestRunContract trava o contrato de /run: stderr e exception null, executionTime em ms.
func TestRunContract(t *testing.T) {
	r := &runner{respond: echo}
	s := newServer(r)
	code, body := do(t, s, "POST", "/api/v1/run", `{"language":"python","stdin":"oi","files":[{"name":"main.py","content":"print(input())"}]}`)
	if code != 200 {
		t.Fatalf("status %d: %s", code, body)
	}
	var m map[string]interface{}
	json.Unmarshal([]byte(body), &m)
	if m["status"] != "success" || m["stdout"] != "oi" || m["stderr"] != nil || m["exception"] != nil || m["executionTime"].(float64) != 15 {
		t.Fatalf("contract broken: %s", body)
	}
	if _, has := m["stdin"]; has {
		t.Fatal("stdin is echoed only in batch mode")
	}
	if len(r.jobs) != 1 || r.jobs[0].Files[0].Name != "main.py" {
		t.Fatalf("wrong job: %+v", r.jobs)
	}
}

// TestRunBatch: stdin em lista roda uma vez por item e ecoa cada stdin.
func TestRunBatch(t *testing.T) {
	r := &runner{respond: echo}
	code, body := do(t, newServer(r), "POST", "/api/v1/run", `{"language":"python","stdin":["a",2],"files":[{"name":"m.py","content":"x"}]}`)
	var out []RunResponse
	json.Unmarshal([]byte(body), &out)
	if code != 200 || len(out) != 2 || out[0].Stdin != "a" || out[1].Stdout != "2" || len(r.jobs) != 2 {
		t.Fatalf("batch: %d %s", code, body)
	}
}

// TestExecute: formato interno com code solto, timeout em segundos e stderr preenchido.
func TestExecute(t *testing.T) {
	r := &runner{respond: func(j domain.Job) (domain.Result, error) {
		return domain.Result{Status: "failed", Stderr: "Traceback", Exception: "ZeroDivisionError"}, nil
	}}
	code, body := do(t, newServer(r), "POST", "/api/v1/execute", `{"language":"python","code":"1/0","timeout":7}`)
	if code != 200 || !strings.Contains(body, `"stderr":"Traceback"`) || !strings.Contains(body, `"exception":"ZeroDivisionError"`) {
		t.Fatalf("execute: %d %s", code, body)
	}
	if r.jobs[0].Timeout != 7*time.Second || r.jobs[0].Code != "1/0" {
		t.Fatalf("wrong job: %+v", r.jobs[0])
	}
}

// TestStatusCodes: 400 para JSON inválido e validação, 500 para infraestrutura,
// e status vazio do guest vira "failed".
func TestStatusCodes(t *testing.T) {
	bad := newServer(&runner{respond: echo})
	if code, _ := do(t, bad, "POST", "/api/v1/run", `{`); code != 400 {
		t.Fatalf("invalid JSON: %d", code)
	}
	if code, _ := do(t, bad, "POST", "/api/v1/execute", `{"code":"x"}`); code != 400 {
		t.Fatalf("missing language: %d", code)
	}
	val := newServer(&runner{respond: func(domain.Job) (domain.Result, error) {
		return domain.Result{}, domain.ValidationError{Err: domain.ErrUnsupportedLanguage{Name: "cobol"}}
	}})
	if code, body := do(t, val, "POST", "/api/v1/execute", `{"language":"cobol","code":"x"}`); code != 400 || !strings.Contains(body, "E006") {
		t.Fatalf("validation: %d %s", code, body)
	}
	infra := newServer(&runner{respond: func(domain.Job) (domain.Result, error) {
		return domain.Result{}, errors.New("get VM: no hypervisor")
	}})
	if code, body := do(t, infra, "POST", "/api/v1/execute", `{"language":"python","code":"x"}`); code != 500 || !strings.Contains(body, "no hypervisor") {
		t.Fatalf("infra: %d %s", code, body)
	}
	empty := newServer(&runner{respond: func(domain.Job) (domain.Result, error) { return domain.Result{}, nil }})
	if _, body := do(t, empty, "POST", "/api/v1/execute", `{"language":"python","code":"x"}`); !strings.Contains(body, `"status":"failed"`) {
		t.Fatalf("empty status: %s", body)
	}
}
