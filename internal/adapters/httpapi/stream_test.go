package httpapi

import (
	"encoding/json"
	"errors"
	"net/http/httptest"
	"strings"
	"testing"
	"time"

	"runner.codes/internal/domain"
)

// sseEvent é um evento lido da resposta: nome e o JSON cru do data.
type sseEvent struct {
	name, data string
}

// parseSSE separa a resposta em eventos (blocos separados por linha em branco).
func parseSSE(t *testing.T, body string) []sseEvent {
	t.Helper()
	var out []sseEvent
	for _, block := range strings.Split(strings.TrimSpace(body), "\n\n") {
		var ev sseEvent
		for _, line := range strings.Split(block, "\n") {
			switch {
			case strings.HasPrefix(line, "event: "):
				ev.name = strings.TrimPrefix(line, "event: ")
			case strings.HasPrefix(line, "data: "):
				ev.data = strings.TrimPrefix(line, "data: ")
			default:
				t.Fatalf("unexpected SSE line %q in %q", line, body)
			}
		}
		out = append(out, ev)
	}
	return out
}

// TestExecuteStreamSSE trava o formato da rota: Content-Type de SSE, pedaços na ordem
// como string JSON (a quebra de linha vai escapada, senão quebraria o SSE) e o `result`
// por último sem repetir o stdout.
func TestExecuteStreamSSE(t *testing.T) {
	r := &runner{
		chunks: [][2]string{{"stdout", "line 1\n"}, {"stderr", "warn\n"}, {"stdout", "line 2\n"}},
		respond: func(domain.Job) (domain.Result, error) {
			return domain.Result{Status: "success", Stdout: "line 1\nline 2\n", ExitCode: 3, Duration: 42 * time.Millisecond}, nil
		},
	}
	req := httptest.NewRequest("POST", "/api/v1/execute/stream", strings.NewReader(`{"language":"python","code":"x","timeout":5}`))
	req.Header.Set("Content-Type", "application/json")
	w := httptest.NewRecorder()
	newServer(r).Handler().ServeHTTP(w, req)
	if w.Code != 200 || w.Header().Get("Content-Type") != "text/event-stream" || w.Header().Get("X-Accel-Buffering") != "no" {
		t.Fatalf("status %d headers %v", w.Code, w.Header())
	}
	evs := parseSSE(t, w.Body.String())
	if len(evs) != 4 {
		t.Fatalf("expected 3 chunks + result: %+v", evs)
	}
	want := []sseEvent{{"stdout", `"line 1\n"`}, {"stderr", `"warn\n"`}, {"stdout", `"line 2\n"`}}
	for i, w := range want {
		if evs[i] != w {
			t.Fatalf("event %d = %+v, want %+v", i, evs[i], w)
		}
	}
	if evs[3].name != "result" {
		t.Fatalf("last event must be result: %+v", evs[3])
	}
	var res StreamResult
	if err := json.Unmarshal([]byte(evs[3].data), &res); err != nil {
		t.Fatal(err)
	}
	if res.Status != "success" || res.ExitCode != 3 || res.ExecutionTime != 42 || res.Exception != nil {
		t.Fatalf("result %+v", res)
	}
	if strings.Contains(evs[3].data, "stdout") {
		t.Fatalf("result must not repeat the output: %s", evs[3].data)
	}
	if len(r.jobs) != 1 || r.jobs[0].Timeout != 5*time.Second || r.jobs[0].Code != "x" {
		t.Fatalf("job converted wrong: %+v", r.jobs)
	}
}

// TestExecuteStreamErrors: antes do primeiro evento a resposta é JSON com o status do
// /execute (400 validação, 400 lote, 500 infraestrutura); depois, vira evento `error`.
func TestExecuteStreamErrors(t *testing.T) {
	s := newServer(&runner{respond: echo})
	if code, _ := do(t, s, "POST", "/api/v1/execute/stream", `{`); code != 400 {
		t.Fatalf("invalid JSON: %d", code)
	}
	if code, body := do(t, s, "POST", "/api/v1/execute/stream", `{"language":"cobol","code":"x"}`); code != 400 || !strings.Contains(body, "E006") {
		t.Fatalf("validation: %d %s", code, body)
	}
	if code, body := do(t, s, "POST", "/api/v1/execute/stream", `{"language":"python","code":"x","stdin":["a","b"]}`); code != 400 || !strings.Contains(body, "batch") {
		t.Fatalf("batch: %d %s", code, body)
	}
	infra := newServer(&runner{streamErr: errors.New("get VM: no hypervisor"), respond: echo})
	if code, body := do(t, infra, "POST", "/api/v1/execute/stream", `{"language":"python","code":"x"}`); code != 500 || !strings.Contains(body, "no hypervisor") {
		t.Fatalf("infra before start: %d %s", code, body)
	}
	mid := newServer(&runner{chunks: [][2]string{{"stdout", "a"}}, streamErr: errors.New("run in guest: connection reset"), respond: echo})
	code, body := do(t, mid, "POST", "/api/v1/execute/stream", `{"language":"python","code":"x"}`)
	evs := parseSSE(t, body)
	if code != 200 || len(evs) != 2 || evs[1].name != "error" || !strings.Contains(evs[1].data, "connection reset") {
		t.Fatalf("error after start must be an event: %d %+v", code, evs)
	}
	empty := newServer(&runner{respond: func(domain.Job) (domain.Result, error) { return domain.Result{}, nil }})
	if _, body := do(t, empty, "POST", "/api/v1/execute/stream", `{"language":"python","code":"x"}`); !strings.Contains(body, `"status":"failed"`) {
		t.Fatalf("empty status: %s", body)
	}
}
