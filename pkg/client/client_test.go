package client

import (
	"context"
	"encoding/json"
	"errors"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
	"time"
)

// call é o que o servidor falso recebeu: método, caminho com query e corpo.
type call struct {
	method, uri, body, ctype string
}

// fakeAPI responde cada rota com a resposta registrada e guarda as chamadas.
type fakeAPI struct {
	calls  []call
	routes map[string]func(w http.ResponseWriter)
}

// serve registra a chamada e devolve a resposta da rota "MÉTODO caminho" (sem query).
func (f *fakeAPI) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	b, _ := io.ReadAll(r.Body)
	f.calls = append(f.calls, call{r.Method, r.URL.RequestURI(), string(b), r.Header.Get("Content-Type")})
	if h, ok := f.routes[r.Method+" "+r.URL.Path]; ok {
		h(w)
		return
	}
	w.WriteHeader(http.StatusTeapot)
}

// reply devolve um handler que escreve status e corpo.
func reply(status int, body string) func(w http.ResponseWriter) {
	return func(w http.ResponseWriter) {
		w.WriteHeader(status)
		io.WriteString(w, body)
	}
}

// newFake sobe o servidor falso e um cliente apontado para ele (com barra no fim, que o New tira).
func newFake(t *testing.T, routes map[string]func(w http.ResponseWriter)) (*Client, *fakeAPI) {
	t.Helper()
	f := &fakeAPI{routes: routes}
	s := httptest.NewServer(f)
	t.Cleanup(s.Close)
	return New(s.URL + "/"), f
}

const machineJSON = `{"name":"dev","stack":"python","cpus":4,"memory_mib":8192,"disk_gib":10,"created_at":"2026-10-19T00:00:00Z",` +
	`"state":"running","pid":42,"network":{"name":"lab","address":"100.98.0.10"}}`

// TestMachineLifecycle: cada método chama a rota certa, com o corpo e a query certos, e
// decodifica a resposta; nome com caractere especial vai escapado no caminho.
func TestMachineLifecycle(t *testing.T) {
	c, f := newFake(t, map[string]func(http.ResponseWriter){
		"POST /api/v1/machines":            reply(201, machineJSON),
		"GET /api/v1/machines":             reply(200, "["+machineJSON+"]"),
		"GET /api/v1/machines/dev":         reply(200, machineJSON),
		"DELETE /api/v1/machines/dev":      reply(204, ""),
		"POST /api/v1/machines/dev/start":  reply(200, machineJSON),
		"POST /api/v1/machines/dev/stop":   reply(204, ""),
		"POST /api/v1/machines/dev/exec":   reply(200, `{"exitCode":3,"stdout":"o","stderr":"e","timedOut":false,"executionTime":7}`),
		"PUT /api/v1/machines/dev/files":   reply(200, `{"path":"/a","bytes":2}`),
		"GET /api/v1/machines/dev/files":   reply(200, "hi"),
		"GET /api/v1/machines/dev/logs":    reply(200, "boot\n"),
		"PUT /api/v1/machines/dev/egress":  reply(200, machineJSON),
		"POST /api/v1/machines/dev/pause":  reply(204, ""),
		"POST /api/v1/machines/dev/resume": reply(204, ""),
		"POST /api/v1/machines/dev/branch": reply(201, strings.Replace(machineJSON, `"dev"`, `"kid"`, 1)),
		"GET /api/v1/machines/a/b":         reply(200, machineJSON),
	})
	ctx := context.Background()
	m, err := c.CreateMachine(ctx, MachineSpec{Name: "dev", Stack: "python", Network: "lab", Egress: &Egress{AllowHosts: []string{"pypi.org"}}})
	if err != nil || m.State != "running" || m.Network.Address != "100.98.0.10" || m.CPUs != 4 {
		t.Fatalf("create: %+v %v", m, err)
	}
	var sent map[string]any
	json.Unmarshal([]byte(f.calls[0].body), &sent)
	if sent["network"] != "lab" || sent["stack"] != "python" || f.calls[0].ctype != "application/json" {
		t.Fatalf("create body: %s", f.calls[0].body)
	}
	if _, ok := sent["cpus"]; ok {
		t.Fatal("empty fields must not be sent: the server would read them as an override")
	}
	if ms, err := c.Machines(ctx); err != nil || len(ms) != 1 || ms[0].PID != 42 {
		t.Fatalf("list: %+v %v", ms, err)
	}
	if m, err := c.Machine(ctx, "dev"); err != nil || m.Name != "dev" {
		t.Fatalf("get: %v", err)
	}
	// "/" no nome: sem escape, viraria outro caminho (o net/http já escapa espaço sozinho, e por
	// isso espaço não serve de prova).
	if _, err := c.Machine(ctx, "a/b"); err != nil || f.calls[len(f.calls)-1].uri != "/api/v1/machines/a%2Fb" {
		t.Fatalf("names must be escaped: %v %s", err, f.calls[len(f.calls)-1].uri)
	}
	if _, err := c.Start(ctx, "dev"); err != nil {
		t.Fatal(err)
	}
	r, err := c.Exec(ctx, "dev", []string{"sh", "-c", "x"}, ExecOptions{Env: []string{"A=1"}, Stdin: "in", Timeout: 5})
	if err != nil || r.ExitCode != 3 || r.Stdout != "o" || r.Stderr != "e" {
		t.Fatalf("exec: %+v %v", r, err)
	}
	if b := f.calls[len(f.calls)-1].body; b != `{"command":["sh","-c","x"],"env":["A=1"],"stdin":"in","timeout":5}` {
		t.Fatalf("exec body: %s", b)
	}
	if err := c.WriteFile(ctx, "dev", "/a b", []byte("hi"), 0o600); err != nil {
		t.Fatal(err)
	}
	if w := f.calls[len(f.calls)-1]; w.uri != "/api/v1/machines/dev/files?mode=600&path=%2Fa+b" || w.body != "hi" || w.ctype != "application/octet-stream" {
		t.Fatalf("write: %+v", w)
	}
	c.WriteFile(ctx, "dev", "/a", nil, 0)
	if u := f.calls[len(f.calls)-1].uri; strings.Contains(u, "mode") {
		t.Fatalf("mode 0 must use the server default: %s", u)
	}
	if b, err := c.ReadFile(ctx, "dev", "/a"); err != nil || string(b) != "hi" || f.calls[len(f.calls)-1].uri != "/api/v1/machines/dev/files?path=%2Fa" {
		t.Fatalf("read: %q %v", b, err)
	}
	if l, err := c.Logs(ctx, "dev", 5, true); err != nil || l != "boot\n" || f.calls[len(f.calls)-1].uri != "/api/v1/machines/dev/logs?source=workload&tail=5" {
		t.Fatalf("logs: %q %v %s", l, err, f.calls[len(f.calls)-1].uri)
	}
	c.Logs(ctx, "dev", 0, false)
	if u := f.calls[len(f.calls)-1].uri; u != "/api/v1/machines/dev/logs?" {
		t.Fatalf("logs without options: %s", u)
	}
	if _, err := c.UpdateEgress(ctx, "dev", Egress{BlockPrivate: true}); err != nil || f.calls[len(f.calls)-1].body != `{"block_private":true}` {
		t.Fatalf("egress: %v %s", err, f.calls[len(f.calls)-1].body)
	}
	if c.Pause(ctx, "dev") != nil || c.Resume(ctx, "dev") != nil {
		t.Fatal("pause/resume")
	}
	if k, err := c.Branch(ctx, "dev", "kid"); err != nil || k.Name != "kid" || f.calls[len(f.calls)-1].body != `{"name":"kid"}` {
		t.Fatalf("branch: %+v %v", k, err)
	}
	for _, tc := range []struct {
		timeout time.Duration
		uri     string
	}{{-1, "/api/v1/machines/dev/stop"}, {0, "/api/v1/machines/dev/stop?timeout=0"}, {30 * time.Second, "/api/v1/machines/dev/stop?timeout=30"}} {
		if err := c.Stop(ctx, "dev", tc.timeout); err != nil || f.calls[len(f.calls)-1].uri != tc.uri {
			t.Fatalf("stop %v: %v %s", tc.timeout, err, f.calls[len(f.calls)-1].uri)
		}
	}
	for force, uri := range map[bool]string{true: "/api/v1/machines/dev?force=true", false: "/api/v1/machines/dev"} {
		if err := c.DeleteMachine(ctx, "dev", force); err != nil || f.calls[len(f.calls)-1].uri != uri {
			t.Fatalf("delete force=%v: %v", force, err)
		}
	}
}

// TestExecute: o job vai em /execute; stderr e exception nulos viram texto vazio, e os
// preenchidos chegam como vieram.
func TestExecute(t *testing.T) {
	c, f := newFake(t, map[string]func(http.ResponseWriter){
		"POST /api/v1/execute": reply(200, `{"status":"success","stdout":"42\n","stderr":null,"exception":null,"executionTime":31}`),
	})
	r, err := c.Execute(context.Background(), Job{Language: "python", Code: "print(42)"})
	if err != nil || r.Stdout != "42\n" || r.Stderr != "" || r.Exception != "" || r.ExecutionTime != 31 {
		t.Fatalf("execute: %+v %v", r, err)
	}
	if f.calls[0].body != `{"language":"python","code":"print(42)"}` {
		t.Fatalf("job body: %s", f.calls[0].body)
	}
	f.routes["POST /api/v1/execute"] = reply(200, `{"status":"success","stderr":"boom","exception":"Timeout"}`)
	if r, _ := c.Execute(context.Background(), Job{Language: "python"}); r.Stderr != "boom" || r.Exception != "Timeout" {
		t.Fatalf("filled stderr/exception: %+v", r)
	}
	f.routes["POST /api/v1/execute"] = reply(200, `{"status":1}`)
	if _, err := c.Execute(context.Background(), Job{Language: "python"}); err == nil {
		t.Fatal("a malformed response must fail")
	}
}

// TestErrors: os dois formatos de erro da API e o corpo cru viram *Error com status, código
// e mensagem; falha de transporte e de montagem da requisição chegam como erro comum.
func TestErrors(t *testing.T) {
	c, _ := newFake(t, map[string]func(http.ResponseWriter){
		"GET /api/v1/machines/ghost": reply(404, `{"error":{"code":"NOT_FOUND","message":"machine ghost not found"}}`),
		"POST /api/v1/execute":       reply(400, `{"status":"failed","error":"invalid request: language"}`),
		"GET /api/v1/machines":       reply(502, "bad gateway"),
		"POST /api/v1/machines":      reply(500, `{"error":42}`),
	})
	ctx := context.Background()
	var e *Error
	_, err := c.Machine(ctx, "ghost")
	if !errors.As(err, &e) || e.Status != 404 || e.Code != "NOT_FOUND" || e.Message != "machine ghost not found" ||
		err.Error() != "runner.code: 404 NOT_FOUND: machine ghost not found" {
		t.Fatalf("structured error: %#v", err)
	}
	_, err = c.Execute(ctx, Job{})
	if !errors.As(err, &e) || e.Status != 400 || e.Code != "" || e.Message != "invalid request: language" ||
		err.Error() != "runner.code: 400: invalid request: language" {
		t.Fatalf("job error: %#v", err)
	}
	if _, err := c.Machines(ctx); !errors.As(err, &e) || e.Message != "bad gateway" {
		t.Fatalf("raw body: %#v", err)
	}
	if _, err := c.CreateMachine(ctx, MachineSpec{Name: "x"}); !errors.As(err, &e) || e.Message != `{"error":42}` {
		t.Fatalf("unknown error shape keeps the raw body: %#v", err)
	}
	if err := c.Pause(ctx, "nope"); !errors.As(err, &e) || e.Status != http.StatusTeapot {
		t.Fatalf("unknown route: %v", err)
	}
	dead := &Client{BaseURL: "http://127.0.0.1:1"}
	if _, err := dead.Machines(ctx); err == nil || errors.As(err, &e) {
		t.Fatalf("transport failure is not an API error: %v", err)
	}
	if _, err := (&Client{BaseURL: "http://\x7f"}).Machines(ctx); err == nil {
		t.Fatal("a bad base URL must fail")
	}
	if _, err := c.Exec(ctx, "dev", nil, ExecOptions{}); err == nil {
		t.Fatal("exec on a missing route must fail")
	}
	if err := c.doJSON(ctx, http.MethodPost, "/x", func() {}, nil); err == nil {
		t.Fatal("a body that does not encode must fail before the request")
	}
}

// TestBodyReadError: corpo que quebra no meio da leitura vira erro, não resposta vazia.
func TestBodyReadError(t *testing.T) {
	s := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		w.Header().Set("Content-Length", "100")
		w.WriteHeader(200)
		io.WriteString(w, "short")
	}))
	defer s.Close()
	if _, err := New(s.URL).Logs(context.Background(), "x", 0, false); err == nil {
		t.Fatal("a truncated body must fail")
	}
}
