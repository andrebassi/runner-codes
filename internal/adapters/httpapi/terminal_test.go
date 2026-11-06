package httpapi

import (
	"bufio"
	"context"
	"encoding/base64"
	"encoding/json"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"sync"
	"testing"
	"time"

	"runner.codes/internal/app"
	"runner.codes/internal/ports"
)

// shellMachines é o fakeMachines com um PTY falso: escreve o prompt, ecoa o teclado em maiúsculas e guarda o pedido
// e cada resize; termina quando o contexto acaba (DELETE) ou quando recebe "exit".
type shellMachines struct {
	*fakeMachines
	mu      sync.Mutex
	req     app.ShellRequest
	resizes []ports.Winsize
}

// Shell faz o papel do PTY do guest: prompt "$ ", eco em maiúsculas por linha, "exit" sai com 0 e o fim do contexto
// sai com 130 (como o Ctrl+C de um shell).
func (s *shellMachines) Shell(ctx context.Context, _ string, req app.ShellRequest, in io.Reader, out io.Writer, resize <-chan ports.Winsize) (int, error) {
	s.mu.Lock()
	s.req = req
	s.mu.Unlock()
	go func() {
		for {
			select {
			case sz := <-resize:
				s.mu.Lock()
				s.resizes = append(s.resizes, sz)
				s.mu.Unlock()
			case <-ctx.Done():
				return
			}
		}
	}()
	out.Write([]byte("$ "))
	lines := make(chan string)
	go func() {
		sc := bufio.NewScanner(in)
		for sc.Scan() {
			lines <- sc.Text()
		}
		close(lines)
	}()
	for {
		select {
		case <-ctx.Done():
			return 130, nil
		case l, ok := <-lines:
			if !ok || l == "exit" {
				return 0, nil
			}
			out.Write([]byte(strings.ToUpper(l) + "\n"))
		}
	}
}

// openTerm abre um terminal pela rota e devolve o id.
func openTerm(t *testing.T, s *Server, body string) string {
	t.Helper()
	code, out := do(t, s, "POST", "/api/v1/machines/dev/terminal", body)
	if code != http.StatusCreated {
		t.Fatalf("open: %d %s", code, out)
	}
	var r struct{ ID string }
	if err := json.Unmarshal([]byte(out), &r); err != nil || len(r.ID) != 24 {
		t.Fatalf("id: %q %v", out, err)
	}
	return r.ID
}

// readEvents lê a SSE do terminal até achar want (decodificando os data em base64) ou o prazo vencer.
func readEvents(t *testing.T, url, want string) string {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), 3*time.Second)
	defer cancel()
	req, _ := http.NewRequestWithContext(ctx, "GET", url, nil)
	res, err := http.DefaultClient.Do(req)
	if err != nil {
		t.Fatal(err)
	}
	defer res.Body.Close()
	var got strings.Builder
	sc := bufio.NewScanner(res.Body)
	for sc.Scan() {
		if d, ok := strings.CutPrefix(sc.Text(), "data: "); ok {
			if raw, err := base64.StdEncoding.DecodeString(d); err == nil {
				got.Write(raw)
			} else {
				got.WriteString(d)
			}
			if strings.Contains(got.String(), want) {
				return got.String()
			}
		}
	}
	t.Fatalf("SSE sem %q: %q", want, got.String())
	return ""
}

// TestTerminalRoutes trava o terminal do console: abrir liga o shell com TERM e o tamanho pedido; a SSE traz o
// prompt e o eco; o teclado e o resize chegam ao PTY; id de outra máquina é 404; o DELETE encerra e esquece.
func TestTerminalRoutes(t *testing.T) {
	m := &shellMachines{fakeMachines: &fakeMachines{files: map[string][]byte{}}}
	s := &Server{Machines: m}
	id := openTerm(t, s, `{"rows":40,"cols":100,"user":"agent"}`)
	srv := httptest.NewServer(s.Handler())
	defer srv.Close()
	base := srv.URL + "/api/v1/machines/dev/terminal/" + id

	readEvents(t, base+"/stream", "$ ")
	if res, err := http.Post(base+"/input", "application/octet-stream", strings.NewReader("ola\n")); err != nil || res.StatusCode != 204 {
		t.Fatalf("input: %v %v", res, err)
	}
	if out := readEvents(t, base+"/stream", "OLA"); !strings.HasPrefix(out, "$ ") {
		t.Fatalf("replay perdeu o prompt: %q", out)
	}
	if res, err := http.Post(base+"/resize", "application/json", strings.NewReader(`{"rows":50,"cols":160}`)); err != nil || res.StatusCode != 204 {
		t.Fatalf("resize: %v %v", res, err)
	}
	deadline := time.Now().Add(2 * time.Second)
	for {
		m.mu.Lock()
		n, req := len(m.resizes), m.req
		m.mu.Unlock()
		if n > 0 {
			if req.User != "agent" || req.Size != (ports.Winsize{Rows: 40, Cols: 100}) || !strings.Contains(strings.Join(req.Env, " "), "TERM=xterm-256color") {
				t.Fatalf("pedido do shell: %+v", req)
			}
			break
		}
		if time.Now().After(deadline) {
			t.Fatal("resize não chegou ao PTY")
		}
		time.Sleep(10 * time.Millisecond)
	}
	if code, _ := do(t, s, "GET", "/api/v1/machines/outra/terminal/"+id+"/stream", ""); code != 404 {
		t.Fatalf("outra máquina: %d", code)
	}
	if code, _ := do(t, s, "DELETE", "/api/v1/machines/outra/terminal/"+id, ""); code != 404 {
		t.Fatalf("delete de outra máquina: %d", code)
	}
	if code, _ := do(t, s, "DELETE", "/api/v1/machines/dev/terminal/"+id, ""); code != 204 {
		t.Fatalf("delete: %d", code)
	}
	if code, _ := do(t, s, "POST", "/api/v1/machines/dev/terminal/"+id+"/input", "x"); code != 404 {
		t.Fatalf("depois do delete: %d", code)
	}
}

// TestTerminalLimits: sem corpo abre no tamanho padrão; máquina desligada 409; corpo com campo desconhecido 400;
// servidor sem shell 501; a nona sessão da mesma máquina 429.
func TestTerminalLimits(t *testing.T) {
	m := &shellMachines{fakeMachines: &fakeMachines{files: map[string][]byte{}}}
	s := &Server{Machines: m}
	openTerm(t, s, "")
	if code, _ := do(t, s, "POST", "/api/v1/machines/off-x/terminal", ""); code != 409 {
		t.Fatalf("desligada: %d", code)
	}
	if code, _ := do(t, s, "POST", "/api/v1/machines/dev/terminal", `{"bogus":1}`); code != 400 {
		t.Fatalf("campo desconhecido: %d", code)
	}
	for i := 1; i < maxTerminalsPerMachine; i++ {
		openTerm(t, s, "")
	}
	if code, _ := do(t, s, "POST", "/api/v1/machines/dev/terminal", ""); code != 429 {
		t.Fatalf("teto: %d", code)
	}
	plain := &Server{Machines: &fakeMachines{files: map[string][]byte{}}}
	if code, _ := do(t, plain, "POST", "/api/v1/machines/dev/terminal", ""); code != 501 {
		t.Fatalf("sem shell: %d", code)
	}
}

// TestTerminalSweep: a sessão sem tela é marcada, sobrevive antes dos 10 min e é encerrada depois; a que acabou sai
// na primeira varredura.
func TestTerminalSweep(t *testing.T) {
	m := &shellMachines{fakeMachines: &fakeMachines{files: map[string][]byte{}}}
	s := &Server{Machines: m}
	id := openTerm(t, s, "")
	done := openTerm(t, s, "")
	tab := s.table()
	now := time.Unix(1000, 0)
	tab.mu.Lock()
	tab.now = func() time.Time { return now }
	ended := tab.byID[done].sess
	tab.mu.Unlock()
	if code, _ := do(t, s, "POST", "/api/v1/machines/dev/terminal/"+done+"/input", "exit\n"); code != 204 {
		t.Fatalf("exit: %d", code)
	}
	<-ended.Done()
	tab.sweep()
	now = now.Add(terminalIdle - time.Second)
	tab.sweep()
	tab.mu.Lock()
	_, alive := tab.byID[id]
	_, gone := tab.byID[done]
	tab.mu.Unlock()
	if !alive || gone {
		t.Fatalf("antes do prazo: viva=%v acabada=%v", alive, gone)
	}
	now = now.Add(2 * time.Second)
	tab.sweep()
	tab.mu.Lock()
	_, alive = tab.byID[id]
	tab.mu.Unlock()
	if alive {
		t.Fatal("sessão sem tela passou do prazo e continuou")
	}
}
