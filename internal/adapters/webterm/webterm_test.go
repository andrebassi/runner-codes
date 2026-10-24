package webterm

import (
	"bufio"
	"context"
	"encoding/base64"
	"io"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
	"time"

	"runner.codes/internal/ports"
)

const tok = "segredo123"

// echoShell é um PTY de mentira: ecoa o que chega em in com o prefixo "eco:", repassa o tamanho recebido e sai com
// código 7 quando lê "sair".
func echoShell(sizes chan<- ports.Winsize) ShellFunc {
	return func(ctx context.Context, size ports.Winsize, in io.Reader, out io.Writer, resize <-chan ports.Winsize) (int, error) {
		out.Write([]byte("pronto\n"))
		go func() {
			for s := range resize {
				sizes <- s
			}
		}()
		buf := make([]byte, 1024)
		for {
			n, err := in.Read(buf)
			if err != nil {
				return 1, err
			}
			line := string(buf[:n])
			if strings.Contains(line, "sair") {
				return 7, nil
			}
			out.Write([]byte("eco:" + line))
		}
	}
}

// hubOf monta um Hub de teste com a sessão principal s, a fábrica de shell newShell e o encaminhador fw.
func hubOf(s *Session, newShell ShellFunc, fw *Forwarder) *Hub {
	return NewHub(context.Background(), tok, "m1", s, "main", newShell, fw)
}

// newServer liga uma sessão com echoShell num httptest.Server de loopback.
func newServer(t *testing.T) (*Session, *httptest.Server, chan ports.Winsize) {
	t.Helper()
	sizes := make(chan ports.Winsize, 4)
	s := Start(context.Background(), echoShell(sizes), ports.Winsize{Rows: 24, Cols: 80})
	srv := httptest.NewServer(Handler(hubOf(s, nil, nil)))
	t.Cleanup(srv.Close)
	return s, srv, sizes
}

// readEvents lê eventos SSE até achar um com o texto want (decodificando data em base64) ou o prazo vencer.
func readEvents(t *testing.T, body io.Reader, want string) string {
	t.Helper()
	got := make(chan string, 1)
	go func() {
		var all strings.Builder
		sc := bufio.NewScanner(body)
		event := ""
		for sc.Scan() {
			line := sc.Text()
			switch {
			case strings.HasPrefix(line, "event: "):
				event = strings.TrimPrefix(line, "event: ")
			case strings.HasPrefix(line, "data: "):
				data := strings.TrimPrefix(line, "data: ")
				if event == "exit" {
					all.WriteString("[exit " + data + "]")
				} else if b, err := base64.StdEncoding.DecodeString(data); err == nil {
					all.WriteString(string(b))
				}
				if strings.Contains(all.String(), want) {
					got <- all.String()
					return
				}
			case line == "":
				event = ""
			}
		}
		got <- all.String()
	}()
	select {
	case s := <-got:
		return s
	case <-time.After(5 * time.Second):
		t.Fatalf("SSE não trouxe %q a tempo", want)
		return ""
	}
}

// post manda um POST com o token e devolve o status.
func post(t *testing.T, srv *httptest.Server, path, body string) int {
	t.Helper()
	resp, err := http.Post(srv.URL+path+"?t="+tok, "text/plain", strings.NewReader(body))
	if err != nil {
		t.Fatal(err)
	}
	resp.Body.Close()
	return resp.StatusCode
}

// TestRoundTrip: o teclado chega ao PTY e a tela volta pelo SSE, primeiro o replay e depois o ao vivo.
func TestRoundTrip(t *testing.T) {
	_, srv, _ := newServer(t)
	resp, err := http.Get(srv.URL + "/stream?t=" + tok)
	if err != nil {
		t.Fatal(err)
	}
	defer resp.Body.Close()
	if ct := resp.Header.Get("Content-Type"); ct != "text/event-stream" {
		t.Fatalf("content-type %q", ct)
	}
	readEvents(t, resp.Body, "pronto")
	if code := post(t, srv, "/input", "olá\n"); code != http.StatusNoContent {
		t.Fatalf("input: %d", code)
	}
	readEvents(t, resp.Body, "eco:olá")
}

// TestReplayOnReconnect: quem conecta depois recebe a tela recente (o F5 não começa no escuro).
func TestReplayOnReconnect(t *testing.T) {
	_, srv, _ := newServer(t)
	post(t, srv, "/input", "antes\n")
	time.Sleep(50 * time.Millisecond)
	resp, err := http.Get(srv.URL + "/stream?t=" + tok)
	if err != nil {
		t.Fatal(err)
	}
	defer resp.Body.Close()
	out := readEvents(t, resp.Body, "eco:antes")
	if !strings.HasPrefix(out, "pronto") {
		t.Fatalf("replay sem o começo da tela: %q", out)
	}
}

// TestExit: o programa sai, o SSE avisa o código, o Wait devolve e o teclado passa a dar 410.
func TestExit(t *testing.T) {
	s, srv, _ := newServer(t)
	resp, err := http.Get(srv.URL + "/stream?t=" + tok)
	if err != nil {
		t.Fatal(err)
	}
	defer resp.Body.Close()
	readEvents(t, resp.Body, "pronto")
	post(t, srv, "/input", "sair\n")
	readEvents(t, resp.Body, `[exit {"code":7}]`)
	if code, err := s.Wait(); code != 7 || err != nil {
		t.Fatalf("wait: %d %v", code, err)
	}
	if code := post(t, srv, "/input", "x"); code != http.StatusGone {
		t.Fatalf("input depois do fim: %d", code)
	}
	// Quem abre a página depois do fim recebe a tela e o código, sem ficar esperando.
	late, err := http.Get(srv.URL + "/stream?t=" + tok)
	if err != nil {
		t.Fatal(err)
	}
	defer late.Body.Close()
	readEvents(t, late.Body, `[exit {"code":7}]`)
}

// TestResize: o tamanho do navegador chega ao PTY; tamanho absurdo ou JSON quebrado é 400.
func TestResize(t *testing.T) {
	_, srv, sizes := newServer(t)
	if code := post(t, srv, "/resize", `{"rows":40,"cols":132}`); code != http.StatusNoContent {
		t.Fatalf("resize: %d", code)
	}
	select {
	case got := <-sizes:
		if got != (ports.Winsize{Rows: 40, Cols: 132}) {
			t.Fatalf("tamanho %+v", got)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("resize não chegou ao PTY")
	}
	for _, bad := range []string{`{"rows":0,"cols":80}`, `{"rows":40,"cols":5000}`, `{`} {
		if code := post(t, srv, "/resize", bad); code != http.StatusBadRequest {
			t.Fatalf("%s: %d", bad, code)
		}
	}
}

// TestResizeNeverBlocks: vários resizes sem ninguém lendo não travam o handler (só o último vale).
func TestResizeNeverBlocks(t *testing.T) {
	block := func(ctx context.Context, _ ports.Winsize, _ io.Reader, _ io.Writer, _ <-chan ports.Winsize) (int, error) {
		<-ctx.Done()
		return 0, nil
	}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	srv := httptest.NewServer(Handler(hubOf(Start(ctx, block, ports.Winsize{}), nil, nil)))
	defer srv.Close()
	done := make(chan struct{})
	go func() {
		for i := 1; i <= 5; i++ {
			post(t, srv, "/resize", `{"rows":10,"cols":10}`)
		}
		close(done)
	}()
	select {
	case <-done:
	case <-time.After(3 * time.Second):
		t.Fatal("resize travou com o canal cheio")
	}
}

// TestGuards: sem token, com token errado, Host de outro domínio ou Origin de outro site, tudo 403.
func TestGuards(t *testing.T) {
	_, srv, _ := newServer(t)
	cases := []struct {
		name, method, path, host, origin string
	}{
		{"sem token", "GET", "/stream", "", ""},
		{"token errado", "POST", "/input?t=outro", "", ""},
		{"DNS rebinding", "GET", "/stream?t=" + tok, "atacante.example.com:8080", ""},
		{"página de outro site", "POST", "/input?t=" + tok, "", "http://atacante.example.com"},
		{"resize sem token", "POST", "/resize", "", ""},
	}
	for _, c := range cases {
		req, _ := http.NewRequest(c.method, srv.URL+c.path, strings.NewReader("x"))
		if c.host != "" {
			req.Host = c.host
		}
		if c.origin != "" {
			req.Header.Set("Origin", c.origin)
		}
		resp, err := http.DefaultClient.Do(req)
		if err != nil {
			t.Fatal(err)
		}
		resp.Body.Close()
		if resp.StatusCode != http.StatusForbidden {
			t.Errorf("%s: %d, queria 403", c.name, resp.StatusCode)
		}
	}
	// A mesma origem passa: a própria página manda o teclado.
	req, _ := http.NewRequest("POST", srv.URL+"/input?t="+tok, strings.NewReader("x"))
	req.Header.Set("Origin", srv.URL)
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		t.Fatal(err)
	}
	resp.Body.Close()
	if resp.StatusCode != http.StatusNoContent {
		t.Fatalf("mesma origem: %d", resp.StatusCode)
	}
}

// TestStaticPages: a página e o xterm saem sem token (não abrem a sessão), com o tipo certo.
func TestStaticPages(t *testing.T) {
	_, srv, _ := newServer(t)
	for path, ctype := range map[string]string{"/": "text/html", "/xterm.js": "text/javascript", "/xterm.css": "text/css", "/addon-fit.js": "text/javascript", "/addon-web-links.js": "text/javascript"} {
		resp, err := http.Get(srv.URL + path)
		if err != nil {
			t.Fatal(err)
		}
		body, _ := io.ReadAll(resp.Body)
		resp.Body.Close()
		if resp.StatusCode != 200 || !strings.HasPrefix(resp.Header.Get("Content-Type"), ctype) || len(body) == 0 {
			t.Errorf("%s: %d %q %d bytes", path, resp.StatusCode, resp.Header.Get("Content-Type"), len(body))
		}
	}
}

// TestInputTooLarge: corpo acima do teto é 413 e não chega ao PTY.
func TestInputTooLarge(t *testing.T) {
	_, srv, _ := newServer(t)
	if code := post(t, srv, "/input", strings.Repeat("a", maxInput+1)); code != http.StatusRequestEntityTooLarge {
		t.Fatalf("corpo grande: %d", code)
	}
}

// TestReplayKeepsTail: o replay guarda só o fim da tela (maxReplay), não cresce sem limite.
func TestReplayKeepsTail(t *testing.T) {
	s := &Session{subs: map[chan []byte]struct{}{}, done: make(chan struct{})}
	s.Write([]byte(strings.Repeat("a", maxReplay)))
	s.Write([]byte("FIM"))
	if len(s.buf) != maxReplay || !strings.HasSuffix(string(s.buf), "FIM") {
		t.Fatalf("buf %d bytes, sufixo %q", len(s.buf), s.buf[len(s.buf)-3:])
	}
}

// TestSlowSubscriberDropped: aba que não lê é desligada em vez de travar a escrita do PTY.
func TestSlowSubscriberDropped(t *testing.T) {
	s := &Session{subs: map[chan []byte]struct{}{}, done: make(chan struct{})}
	_, ch, _ := s.subscribe()
	for i := 0; i <= subBuffer; i++ {
		s.Write([]byte("x"))
	}
	if len(s.subs) != 0 {
		t.Fatal("aba lenta continuou inscrita")
	}
	n := 0
	for range ch {
		n++
	}
	if n != subBuffer {
		t.Fatalf("recebeu %d pedaços antes de ser desligada", n)
	}
	s.unsubscribe(ch) // já fora da lista: não fecha de novo (não entra em pânico)
}

// TestLoopbackHost: só nomes de loopback passam.
func TestLoopbackHost(t *testing.T) {
	for host, want := range map[string]bool{"127.0.0.1:7681": true, "localhost:1": true, "[::1]:9": true, "127.0.0.1": true,
		"192.168.0.5:7681": false, "atacante.example.com": false, "": false} {
		if got := isLoopbackHost(host); got != want {
			t.Errorf("%q: %v", host, got)
		}
	}
}

// noFlush é um ResponseWriter sem Flush: o SSE recusa com 500 em vez de entregar tudo só no fim.
type noFlush struct{ http.ResponseWriter }

// TestStreamNeedsFlusher: servidor sem streaming responde 500.
func TestStreamNeedsFlusher(t *testing.T) {
	s, _, _ := newServer(t)
	rec := httptest.NewRecorder()
	s.stream(noFlush{rec}, httptest.NewRequest("GET", "/stream", nil))
	if rec.Code != http.StatusInternalServerError {
		t.Fatalf("status %d", rec.Code)
	}
}
