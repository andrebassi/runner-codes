package main

import (
	"bytes"
	"context"
	"io"
	"net/http"
	"os/exec"
	"regexp"
	"strings"
	"sync"
	"testing"
	"time"

	"runner.code/internal/ports"
)

// syncBuffer é um bytes.Buffer seguro para o teste ler enquanto o servidor escreve a URL.
type syncBuffer struct {
	mu sync.Mutex
	b  bytes.Buffer
}

// Write grava sob o lock.
func (s *syncBuffer) Write(p []byte) (int, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.b.Write(p)
}

// String lê sob o lock.
func (s *syncBuffer) String() string { s.mu.Lock(); defer s.mu.Unlock(); return s.b.String() }

// urlRe acha a URL impressa: só loopback e com token de 32 hex.
var urlRe = regexp.MustCompile(`http://127\.0\.0\.1:\d+/\?t=[0-9a-f]{32}`)

// TestServeWebTerminal: URL em 127.0.0.1 com token, o teclado chega ao programa, e o código de saída dele volta.
func TestServeWebTerminal(t *testing.T) {
	shell := func(ctx context.Context, _ ports.Winsize, in io.Reader, out io.Writer, _ <-chan ports.Winsize) (int, error) {
		buf := make([]byte, 64)
		n, _ := in.Read(buf)
		out.Write(buf[:n])
		return 3, nil
	}
	var w syncBuffer
	done := make(chan int, 1)
	go func() {
		code, err := serveWebTerminal(context.Background(), webTermSpec{Machine: "m", Label: "x", Shell: shell}, 0, false, &w)
		if err != nil {
			t.Error(err)
		}
		done <- code
	}()
	var url string
	for i := 0; i < 100 && url == ""; i++ {
		url = urlRe.FindString(w.String())
		time.Sleep(20 * time.Millisecond)
	}
	if url == "" {
		t.Fatalf("URL não impressa: %q", w.String())
	}
	token := url[strings.Index(url, "t=")+2:]
	base := url[:strings.Index(url, "/?")]
	resp, err := http.Post(base+"/input?t="+token, "text/plain", strings.NewReader("oi"))
	if err != nil {
		t.Fatal(err)
	}
	resp.Body.Close()
	select {
	case code := <-done:
		if code != 3 {
			t.Fatalf("código %d, queria 3", code)
		}
	case <-time.After(5 * time.Second):
		t.Fatal("serveWebTerminal não terminou depois do programa sair")
	}
}

// TestServeWebTerminalCancel: Ctrl+C (contexto cancelado) encerra a sessão sem erro.
func TestServeWebTerminalCancel(t *testing.T) {
	shell := func(ctx context.Context, _ ports.Winsize, _ io.Reader, _ io.Writer, _ <-chan ports.Winsize) (int, error) {
		<-ctx.Done()
		return 130, ctx.Err()
	}
	ctx, cancel := context.WithCancel(context.Background())
	go func() { time.Sleep(100 * time.Millisecond); cancel() }()
	code, err := serveWebTerminal(ctx, webTermSpec{Shell: shell}, 0, false, io.Discard)
	if err != nil || code != 130 {
		t.Fatalf("code %d err %v", code, err)
	}
}

// TestServeWebTerminalBusyPort: porta ocupada é erro com o motivo, sem abrir sessão.
func TestServeWebTerminalBusyPort(t *testing.T) {
	var w syncBuffer
	go serveWebTerminal(context.Background(), webTermSpec{Shell: func(ctx context.Context, _ ports.Winsize, _ io.Reader, _ io.Writer, _ <-chan ports.Winsize) (int, error) {
		<-ctx.Done()
		return 0, nil
	}}, 0, false, &w)
	var url string
	for i := 0; i < 100 && url == ""; i++ {
		url = urlRe.FindString(w.String())
		time.Sleep(20 * time.Millisecond)
	}
	port := regexp.MustCompile(`:(\d+)/`).FindStringSubmatch(url)[1]
	n := 0
	for _, c := range port {
		n = n*10 + int(c-'0')
	}
	if _, err := serveWebTerminal(context.Background(), webTermSpec{}, n, false, io.Discard); err == nil || !strings.Contains(err.Error(), "web terminal") {
		t.Fatalf("porta ocupada: %v", err)
	}
}

// TestOpenBrowser: abre pelo comando do sistema com a URL; comando inexistente devolve erro (o rc só avisa).
func TestOpenBrowser(t *testing.T) {
	orig := browserCommand
	defer func() { browserCommand = orig }()
	var got string
	browserCommand = func(url string) *exec.Cmd { got = url; return exec.Command("true") }
	if err := openBrowser("http://127.0.0.1:1/?t=x"); err != nil || got != "http://127.0.0.1:1/?t=x" {
		t.Fatalf("err %v url %q", err, got)
	}
	browserCommand = func(string) *exec.Cmd { return exec.Command("/nao/existe/abridor") }
	if err := openBrowser("x"); err == nil {
		t.Fatal("abridor inexistente não deu erro")
	}
}

// TestTabLabel: a aba principal leva o nome do programa sem o caminho; sem comando, "shell".
func TestTabLabel(t *testing.T) {
	for argv, want := range map[string]string{"": "shell", "claude": "claude", "/usr/local/bin/claude": "claude"} {
		var args []string
		if argv != "" {
			args = []string{argv, "--x"}
		}
		if got := tabLabel(args); got != want {
			t.Errorf("%q: %q", argv, got)
		}
	}
}
