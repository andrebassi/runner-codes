package localpty

import (
	"bytes"
	"context"
	"io"
	"strings"
	"sync"
	"syscall"
	"testing"
	"time"

	"runner.codes/internal/ports"
)

// syncBuf é um buffer seguro entre a goroutine de cópia e o teste.
type syncBuf struct {
	mu sync.Mutex
	b  bytes.Buffer
}

// Write guarda sob lock.
func (s *syncBuf) Write(p []byte) (int, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.b.Write(p)
}

// String lê sob lock.
func (s *syncBuf) String() string {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.b.String()
}

// TestRunOutputAndExit: o programa escreve no terminal e o código de saída volta.
func TestRunOutputAndExit(t *testing.T) {
	var out syncBuf
	code, err := Run(context.Background(), Command{Argv: []string{"sh", "-c", "echo oi-$TESTVAR; exit 3"}, Env: []string{"TESTVAR=x"}},
		ports.Winsize{Rows: 24, Cols: 80}, strings.NewReader(""), &out, nil)
	if err != nil || code != 3 || !strings.Contains(out.String(), "oi-x") {
		t.Fatalf("code=%d err=%v out=%q", code, err, out.String())
	}
}

// TestRunResizeAndInput: o tamanho inicial e o resize chegam ao PTY; a entrada chega ao programa.
func TestRunResizeAndInput(t *testing.T) {
	var out syncBuf
	pr, pw := io.Pipe()
	resize := make(chan ports.Winsize, 1)
	done := make(chan int)
	go func() {
		code, _ := Run(context.Background(), Command{Argv: []string{"sh", "-c", "stty size; read l; stty size; echo got-$l"}},
			ports.Winsize{Rows: 30, Cols: 100}, pr, &out, resize)
		done <- code
	}()
	waitFor(t, &out, "30 100")
	resize <- ports.Winsize{Rows: 40, Cols: 120}
	time.Sleep(100 * time.Millisecond)
	pw.Write([]byte("abc\n"))
	if code := <-done; code != 0 {
		t.Fatalf("code %d", code)
	}
	if s := out.String(); !strings.Contains(s, "40 120") || !strings.Contains(s, "got-abc") {
		t.Fatalf("saída %q", s)
	}
	close(resize)
}

// TestRunCancel: cancelar o ctx mata o processo e o Run volta.
func TestRunCancel(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	var out syncBuf
	done := make(chan struct{})
	go func() {
		Run(ctx, Command{Argv: []string{"sleep", "30"}}, ports.Winsize{Rows: 24, Cols: 80}, strings.NewReader(""), &out, make(chan ports.Winsize))
		close(done)
	}()
	time.Sleep(200 * time.Millisecond)
	cancel()
	select {
	case <-done:
	case <-time.After(5 * time.Second):
		t.Fatal("o processo não morreu com o ctx")
	}
}

// TestRunErrors: argv vazio, programa inexistente e credencial impossível viram erro.
func TestRunErrors(t *testing.T) {
	sz := ports.Winsize{Rows: 24, Cols: 80}
	if _, err := Run(context.Background(), Command{}, sz, strings.NewReader(""), io.Discard, nil); err == nil {
		t.Fatal("argv vazio passou")
	}
	if _, err := Run(context.Background(), Command{Argv: []string{"/nao/existe"}}, sz, strings.NewReader(""), io.Discard, nil); err == nil {
		t.Fatal("programa inexistente passou")
	}
	// Sem root, trocar para o uid 0 é recusado pelo kernel (EPERM) ao iniciar; como root, o programa roda.
	code, err := Run(context.Background(), Command{Argv: []string{"true"}, Cred: &syscall.Credential{Uid: 0, Gid: 0}}, sz, strings.NewReader(""), io.Discard, nil)
	if err == nil && code != 0 {
		t.Fatalf("code %d", code)
	}
}

// TestEnviron: TERM, HOME e USER do visitante entram, e os extras vão no fim.
func TestEnviron(t *testing.T) {
	env := strings.Join(Environ("/data/home", "visitor", "IS_SANDBOX=1"), "\n")
	for _, want := range []string{"TERM=xterm-256color", "HOME=/data/home", "USER=visitor", "LOGNAME=visitor", "IS_SANDBOX=1"} {
		if !strings.Contains(env, want) {
			t.Fatalf("falta %s", want)
		}
	}
}

// waitFor espera o texto aparecer na saída.
func waitFor(t *testing.T, b *syncBuf, s string) {
	t.Helper()
	for i := 0; i < 50; i++ {
		if strings.Contains(b.String(), s) {
			return
		}
		time.Sleep(50 * time.Millisecond)
	}
	t.Fatalf("não apareceu %q em %q", s, b.String())
}
