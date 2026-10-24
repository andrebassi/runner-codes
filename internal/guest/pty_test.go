package guest

import (
	"net"
	"strings"
	"testing"
	"time"

	"runner.codes/internal/protocol"
)

// ptyClient abre uma sessão pelo fio e devolve a conexão do lado do host.
func ptyClient(t *testing.T, job Job) net.Conn {
	t.Helper()
	c, srv := net.Pipe()
	go NewServer(DefaultServerConfig()).handleConnection(srv)
	c.SetDeadline(time.Now().Add(15 * time.Second))
	job.Op = protocol.OpPTY
	if err := protocol.WriteFrame(c, job); err != nil {
		t.Fatal(err)
	}
	return c
}

// collect lê a saída até o exit e devolve o texto e o código.
func collect(t *testing.T, c net.Conn) (string, int) {
	t.Helper()
	var out strings.Builder
	for {
		var ev protocol.Event
		if err := protocol.ReadFrame(c, &ev); err != nil {
			t.Fatalf("read: %v (output so far %q)", err, out.String())
		}
		switch ev.Type {
		case protocol.EventOutput:
			out.Write(ev.Bytes)
		case protocol.EventExit:
			return out.String(), ev.Code
		}
	}
}

// TestPTYSession trava o que faz a sessão ser um terminal: o programa vê um TTY, o
// tamanho pedido chega, a entrada do host chega ao programa e o código de saída volta.
func TestPTYSession(t *testing.T) {
	c := ptyClient(t, Job{Argv: []string{"sh", "-c", `test -t 0 && echo IS-TTY; stty size; read l; echo "got:$l"; exit 5`},
		Rows: 30, Cols: 100, Env: []string{"FOO=bar"}})
	defer c.Close()
	protocol.WriteFrame(c, protocol.Event{Type: protocol.EventInput, Bytes: []byte("hello\n")})
	out, code := collect(t, c)
	if code != 5 || !strings.Contains(out, "IS-TTY") || !strings.Contains(out, "30 100") || !strings.Contains(out, "got:hello") {
		t.Fatalf("code=%d out=%q", code, out)
	}
}

// TestPTYResize: o resize do host muda o tamanho que o programa enxerga.
func TestPTYResize(t *testing.T) {
	c := ptyClient(t, Job{Argv: []string{"sh", "-c", "read l; stty size"}, Rows: 24, Cols: 80})
	defer c.Close()
	protocol.WriteFrame(c, protocol.Event{Type: protocol.EventResize, Rows: 40, Cols: 120})
	protocol.WriteFrame(c, protocol.Event{Type: protocol.EventResize}) // zero é ignorado
	protocol.WriteFrame(c, protocol.Event{Type: protocol.EventInput, Bytes: []byte("go\n")})
	if out, _ := collect(t, c); !strings.Contains(out, "40 120") {
		t.Fatalf("resize not applied: %q", out)
	}
}

// TestPTYCtrlC: o byte 0x03 vira SIGINT no programa em primeiro plano (é o PTY, não o host,
// que interpreta) e a sessão continua: o shell segue rodando depois do comando interrompido.
func TestPTYCtrlC(t *testing.T) {
	c := ptyClient(t, Job{Argv: []string{"sh", "-c", `trap 'echo CAUGHT' INT; sleep 30 & wait; echo after; exit 0`}})
	defer c.Close()
	time.Sleep(300 * time.Millisecond)
	start := time.Now()
	protocol.WriteFrame(c, protocol.Event{Type: protocol.EventInput, Bytes: []byte{0x03}})
	out, code := collect(t, c)
	if !strings.Contains(out, "CAUGHT") || time.Since(start) > 5*time.Second || code != 0 {
		t.Fatalf("ctrl-c: code=%d out=%q after %v", code, out, time.Since(start))
	}
}

// TestPTYHostGone: o host fecha a conexão e o programa é morto, sem ficar órfão.
func TestPTYHostGone(t *testing.T) {
	c, srv := net.Pipe()
	done := make(chan struct{})
	go func() { ptySession(srv, Job{Argv: []string{"sleep", "30"}}); close(done) }()
	c.Close()
	select {
	case <-done:
	case <-time.After(5 * time.Second):
		t.Fatal("session survived the host going away")
	}
}

// TestPTYDefaultsAndErrors: sem argv roda o shell padrão; programa que não existe sai 127.
func TestPTYDefaultsAndErrors(t *testing.T) {
	c := ptyClient(t, Job{})
	protocol.WriteFrame(c, protocol.Event{Type: protocol.EventInput, Bytes: []byte("echo DEFAULT-SHELL; exit 3\n")})
	if out, code := collect(t, c); code != 3 || !strings.Contains(out, "DEFAULT-SHELL") {
		t.Fatalf("default shell: code=%d out=%q", code, out)
	}
	c.Close()
	c = ptyClient(t, Job{Argv: []string{"/does/not/exist"}})
	defer c.Close()
	if _, code := collect(t, c); code != 127 {
		t.Fatalf("missing program: %d", code)
	}
}
