package guest

import (
	"io"
	"net"
	"path/filepath"
	"strings"
	"testing"

	"runner.code/internal/protocol"
)

// connPair devolve as duas pontas de uma conexão TCP local: a do "host" e a do "guest".
// TCP e não net.Pipe porque o teste precisa de half-close (CloseWrite).
func connPair(t *testing.T) (*net.TCPConn, net.Conn) {
	l, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	defer l.Close()
	host, err := net.Dial("tcp", l.Addr().String())
	if err != nil {
		t.Fatal(err)
	}
	guest, err := l.Accept()
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { host.Close(); guest.Close() })
	return host.(*net.TCPConn), guest
}

// TestDialSession: com alguém escutando no 127.0.0.1 do guest, o resultado é sucesso e a
// conexão vira tubo nos dois sentidos, com half-close (o servidor lê até EOF e responde);
// sem ninguém escutando, falha com o motivo.
// TestDialSessionUnix: com Path, o guest conecta no socket Unix (a ponte do docker.sock).
func TestDialSessionUnix(t *testing.T) {
	sock := filepath.Join(t.TempDir(), "d.sock")
	l, err := net.Listen("unix", sock)
	if err != nil {
		t.Fatal(err)
	}
	defer l.Close()
	go func() {
		c, err := l.Accept()
		if err != nil {
			return
		}
		b, _ := io.ReadAll(c)
		c.Write(append([]byte("daemon:"), b...))
		c.Close()
	}()
	host, guest := connPair(t)
	go dialSession(guest, Job{TraceID: "d", Op: protocol.OpDial, Path: sock})
	var res protocol.Result
	if err := protocol.ReadFrame(host, &res); err != nil || res.Status != "success" {
		t.Fatalf("result: %+v %v", res, err)
	}
	host.Write([]byte("_ping"))
	host.CloseWrite()
	if b, _ := io.ReadAll(host); string(b) != "daemon:_ping" {
		t.Fatalf("pipe: %q", b)
	}
}

func TestDialSession(t *testing.T) {
	l, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	defer l.Close()
	go func() {
		c, err := l.Accept()
		if err != nil {
			return
		}
		b, _ := io.ReadAll(c)
		c.Write(append([]byte("eco:"), b...))
		c.Close()
	}()
	host, guest := connPair(t)
	go dialSession(guest, Job{TraceID: "d", Op: protocol.OpDial, Port: uint16(l.Addr().(*net.TCPAddr).Port)})
	var res protocol.Result
	if err := protocol.ReadFrame(host, &res); err != nil || res.Status != "success" {
		t.Fatalf("result: %+v %v", res, err)
	}
	host.Write([]byte("oi"))
	host.CloseWrite()
	if b, _ := io.ReadAll(host); string(b) != "eco:oi" {
		t.Fatalf("pipe: %q", b)
	}

	free, _ := net.Listen("tcp", "127.0.0.1:0")
	closed := uint16(free.Addr().(*net.TCPAddr).Port)
	free.Close()
	h2, g2 := connPair(t)
	go dialSession(g2, Job{Op: protocol.OpDial, Port: closed})
	if err := protocol.ReadFrame(h2, &res); err != nil || res.Status != "failed" || !strings.Contains(res.Error, "dial:") {
		t.Fatalf("nobody listening: %+v %v", res, err)
	}
}
