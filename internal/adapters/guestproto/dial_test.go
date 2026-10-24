package guestproto

import (
	"context"
	"io"
	"net"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"runner.codes/internal/protocol"
)

// fakeDialGuest serve um socket Unix que responde ao OpDial com o status dado e, no
// sucesso, devolve "eco:" + o que receber.
func fakeDialGuest(t *testing.T, status string) string {
	dir, _ := os.MkdirTemp("/tmp", "gpd")
	t.Cleanup(func() { os.RemoveAll(dir) })
	sock := filepath.Join(dir, "v.sock")
	l, err := net.Listen("unix", sock)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { l.Close() })
	go func() {
		c, err := l.Accept()
		if err != nil {
			return
		}
		defer c.Close()
		var job protocol.Job
		protocol.ReadFrame(c, &job)
		if job.Op != protocol.OpDial || (job.Port != 8080 && job.Path != "/var/run/docker.sock") {
			protocol.WriteFrame(c, protocol.Result{Status: "failed", Error: "wrong job"})
			return
		}
		protocol.WriteFrame(c, protocol.Result{Status: status, Error: "dial: connection refused"})
		if status == "success" {
			// Uma leitura só: o cliente não meio-fecha mais (vsockConn), então ler até EOF
			// esperaria para sempre — é o comportamento que o teste trava.
			buf := make([]byte, 64)
			n, _ := c.Read(buf)
			c.Write(append([]byte("eco:"), buf[:n]...))
		}
	}()
	return sock
}

// TestDialPort: sucesso devolve a conexão já como tubo; recusa do guest vira erro com a
// porta e o motivo; socket ausente é erro de conexão.
// TestDialUnix: o OpDial com Path chega ao guest e a conexão vira tubo; recusa traz o caminho.
func TestDialUnix(t *testing.T) {
	c, err := DialUnix(context.Background(), fakeDialGuest(t, "success"), "/var/run/docker.sock")
	if err != nil {
		t.Fatal(err)
	}
	c.Write([]byte("ping"))
	// O meio-fechamento não chega ao guest (no vsock da libkrun ele derruba a conexão e a
	// resposta se perde): CloseWrite é aceito e a resposta ainda vem inteira.
	if err := c.(interface{ CloseWrite() error }).CloseWrite(); err != nil {
		t.Fatal(err)
	}
	if b, _ := io.ReadAll(c); string(b) != "eco:ping" {
		t.Fatalf("pipe: %q", b)
	}
	if _, err := DialUnix(context.Background(), fakeDialGuest(t, "failed"), "/var/run/docker.sock"); err == nil || !strings.Contains(err.Error(), "guest socket /var/run/docker.sock") {
		t.Fatalf("refusal: %v", err)
	}
}

func TestDialPort(t *testing.T) {
	c, err := DialPort(context.Background(), fakeDialGuest(t, "success"), 8080)
	if err != nil {
		t.Fatal(err)
	}
	c.Write([]byte("oi"))
	if b, _ := io.ReadAll(c); string(b) != "eco:oi" {
		t.Fatalf("pipe: %q", b)
	}
	if _, err := DialPort(context.Background(), fakeDialGuest(t, "failed"), 8080); err == nil || !strings.Contains(err.Error(), "guest port 8080: dial: connection refused") {
		t.Fatalf("refusal: %v", err)
	}
	if _, err := DialPort(context.Background(), "/nonexistent/v.sock", 8080); err == nil {
		t.Fatal("missing socket must fail")
	}
}
