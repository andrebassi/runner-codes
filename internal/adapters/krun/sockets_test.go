package krun

import (
	"context"
	"errors"
	"io"
	"net"
	"os"
	"path/filepath"
	"sync"
	"testing"

	"runner.code/internal/ports"
)

// TestSocketBridges: quem conecta no socket do host fala com o socket do guest (o dial
// recebe o caminho do guest e o socket da máquina); o socket do host nasce 0600 e some no
// Close; guest que recusa fecha a conexão do cliente; caminho impossível falha no start.
func TestSocketBridges(t *testing.T) {
	dir, _ := os.MkdirTemp("/tmp", "skb")
	defer os.RemoveAll(dir)
	// gotVsock e gotPath são escritos pelas goroutines das conexões: mu os protege.
	var mu sync.Mutex
	var gotVsock, gotPath string
	orig := dialUnix
	defer func() { dialUnix = orig }()
	dialUnix = func(_ context.Context, vsock, path string) (net.Conn, error) {
		mu.Lock()
		gotVsock, gotPath = vsock, path
		mu.Unlock()
		if path == "/refuse" {
			return nil, errors.New("refused")
		}
		a, b := net.Pipe()
		go func() {
			buf := make([]byte, 4)
			io.ReadFull(b, buf)
			b.Write(append([]byte("eco:"), buf...))
			b.Close()
		}()
		return a, nil
	}
	host := filepath.Join(dir, "docker.sock")
	refuse := filepath.Join(dir, "refuse.sock")
	b, err := startSockets([]ports.VMSocket{{Host: host, Guest: "/var/run/docker.sock"}, {Host: refuse, Guest: "/refuse"}}, "/m/v.sock")
	if err != nil {
		t.Fatal(err)
	}
	if fi, err := os.Stat(host); err != nil || fi.Mode().Perm() != 0o600 {
		t.Fatalf("host socket must be 0600: %v", err)
	}
	c, err := net.Dial("unix", host)
	if err != nil {
		t.Fatal(err)
	}
	c.Write([]byte("ping"))
	got, _ := io.ReadAll(c)
	mu.Lock()
	vs, gp := gotVsock, gotPath
	mu.Unlock()
	if string(got) != "eco:ping" || vs != "/m/v.sock" || gp != "/var/run/docker.sock" {
		t.Fatalf("bridge: %q via %s to %s", got, vs, gp)
	}
	r, _ := net.Dial("unix", refuse)
	if got, _ := io.ReadAll(r); len(got) != 0 {
		t.Fatal("a refused guest socket must close the client")
	}
	b.Close()
	if _, err := os.Stat(host); !os.IsNotExist(err) {
		t.Fatal("Close must remove the host socket")
	}
	if _, err := startSockets([]ports.VMSocket{{Host: filepath.Join(dir, "no", "x.sock"), Guest: "/g"}}, ""); err == nil {
		t.Fatal("an impossible host path must fail")
	}
}
