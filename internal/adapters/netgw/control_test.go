package netgw

import (
	"fmt"
	"net"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"runner.codes/internal/domain"
)

// controlSocket devolve um caminho curto: socket Unix no macOS tem teto de 104 bytes.
func controlSocket(t *testing.T) string {
	d, err := os.MkdirTemp("/tmp", "ngc")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { os.RemoveAll(d) })
	return filepath.Join(d, "egress.sock")
}

// TestControl: a política nova chega pelo socket (0600) e passa a valer; JSON inválido,
// política inválida e linha vazia recebem o motivo; socket de máquina parada é erro claro;
// caminho inutilizável falha no ServeControl.
func TestControl(t *testing.T) {
	r := newRig(t, domain.Egress{}, true, nil)
	sock := controlSocket(t)
	if err := r.gw.ServeControl(sock, true); err != nil {
		t.Fatal(err)
	}
	if fi, _ := os.Stat(sock); fi.Mode().Perm() != 0o600 {
		t.Fatalf("mode %v", fi.Mode().Perm())
	}
	if err := UpdatePolicy(sock, domain.Egress{AllowCIDRs: []string{"9.9.9.9"}}); err != nil {
		t.Fatal(err)
	}
	if _, err := r.guest.dial("1.2.3.4:80"); err == nil {
		t.Fatal("the new policy must apply to the next connection")
	}
	if err := UpdatePolicy(sock, domain.Egress{DenyCIDRs: []string{"x"}}); err == nil || !strings.Contains(err.Error(), "deny_cidrs") {
		t.Fatalf("invalid policy: %v", err)
	}
	c, _ := net.Dial("unix", sock)
	fmt.Fprintln(c, "{not json")
	buf := make([]byte, 256)
	n, _ := c.Read(buf)
	c.Close()
	if !strings.HasPrefix(string(buf[:n]), "error:") {
		t.Fatalf("bad json: %q", buf[:n])
	}
	c, _ = net.Dial("unix", sock)
	c.Close() // conexão sem linha nenhuma: o servidor só fecha
	if err := UpdatePolicy(filepath.Join(filepath.Dir(sock), "none.sock"), domain.Egress{}); err == nil || !strings.Contains(err.Error(), "not reachable") {
		t.Fatalf("stopped machine: %v", err)
	}
	if err := r.gw.ServeControl("/nonexistent/dir/x.sock", true); err == nil {
		t.Fatal("unusable path must fail")
	}
	// Servidor que fecha sem responder: o cliente diz que não houve resposta.
	mute := controlSocket(t)
	l, _ := net.Listen("unix", mute)
	defer l.Close()
	go func() {
		c, err := l.Accept()
		if err == nil {
			c.Close()
		}
	}()
	if err := UpdatePolicy(mute, domain.Egress{}); err == nil || !strings.Contains(err.Error(), "no answer") {
		t.Fatalf("no answer: %v", err)
	}
}
