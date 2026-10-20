package app

import (
	"context"
	"net/netip"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"runner.code/internal/domain"
)

// TestNamedNetwork: o create dá a cada máquina da rede o primeiro IP livre dela (redes
// diferentes contam separado); o start leva à spec o diretório da rede, o mesmo para as
// máquinas da mesma rede e outro para outra rede, com o nome da máquina; sem NetworksDir a
// rede nomeada é recusada antes de gravar qualquer coisa.
func TestNamedNetwork(t *testing.T) {
	m, st, _, d, _ := machinesFixture(t)
	ctx := context.Background()
	if _, err := m.Create(ctx, domain.Machine{Name: "x", Stack: "python", Network: domain.Network{Name: "lab"}}); errCode(err) != domain.CodeConfig {
		t.Fatalf("named network without a networks dir: %v", err)
	}
	if _, err := st.Load("x"); err == nil {
		t.Fatal("a refused create must not leave a record")
	}
	m.NetworksDir = "/n"
	a, err := m.Create(ctx, domain.Machine{Name: "a", Stack: "python", Network: domain.Network{Name: "lab"}})
	if err != nil || a.Network.Address != "100.98.0.10" {
		t.Fatalf("first member: %+v %v", a.Network, err)
	}
	b, _ := m.Create(ctx, domain.Machine{Name: "b", Stack: "python", Network: domain.Network{Name: "lab"}})
	c, _ := m.Create(ctx, domain.Machine{Name: "c", Stack: "python", Network: domain.Network{Name: "other"}})
	if b.Network.Address != "100.98.0.11" || c.Network.Address != "100.98.0.10" {
		t.Fatalf("addresses are per network: b=%s c=%s", b.Network.Address, c.Network.Address)
	}
	plain, _ := m.Create(ctx, domain.Machine{Name: "p", Stack: "python", Network: domain.Network{Enabled: true}})
	if plain.Network.Address != "" {
		t.Fatal("no named network, no address")
	}
	dirs := map[string]string{}
	for _, n := range []string{"a", "b", "c", "p"} {
		if _, err := m.Start(ctx, n); err != nil {
			t.Fatal(err)
		}
		s := d.specs[len(d.specs)-1].Net
		if s == nil || s.Machine != n {
			t.Fatalf("%s: the spec must carry the machine name: %+v", n, s)
		}
		dirs[n] = s.FabricDir
	}
	if dirs["a"] == "" || dirs["a"] != dirs["b"] || dirs["a"] == dirs["c"] || dirs["p"] != "" {
		t.Fatalf("fabric dirs: %v", dirs)
	}
	if !strings.HasPrefix(dirs["a"], "/n/") || len(dirs["a"]) != len("/n/")+12 {
		t.Fatalf("the dir is a short hash under NetworksDir: %s", dirs["a"])
	}
}

// TestNamedNetworkFull: rede com todos os IPs tomados recusa o create com o motivo, e uma
// rede cheia não impede outra rede de alocar.
func TestNamedNetworkFull(t *testing.T) {
	m, st, _, _, _ := machinesFixture(t)
	m.NetworksDir = "/n"
	// Registros gravados direto no store, um por IP da faixa: o create não tem o que dar.
	for a := netip.MustParseAddr("100.98.0.10"); domain.FabricPrefix.Contains(a); a = a.Next() {
		n := "m" + strings.ReplaceAll(a.String(), ".", "-")
		st.ms[n] = domain.Machine{Name: n, Stack: "python", Network: domain.Network{Name: "lab", Address: a.String()}}
	}
	ctx := context.Background()
	if _, err := m.Create(ctx, domain.Machine{Name: "y", Stack: "python", Network: domain.Network{Name: "lab"}}); errCode(err) != domain.CodeConfig || !strings.Contains(err.Error(), "full") {
		t.Fatalf("a full network must refuse with the reason: %v", err)
	}
	if mc, err := m.Create(ctx, domain.Machine{Name: "w", Stack: "python", Network: domain.Network{Name: "lab2"}}); err != nil || mc.Network.Address != "100.98.0.10" {
		t.Fatalf("another network allocates on its own: %+v %v", mc.Network, err)
	}
}

// TestNamedNetworkLeave: stop e delete tiram o nome e o socket da máquina do diretório da
// rede (o SIGKILL não deixa o gateway limpar), sem tocar nos de outra máquina.
func TestNamedNetworkLeave(t *testing.T) {
	m, _, ps, _, g := machinesFixture(t)
	ctx := context.Background()
	m.NetworksDir = t.TempDir()
	m.Create(ctx, domain.Machine{Name: "a", Stack: "python", Network: domain.Network{Name: "lab"}})
	m.Create(ctx, domain.Machine{Name: "b", Stack: "python", Network: domain.Network{Name: "lab"}})
	dir := m.fabricDir("lab")
	os.MkdirAll(dir, 0o700)
	plant := func() {
		for _, f := range []string{"a.ip", "100.98.0.10.sock", "b.ip", "100.98.0.11.sock"} {
			os.WriteFile(filepath.Join(dir, f), nil, 0o600)
		}
	}
	gone := func(f string) bool { _, err := os.Stat(filepath.Join(dir, f)); return os.IsNotExist(err) }
	plant()
	m.Start(ctx, "a")
	g.shutdown = func() { ps.mu.Lock(); ps.alive = map[int]bool{}; ps.mu.Unlock() }
	if err := m.Stop(ctx, "a", -1); err != nil {
		t.Fatal(err)
	}
	if !gone("a.ip") || !gone("100.98.0.10.sock") || gone("b.ip") || gone("100.98.0.11.sock") {
		t.Fatal("stop must remove only this machine from the network")
	}
	plant()
	if err := m.Stop(ctx, "a", -1); err != nil || !gone("a.ip") {
		t.Fatal("stopping a stopped machine still cleans leftovers of a crash")
	}
	plant()
	m.Start(ctx, "b")
	if err := m.Delete(ctx, "b", true); err != nil || !gone("b.ip") || !gone("100.98.0.11.sock") {
		t.Fatalf("delete --force must leave the network: %v", err)
	}
}
