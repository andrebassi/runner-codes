package guest

import (
	"errors"
	"net/netip"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"testing"

	"runner.code/internal/protocol"
)

// envOf devolve um get sobre um mapa.
func envOf(m map[string]string) func(string) string { return func(k string) string { return m[k] } }

// TestGatewayOnLink: gateway dentro do prefixo (gateway em userspace, flannel) não precisa de rota de
// link; o do Calico (/32 com 169.254.1.1) precisa; sem gateway, não.
func TestGatewayOnLink(t *testing.T) {
	in := NetConfig{Addr: netip.MustParsePrefix("10.42.0.7/24"), Gateway: netip.MustParseAddr("10.42.0.1")}
	calico := NetConfig{Addr: netip.MustParsePrefix("10.1.1.66/32"), Gateway: netip.MustParseAddr("169.254.1.1")}
	if in.GatewayOnLink() || !calico.GatewayOnLink() || (NetConfig{Addr: in.Addr}).GatewayOnLink() {
		t.Fatal("GatewayOnLink")
	}
}

// TestNetConfigFromEnv: sem endereço não há rede; com tudo certo, endereço, gateway e DNS
// (com espaço e vazio ignorados); cada campo inválido é erro com o nome da variável.
func TestNetConfigFromEnv(t *testing.T) {
	if _, ok, err := NetConfigFromEnv(envOf(nil)); ok || err != nil {
		t.Fatal("no address means no network")
	}
	good := map[string]string{protocol.EnvNetAddr: "100.96.0.2/30", protocol.EnvNetGateway: "100.96.0.1", protocol.EnvNetDNS: "100.96.0.1, ,1.1.1.1"}
	cfg, ok, err := NetConfigFromEnv(envOf(good))
	if !ok || err != nil || cfg.Addr.String() != "100.96.0.2/30" || cfg.Gateway.String() != "100.96.0.1" || len(cfg.DNS) != 2 {
		t.Fatalf("%+v %v %v", cfg, ok, err)
	}
	for key, bad := range map[string]string{protocol.EnvNetAddr: "nope", protocol.EnvNetGateway: "x", protocol.EnvNetDNS: "1.1.1.1,y"} {
		m := map[string]string{}
		for k, v := range good {
			m[k] = v
		}
		m[key] = bad
		if _, _, err := NetConfigFromEnv(envOf(m)); err == nil || !strings.Contains(err.Error(), key) {
			t.Fatalf("%s: %v", key, err)
		}
	}
}

// TestConfigureNetwork: monta a placa e grava o resolv.conf (trocando um link simbólico
// por arquivo); sem DNS não mexe no resolv.conf; falha da placa é erro; sem rede não faz
// nada; fora do Linux a configuração real recusa.
func TestConfigureNetwork(t *testing.T) {
	dir := t.TempDir()
	target := filepath.Join(dir, "stub")
	os.WriteFile(target, []byte("old"), 0o644)
	resolvConfPath = filepath.Join(dir, "resolv.conf")
	os.Symlink(target, resolvConfPath)
	var got NetConfig
	configureLink = func(c NetConfig) error { got = c; return nil }
	defer func() { configureLink, resolvConfPath = configureLinkOS, "/etc/resolv.conf" }()

	env := map[string]string{protocol.EnvNetAddr: "100.96.0.2/30", protocol.EnvNetGateway: "100.96.0.1", protocol.EnvNetDNS: "100.96.0.1"}
	if err := ConfigureNetwork(envOf(env)); err != nil || got.Gateway != netip.MustParseAddr("100.96.0.1") {
		t.Fatalf("%v %+v", err, got)
	}
	fi, _ := os.Lstat(resolvConfPath)
	b, _ := os.ReadFile(resolvConfPath)
	if fi.Mode()&os.ModeSymlink != 0 || !strings.Contains(string(b), "nameserver 100.96.0.1\noptions timeout:2 attempts:2") {
		t.Fatalf("resolv.conf: %q symlink=%v", b, fi.Mode()&os.ModeSymlink != 0)
	}
	if old, _ := os.ReadFile(target); string(old) != "old" {
		t.Fatal("the symlink target must be left alone")
	}
	os.WriteFile(resolvConfPath, []byte("keep"), 0o644)
	delete(env, protocol.EnvNetDNS)
	ConfigureNetwork(envOf(env))
	if b, _ := os.ReadFile(resolvConfPath); string(b) != "keep" {
		t.Fatal("no DNS must leave resolv.conf alone")
	}
	env[protocol.EnvNetDNS] = "1.1.1.1"
	resolvConfPath = filepath.Join(dir, "missing", "resolv.conf")
	if err := ConfigureNetwork(envOf(env)); err == nil || !strings.Contains(err.Error(), "resolv.conf") {
		t.Fatalf("unwritable resolv.conf: %v", err)
	}
	configureLink = func(NetConfig) error { return errors.New("no eth0") }
	if err := ConfigureNetwork(envOf(env)); err == nil || !strings.Contains(err.Error(), "eth0") {
		t.Fatalf("link failure: %v", err)
	}
	if err := ConfigureNetwork(envOf(nil)); err != nil {
		t.Fatal("no network is not an error")
	}
	if runtime.GOOS != "linux" && configureLinkOS(NetConfig{}) != errNoNetSupport {
		t.Fatal("outside Linux the real configuration must refuse")
	}
}
