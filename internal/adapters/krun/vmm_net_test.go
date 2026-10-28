package krun

import (
	"net"
	"os"
	"path/filepath"
	"strings"
	"syscall"
	"testing"
	"time"

	"runner.codes/internal/adapters/netgw"
	"runner.codes/internal/credca"
	"runner.codes/internal/domain"
	"runner.codes/internal/ports"
	"runner.codes/internal/protocol"
)

// shortDir devolve um diretório curto: socket Unix no macOS tem teto de 104 bytes no
// caminho, e o TempDir do teste passa disso.
func shortDir(t *testing.T) string {
	d, err := os.MkdirTemp("/tmp", "rcn")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { os.RemoveAll(d) })
	return d
}

// TestConfigureNet: com o fd da rede, a virtio-net entra depois do vsock sem TSI e antes
// do exec, com o MAC do guest; sem fd não há placa; a falha da chamada vira erro com o nome.
func TestConfigureNet(t *testing.T) {
	f := &fakeAPI{}
	if _, err := configure(f, jobSpec(), 9); err != nil {
		t.Fatal(err)
	}
	got := strings.Join(f.calls, ",")
	if f.netFD != 9 || string(f.netMAC) != string(netgw.GuestMAC) || f.tsi != 0 ||
		!strings.Contains(got, "krun_add_vsock,krun_add_net_unixstream") ||
		strings.Index(got, "krun_add_net_unixstream") > strings.Index(got, "krun_set_exec") {
		t.Fatalf("net: fd=%d tsi=%d calls=%s", f.netFD, f.tsi, got)
	}
	f = &fakeAPI{}
	Configure(f, jobSpec())
	if strings.Contains(strings.Join(f.calls, ","), "net") {
		t.Fatal("no fd, no NIC")
	}
	if _, err := configure(&fakeAPI{failOn: "krun_add_net_unixstream"}, jobSpec(), 9); err == nil || !strings.Contains(err.Error(), "krun_add_net_unixstream") {
		t.Fatalf("failure: %v", err)
	}
}

// TestValidateNet: gateway e TSI não vão juntos; porta inválida na rede é recusada.
func TestValidateNet(t *testing.T) {
	s := jobSpec()
	s.Net = &ports.VMNet{Network: domain.Network{Enabled: true}}
	if err := s.Validate(); err != nil {
		t.Fatal(err)
	}
	s.Network = true
	if err := s.Validate(); err == nil {
		t.Fatal("net and network together must be refused")
	}
	s.Network = false
	s.Net.Network.Ports = []domain.PortMap{{Host: 0, Guest: 1}}
	if err := s.Validate(); err == nil {
		t.Fatal("bad port must be refused")
	}
}

// TestStartNetwork: o gateway sobe no socketpair, o env manda o guest montar a eth0 com o
// gateway como rota e DNS, o log de recusas é criado e o socket de controle troca a política.
func TestStartNetwork(t *testing.T) {
	dir := shortDir(t)
	n := &ports.VMNet{Network: domain.Network{Enabled: true}, DenialsLog: filepath.Join(dir, "deny.log"),
		ControlSocket: filepath.Join(dir, "egress.sock")}
	fd, env, gw, err := startNetwork(n, "")
	if err != nil {
		t.Fatal(err)
	}
	defer gw.Close()
	defer syscall.Close(fd)
	want := []string{protocol.EnvNetAddr + "=100.96.0.2/30", protocol.EnvNetGateway + "=100.96.0.1", protocol.EnvNetDNS + "=100.96.0.1"}
	if strings.Join(env, " ") != strings.Join(want, " ") {
		t.Fatalf("env: %v", env)
	}
	if _, err := os.Stat(n.DenialsLog); err != nil {
		t.Fatal("denials log must exist")
	}
	if fi, _ := os.Stat(n.ControlSocket); fi.Mode().Perm() != 0o600 {
		t.Fatalf("control socket mode %v", fi.Mode().Perm())
	}
	if err := netgw.UpdatePolicy(n.ControlSocket, domain.Egress{AllowHosts: []string{"example.com"}}); err != nil {
		t.Fatal(err)
	}
	if err := netgw.UpdatePolicy(n.ControlSocket, domain.Egress{AllowCIDRs: []string{"bad"}}); err == nil || !strings.Contains(err.Error(), "allow_cidrs") {
		t.Fatalf("bad policy must be refused with the reason: %v", err)
	}
	if err := netgw.UpdatePolicy(filepath.Join(dir, "none.sock"), domain.Egress{}); err == nil {
		t.Fatal("no machine, no update")
	}
}

// TestStartNetworkFabric: com rede nomeada o gateway publica a máquina no diretório da rede
// (socket pelo IP, arquivo pelo nome) e some dele no Close; endereço inválido falha antes.
func TestStartNetworkFabric(t *testing.T) {
	dir := shortDir(t)
	n := &ports.VMNet{Network: domain.Network{Name: "lab", Address: "100.98.0.10"}, FabricDir: dir, Machine: "alpha"}
	fd, _, gw, err := startNetwork(n, "")
	if err != nil {
		t.Fatal(err)
	}
	defer syscall.Close(fd)
	if b, err := os.ReadFile(filepath.Join(dir, "alpha.ip")); err != nil || string(b) != "100.98.0.10" {
		t.Fatalf("name file: %q %v", b, err)
	}
	sock := filepath.Join(dir, "100.98.0.10.sock")
	if _, err := os.Stat(sock); err != nil {
		t.Fatal("the machine socket must exist while it runs")
	}
	gw.Close()
	if _, err := os.Stat(sock); !os.IsNotExist(err) {
		t.Fatal("the socket must go with the gateway")
	}
	if _, err := os.Stat(filepath.Join(dir, "alpha.ip")); !os.IsNotExist(err) {
		t.Fatal("the name must go with the gateway, or a reused IP answers for a deleted machine")
	}
	bad := &ports.VMNet{Network: domain.Network{Name: "lab", Address: "nope"}, FabricDir: dir, Machine: "beta"}
	if _, _, _, err := startNetwork(bad, ""); err == nil || !strings.Contains(err.Error(), "named network") {
		t.Fatalf("bad address: %v", err)
	}
}

// TestInterceptRoots: sem proxy de saída, o interceptador fica nas raízes do sistema (nil); com ele, as raízes ganham
// a CA do proxy — senão a troca de token recusaria o certificado que o proxy apresenta. Arquivo ausente ou sem
// certificado é erro no start, não uma troca que falha calada depois.
func TestInterceptRoots(t *testing.T) {
	if pool, err := interceptRoots(""); pool != nil || err != nil {
		t.Fatalf("no proxy: %v %v", pool, err)
	}
	dir := shortDir(t)
	cert, _, _ := credca.New([]string{"x.io"}, time.Now())
	good := filepath.Join(dir, "egress-ca.pem")
	os.WriteFile(good, cert, 0o644)
	if pool, err := interceptRoots(good); pool == nil || err != nil {
		t.Fatalf("with the proxy CA: %v", err)
	}
	if _, err := interceptRoots(filepath.Join(dir, "none.pem")); err == nil {
		t.Fatal("missing CA file")
	}
	bad := filepath.Join(dir, "bad.pem")
	os.WriteFile(bad, []byte("não é PEM"), 0o644)
	if _, err := interceptRoots(bad); err == nil || !strings.Contains(err.Error(), "no certificate") {
		t.Fatalf("file without a certificate: %v", err)
	}
	creds := []domain.Credential{{Name: "gh", Env: "RC_TEST_GH", Hosts: []string{"x.io"}, Placeholder: "RC_PLACEHOLDER_GH_1"}}
	ccert, ckey, _ := credca.New([]string{"x.io"}, time.Now())
	os.WriteFile(filepath.Join(dir, "ca.pem"), ccert, 0o644)
	os.WriteFile(filepath.Join(dir, "ca.key"), ckey, 0o600)
	n := &ports.VMNet{Network: domain.Network{Credentials: creds}, CACert: filepath.Join(dir, "ca.pem"), CAKey: filepath.Join(dir, "ca.key"), EgressCA: bad}
	if _, err := newInterceptor(n); err == nil {
		t.Fatal("a broken proxy CA must fail the interceptor")
	}
	n.EgressCA = good
	if ic, err := newInterceptor(n); err != nil || ic.Roots == nil {
		t.Fatalf("interceptor with the proxy CA: %v", err)
	}
}

// TestStartNetworkCredentials: com bindings, o gateway sobe com o interceptador carregado
// da CA da máquina, e o valor sai do ambiente deste processo; CA ausente ou quebrada falha
// antes de abrir descritor.
func TestStartNetworkCredentials(t *testing.T) {
	dir := shortDir(t)
	cert, key, _ := credca.New([]string{"api.github.com"}, time.Now())
	os.WriteFile(filepath.Join(dir, "ca.pem"), cert, 0o644)
	os.WriteFile(filepath.Join(dir, "ca.key"), key, 0o600)
	creds := []domain.Credential{{Name: "gh", Env: "RC_TEST_GH", Hosts: []string{"api.github.com"}, Placeholder: "RC_PLACEHOLDER_GH_1"}}
	n := &ports.VMNet{Network: domain.Network{Credentials: creds}, CACert: filepath.Join(dir, "ca.pem"), CAKey: filepath.Join(dir, "ca.key")}
	ic, err := newInterceptor(n)
	if err != nil || ic.Port != 443 || len(ic.Bindings) != 1 {
		t.Fatalf("interceptor: %+v %v", ic, err)
	}
	if _, err := ic.Resolve(creds[0]); err == nil || !strings.Contains(err.Error(), "RC_TEST_GH is not set") {
		t.Fatalf("unset variable: %v", err)
	}
	t.Setenv("RC_TEST_GH", "ghp-test")
	if v, err := ic.Resolve(creds[0]); err != nil || v != "ghp-test" {
		t.Fatalf("value from the environment: %q %v", v, err)
	}
	// Segredo de mesmo nome: a binding lê o arquivo (a cada requisição) ou a variável apontada.
	f := filepath.Join(dir, "tok")
	os.WriteFile(f, []byte("do-arquivo\n"), 0o600)
	if v, err := envValue(domain.Credential{Env: "X", SourceFile: f}); err != nil || v != "do-arquivo" {
		t.Fatalf("source file: %q %v", v, err)
	}
	os.WriteFile(f, []byte("rotacionado"), 0o600)
	if v, _ := envValue(domain.Credential{Env: "X", SourceFile: f}); v != "rotacionado" {
		t.Fatalf("the file is read per request: %q", v)
	}
	if _, err := envValue(domain.Credential{Env: "X", SourceFile: filepath.Join(dir, "none")}); err == nil {
		t.Fatal("missing source file")
	}
	if v, err := envValue(domain.Credential{Env: "X", SourceEnv: "RC_TEST_GH"}); err != nil || v != "ghp-test" {
		t.Fatalf("source env: %q %v", v, err)
	}
	fd, _, gw, err := startNetwork(n, "")
	if err != nil {
		t.Fatal(err)
	}
	gw.Close()
	syscall.Close(fd)
	for name, bad := range map[string]*ports.VMNet{
		"no cert": {Network: n.Network, CACert: filepath.Join(dir, "none"), CAKey: n.CAKey},
		"no key":  {Network: n.Network, CACert: n.CACert, CAKey: filepath.Join(dir, "none")},
		"bad pem": {Network: n.Network, CACert: n.CAKey, CAKey: n.CAKey},
	} {
		if _, _, _, err := startNetwork(bad, ""); err == nil || !strings.Contains(err.Error(), "credential CA") {
			t.Fatalf("%s: %v", name, err)
		}
	}
}

// TestStartNetworkErrors: política inválida, porta ocupada e socket de controle num
// diretório que não existe falham sem deixar o gateway de pé.
func TestStartNetworkErrors(t *testing.T) {
	if _, _, _, err := startNetwork(&ports.VMNet{Network: domain.Network{Egress: domain.Egress{DenyCIDRs: []string{"x"}}}}, ""); err == nil {
		t.Fatal("bad policy must fail")
	}
	l, _ := net.Listen("tcp", "127.0.0.1:0")
	defer l.Close()
	busy := uint16(l.Addr().(*net.TCPAddr).Port)
	if _, _, _, err := startNetwork(&ports.VMNet{Network: domain.Network{Ports: []domain.PortMap{{Host: busy, Guest: 1}}}}, ""); err == nil {
		t.Fatal("busy port must fail")
	}
	if _, _, _, err := startNetwork(&ports.VMNet{Network: domain.Network{Enabled: true}, ControlSocket: "/nonexistent/dir/x.sock"}, ""); err == nil {
		t.Fatal("unusable control socket must fail")
	}
}

// TestRunVMMWithNet: o RunVMM sobe a rede, passa o fd à libkrun e acrescenta o env do
// guest sem mexer no da spec; com erro de rede nem configura a VM.
func TestRunVMMWithNet(t *testing.T) {
	t.Setenv(sandboxDisabledEnv, "off")
	s := jobSpec()
	s.Env = []string{"A=1"}
	s.Net = &ports.VMNet{Network: domain.Network{Enabled: true}}
	f := &fakeAPI{failOn: "krun_start_enter"}
	if err := RunVMM(f, s, nil); err == nil {
		t.Fatal("fake start_enter fails")
	}
	if f.netFD <= 0 || len(s.Env) != 1 {
		t.Fatalf("fd=%d env=%v", f.netFD, s.Env)
	}
	s.Net = &ports.VMNet{Network: domain.Network{Egress: domain.Egress{AllowCIDRs: []string{"x"}}}}
	f = &fakeAPI{}
	if err := RunVMM(f, s, nil); err == nil || len(f.calls) != 0 {
		t.Fatalf("network error must stop before configuring: %v %v", err, f.calls)
	}
}
