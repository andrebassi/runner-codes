package netgw

import (
	"io"
	"net"
	"net/netip"
	"os"
	"path/filepath"
	"testing"
	"time"

	"github.com/miekg/dns"
	"gvisor.dev/gvisor/pkg/tcpip"
	"gvisor.dev/gvisor/pkg/tcpip/adapters/gonet"
	"gvisor.dev/gvisor/pkg/tcpip/network/ipv4"

	"runner.codes/internal/domain"
)

// fabricMachine sobe um gateway com guest falso numa rede nomeada.
func fabricMachine(t *testing.T, dir, name, ip string, enabled bool) *guestStack {
	t.Helper()
	p, _ := domain.Network{Enabled: enabled, Name: "lab"}.OutboundPolicy()
	host, guest := net.Pipe()
	gw, err := Start(host, Config{Policy: p, Upstream: "127.0.0.1:1", MachineName: name,
		Fabric: &Fabric{Dir: dir, Name: "lab", Self: netip.MustParseAddr(ip)}})
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { gw.Close() })
	return newGuest(t, guest)
}

// TestFabric: duas máquinas da mesma rede se alcançam pelo IP da rede (mesmo sem --net: é
// tráfego interno), o guest de destino vê o IP de quem conectou, o DNS da rede resolve
// <máquina>.lab.internal, máquina desconhecida ou nome forjado dá NXDOMAIN, e máquina parada
// (sem socket) recusa na hora.
func TestFabric(t *testing.T) {
	dir, err := os.MkdirTemp("/tmp", "fab")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(dir)
	a := fabricMachine(t, dir, "alpha", "100.98.0.10", false)
	b := fabricMachine(t, dir, "beta", "100.98.0.11", false)

	l, err := gonet.ListenTCP(b.s, tcpip.FullAddress{NIC: 1, Port: 8080}, ipv4.ProtocolNumber)
	if err != nil {
		t.Fatal(err)
	}
	defer l.Close()
	from := make(chan string, 1)
	go func() {
		c, err := l.Accept()
		if err != nil {
			return
		}
		from <- c.RemoteAddr().String()
		c.Write([]byte("ola de beta"))
		c.Close()
	}()
	c, err := a.dial("100.98.0.11:8080")
	if err != nil {
		t.Fatal(err)
	}
	c.SetReadDeadline(time.Now().Add(5 * time.Second))
	if got, _ := io.ReadAll(c); string(got) != "ola de beta" {
		t.Fatalf("got %q", got)
	}
	if src := <-from; src != "100.98.0.10:"+src[len("100.98.0.10:"):] || src[:len("100.98.0.10")] != "100.98.0.10" {
		t.Fatalf("the destination guest must see the caller's network IP, got %s", src)
	}
	if m := a.lookup(t, "100.96.0.1", "beta.lab.internal"); m.Rcode != dns.RcodeSuccess || m.Answer[0].(*dns.A).A.String() != "100.98.0.11" {
		t.Fatalf("dns: %v", m)
	}
	// AAAA de nome que existe: NOERROR vazio. NXDOMAIN aqui fazia o musl descartar o A também.
	if m := a.lookupType(t, "100.96.0.1", "beta.lab.internal", dns.TypeAAAA); m.Rcode != dns.RcodeSuccess || len(m.Answer) != 0 {
		t.Fatalf("aaaa of a known name must be NODATA: %v", m)
	}
	for _, name := range []string{"ghost.lab.internal", "x.y.lab.internal"} {
		if m := a.lookup(t, "100.96.0.1", name); m.Rcode != dns.RcodeNameError {
			t.Fatalf("%s must be NXDOMAIN: %v", name, m)
		}
	}
	start := time.Now()
	if _, err := a.dial("100.98.0.99:80"); err == nil || time.Since(start) > 3*time.Second {
		t.Fatalf("a stopped machine must refuse fast: %v", err)
	}
	if _, err := os.Stat(filepath.Join(dir, "beta.ip")); err != nil {
		t.Fatal("the machine must publish its name")
	}
}

// TestFabricHeaderErrors: conexão que não manda o cabeçalho é fechada; porta sem ninguém no
// guest de destino fecha a conexão do outro lado.
func TestFabricHeaderErrors(t *testing.T) {
	dir, _ := os.MkdirTemp("/tmp", "fab")
	defer os.RemoveAll(dir)
	fabricMachine(t, dir, "solo", "100.98.0.20", false)
	sock := filepath.Join(dir, "100.98.0.20.sock")
	c, err := net.Dial("unix", sock)
	if err != nil {
		t.Fatal(err)
	}
	c.Write([]byte{1, 2}) // cabeçalho incompleto
	c.(*net.UnixConn).CloseWrite()
	c.SetReadDeadline(time.Now().Add(7 * time.Second))
	if _, err := io.ReadAll(c); err != nil {
		t.Fatalf("a short header must just close: %v", err)
	}
	c, _ = net.Dial("unix", sock)
	c.Write([]byte{100, 98, 0, 1, 0, 9}) // porta 9: ninguém escuta no guest
	c.SetReadDeadline(time.Now().Add(7 * time.Second))
	if b, _ := io.ReadAll(c); len(b) != 0 {
		t.Fatal("nothing listening: the connection must close")
	}
}

// TestFabricSetupErrors: diretório inutilizável falha no Start (sem deixar gateway de pé).
func TestFabricSetupErrors(t *testing.T) {
	p, _ := domain.Network{Name: "lab"}.OutboundPolicy()
	file := filepath.Join(t.TempDir(), "file")
	os.WriteFile(file, nil, 0o600)
	host, _ := net.Pipe()
	if _, err := Start(host, Config{Policy: p, Fabric: &Fabric{Dir: filepath.Join(file, "sub"), Name: "lab", Self: netip.MustParseAddr("100.98.0.30")}}); err == nil {
		t.Fatal("a directory under a file must fail")
	}
}

// TestFabricPing: o ping na rede nomeada volta do vizinho ligado e do próprio IP da rede,
// mesmo sem --net; máquina parada não responde, e uma rede com outro diretório não alcança o
// mesmo endereço (o isolamento das redes vale para o ICMP como vale para o TCP).
func TestFabricPing(t *testing.T) {
	dir, err := os.MkdirTemp("/tmp", "fabp")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(dir)
	other, err := os.MkdirTemp("/tmp", "fabo")
	if err != nil {
		t.Fatal(err)
	}
	defer os.RemoveAll(other)
	a := fabricMachine(t, dir, "alpha", "100.98.0.10", false)
	fabricMachine(t, dir, "beta", "100.98.0.11", false)
	x := fabricMachine(t, other, "xis", "100.98.0.20", false)
	if !a.ping(t, "100.98.0.11", 3*time.Second) {
		t.Fatal("the running neighbor answers")
	}
	if !a.ping(t, "100.98.0.10", 3*time.Second) {
		t.Fatal("the machine's own network IP answers")
	}
	if a.ping(t, "100.98.0.12", 500*time.Millisecond) {
		t.Fatal("an address no machine holds must not answer")
	}
	if x.ping(t, "100.98.0.11", 500*time.Millisecond) {
		t.Fatal("another network must not reach this one")
	}
}
