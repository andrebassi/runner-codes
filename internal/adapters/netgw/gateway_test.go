package netgw

import (
	"bytes"
	"context"
	"encoding/binary"
	"io"
	"net"
	"net/netip"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/miekg/dns"
	"gvisor.dev/gvisor/pkg/buffer"
	"gvisor.dev/gvisor/pkg/tcpip"
	"gvisor.dev/gvisor/pkg/tcpip/adapters/gonet"
	"gvisor.dev/gvisor/pkg/tcpip/link/channel"
	"gvisor.dev/gvisor/pkg/tcpip/link/ethernet"
	"gvisor.dev/gvisor/pkg/tcpip/network/arp"
	"gvisor.dev/gvisor/pkg/tcpip/network/ipv4"
	"gvisor.dev/gvisor/pkg/tcpip/stack"
	gicmp "gvisor.dev/gvisor/pkg/tcpip/transport/icmp"
	"gvisor.dev/gvisor/pkg/tcpip/transport/tcp"
	"gvisor.dev/gvisor/pkg/tcpip/transport/udp"

	"runner.codes/internal/domain"
)

// guestStack é uma segunda pilha gVisor no papel do kernel do guest, do outro lado do
// socket, com o mesmo enquadramento da libkrun (4 bytes BE + frame Ethernet).
type guestStack struct {
	s    *stack.Stack
	link *channel.Endpoint
}

// newGuest liga a pilha do guest ao conn e configura IP, rota e ARP como o guest real.
func newGuest(t *testing.T, conn net.Conn) *guestStack {
	t.Helper()
	s := stack.New(stack.Options{
		NetworkProtocols:   []stack.NetworkProtocolFactory{ipv4.NewProtocol, arp.NewProtocol},
		TransportProtocols: []stack.TransportProtocolFactory{tcp.NewProtocol, udp.NewProtocol, gicmp.NewProtocol4},
	})
	link := channel.New(1024, mtu, tcpip.LinkAddress(GuestMAC))
	s.CreateNIC(1, ethernet.New(link))
	addr := tcpip.ProtocolAddress{Protocol: ipv4.ProtocolNumber, AddressWithPrefix: tcpip.AddrFrom4(GuestIP.As4()).WithPrefix()}
	addr.AddressWithPrefix.PrefixLen = PrefixLen
	s.AddProtocolAddress(1, addr, stack.AddressProperties{})
	subnet := tcpip.AddressWithPrefix{Address: tcpip.AddrFrom4(GuestIP.As4()), PrefixLen: PrefixLen}.Subnet()
	s.SetRouteTable([]tcpip.Route{{Destination: subnet, NIC: 1},
		{Destination: tcpip.AddressWithPrefix{Address: tcpip.AddrFrom4([4]byte{}), PrefixLen: 0}.Subnet(), Gateway: tcpip.AddrFrom4(GatewayIP.As4()), NIC: 1}})
	ctx, cancel := context.WithCancel(context.Background())
	go func() {
		for {
			pkt := link.ReadContext(ctx)
			if pkt == nil {
				return
			}
			v := pkt.ToView()
			b := v.AsSlice()
			out := make([]byte, 4+len(b))
			binary.BigEndian.PutUint32(out, uint32(len(b)))
			copy(out[4:], b)
			v.Release()
			pkt.DecRef()
			if _, err := conn.Write(out); err != nil {
				return
			}
		}
	}()
	go func() {
		var hdr [4]byte
		for {
			if _, err := io.ReadFull(conn, hdr[:]); err != nil {
				return
			}
			f := make([]byte, binary.BigEndian.Uint32(hdr[:]))
			if _, err := io.ReadFull(conn, f); err != nil {
				return
			}
			pkt := stack.NewPacketBuffer(stack.PacketBufferOptions{Payload: buffer.MakeWithData(f)})
			link.InjectInbound(0, pkt)
			pkt.DecRef()
		}
	}()
	t.Cleanup(func() { cancel(); link.Close(); s.Close() })
	return &guestStack{s: s, link: link}
}

// dial abre TCP do guest para dst.
func (g *guestStack) dial(dst string) (net.Conn, error) {
	ap := netip.MustParseAddrPort(dst)
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	return gonet.DialContextTCP(ctx, g.s, tcpip.FullAddress{NIC: 1, Addr: tcpip.AddrFrom4(ap.Addr().As4()), Port: ap.Port()}, ipv4.ProtocolNumber)
}

// udp abre UDP do guest para dst.
func (g *guestStack) udp(t *testing.T, dst string) *gonet.UDPConn {
	ap := netip.MustParseAddrPort(dst)
	c, err := gonet.DialUDP(g.s, nil, &tcpip.FullAddress{NIC: 1, Addr: tcpip.AddrFrom4(ap.Addr().As4()), Port: ap.Port()}, ipv4.ProtocolNumber)
	if err != nil {
		t.Fatal(err)
	}
	return c
}

// lookup pergunta um A ao resolvedor em server (porta 53) pelo guest.
func (g *guestStack) lookup(t *testing.T, server, name string) *dns.Msg {
	t.Helper()
	return g.lookupType(t, server, name, dns.TypeA)
}

// lookupType pergunta um tipo de registro qualquer ao DNS do gateway.
func (g *guestStack) lookupType(t *testing.T, server, name string, qtype uint16) *dns.Msg {
	t.Helper()
	c := g.udp(t, server+":53")
	defer c.Close()
	q := new(dns.Msg)
	q.SetQuestion(dns.Fqdn(name), qtype)
	raw, _ := q.Pack()
	c.Write(raw)
	c.SetReadDeadline(time.Now().Add(5 * time.Second))
	buf := make([]byte, 4096)
	n, err := c.Read(buf)
	if err != nil {
		t.Fatalf("lookup %s: %v", name, err)
	}
	m := new(dns.Msg)
	if err := m.Unpack(buf[:n]); err != nil {
		t.Fatal(err)
	}
	return m
}

// syncBuf é um log de auditoria seguro para o -race.
type syncBuf struct {
	mu sync.Mutex
	b  bytes.Buffer
}

// Write grava sob lock.
func (s *syncBuf) Write(p []byte) (int, error) { s.mu.Lock(); defer s.mu.Unlock(); return s.b.Write(p) }

// String lê sob lock.
func (s *syncBuf) String() string { s.mu.Lock(); defer s.mu.Unlock(); return s.b.String() }

// rig é um gateway com guest falso, servidores locais e o registro do que foi discado.
type rig struct {
	gw     *Gateway
	guest  *guestStack
	log    *syncBuf
	mu     sync.Mutex
	dialed []string
}

// echoTCP sobe um servidor TCP local que devolve "echo:" + o que receber.
func echoTCP(t *testing.T) string {
	l, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { l.Close() })
	go func() {
		for {
			c, err := l.Accept()
			if err != nil {
				return
			}
			go func() {
				b, _ := io.ReadAll(c)
				c.Write(append([]byte("echo:"), b...))
				c.Close()
			}()
		}
	}()
	return l.Addr().String()
}

// echoUDP sobe um servidor UDP local de eco.
func echoUDP(t *testing.T) string {
	pc, err := net.ListenPacket("udp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { pc.Close() })
	go func() {
		buf := make([]byte, 2048)
		for {
			n, a, err := pc.ReadFrom(buf)
			if err != nil {
				return
			}
			pc.WriteTo(append([]byte("u:"), buf[:n]...), a)
		}
	}()
	return pc.LocalAddr().String()
}

// fakeUpstream sobe um resolvedor que responde A 5.6.7.8 e AAAA (TTL 10) a qualquer nome.
func fakeUpstream(t *testing.T) string {
	pc, err := net.ListenPacket("udp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	srv := &dns.Server{PacketConn: pc, Handler: dns.HandlerFunc(func(w dns.ResponseWriter, r *dns.Msg) {
		m := new(dns.Msg)
		m.SetReply(r)
		m.Answer = append(m.Answer,
			&dns.A{Hdr: dns.RR_Header{Name: r.Question[0].Name, Rrtype: dns.TypeA, Class: dns.ClassINET, Ttl: 10}, A: net.IPv4(5, 6, 7, 8)},
			&dns.AAAA{Hdr: dns.RR_Header{Name: r.Question[0].Name, Rrtype: dns.TypeAAAA, Class: dns.ClassINET, Ttl: 10}, AAAA: net.ParseIP("2001:db8::1")})
		w.WriteMsg(m)
	})}
	go srv.ActivateAndServe()
	t.Cleanup(func() { srv.Shutdown() })
	return pc.LocalAddr().String()
}

// newRig liga gateway e guest por um par de sockets; todo destino discado vai para os
// servidores locais, e o rig anota o que o gateway quis discar.
func newRig(t *testing.T, e domain.Egress, enabled bool, ports []domain.PortMap, opts ...func(*Config)) *rig {
	t.Helper()
	p, err := domain.Network{Enabled: enabled, Egress: e, Ports: ports}.OutboundPolicy()
	if err != nil {
		t.Fatal(err)
	}
	tcpTo, udpTo := echoTCP(t), echoUDP(t)
	host, guest := net.Pipe()
	r := &rig{log: &syncBuf{}}
	var d net.Dialer
	cfg := Config{Policy: p, Ports: ports, Upstream: fakeUpstream(t), Denials: r.log,
		Dial: func(ctx context.Context, network, addr string) (net.Conn, error) {
			r.mu.Lock()
			r.dialed = append(r.dialed, network+" "+addr)
			r.mu.Unlock()
			if network == "udp" {
				return d.DialContext(ctx, network, udpTo)
			}
			if strings.HasSuffix(addr, ":1") {
				return nil, io.ErrClosedPipe // destino que recusa
			}
			return d.DialContext(ctx, network, tcpTo)
		}}
	for _, o := range opts {
		o(&cfg)
	}
	r.gw, err = Start(host, cfg)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { r.gw.Close() })
	r.guest = newGuest(t, guest)
	return r
}

// roundTrip manda msg por TCP do guest e devolve a resposta.
func roundTrip(t *testing.T, c net.Conn, msg string) string {
	t.Helper()
	c.Write([]byte(msg))
	c.(*gonet.TCPConn).CloseWrite()
	c.SetReadDeadline(time.Now().Add(5 * time.Second))
	b, _ := io.ReadAll(c)
	c.Close()
	return string(b)
}

// TestTCPOpen: com a rede aberta, o guest conecta a um IP qualquer e o gateway disca o
// mesmo destino no host; o dado vai e volta.
func TestTCPOpen(t *testing.T) {
	r := newRig(t, domain.Egress{}, true, nil)
	c, err := r.guest.dial("1.2.3.4:80")
	if err != nil {
		t.Fatal(err)
	}
	if got := roundTrip(t, c, "ping"); got != "echo:ping" {
		t.Fatalf("got %q", got)
	}
	r.mu.Lock()
	defer r.mu.Unlock()
	if len(r.dialed) != 1 || r.dialed[0] != "tcp 1.2.3.4:80" {
		t.Fatalf("dialed: %v", r.dialed)
	}
}

// TestTCPDenied: destino fora da política recebe RST na hora (nada de timeout), nada é
// discado no host, e a recusa vai para o log; metadata é negada até com rede aberta; o
// destino que recusa no host aparece como recusa no guest.
func TestTCPDenied(t *testing.T) {
	r := newRig(t, domain.Egress{AllowCIDRs: []string{"9.9.9.9"}}, true, nil)
	start := time.Now()
	if _, err := r.guest.dial("1.2.3.4:443"); err == nil {
		t.Fatal("must be refused")
	}
	if d := time.Since(start); d > 2*time.Second {
		t.Fatalf("refusal must be fast, took %v", d)
	}
	if !strings.Contains(r.log.String(), "deny tcp 1.2.3.4:443: not in the allow list") {
		t.Fatalf("audit log: %q", r.log.String())
	}
	if _, err := r.guest.dial("9.9.9.9:1"); err == nil {
		t.Fatal("a destination refused on the host must be refused in the guest")
	}
	open := newRig(t, domain.Egress{}, true, nil)
	if _, err := open.guest.dial("169.254.169.254:80"); err == nil {
		t.Fatal("cloud metadata must be blocked even with open network")
	}
	open.mu.Lock()
	defer open.mu.Unlock()
	if len(open.dialed) != 0 {
		t.Fatalf("nothing may be dialed for a denied destination: %v", open.dialed)
	}
}

// TestGatewayIsHostLoopback: o IP do gateway é o 127.0.0.1 do host, fechado por padrão e
// aberto só com allow_cidrs explícito.
func TestGatewayIsHostLoopback(t *testing.T) {
	r := newRig(t, domain.Egress{}, true, nil)
	if _, err := r.guest.dial("100.96.0.1:8000"); err == nil {
		t.Fatal("host loopback must be closed by default")
	}
	ok := newRig(t, domain.Egress{AllowCIDRs: []string{"127.0.0.1"}}, true, nil)
	c, err := ok.guest.dial("100.96.0.1:8000")
	if err != nil {
		t.Fatal(err)
	}
	roundTrip(t, c, "x")
	ok.mu.Lock()
	defer ok.mu.Unlock()
	if ok.dialed[0] != "tcp 127.0.0.1:8000" {
		t.Fatalf("dialed: %v", ok.dialed)
	}
}

// TestDNSFilterAndLearning: nome permitido resolve pelo upstream e o IP vira aprendido
// (a conexão a ele passa); nome negado recebe NXDOMAIN e vai para o log; resolvedor fixo
// (8.8.8.8) também cai no filtro; revogar o nome esquece o IP.
func TestDNSFilterAndLearning(t *testing.T) {
	r := newRig(t, domain.Egress{AllowHosts: []string{"allowed.example"}}, true, nil)
	m := r.guest.lookup(t, "100.96.0.1", "api.allowed.example")
	if m.Rcode != dns.RcodeSuccess || len(m.Answer) != 2 || m.Answer[0].(*dns.A).A.String() != "5.6.7.8" {
		t.Fatalf("allowed lookup: %v", m)
	}
	c, err := r.guest.dial("5.6.7.8:443")
	if err != nil {
		t.Fatalf("a learned IP must pass: %v", err)
	}
	c.Close()
	if m := r.guest.lookup(t, "8.8.8.8", "atacante.example"); m.Rcode != dns.RcodeNameError {
		t.Fatalf("blocked name through a hardcoded resolver: %v", m)
	}
	if !strings.Contains(r.log.String(), "deny dns atacante.example") {
		t.Fatalf("audit: %q", r.log.String())
	}
	p, _ := domain.Egress{AllowHosts: []string{"other.example"}}.Compile()
	r.gw.SetPolicy(p)
	if _, err := r.guest.dial("5.6.7.8:443"); err == nil {
		t.Fatal("revoking the name must forget its IPs")
	}
}

// TestPortsOnly: com porta publicada e sem rede, o host alcança o serviço do guest, mas o
// guest não sai nem resolve nada.
func TestPortsOnly(t *testing.T) {
	port := freePort(t)
	r := newRig(t, domain.Egress{}, false, []domain.PortMap{{Host: port, Guest: 8080}})
	l, err := gonet.ListenTCP(r.guest.s, tcpip.FullAddress{NIC: 1, Port: 8080}, ipv4.ProtocolNumber)
	if err != nil {
		t.Fatal(err)
	}
	defer l.Close()
	go func() {
		c, err := l.Accept()
		if err == nil {
			c.Write([]byte("hello from guest"))
			c.Close()
		}
	}()
	c, err := net.Dial("tcp", "127.0.0.1:"+strconv.Itoa(int(port)))
	if err != nil {
		t.Fatal(err)
	}
	c.SetReadDeadline(time.Now().Add(5 * time.Second))
	if b, _ := io.ReadAll(c); string(b) != "hello from guest" {
		t.Fatalf("published port: %q", b)
	}
	if _, err := r.guest.dial("1.1.1.1:443"); err == nil {
		t.Fatal("ports without net must not dial out")
	}
	if m := r.guest.lookup(t, "100.96.0.1", "example.com"); m.Rcode != dns.RcodeNameError {
		t.Fatal("ports without net must not resolve")
	}
}

// TestPortsViaGuestDial: com DialGuest a porta publicada entra pelo guest (a pilha não é
// usada), com a porta pedida; falha do DialGuest fecha a conexão do cliente.
func TestPortsViaGuestDial(t *testing.T) {
	port := freePort(t)
	p, _ := domain.Network{Ports: []domain.PortMap{{Host: port, Guest: 3000}}}.OutboundPolicy()
	host, _ := net.Pipe()
	var asked []uint16
	var mu sync.Mutex
	gw, err := Start(host, Config{Policy: p, Ports: []domain.PortMap{{Host: port, Guest: 3000}},
		DialGuest: func(_ context.Context, gp uint16) (net.Conn, error) {
			mu.Lock()
			asked = append(asked, gp)
			n := len(asked)
			mu.Unlock()
			if n > 1 {
				return nil, io.ErrClosedPipe
			}
			a, b := net.Pipe()
			go func() { b.Write([]byte("via guest")); b.Close() }()
			return a, nil
		}})
	if err != nil {
		t.Fatal(err)
	}
	defer gw.Close()
	c, err := net.Dial("tcp", "127.0.0.1:"+strconv.Itoa(int(port)))
	if err != nil {
		t.Fatal(err)
	}
	c.SetReadDeadline(time.Now().Add(5 * time.Second))
	if b, _ := io.ReadAll(c); string(b) != "via guest" {
		t.Fatalf("got %q", b)
	}
	c2, _ := net.Dial("tcp", "127.0.0.1:"+strconv.Itoa(int(port)))
	c2.SetReadDeadline(time.Now().Add(5 * time.Second))
	if b, _ := io.ReadAll(c2); len(b) != 0 {
		t.Fatal("a failed guest dial must just close the client")
	}
	mu.Lock()
	defer mu.Unlock()
	if asked[0] != 3000 {
		t.Fatalf("guest port: %v", asked)
	}
}

// TestUDP: UDP permitido vai e volta pelo host; negado é descartado; porta UDP publicada
// leva o datagrama do host ao guest e a resposta de volta.
func TestUDP(t *testing.T) {
	port := freePort(t)
	r := newRig(t, domain.Egress{AllowCIDRs: []string{"1.2.3.4"}}, true, []domain.PortMap{{Host: port, Guest: 5353, UDP: true}})
	c := r.guest.udp(t, "1.2.3.4:9999")
	c.Write([]byte("hi"))
	c.SetReadDeadline(time.Now().Add(5 * time.Second))
	buf := make([]byte, 64)
	n, err := c.Read(buf)
	if err != nil || string(buf[:n]) != "u:hi" {
		t.Fatalf("udp relay: %q %v", buf[:n], err)
	}
	d := r.guest.udp(t, "4.3.2.1:9999")
	d.Write([]byte("hi"))
	d.SetReadDeadline(time.Now().Add(300 * time.Millisecond))
	if _, err := d.Read(buf); err == nil {
		t.Fatal("denied udp must be dropped")
	}
	if !strings.Contains(r.log.String(), "deny udp 4.3.2.1:9999") {
		t.Fatalf("audit: %q", r.log.String())
	}
	srv, err := gonet.DialUDP(r.guest.s, &tcpip.FullAddress{NIC: 1, Port: 5353}, nil, ipv4.ProtocolNumber)
	if err != nil {
		t.Fatal(err)
	}
	defer srv.Close()
	go func() {
		b := make([]byte, 64)
		for {
			n, a, err := srv.ReadFrom(b)
			if err != nil {
				return
			}
			srv.WriteTo(append([]byte("g:"), b[:n]...), a)
		}
	}()
	hc, err := net.Dial("udp", "127.0.0.1:"+strconv.Itoa(int(port)))
	if err != nil {
		t.Fatal(err)
	}
	hc.Write([]byte("q"))
	hc.SetReadDeadline(time.Now().Add(5 * time.Second))
	n, err = hc.Read(buf)
	if err != nil || string(buf[:n]) != "g:q" {
		t.Fatalf("published udp: %q %v", buf[:n], err)
	}
}

// freePort reserva uma porta livre do host.
func freePort(t *testing.T) uint16 {
	l, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	defer l.Close()
	return uint16(l.Addr().(*net.TCPAddr).Port)
}

// TestStartErrors: sem política é erro; porta do host ocupada é erro com a porta; frame
// com tamanho absurdo derruba o enlace em vez de alocar.
func TestStartErrors(t *testing.T) {
	a, _ := net.Pipe()
	if _, err := Start(a, Config{}); err == nil {
		t.Fatal("nil policy must fail")
	}
	busy, _ := net.Listen("tcp", "127.0.0.1:0")
	defer busy.Close()
	port := uint16(busy.Addr().(*net.TCPAddr).Port)
	p, _ := domain.Egress{}.Compile()
	a, _ = net.Pipe()
	if _, err := Start(a, Config{Policy: p, Ports: []domain.PortMap{{Host: port, Guest: 1}}}); err == nil || !strings.Contains(err.Error(), strconv.Itoa(int(port))) {
		t.Fatalf("busy port: %v", err)
	}
	pc, _ := net.ListenPacket("udp", "127.0.0.1:0")
	defer pc.Close()
	a, _ = net.Pipe()
	if _, err := Start(a, Config{Policy: p, Ports: []domain.PortMap{{Host: uint16(pc.LocalAddr().(*net.UDPAddr).Port), Guest: 1, UDP: true}}}); err == nil {
		t.Fatal("busy udp port must fail")
	}
	for _, frame := range [][]byte{{0, 0, 0, 0}, {0xff, 0xff, 0xff, 0xff}, {0, 0, 0, 9, 1}} {
		host, guest := net.Pipe()
		g, err := Start(host, Config{Policy: p})
		if err != nil {
			t.Fatal(err)
		}
		go guest.Write(frame)
		time.Sleep(50 * time.Millisecond)
		guest.Close()
		done := make(chan struct{})
		go func() { g.Close(); close(done) }()
		select {
		case <-done:
		case <-time.After(5 * time.Second):
			t.Fatalf("frame %v must end the link", frame)
		}
	}
}

// TestAnswerEdges: lixo curto não tem resposta; lixo com cabeçalho recebe SERVFAIL com o
// mesmo id; upstream fora do ar vira SERVFAIL.
func TestAnswerEdges(t *testing.T) {
	p, _ := domain.Egress{}.Compile()
	g := &Gateway{learned: newLearnedSet(), ctx: context.Background(), cfg: Config{Upstream: "127.0.0.1:1"}}
	g.policy.Store(p)
	if g.answer([]byte{1, 2}) != nil {
		t.Fatal("short garbage gets no answer")
	}
	m := new(dns.Msg)
	if err := m.Unpack(g.answer([]byte{0xab, 0xcd, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0xff})); err != nil || m.Id != 0xabcd || m.Rcode != dns.RcodeServerFailure {
		t.Fatalf("garbage with header: %v %v", m, err)
	}
	q := new(dns.Msg)
	q.SetQuestion("x.example.", dns.TypeA)
	raw, _ := q.Pack()
	if err := m.Unpack(g.answer(raw)); err != nil || m.Rcode != dns.RcodeServerFailure {
		t.Fatalf("upstream down: %v", m)
	}
}

// TestLearnedTTL: o TTL é limitado a [60 s, 1 h] e o IP expira no prazo.
func TestLearnedTTL(t *testing.T) {
	l := newLearnedSet()
	now := time.Unix(0, 0)
	l.now = func() time.Time { return now }
	a, b := netip.MustParseAddr("1.1.1.1"), netip.MustParseAddr("2.2.2.2")
	l.add(a, "a", time.Second)
	l.add(b, "b", 48*time.Hour)
	now = now.Add(59 * time.Second)
	if !l.has(a) {
		t.Fatal("ttl below 60s is raised to 60s")
	}
	now = now.Add(2 * time.Second)
	if l.has(a) || !l.has(b) {
		t.Fatal("a expires after 60s, b lives on")
	}
	now = now.Add(time.Hour)
	if l.has(b) {
		t.Fatal("ttl above 1h is capped")
	}
}

// TestResolverFrom: o primeiro nameserver IPv4 do arquivo; sem nenhum (ou sem arquivo),
// 1.1.1.1.
func TestResolverFrom(t *testing.T) {
	dir := t.TempDir()
	p := filepath.Join(dir, "resolv.conf")
	os.WriteFile(p, []byte("# x\nsearch lan\nnameserver fe80::1\nnameserver 192.168.0.1\nnameserver 8.8.8.8\n"), 0o600)
	if got := resolverFrom(p); got != "192.168.0.1:53" {
		t.Fatalf("got %s", got)
	}
	os.WriteFile(p, []byte("nameserver nope\n"), 0o600)
	if resolverFrom(p) != "1.1.1.1:53" || resolverFrom(filepath.Join(dir, "none")) != "1.1.1.1:53" {
		t.Fatal("fallback must be 1.1.1.1")
	}
	if SystemResolver() == "" {
		t.Fatal("system resolver must never be empty")
	}
}
