package netgw

import (
	"bytes"
	"context"
	"encoding/binary"
	"io"
	"net"
	"net/netip"
	"strings"
	"testing"
	"time"

	"github.com/miekg/dns"

	"runner.codes/internal/domain"
)

// proxyAddr é o endereço que o gateway do teste usa como proxy de saída; o Dial do teste o troca pelo proxy falso.
const proxyAddr = "egress.test:15080"

// seenHeader é o que o proxy falso leu do header PROXY v2 de uma conexão.
type seenHeader struct {
	dst                    netip.AddrPort
	machine, name, allowed, paths string
	denied                        bool
}

// fakeProxy sobe um proxy de saída falso: lê o header, publica o que leu e devolve "proxy:" + o resto da conexão.
func fakeProxy(t *testing.T) (string, chan seenHeader) {
	l, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { l.Close() })
	seen := make(chan seenHeader, 10)
	go func() {
		for {
			c, err := l.Accept()
			if err != nil {
				return
			}
			go func() {
				defer c.Close()
				h, err := readProxyHeader(c)
				if err != nil {
					return
				}
				seen <- h
				b, _ := io.ReadAll(c)
				c.Write(append([]byte("proxy:"), b...))
			}()
		}
	}()
	return l.Addr().String(), seen
}

// readProxyHeader lê um header PROXY v2 do jeito que o edgeproxy lê (assinatura, IPv4/TCP, endereços, TLVs).
func readProxyHeader(r io.Reader) (seenHeader, error) {
	var fixed [16]byte
	if _, err := io.ReadFull(r, fixed[:]); err != nil {
		return seenHeader{}, err
	}
	if !bytes.Equal(fixed[:12], proxySignature) || fixed[12] != 0x21 || fixed[13] != 0x11 {
		return seenHeader{}, io.ErrUnexpectedEOF
	}
	body := make([]byte, binary.BigEndian.Uint16(fixed[14:]))
	if _, err := io.ReadFull(r, body); err != nil {
		return seenHeader{}, err
	}
	dst := netip.AddrPortFrom(netip.AddrFrom4([4]byte(body[4:8])), binary.BigEndian.Uint16(body[10:12]))
	h := seenHeader{dst: dst}
	for tl := body[12:]; len(tl) >= 3; {
		n := int(binary.BigEndian.Uint16(tl[1:3]))
		v := string(tl[3 : 3+n])
		switch tl[0] {
		case tlvMachine:
			h.machine = v
		case tlvName:
			h.name = v
		case tlvDenied:
			h.denied = true
		case tlvAllowed:
			h.allowed = v
		case tlvPaths:
			h.paths = v
		}
		tl = tl[3+n:]
	}
	return h, nil
}

// withProxy liga o proxy de saída no gateway do teste: o Dial ao proxyAddr vai ao proxy falso (target), o resto segue
// o Dial do rig (que anota e manda aos servidores de eco).
func withProxy(target string) func(*Config) {
	return func(c *Config) {
		next := c.Dial
		c.EgressProxy = proxyAddr
		c.MachineName = "sbot-teste"
		c.Dial = func(ctx context.Context, network, addr string) (net.Conn, error) {
			if addr == proxyAddr {
				var d net.Dialer
				return d.DialContext(ctx, network, target)
			}
			return next(ctx, network, addr)
		}
	}
}

// awaitHeader espera o header que o proxy falso leu.
func awaitHeader(t *testing.T, seen chan seenHeader) seenHeader {
	t.Helper()
	select {
	case h := <-seen:
		return h
	case <-time.After(5 * time.Second):
		t.Fatal("the proxy never got a connection")
		return seenHeader{}
	}
}

// TestEgressProxyCarriesTheConnection: com o proxy de saída, a conexão permitida vai a ele com destino, máquina e o
// nome que o guest resolveu — e o destino nunca é discado direto pelo gateway.
func TestEgressProxyCarriesTheConnection(t *testing.T) {
	addr, seen := fakeProxy(t)
	r := newRig(t, domain.Egress{AllowHosts: []string{"allowed.example"}}, true, nil, withProxy(addr))
	r.guest.lookup(t, "100.96.0.1", "api.allowed.example")
	c, err := r.guest.dial("5.6.7.8:443")
	if err != nil {
		t.Fatal(err)
	}
	if got := roundTrip(t, c, "ping"); got != "proxy:ping" {
		t.Fatalf("got %q", got)
	}
	h := awaitHeader(t, seen)
	want := seenHeader{dst: netip.MustParseAddrPort("5.6.7.8:443"), machine: "sbot-teste", name: "api.allowed.example"}
	if h != want {
		t.Fatalf("header %+v, want %+v", h, want)
	}
	r.mu.Lock()
	defer r.mu.Unlock()
	if len(r.dialed) != 0 {
		t.Fatalf("the gateway dialed the destination itself: %v", r.dialed)
	}
}

// TestEgressBlockPageSinkhole: nome negado com BlockPage e proxy resolve para o sinkhole (A com TTL curto; AAAA vazio,
// sem NXDOMAIN), a recusa segue no log, e a conexão ao sinkhole vai ao proxy marcada como negada, com a lista liberada.
func TestEgressBlockPageSinkhole(t *testing.T) {
	addr, seen := fakeProxy(t)
	r := newRig(t, domain.Egress{AllowHosts: []string{"allowed.example"}, AllowHostPatterns: []string{"*.cdn.net"}, BlockPage: true,
		AllowPaths: []string{"cdnjs.cloudflare.com/polyfill/", "x.io/a.js"}}, true, nil, withProxy(addr))
	// O host de uma regra de caminho também vai ao sinkhole: é o proxy que decide request a request.
	if m := r.guest.lookup(t, "100.96.0.1", "cdnjs.cloudflare.com"); len(m.Answer) != 1 || m.Answer[0].(*dns.A).A.String() != SinkholeIP.String() {
		t.Fatalf("path-rule host must hit the sinkhole: %v", m)
	}
	m := r.guest.lookup(t, "100.96.0.1", "www.reddit.com")
	if m.Rcode != dns.RcodeSuccess || len(m.Answer) != 1 {
		t.Fatalf("sinkhole answer: %v", m)
	}
	a := m.Answer[0].(*dns.A)
	if a.A.String() != SinkholeIP.String() || a.Hdr.Ttl != sinkholeTTL {
		t.Fatalf("sinkhole record: %v", a)
	}
	if m := r.guest.lookupType(t, "100.96.0.1", "www.reddit.com", dns.TypeAAAA); m.Rcode != dns.RcodeSuccess || len(m.Answer) != 0 {
		t.Fatalf("AAAA must be empty, not NXDOMAIN: %v", m)
	}
	if !strings.Contains(r.log.String(), "deny dns www.reddit.com") {
		t.Fatalf("audit: %q", r.log.String())
	}
	c, err := r.guest.dial(SinkholeIP.String() + ":443")
	if err != nil {
		t.Fatal(err)
	}
	if got := roundTrip(t, c, "hello"); got != "proxy:hello" {
		t.Fatalf("got %q", got)
	}
	h := awaitHeader(t, seen)
	if !h.denied || h.allowed != "allowed.example,*.cdn.net" || h.paths != "cdnjs.cloudflare.com/polyfill/,x.io/a.js" ||
		h.name != "" || h.dst.Port() != 443 {
		t.Fatalf("denied header: %+v", h)
	}
}

// TestEgressBlockPageNeedsBoth: sem BlockPage (com proxy) ou sem proxy (com BlockPage), o nome negado segue em
// NXDOMAIN; e sem proxy o sinkhole não é atalho para lugar nenhum (é CGNAT, que a política fecha).
func TestEgressBlockPageNeedsBoth(t *testing.T) {
	addr, _ := fakeProxy(t)
	r := newRig(t, domain.Egress{AllowHosts: []string{"allowed.example"}}, true, nil, withProxy(addr))
	if m := r.guest.lookup(t, "100.96.0.1", "reddit.com"); m.Rcode != dns.RcodeNameError {
		t.Fatalf("no block page: %v", m)
	}
	r = newRig(t, domain.Egress{AllowHosts: []string{"allowed.example"}, BlockPage: true}, true, nil)
	if m := r.guest.lookup(t, "100.96.0.1", "reddit.com"); m.Rcode != dns.RcodeNameError {
		t.Fatalf("no proxy: %v", m)
	}
	if _, err := r.guest.dial(SinkholeIP.String() + ":443"); err == nil {
		t.Fatal("without the proxy the sinkhole must be refused")
	}
}

// TestEgressProxyDownFailsClosed: proxy fora do ar recusa a conexão do guest na hora (nada sai sem registro) e diz
// por quê no log — tanto a conexão permitida quanto a do sinkhole.
func TestEgressProxyDownFailsClosed(t *testing.T) {
	l, _ := net.Listen("tcp", "127.0.0.1:0")
	gone := l.Addr().String()
	l.Close()
	r := newRig(t, domain.Egress{BlockPage: true, AllowHosts: []string{"allowed.example"}}, true, nil, withProxy(gone))
	r.guest.lookup(t, "100.96.0.1", "allowed.example")
	start := time.Now()
	if _, err := r.guest.dial("5.6.7.8:443"); err == nil {
		t.Fatal("must be refused with the proxy down")
	}
	if d := time.Since(start); d > 4*time.Second {
		t.Fatalf("refusal took %v", d)
	}
	if _, err := r.guest.dial(SinkholeIP.String() + ":80"); err == nil {
		t.Fatal("sinkhole must be refused with the proxy down")
	}
	if n := strings.Count(r.log.String(), "egress proxy "+proxyAddr); n != 2 {
		t.Fatalf("audit (%d): %q", n, r.log.String())
	}
}

// TestEgressRefusesQUIC: com o proxy de saída, UDP 443 e 80 (HTTP/3) não saem — passariam sem registro —, e o resto
// do UDP segue a política; sem proxy, o 443 passa como sempre.
func TestEgressRefusesQUIC(t *testing.T) {
	addr, _ := fakeProxy(t)
	r := newRig(t, domain.Egress{}, true, nil, withProxy(addr))
	if !r.gw.uninspectedUDP(443) || !r.gw.uninspectedUDP(80) || r.gw.uninspectedUDP(123) {
		t.Fatal("QUIC ports")
	}
	u := r.guest.udp(t, "1.2.3.4:443")
	u.Write([]byte("quic"))
	u.SetReadDeadline(time.Now().Add(500 * time.Millisecond))
	if _, err := u.Read(make([]byte, 16)); err == nil {
		t.Fatal("QUIC must not get an answer")
	}
	if !strings.Contains(r.log.String(), "deny udp 1.2.3.4:443: QUIC is not inspected") {
		t.Fatalf("audit: %q", r.log.String())
	}
	u = r.guest.udp(t, "1.2.3.4:123")
	u.Write([]byte("ntp"))
	u.SetReadDeadline(time.Now().Add(5 * time.Second))
	b := make([]byte, 16)
	if n, err := u.Read(b); err != nil || string(b[:n]) != "u:ntp" {
		t.Fatalf("other UDP must pass: %q %v", b[:n], err)
	}
	plain := newRig(t, domain.Egress{}, true, nil)
	if plain.gw.uninspectedUDP(443) {
		t.Fatal("without the proxy QUIC is not touched")
	}
}

// TestSinkholeIsNotLocal: o sinkhole não pode cair em faixa que o navegador trata como rede local (privada, CGNAT,
// loopback, link-local) — senão o Firefox pede ao visitante permissão de "acesso à rede local" a cada site negado.
func TestSinkholeIsNotLocal(t *testing.T) {
	cgnat := netip.MustParsePrefix("100.64.0.0/10")
	if SinkholeIP.IsPrivate() || SinkholeIP.IsLoopback() || SinkholeIP.IsLinkLocalUnicast() || cgnat.Contains(SinkholeIP) {
		t.Fatalf("sinkhole %s is in a local range", SinkholeIP)
	}
	if !netip.MustParsePrefix("192.0.2.0/24").Contains(SinkholeIP) {
		t.Fatalf("sinkhole %s must stay in TEST-NET-1 (never routed)", SinkholeIP)
	}
}

// TestProxyHeaderBytes: o header byte a byte — é o contrato com o edgeproxy (Rust), então o formato não pode mudar
// por acidente: assinatura, versão/comando, família, tamanho, origem (o IP do guest, porta 0), destino e TLVs.
func TestProxyHeaderBytes(t *testing.T) {
	got := proxyHeader(netip.MustParseAddrPort("1.2.3.4:443"), "m", "x.io", true, []string{"a", "b"}, []string{"h/p"})
	want := append(append([]byte{}, proxySignature...), 0x21, 0x11, 0, 12+4+7+4+6+6,
		100, 96, 0, 2, 1, 2, 3, 4, 0, 0, 0x01, 0xBB,
		tlvMachine, 0, 1, 'm',
		tlvName, 0, 4, 'x', '.', 'i', 'o',
		tlvDenied, 0, 1, '1',
		tlvAllowed, 0, 3, 'a', ',', 'b',
		tlvPaths, 0, 3, 'h', '/', 'p')
	if !bytes.Equal(got, want) {
		t.Fatalf("header\n got % x\nwant % x", got, want)
	}
	// Permitida: sem nome, sem marca de negada e sem listas (mesmo se vierem).
	got = proxyHeader(netip.MustParseAddrPort("1.2.3.4:80"), "m", "", false, []string{"a"}, []string{"h/p"})
	if len(got) != 16+12+4 {
		t.Fatalf("allowed header length %d", len(got))
	}
}

// TestLearnedName: o nome de um IP aprendido sai enquanto vale; vencido ou desconhecido é vazio.
func TestLearnedName(t *testing.T) {
	l := newLearnedSet()
	now := time.Unix(1000, 0)
	l.now = func() time.Time { return now }
	a := netip.MustParseAddr("5.6.7.8")
	l.add(a, "github.com", time.Minute)
	if l.name(a) != "github.com" || l.name(netip.MustParseAddr("9.9.9.9")) != "" {
		t.Fatal("learned name")
	}
	now = now.Add(2 * time.Minute)
	if l.name(a) != "" {
		t.Fatal("an expired IP has no name")
	}
}
