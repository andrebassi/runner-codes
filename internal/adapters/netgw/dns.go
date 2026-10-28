package netgw

import (
	"bufio"
	"net"
	"net/netip"
	"os"
	"strings"
	"sync"
	"time"

	"github.com/miekg/dns"
)

// Limites do TTL de um IP aprendido: abaixo de 60 s um cliente que guarda a resposta
// (navegador, JVM) reconectaria a um IP já esquecido; acima de 1 h, um nome revogado
// continuaria valendo por tempo demais.
const (
	minLearnTTL = 60 * time.Second
	maxLearnTTL = time.Hour
)

// dnsTimeout é a espera pelo resolvedor do host: 2 s; o cliente do guest
// repete a pergunta sozinho, então esperar mais só prende o fluxo.
const dnsTimeout = 2 * time.Second

// learnedSet são os IPs que o DNS deu para nomes permitidos, com validade e o nome de
// origem (para esquecer quando a política revogar o nome).
type learnedSet struct {
	mu  sync.Mutex
	ips map[netip.Addr]learned
	now func() time.Time
}

// learned é um IP aprendido.
type learned struct {
	name    string
	expires time.Time
}

// newLearnedSet cria o conjunto vazio.
func newLearnedSet() *learnedSet {
	return &learnedSet{ips: map[netip.Addr]learned{}, now: time.Now}
}

// add grava o IP com o TTL limitado.
func (l *learnedSet) add(a netip.Addr, name string, ttl time.Duration) {
	ttl = min(max(ttl, minLearnTTL), maxLearnTTL)
	l.mu.Lock()
	defer l.mu.Unlock()
	l.ips[a.Unmap()] = learned{name: name, expires: l.now().Add(ttl)}
}

// name devolve o nome de onde o IP foi aprendido (vazio se não foi, ou venceu): é o que vai ao proxy de saída, para o
// registro mostrar "github.com" em vez do IP numa conexão que não é HTTP nem TLS.
func (l *learnedSet) name(a netip.Addr) string {
	l.mu.Lock()
	defer l.mu.Unlock()
	e, ok := l.ips[a.Unmap()]
	if !ok || !l.now().Before(e.expires) {
		return ""
	}
	return e.name
}

// has diz se o IP está aprendido e na validade.
func (l *learnedSet) has(a netip.Addr) bool {
	l.mu.Lock()
	defer l.mu.Unlock()
	e, ok := l.ips[a.Unmap()]
	return ok && l.now().Before(e.expires)
}

// prune esquece os IPs de nomes que a política nova não permite mais.
func (l *learnedSet) prune(allowed func(string) bool) {
	l.mu.Lock()
	defer l.mu.Unlock()
	for a, e := range l.ips {
		if !allowed(e.name) {
			delete(l.ips, a)
		}
	}
}

// serveDNS responde as perguntas que chegam num fluxo UDP/53 do guest até ele ficar
// ocioso (udpIdle).
func (g *Gateway) serveDNS(c net.Conn) {
	defer c.Close()
	buf := make([]byte, 64<<10)
	for {
		c.SetReadDeadline(time.Now().Add(udpIdle))
		n, err := c.Read(buf)
		if err != nil {
			return
		}
		if out := g.answer(buf[:n]); out != nil {
			c.Write(out)
		}
	}
}

// answer decide uma consulta: nome fora da política recebe NXDOMAIN (o programa vê "host
// não encontrado", não um timeout); o resto vai ao resolvedor do host, e os A/AAAA da
// resposta viram IPs aprendidos. Mensagem que não parseia recebe SERVFAIL se der para
// responder, ou nada.
func (g *Gateway) answer(raw []byte) []byte {
	q := new(dns.Msg)
	if err := q.Unpack(raw); err != nil || len(q.Question) != 1 {
		if len(raw) < 12 {
			return nil
		}
		fail := new(dns.Msg)
		fail.Id = uint16(raw[0])<<8 | uint16(raw[1])
		fail.Response, fail.Rcode = true, dns.RcodeServerFailure
		out, _ := fail.Pack()
		return out
	}
	name := strings.TrimSuffix(q.Question[0].Name, ".")
	if a, found, ours := g.resolveFabric(name); ours {
		if !found {
			return reply(q, dns.RcodeNameError)
		}
		m := new(dns.Msg)
		m.SetReply(q)
		if q.Question[0].Qtype != dns.TypeA {
			// O nome existe, só não tem esse tipo (AAAA, por exemplo): NOERROR vazio. Com
			// NXDOMAIN o resolvedor do musl/busybox descarta também o A e o wget falha (medido).
			out, _ := m.Pack()
			return out
		}
		// TTL curto: a máquina pode ser recriada com outro IP.
		m.Answer = []dns.RR{&dns.A{Hdr: dns.RR_Header{Name: q.Question[0].Name, Rrtype: dns.TypeA, Class: dns.ClassINET, Ttl: 5}, A: a.AsSlice()}}
		out, _ := m.Pack()
		return out
	}
	if pol := g.policy.Load(); !pol.NameAllowed(name) {
		g.deny("dns", name, "name not in the allow list")
		if g.cfg.EgressProxy != "" && pol.BlockPage() {
			return sinkhole(q)
		}
		return reply(q, dns.RcodeNameError)
	}
	cl := &dns.Client{Net: "udp", Timeout: dnsTimeout}
	resp, _, err := cl.ExchangeContext(g.ctx, q, g.cfg.Upstream)
	if err != nil {
		return reply(q, dns.RcodeServerFailure)
	}
	for _, rr := range resp.Answer {
		ttl := time.Duration(rr.Header().Ttl) * time.Second
		switch v := rr.(type) {
		case *dns.A:
			if a, ok := netip.AddrFromSlice(v.A); ok {
				g.learned.add(a, name, ttl)
			}
		case *dns.AAAA:
			if a, ok := netip.AddrFromSlice(v.AAAA); ok {
				g.learned.add(a, name, ttl)
			}
		}
	}
	out, err := resp.Pack()
	if err != nil {
		return reply(q, dns.RcodeServerFailure)
	}
	return out
}

// sinkhole responde um nome negado com o SinkholeIP (só o A; os outros tipos voltam vazios, sem NXDOMAIN, que faria o
// resolvedor do musl descartar também o A — mesmo motivo da rede nomeada).
func sinkhole(q *dns.Msg) []byte {
	m := new(dns.Msg)
	m.SetReply(q)
	if q.Question[0].Qtype == dns.TypeA {
		m.Answer = []dns.RR{&dns.A{Hdr: dns.RR_Header{Name: q.Question[0].Name, Rrtype: dns.TypeA, Class: dns.ClassINET, Ttl: sinkholeTTL},
			A: SinkholeIP.AsSlice()}}
	}
	out, _ := m.Pack()
	return out
}

// reply monta uma resposta vazia com o código dado.
func reply(q *dns.Msg, rcode int) []byte {
	m := new(dns.Msg)
	m.SetRcode(q, rcode)
	out, _ := m.Pack()
	return out
}

// SystemResolver devolve o primeiro nameserver IPv4 do /etc/resolv.conf do host, ou
// 1.1.1.1 se não houver. Loopback serve: quem consulta é o host, não o guest.
func SystemResolver() string { return resolverFrom("/etc/resolv.conf") }

// resolverFrom lê o arquivo de resolv.conf dado.
func resolverFrom(path string) string {
	f, err := os.Open(path)
	if err == nil {
		defer f.Close()
		sc := bufio.NewScanner(f)
		for sc.Scan() {
			fields := strings.Fields(sc.Text())
			if len(fields) >= 2 && fields[0] == "nameserver" {
				if a, err := netip.ParseAddr(fields[1]); err == nil && a.Is4() {
					return net.JoinHostPort(a.String(), "53")
				}
			}
		}
	}
	return "1.1.1.1:53"
}
