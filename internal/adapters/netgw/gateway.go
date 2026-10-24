// Package netgw é o gateway de rede de uma máquina: uma pilha TCP/IP em userspace (o
// netstack do gVisor) do lado do host de um socketpair que a libkrun liga à virtio-net do
// guest (krun_add_net_unixstream). Todo pacote do guest para fora passa por aqui: o TCP e
// o UDP viram conexões do host (NAT), o DNS é respondido aqui com o filtro de nomes, e
// cada conexão é decidida pela política (domain.Policy) antes de existir um socket no host.
// A pilha TCP/IP é a do gVisor, em espaço de usuário, sem tocar na rede do host.
package netgw

import (
	"context"
	"encoding/binary"
	"errors"
	"fmt"
	"io"
	"net"
	"net/netip"
	"sync"
	"sync/atomic"
	"time"

	"gvisor.dev/gvisor/pkg/buffer"
	"gvisor.dev/gvisor/pkg/tcpip"
	"gvisor.dev/gvisor/pkg/tcpip/header"
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

// Endereços do enlace guest↔gateway: um /30 em 100.96.0.0/10 (CGNAT),
// escolhido porque quase nenhuma rede doméstica ou de nuvem usa essa faixa, então a rota
// do guest não colide com a rede que ele quer alcançar.
var (
	GatewayIP  = netip.MustParseAddr("100.96.0.1")
	GuestIP    = netip.MustParseAddr("100.96.0.2")
	PrefixLen  = 30
	GatewayMAC = net.HardwareAddr{0x02, 0x52, 0x43, 0x00, 0x00, 0x01}
	GuestMAC   = net.HardwareAddr{0x02, 0x52, 0x43, 0x00, 0x00, 0x02}
)

// mtu é o da virtio-net sem offload: 1500, o padrão Ethernet.
const mtu = 1500

// nicID é a única placa da pilha.
const nicID tcpip.NICID = 1

// maxFrame limita o frame lido do socket: Ethernet com MTU 1500 cabe com folga em 64 KiB;
// um prefixo maior é corrupção, e alocar o que ele diz seria dar ao guest um botão de OOM.
const maxFrame = 64 << 10

// Dialer abre a conexão do host para um destino já aprovado; o teste troca por um que
// aponta para servidores locais.
type Dialer func(ctx context.Context, network, addr string) (net.Conn, error)

// Config é o que o gateway precisa para uma máquina.
type Config struct {
	Policy *domain.Policy
	Ports  []domain.PortMap
	// Upstream é o resolvedor DNS do host ("ip:53"); vazio usa o do sistema ou 1.1.1.1.
	Upstream string
	// PublishAddr é onde as portas escutam no host; vazio é 127.0.0.1.
	PublishAddr string
	// Dial abre conexões do host; nil usa net.Dialer.
	Dial Dialer
	// Ping manda o echo ICMP pelo host (o ping do guest); nil usa um socket ICMP de datagrama.
	Ping Pinger
	// Denials recebe uma linha por conexão ou nome negado (o log de auditoria).
	Denials io.Writer
	// DialGuest abre a conexão de uma porta publicada TCP até o 127.0.0.1 do guest (pelo
	// vsock): alcança servidor que escuta só em localhost. nil disca o IP do guest pela pilha.
	DialGuest func(ctx context.Context, port uint16) (net.Conn, error)
	// Fabric liga a máquina a uma rede nomeada (nil: só a internet, pela política).
	Fabric *Fabric
	// MachineName é o nome desta máquina, publicado no DNS da rede nomeada.
	MachineName string
	// Intercept troca credenciais nas requisições HTTPS (nil: nenhuma binding).
	Intercept *Interceptor
}

// Gateway é a pilha ligada a uma máquina.
type Gateway struct {
	// pings é o semáforo dos echos ICMP no ar (maxPingsInFlight).
	pings   chan struct{}
	stack   *stack.Stack
	link    *channel.Endpoint
	conn    io.ReadWriteCloser
	policy  atomic.Pointer[domain.Policy]
	learned *learnedSet
	cfg     Config
	ctx     context.Context
	cancel  context.CancelFunc
	wg      sync.WaitGroup
	// listeners são as portas publicadas, fechadas no Close.
	listeners []io.Closer
	logMu     sync.Mutex
}

// Start liga a pilha ao lado do host do socketpair e começa a atender. conn carrega frames
// Ethernet com prefixo de 4 bytes big-endian (o formato do unixstream da libkrun).
func Start(conn io.ReadWriteCloser, cfg Config) (*Gateway, error) {
	if cfg.Policy == nil {
		return nil, errors.New("netgw: a policy is required")
	}
	if cfg.Dial == nil {
		d := &net.Dialer{Timeout: 10 * time.Second}
		cfg.Dial = d.DialContext
	}
	if cfg.Upstream == "" {
		cfg.Upstream = SystemResolver()
	}
	if cfg.Ping == nil {
		cfg.Ping = hostPing
	}
	if cfg.PublishAddr == "" {
		cfg.PublishAddr = "127.0.0.1"
	}
	s := stack.New(stack.Options{
		NetworkProtocols: []stack.NetworkProtocolFactory{ipv4.NewProtocol, arp.NewProtocol},
		// icmp.NewProtocol4 é o que faz a pilha entregar o echo a um IP de fora ao handleICMP
		// (sem ele o pacote é de protocolo desconhecido e some).
		TransportProtocols: []stack.TransportProtocolFactory{tcp.NewProtocol, udp.NewProtocol, gicmp.NewProtocol4},
	})
	link := channel.New(1024, mtu, tcpip.LinkAddress(GatewayMAC))
	if err := s.CreateNIC(nicID, ethernet.New(link)); err != nil {
		return nil, fmt.Errorf("netgw: create nic: %s", err)
	}
	addr := tcpip.ProtocolAddress{Protocol: ipv4.ProtocolNumber,
		AddressWithPrefix: tcpip.AddrFrom4(GatewayIP.As4()).WithPrefix()}
	addr.AddressWithPrefix.PrefixLen = PrefixLen
	if err := s.AddProtocolAddress(nicID, addr, stack.AddressProperties{}); err != nil {
		return nil, fmt.Errorf("netgw: add address: %s", err)
	}
	// Promíscuo + spoofing: a pilha aceita pacote para qualquer IP (1.1.1.1, ...) e responde
	// em nome dele. É o que faz o NAT funcionar sem uma tabela de IPs por destino.
	s.SetPromiscuousMode(nicID, true)
	s.SetSpoofing(nicID, true)
	s.SetRouteTable([]tcpip.Route{{Destination: header.IPv4EmptySubnet, NIC: nicID}})

	ctx, cancel := context.WithCancel(context.Background())
	g := &Gateway{stack: s, link: link, conn: conn, learned: newLearnedSet(), cfg: cfg, ctx: ctx, cancel: cancel,
		pings: make(chan struct{}, maxPingsInFlight)}
	g.policy.Store(cfg.Policy)
	s.SetTransportProtocolHandler(tcp.ProtocolNumber, tcp.NewForwarder(s, 0, 2048, g.handleTCP).HandlePacket)
	s.SetTransportProtocolHandler(udp.ProtocolNumber, udp.NewForwarder(s, g.handleUDP).HandlePacket)
	s.SetTransportProtocolHandler(header.ICMPv4ProtocolNumber, g.handleICMP)
	if err := g.publish(); err != nil {
		g.Close()
		return nil, err
	}
	if cfg.Fabric != nil {
		if err := g.serveFabric(cfg.MachineName); err != nil {
			g.Close()
			return nil, err
		}
	}
	g.wg.Add(2)
	go g.readFrames()
	go g.writeFrames()
	return g, nil
}

// SetPolicy troca a política com a máquina ligada: vale para a próxima conexão ou consulta,
// e os IPs aprendidos de nomes que deixaram de valer são esquecidos.
func (g *Gateway) SetPolicy(p *domain.Policy) {
	g.policy.Store(p)
	g.learned.prune(p.NameAllowed)
}

// Close derruba portas, enlace e pilha. Idempotente.
func (g *Gateway) Close() error {
	g.cancel()
	for _, l := range g.listeners {
		l.Close()
	}
	g.conn.Close()
	g.link.Close()
	g.wg.Wait()
	g.stack.Close()
	return nil
}

// readFrames leva os frames do guest para a pilha.
func (g *Gateway) readFrames() {
	defer g.wg.Done()
	var hdr [4]byte
	for {
		if _, err := io.ReadFull(g.conn, hdr[:]); err != nil {
			g.cancel()
			return
		}
		n := binary.BigEndian.Uint32(hdr[:])
		if n == 0 || n > maxFrame {
			g.cancel()
			return
		}
		frame := make([]byte, n)
		if _, err := io.ReadFull(g.conn, frame); err != nil {
			g.cancel()
			return
		}
		pkt := stack.NewPacketBuffer(stack.PacketBufferOptions{Payload: buffer.MakeWithData(frame)})
		g.link.InjectInbound(0, pkt)
		pkt.DecRef()
	}
}

// writeFrames leva os frames da pilha para o guest, com o mesmo prefixo de tamanho.
func (g *Gateway) writeFrames() {
	defer g.wg.Done()
	for {
		pkt := g.link.ReadContext(g.ctx)
		if pkt == nil {
			return
		}
		v := pkt.ToView()
		frame := v.AsSlice()
		out := make([]byte, 4+len(frame))
		binary.BigEndian.PutUint32(out, uint32(len(frame)))
		copy(out[4:], frame)
		v.Release()
		pkt.DecRef()
		if _, err := g.conn.Write(out); err != nil {
			g.cancel()
			return
		}
	}
}

// deny registra a recusa no log de auditoria.
func (g *Gateway) deny(kind, target, why string) {
	if g.cfg.Denials == nil {
		return
	}
	g.logMu.Lock()
	defer g.logMu.Unlock()
	fmt.Fprintf(g.cfg.Denials, "%s deny %s %s: %s\n", time.Now().UTC().Format(time.RFC3339), kind, target, why)
}

// decide aplica a política a um destino. O IP do gateway é o host: vira o loopback, que
// só abre com allow_cidrs explícito (é como o guest alcança um serviço local do dono).
func (g *Gateway) decide(dst netip.Addr) (dial netip.Addr, ok bool, why string) {
	dial = dst
	if dst == GatewayIP {
		dial = netip.MustParseAddr("127.0.0.1")
	}
	ok, why = g.policy.Load().IPAllowed(dial, g.learned.has(dial))
	return dial, ok, why
}

// addrOf converte o endereço do gVisor.
func addrOf(a tcpip.Address) netip.Addr {
	b := a.As4()
	return netip.AddrFrom4(b)
}

// pipe copia nos dois sentidos e fecha os dois lados quando um termina. O half-close
// (CloseWrite) deixa protocolos que esperam EOF (HTTP/1.0, shell) terminarem a resposta.
func pipe(a, b net.Conn) {
	var wg sync.WaitGroup
	cp := func(dst, src net.Conn) {
		defer wg.Done()
		io.Copy(dst, src)
		if cw, ok := dst.(interface{ CloseWrite() error }); ok {
			cw.CloseWrite()
		} else {
			dst.Close()
		}
	}
	wg.Add(2)
	go cp(a, b)
	go cp(b, a)
	wg.Wait()
	a.Close()
	b.Close()
}
