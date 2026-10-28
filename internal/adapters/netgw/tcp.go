package netgw

import (
	"fmt"
	"net"
	"net/netip"
	"strconv"

	"gvisor.dev/gvisor/pkg/tcpip"
	"gvisor.dev/gvisor/pkg/tcpip/adapters/gonet"
	"gvisor.dev/gvisor/pkg/tcpip/network/ipv4"
	"gvisor.dev/gvisor/pkg/tcpip/transport/tcp"
	"gvisor.dev/gvisor/pkg/waiter"
)

// handleTCP decide cada SYN do guest. Negado: RST na hora (o programa no guest falha em
// milissegundos, não num timeout). Aprovado: disca o destino pelo host ANTES de aceitar,
// para o guest ver "connection refused" quando o destino recusa, como numa rede de verdade.
func (g *Gateway) handleTCP(r *tcp.ForwarderRequest) {
	id := r.ID()
	dst := addrOf(id.LocalAddress)
	if g.inFabric(dst) {
		// Rede nomeada: vai direto à máquina dona do IP, fora da política de saída (é tráfego
		// interno). Máquina parada ou de outra rede não tem socket: RST.
		peer, err := g.dialFabric(dst, id.LocalPort)
		if err != nil {
			r.Complete(true)
			return
		}
		var wq waiter.Queue
		ep, terr := r.CreateEndpoint(&wq)
		if terr != nil {
			peer.Close()
			r.Complete(true)
			return
		}
		r.Complete(false)
		go pipe(gonet.NewTCPConn(&wq, ep), peer)
		return
	}
	target := net.JoinHostPort(dst.String(), strconv.Itoa(int(id.LocalPort)))
	if g.cfg.EgressProxy != "" && dst == SinkholeIP {
		// Nome negado com página de bloqueio: vai ao proxy marcado como negado, que serve "Sem acesso" e nunca disca
		// destino nenhum — fora os requests que caem numa regra de caminho, que ele mesmo resolve e disca. O nome pedido
		// ele lê do SNI ou do Host (todo nome negado resolve para este mesmo IP).
		pol := g.policy.Load()
		proxied, err := g.dialEgress(netip.AddrPortFrom(dst, id.LocalPort), "", true, pol.AllowedHosts(), pol.AllowedPaths())
		if err != nil {
			g.deny("tcp", target, err.Error())
			r.Complete(true)
			return
		}
		g.accept(r, proxied, false)
		return
	}
	dial, ok, why := g.decide(dst)
	if !ok {
		g.deny("tcp", target, why)
		r.Complete(true)
		return
	}
	var host net.Conn
	var err error
	if g.cfg.EgressProxy != "" {
		// Com o proxy de saída, a conexão vai a ele (que disca o destino e registra); proxy fora do ar recusa a
		// conexão do guest — falha fechada, nada sai sem registro.
		host, err = g.dialEgress(netip.AddrPortFrom(dial, id.LocalPort), g.learned.name(dial), false, nil, nil)
		if err != nil {
			g.deny("tcp", target, err.Error())
		}
	} else {
		host, err = g.cfg.Dial(g.ctx, "tcp", net.JoinHostPort(dial.String(), strconv.Itoa(int(id.LocalPort))))
	}
	if err != nil {
		r.Complete(true)
		return
	}
	g.accept(r, host, true)
}

// accept aceita o SYN do guest e liga a conexão dele a host (o destino, ou o proxy de saída). intercept deixa a porta
// de HTTPS passar pelo interceptador de credenciais; a conexão negada ao sinkhole não passa (não há o que trocar).
func (g *Gateway) accept(r *tcp.ForwarderRequest, host net.Conn, intercept bool) {
	id := r.ID()
	var wq waiter.Queue
	ep, terr := r.CreateEndpoint(&wq)
	if terr != nil {
		host.Close()
		r.Complete(true)
		return
	}
	r.Complete(false)
	if ic := g.cfg.Intercept; intercept && ic != nil && id.LocalPort == ic.Port {
		// Porta de HTTPS com credencial na máquina: o interceptador decide pelo SNI.
		go ic.serve(gonet.NewTCPConn(&wq, ep), host)
		return
	}
	go pipe(gonet.NewTCPConn(&wq, ep), host)
}

// publish abre as portas publicadas no host. Cada conexão aceita vira uma conexão da pilha
// para o guest, com o gateway como origem; a política de saída não se aplica (é entrada).
func (g *Gateway) publish() error {
	for _, p := range g.cfg.Ports {
		addr := net.JoinHostPort(g.cfg.PublishAddr, strconv.Itoa(int(p.Host)))
		if p.UDP {
			if err := g.publishUDP(addr, p.Guest); err != nil {
				return err
			}
			continue
		}
		l, err := net.Listen("tcp", addr)
		if err != nil {
			return fmt.Errorf("publish %s: %w", p, err)
		}
		g.listeners = append(g.listeners, l)
		guest := tcpip.FullAddress{NIC: nicID, Addr: tcpip.AddrFrom4(GuestIP.As4()), Port: p.Guest}
		local := tcpip.FullAddress{NIC: nicID, Addr: tcpip.AddrFrom4(GatewayIP.As4())}
		g.wg.Add(1)
		go func() {
			defer g.wg.Done()
			for {
				c, err := l.Accept()
				if err != nil {
					return
				}
				go func() {
					var in net.Conn
					var err error
					if g.cfg.DialGuest != nil {
						in, err = g.cfg.DialGuest(g.ctx, guest.Port)
					} else {
						in, err = gonet.DialTCPWithBind(g.ctx, g.stack, local, guest, ipv4.ProtocolNumber)
					}
					if err != nil {
						c.Close()
						return
					}
					pipe(c, in)
				}()
			}
		}()
	}
	return nil
}
