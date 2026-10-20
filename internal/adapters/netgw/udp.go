package netgw

import (
	"fmt"
	"net"
	"strconv"
	"sync"
	"time"

	"gvisor.dev/gvisor/pkg/tcpip"
	"gvisor.dev/gvisor/pkg/tcpip/adapters/gonet"
	"gvisor.dev/gvisor/pkg/tcpip/network/ipv4"
	"gvisor.dev/gvisor/pkg/tcpip/transport/udp"
	"gvisor.dev/gvisor/pkg/waiter"
)

// udpIdle fecha o fluxo UDP sem tráfego após 60 s. UDP não tem fim de conexão, e
// sem prazo cada destino consultado deixaria um socket aberto no host para sempre.
const udpIdle = 60 * time.Second

// handleUDP atende o primeiro datagrama de cada fluxo do guest. Porta 53 é DNS, para
// qualquer IP (um resolvedor fixo como 8.8.8.8 também cai no filtro); o resto passa pela
// política e vira um socket do host. Negado é descartado em silêncio, como faria um firewall.
func (g *Gateway) handleUDP(r *udp.ForwarderRequest) bool {
	id := r.ID()
	dst := addrOf(id.LocalAddress)
	if id.LocalPort == 53 {
		var wq waiter.Queue
		ep, err := r.CreateEndpoint(&wq)
		if err != nil {
			return false
		}
		go g.serveDNS(gonet.NewUDPConn(&wq, ep))
		return true
	}
	target := net.JoinHostPort(dst.String(), strconv.Itoa(int(id.LocalPort)))
	dial, ok, why := g.decide(dst)
	if !ok {
		g.deny("udp", target, why)
		return true
	}
	host, err := g.cfg.Dial(g.ctx, "udp", net.JoinHostPort(dial.String(), strconv.Itoa(int(id.LocalPort))))
	if err != nil {
		return true
	}
	var wq waiter.Queue
	ep, terr := r.CreateEndpoint(&wq)
	if terr != nil {
		host.Close()
		return false
	}
	go relayUDP(gonet.NewUDPConn(&wq, ep), host)
	return true
}

// relayUDP copia datagramas nos dois sentidos até udpIdle sem tráfego.
func relayUDP(guest, host net.Conn) {
	defer guest.Close()
	defer host.Close()
	var once sync.Once
	stop := func() { once.Do(func() { guest.Close(); host.Close() }) }
	cp := func(dst, src net.Conn) {
		defer stop()
		buf := make([]byte, 64<<10)
		for {
			src.SetReadDeadline(time.Now().Add(udpIdle))
			n, err := src.Read(buf)
			if err != nil {
				return
			}
			if _, err := dst.Write(buf[:n]); err != nil {
				return
			}
		}
	}
	go cp(host, guest)
	cp(guest, host)
}

// publishUDP publica uma porta UDP: cada cliente do host ganha a sua sessão até o guest
// (NAT por cliente), fechada depois de udpIdle sem tráfego.
func (g *Gateway) publishUDP(addr string, guestPort uint16) error {
	pc, err := net.ListenPacket("udp", addr)
	if err != nil {
		return fmt.Errorf("publish %s/udp: %w", addr, err)
	}
	g.listeners = append(g.listeners, pc)
	local := &tcpip.FullAddress{NIC: nicID, Addr: tcpip.AddrFrom4(GatewayIP.As4())}
	guest := &tcpip.FullAddress{NIC: nicID, Addr: tcpip.AddrFrom4(GuestIP.As4()), Port: guestPort}
	var mu sync.Mutex
	sessions := map[string]*gonet.UDPConn{}
	g.wg.Add(1)
	go func() {
		defer g.wg.Done()
		buf := make([]byte, 64<<10)
		for {
			n, client, err := pc.ReadFrom(buf)
			if err != nil {
				return
			}
			mu.Lock()
			s, ok := sessions[client.String()]
			if !ok {
				s, err = gonet.DialUDP(g.stack, local, guest, ipv4.ProtocolNumber)
				if err != nil {
					mu.Unlock()
					continue
				}
				sessions[client.String()] = s
				go func(s *gonet.UDPConn, client net.Addr) {
					reply := make([]byte, 64<<10)
					for {
						s.SetReadDeadline(time.Now().Add(udpIdle))
						n, err := s.Read(reply)
						if err != nil {
							break
						}
						pc.WriteTo(reply[:n], client)
					}
					mu.Lock()
					delete(sessions, client.String())
					mu.Unlock()
					s.Close()
				}(s, client)
			}
			mu.Unlock()
			s.Write(buf[:n])
		}
	}()
	return nil
}
