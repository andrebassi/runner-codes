package netgw

import (
	"context"
	"errors"
	"net"
	"net/netip"
	"path/filepath"
	"time"

	"golang.org/x/net/icmp"
	xipv4 "golang.org/x/net/ipv4"
	"gvisor.dev/gvisor/pkg/buffer"
	"gvisor.dev/gvisor/pkg/tcpip"
	"gvisor.dev/gvisor/pkg/tcpip/checksum"
	"gvisor.dev/gvisor/pkg/tcpip/header"
	"gvisor.dev/gvisor/pkg/tcpip/network/ipv4"
	"gvisor.dev/gvisor/pkg/tcpip/stack"
)

// Pinger manda um echo ICMP do host a dst e diz se a resposta voltou dentro do prazo do ctx.
type Pinger func(ctx context.Context, dst netip.Addr, payload []byte) error

// maxPingsInFlight limita os pings simultâneos de uma máquina: cada um segura um socket do
// host por até pingTimeout, e um `ping -f` no guest não deve abrir centenas.
const maxPingsInFlight = 32

// pingTimeout é quanto o host espera a resposta; o ping do guest tem o prazo dele, e uma
// resposta que chega depois de 3 s já não serve a ninguém.
const pingTimeout = 3 * time.Second

// handleICMP atende o echo request que o guest manda a um IP de fora. A camada IPv4 do
// gVisor só responde sozinha a endereço próprio; para o endereço "temporário" do modo
// promíscuo (qualquer IP) ela entrega aqui, e a resposta só sai se o host recebeu a dele:
// ping que passa é ping de verdade, sob a mesma política de saída do TCP.
func (g *Gateway) handleICMP(id stack.TransportEndpointID, pkt *stack.PacketBuffer) bool {
	h := header.ICMPv4(pkt.TransportHeader().Slice())
	if len(h) < header.ICMPv4MinimumSize || h.Type() != header.ICMPv4Echo {
		return false
	}
	// O pkt é do gVisor e não vale depois do retorno: a mensagem (cabeçalho + dados) é copiada.
	data := stack.PayloadSince(pkt.TransportHeader())
	msg := append([]byte(nil), data.AsSlice()...)
	data.Release()
	dst, src := addrOf(id.LocalAddress), id.RemoteAddress
	if g.inFabric(dst) {
		// Rede nomeada: fora da política de saída, como o TCP. Não há kernel do outro lado a
		// quem repassar o echo (o tráfego da rede é por socket, conexão a conexão); responde
		// quando a máquina dona do IP está ligada nesta rede, que é o que o ping quer saber.
		g.inFlight(func() {
			if g.fabricAlive(dst) {
				g.echoReply(id.LocalAddress, src, msg)
			}
		})
		return true
	}
	dial, ok, why := g.decide(dst)
	if !ok {
		g.deny("icmp", dst.String(), why)
		return true
	}
	g.inFlight(func() {
		ctx, cancel := context.WithTimeout(g.ctx, pingTimeout)
		defer cancel()
		if err := g.cfg.Ping(ctx, dial, msg[header.ICMPv4MinimumSize:]); err != nil {
			return
		}
		g.echoReply(id.LocalAddress, src, msg)
	})
	return true
}

// inFlight roda f numa goroutine se houver vaga no semáforo dos pings; sem vaga o echo some,
// como numa rede congestionada (não fila nem trava a pilha).
func (g *Gateway) inFlight(f func()) {
	select {
	case g.pings <- struct{}{}:
	default:
		return
	}
	go func() {
		defer func() { <-g.pings }()
		f()
	}()
}

// fabricAlive diz se a máquina dona de dst está ligada nesta rede nomeada: o próprio IP
// sempre está; outro, se o socket dela atende (o lado de lá lê EOF no cabeçalho e fecha).
func (g *Gateway) fabricAlive(dst netip.Addr) bool {
	if dst == g.cfg.Fabric.Self {
		return true
	}
	c, err := net.DialTimeout("unix", filepath.Join(g.cfg.Fabric.Dir, dst.String()+".sock"), time.Second)
	if err != nil {
		return false
	}
	c.Close()
	return true
}

// echoReply devolve ao guest a resposta em nome de from: a mesma mensagem, com o tipo
// trocado e o checksum refeito (RFC 792), como o próprio gVisor faz para endereço local.
func (g *Gateway) echoReply(from, to tcpip.Address, msg []byte) {
	r, err := g.stack.FindRoute(nicID, from, to, ipv4.ProtocolNumber, false)
	if err != nil {
		return
	}
	defer r.Release()
	reply := header.ICMPv4(msg)
	reply.SetType(header.ICMPv4EchoReply)
	reply.SetChecksum(0)
	reply.SetChecksum(^checksum.Checksum(msg, 0))
	pkt := stack.NewPacketBuffer(stack.PacketBufferOptions{
		ReserveHeaderBytes: int(r.MaxHeaderLength()),
		Payload:            buffer.MakeWithData(msg),
	})
	defer pkt.DecRef()
	pkt.TransportProtocolNumber = header.ICMPv4ProtocolNumber
	r.WritePacket(stack.NetworkHeaderParams{Protocol: header.ICMPv4ProtocolNumber, TTL: r.DefaultTTL()}, pkt)
}

// errNoEchoReply é a falta de resposta válida dentro do prazo.
var errNoEchoReply = errors.New("no echo reply")

// hostPing é o Pinger padrão: socket ICMP de datagrama, que o macOS dá a qualquer usuário e
// o Linux dá a quem está em net.ipv4.ping_group_range (sem raw socket, sem root). O kernel
// troca o identificador; a resposta é reconhecida pela sequência e pelos dados.
func hostPing(ctx context.Context, dst netip.Addr, payload []byte) error {
	c, err := icmp.ListenPacket("udp4", "0.0.0.0")
	if err != nil {
		return err
	}
	defer c.Close()
	deadline, _ := ctx.Deadline()
	c.SetDeadline(deadline)
	const seq = 1
	req, _ := (&icmp.Message{Type: xipv4.ICMPTypeEcho, Body: &icmp.Echo{ID: 1, Seq: seq, Data: payload}}).Marshal(nil)
	if _, err := c.WriteTo(req, &net.UDPAddr{IP: dst.AsSlice()}); err != nil {
		return err
	}
	buf := make([]byte, 64<<10)
	for {
		n, _, err := c.ReadFrom(buf)
		if err != nil {
			return errNoEchoReply
		}
		if isEchoReply(buf[:n], seq, payload) {
			return nil
		}
	}
}

// isEchoReply diz se b é a resposta do echo com esta sequência e estes dados (outro tráfego
// ICMP do socket, como um destination unreachable, não conta).
func isEchoReply(b []byte, seq int, payload []byte) bool {
	m, err := icmp.ParseMessage(1, b) // 1 = ICMP para IPv4
	if err != nil || m.Type != xipv4.ICMPTypeEchoReply {
		return false
	}
	e, ok := m.Body.(*icmp.Echo)
	return ok && e.Seq == seq && string(e.Data) == string(payload)
}
