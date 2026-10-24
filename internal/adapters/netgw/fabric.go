package netgw

import (
	"encoding/binary"
	"fmt"
	"io"
	"net"
	"net/netip"
	"os"
	"path/filepath"
	"strings"
	"time"

	"gvisor.dev/gvisor/pkg/tcpip"
	"gvisor.dev/gvisor/pkg/tcpip/adapters/gonet"
	"gvisor.dev/gvisor/pkg/tcpip/network/ipv4"

	"runner.codes/internal/domain"
)

// Fabric liga o gateway a uma rede nomeada. Dir guarda, por máquina ligada, o socket
// "<ip>.sock" (quem quer falar com ela disca aqui) e o arquivo "<nome>.ip" (o DNS da rede lê
// aqui). Self é o IP desta máquina na rede.
type Fabric struct {
	Dir  string
	Name string
	Self netip.Addr
}

// fabricHeader é o que o gateway de origem manda antes dos bytes: IP de origem (4) e porta de
// destino (2), para o guest de destino ver quem conectou e em que porta.
const fabricHeader = 6

// fabricDomain é o sufixo dos nomes da rede: <máquina>.<rede>.internal.
const fabricDomain = ".internal"

// inFabric diz se o destino é um IP de rede nomeada (que esta máquina alcança).
func (g *Gateway) inFabric(dst netip.Addr) bool {
	return g.cfg.Fabric != nil && domain.FabricPrefix.Contains(dst)
}

// dialFabric abre a conexão até a máquina dona de dst, pelo socket dela, e manda o cabeçalho.
// Máquina parada ou de outra rede não tem socket aqui: a conexão é recusada na hora.
func (g *Gateway) dialFabric(dst netip.Addr, port uint16) (net.Conn, error) {
	c, err := net.DialTimeout("unix", filepath.Join(g.cfg.Fabric.Dir, dst.String()+".sock"), 2*time.Second)
	if err != nil {
		return nil, err
	}
	var hdr [fabricHeader]byte
	src := g.cfg.Fabric.Self.As4()
	copy(hdr[:4], src[:])
	binary.BigEndian.PutUint16(hdr[4:], port)
	if _, err := c.Write(hdr[:]); err != nil {
		c.Close()
		return nil, err
	}
	return c, nil
}

// serveFabric publica esta máquina na rede: o socket "<ip>.sock" recebe conexões de outras
// máquinas e as entrega ao guest, com o IP de origem de quem conectou; o "<nome>.ip" responde
// ao DNS da rede.
func (g *Gateway) serveFabric(machine string) error {
	f := g.cfg.Fabric
	if err := os.MkdirAll(f.Dir, 0o700); err != nil {
		return fmt.Errorf("named network: %w", err)
	}
	sock := filepath.Join(f.Dir, f.Self.String()+".sock")
	os.Remove(sock)
	l, err := net.Listen("unix", sock)
	if err != nil {
		return fmt.Errorf("named network: %w", err)
	}
	if err := os.Chmod(sock, 0o600); err != nil {
		l.Close()
		return fmt.Errorf("named network: %w", err)
	}
	name := filepath.Join(f.Dir, machine+".ip")
	if err := os.WriteFile(name, []byte(f.Self.String()), 0o600); err != nil {
		l.Close()
		return fmt.Errorf("named network: %w", err)
	}
	// O nome sai junto com o socket: se ficasse, uma máquina apagada cujo IP fosse dado a
	// outra continuaria resolvendo — para a máquina nova.
	g.listeners = append(g.listeners, l, closerFunc(func() error {
		os.Remove(name)
		return os.Remove(sock)
	}))
	g.wg.Add(1)
	go func() {
		defer g.wg.Done()
		for {
			c, err := l.Accept()
			if err != nil {
				return
			}
			go g.acceptFabric(c)
		}
	}()
	return nil
}

// acceptFabric lê o cabeçalho e liga a conexão ao guest na porta pedida, com o IP de origem
// da máquina que conectou (spoofing ligado na pilha: o guest vê 100.98.x.y, não o gateway).
func (g *Gateway) acceptFabric(c net.Conn) {
	var hdr [fabricHeader]byte
	c.SetReadDeadline(time.Now().Add(5 * time.Second))
	if _, err := io.ReadFull(c, hdr[:]); err != nil {
		c.Close()
		return
	}
	c.SetReadDeadline(time.Time{})
	src := tcpip.AddrFrom4([4]byte{hdr[0], hdr[1], hdr[2], hdr[3]})
	port := binary.BigEndian.Uint16(hdr[4:])
	local := tcpip.FullAddress{NIC: nicID, Addr: src}
	guest := tcpip.FullAddress{NIC: nicID, Addr: tcpip.AddrFrom4(GuestIP.As4()), Port: port}
	in, err := gonet.DialTCPWithBind(g.ctx, g.stack, local, guest, ipv4.ProtocolNumber)
	if err != nil {
		c.Close()
		return
	}
	pipe(c, in)
}

// resolveFabric responde <máquina>.<rede>.internal com o IP da máquina, lido do Dir. Nome
// fora do domínio da rede devolve ok=false (segue para o DNS normal).
func (g *Gateway) resolveFabric(name string) (netip.Addr, bool, bool) {
	f := g.cfg.Fabric
	if f == nil {
		return netip.Addr{}, false, false
	}
	suffix := "." + f.Name + fabricDomain
	n := strings.ToLower(strings.TrimSuffix(name, "."))
	if !strings.HasSuffix(n, suffix) {
		return netip.Addr{}, false, false
	}
	machine := strings.TrimSuffix(n, suffix)
	// O nome vem do guest: nada de "/" ou "." antes de virar caminho (um "../x" leria fora
	// do diretório da rede).
	if strings.ContainsAny(machine, "/.") {
		return netip.Addr{}, false, true
	}
	b, err := os.ReadFile(filepath.Join(f.Dir, machine+".ip"))
	if err != nil {
		return netip.Addr{}, false, true // nome da rede, mas máquina desconhecida: NXDOMAIN
	}
	a, err := netip.ParseAddr(strings.TrimSpace(string(b)))
	return a, err == nil, true
}

// closerFunc adapta uma função a io.Closer (para limpar arquivos no Close do gateway).
type closerFunc func() error

// Close chama a função.
func (f closerFunc) Close() error { return f() }
