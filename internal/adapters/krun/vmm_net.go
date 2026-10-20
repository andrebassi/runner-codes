package krun

import (
	"context"
	"fmt"
	"io"
	"net"
	"net/netip"
	"os"
	"strings"
	"syscall"

	"runner.code/internal/adapters/guestproto"
	"runner.code/internal/adapters/netgw"
	"runner.code/internal/adapters/podnet"
	"runner.code/internal/credca"
	"runner.code/internal/domain"
	"runner.code/internal/ports"
	"runner.code/internal/protocol"
)

// guestMAC é o MAC da eth0 do guest, o mesmo que o gateway espera do outro lado.
var guestMAC = []byte(netgw.GuestMAC)

// startNetwork cria o socketpair da virtio-net, sobe o gateway no lado do host (neste
// mesmo processo: ele vive e morre com a VM) e devolve o fd do lado do guest, o env que
// manda o guest montar a eth0 e o que fechar se a VM não chegar a subir.
func startNetwork(n *ports.VMNet, vsockSocket string) (int, []string, io.Closer, error) {
	pol, err := n.Network.OutboundPolicy()
	if err != nil {
		return -1, nil, nil, err
	}
	// A config inteira antes do socketpair: um erro aqui não deixa descritor para trás.
	cfg := netgw.Config{Policy: pol, Ports: n.Network.Ports, MachineName: n.Machine}
	if n.FabricDir != "" {
		self, err := netip.ParseAddr(n.Network.Address)
		if err != nil {
			return -1, nil, nil, fmt.Errorf("named network address: %w", err)
		}
		cfg.Fabric = &netgw.Fabric{Dir: n.FabricDir, Name: n.Network.Name, Self: self}
	}
	if len(n.Network.Credentials) > 0 {
		if cfg.Intercept, err = newInterceptor(n); err != nil {
			return -1, nil, nil, err
		}
	}
	fds, err := syscall.Socketpair(syscall.AF_UNIX, syscall.SOCK_STREAM, 0)
	if err != nil {
		return -1, nil, nil, fmt.Errorf("network socketpair: %w", err)
	}
	f := os.NewFile(uintptr(fds[0]), "netgw")
	conn, err := net.FileConn(f)
	f.Close()
	if err != nil {
		syscall.Close(fds[1])
		return -1, nil, nil, fmt.Errorf("network socket: %w", err)
	}
	if vsockSocket != "" {
		// Porta publicada entra pelo guest (OpDial no vsock), para alcançar quem escuta só
		// em localhost; sem o socket (nunca numa máquina) cai na rota pela pilha.
		cfg.DialGuest = func(ctx context.Context, port uint16) (net.Conn, error) {
			return guestproto.DialPort(ctx, vsockSocket, port)
		}
	}
	if n.DenialsLog != "" {
		if lf, err := os.OpenFile(n.DenialsLog, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0o600); err == nil {
			cfg.Denials = lf
		}
	}
	gw, err := netgw.Start(conn, cfg)
	if err != nil {
		syscall.Close(fds[1])
		return -1, nil, nil, err
	}
	if n.ControlSocket != "" {
		if err := gw.ServeControl(n.ControlSocket, n.Network.Enabled); err != nil {
			gw.Close()
			syscall.Close(fds[1])
			return -1, nil, nil, err
		}
	}
	gwIP := netgw.GatewayIP.String()
	env := []string{
		protocol.EnvNetAddr + "=" + fmt.Sprintf("%s/%d", netgw.GuestIP, netgw.PrefixLen),
		protocol.EnvNetGateway + "=" + gwIP,
		protocol.EnvNetDNS + "=" + gwIP,
	}
	return fds[1], env, gw, nil
}

// attachPod é podnet.Attach; os testes trocam porque criar TAP exige root e um netns.
var attachPod = podnet.Attach

// startPodNet liga a virtio-net à rede do pod: TAP no netns dele, socketpair com a libkrun e a bomba
// de quadros neste processo (vive e morre com a VM). Devolve o fd do lado do guest, o env que monta a
// eth0 com o IP do pod, o MAC da eth0 e o que fechar se a VM não subir.
func startPodNet(nsPath string) (int, []string, []byte, io.Closer, error) {
	tap, info, err := attachPod(nsPath)
	if err != nil {
		return -1, nil, nil, nil, err
	}
	fds, err := syscall.Socketpair(syscall.AF_UNIX, syscall.SOCK_STREAM, 0)
	if err != nil {
		tap.Close()
		return -1, nil, nil, nil, fmt.Errorf("pod network socketpair: %w", err)
	}
	f := os.NewFile(uintptr(fds[0]), "podnet")
	go podnet.Pump(tap, f)
	return fds[1], info.Env(), info.MAC, multiCloser{tap, f}, nil
}

// multiCloser fecha vários descritores de uma vez (o TAP e o lado do socket deste processo).
type multiCloser []io.Closer

// Close fecha todos e devolve o primeiro erro.
func (m multiCloser) Close() error {
	var first error
	for _, c := range m {
		if err := c.Close(); err != nil && first == nil {
			first = err
		}
	}
	return first
}

// httpsPort é a porta interceptada para credenciais: HTTP em claro e outras portas seguem sem
// troca.
const httpsPort = 443

// newInterceptor carrega a CA da máquina e liga as bindings ao ambiente deste processo: o
// vmm herda o env de quem deu o start, e o valor é lido a cada requisição (nunca vai para
// disco, para a spec nem para o guest).
func newInterceptor(n *ports.VMNet) (*netgw.Interceptor, error) {
	cert, err := os.ReadFile(n.CACert)
	if err != nil {
		return nil, fmt.Errorf("credential CA: %w", err)
	}
	key, err := os.ReadFile(n.CAKey)
	if err != nil {
		return nil, fmt.Errorf("credential CA: %w", err)
	}
	auth, err := credca.Load(cert, key)
	if err != nil {
		return nil, fmt.Errorf("credential CA: %w", err)
	}
	return &netgw.Interceptor{Bindings: n.Network.Credentials, Authority: auth, Port: httpsPort, Resolve: envValue}, nil
}

// envValue lê o valor da binding: do arquivo de um segredo de mesmo nome (lido a cada
// requisição: trocar o arquivo dispensa restart), da variável apontada por ele, ou da própria
// variável da binding, no ambiente deste processo (o de quem deu o start).
func envValue(c domain.Credential) (string, error) {
	if c.SourceFile != "" {
		b, err := os.ReadFile(c.SourceFile)
		if err != nil {
			return "", err
		}
		return strings.TrimRight(string(b), "\r\n"), nil
	}
	name := c.Env
	if c.SourceEnv != "" {
		name = c.SourceEnv
	}
	v, ok := os.LookupEnv(name)
	if !ok {
		return "", fmt.Errorf("%s is not set in the environment of machine start", name)
	}
	return v, nil
}
