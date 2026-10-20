// Package podnet liga a virtio-net de uma VM de pod (Kubernetes) à rede que o CNI deu ao pod: um
// TAP no netns do pod, com tc mirred entre a eth0 e o TAP nos dois sentidos, e os quadros bombeados
// entre o TAP e o socket da libkrun (krun_add_net_unixstream). O guest assume o IP, o MAC e o
// gateway da eth0: para o resto do cluster a VM É o pod. Sem gVisor nem NAT neste modo.
package podnet

import (
	"encoding/binary"
	"fmt"
	"io"
	"net"
	"net/netip"
	"sync"

	"runner.code/internal/protocol"
)

// Info é o que o CNI configurou na eth0 do pod, lido do netns.
type Info struct {
	Addr    netip.Prefix
	Gateway netip.Addr
	MAC     net.HardwareAddr
	MTU     int
}

// Env devolve as variáveis com que o guest monta a eth0 no boot (as mesmas do gateway em
// userspace). Sem DNS: o resolv.conf de cada container vem do kubelet, como montagem.
func (i Info) Env() []string {
	env := []string{protocol.EnvNetAddr + "=" + i.Addr.String()}
	if i.Gateway.IsValid() {
		env = append(env, protocol.EnvNetGateway+"="+i.Gateway.String())
	}
	return env
}

// maxFrame é o maior quadro aceito do socket da libkrun: MTU de jumbo (9000) com folga para o
// cabeçalho Ethernet. Prefixo maior que isso é fluxo corrompido, e a bomba para.
const maxFrame = 65535

// Pump liga o TAP ao socket da libkrun até um dos dois fechar. Cada Read do TAP é um quadro; no
// socket, cada quadro leva 4 bytes big-endian de tamanho na frente (o mesmo formato do gateway em
// userspace e do passt). Devolve quando os dois sentidos pararam.
func Pump(tap io.ReadWriter, sock io.ReadWriter) error {
	var wg sync.WaitGroup
	errc := make(chan error, 2)
	wg.Add(2)
	go func() {
		defer wg.Done()
		buf := make([]byte, maxFrame)
		for {
			n, err := tap.Read(buf)
			if err != nil {
				errc <- fmt.Errorf("tap read: %w", err)
				return
			}
			out := make([]byte, 4+n)
			binary.BigEndian.PutUint32(out, uint32(n))
			copy(out[4:], buf[:n])
			if _, err := sock.Write(out); err != nil {
				errc <- fmt.Errorf("vm write: %w", err)
				return
			}
		}
	}()
	go func() {
		defer wg.Done()
		var hdr [4]byte
		frame := make([]byte, maxFrame)
		for {
			if _, err := io.ReadFull(sock, hdr[:]); err != nil {
				errc <- fmt.Errorf("vm read: %w", err)
				return
			}
			n := binary.BigEndian.Uint32(hdr[:])
			if n == 0 || n > maxFrame {
				errc <- fmt.Errorf("vm frame of %d bytes", n)
				return
			}
			if _, err := io.ReadFull(sock, frame[:n]); err != nil {
				errc <- fmt.Errorf("vm read: %w", err)
				return
			}
			if _, err := tap.Write(frame[:n]); err != nil {
				errc <- fmt.Errorf("tap write: %w", err)
				return
			}
		}
	}()
	first := <-errc
	// O primeiro sentido que parou derruba o outro: quem fecha os dois lados é o chamador (a VM
	// morreu, ou o TAP sumiu com o netns); aqui só se espera e devolve o motivo do primeiro.
	if c, ok := tap.(io.Closer); ok {
		c.Close()
	}
	if c, ok := sock.(io.Closer); ok {
		c.Close()
	}
	wg.Wait()
	return first
}
