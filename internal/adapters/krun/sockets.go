package krun

import (
	"context"
	"io"
	"net"
	"os"
	"sync"

	"runner.codes/internal/adapters/guestproto"
	"runner.codes/internal/ports"
)

// dialUnix abre a conexão até o socket do guest; trocado nos testes (sem guest de verdade).
var dialUnix = guestproto.DialUnix

// socketBridges são os sockets Unix que este processo de VM escuta no host.
type socketBridges struct {
	listeners []net.Listener
	paths     []string
	wg        sync.WaitGroup
}

// startSockets escuta cada socket do host (0600: só o dono do rc) e liga cada conexão ao
// socket do guest pelo vsock da máquina. Escuta já no boot: antes de o guest atender, a
// conexão é recusada e o cliente tenta de novo, como com um daemon que ainda sobe.
func startSockets(socks []ports.VMSocket, vsockSocket string) (*socketBridges, error) {
	b := &socketBridges{}
	for _, s := range socks {
		os.Remove(s.Host)
		l, err := net.Listen("unix", s.Host)
		if err != nil {
			b.Close()
			return nil, err
		}
		if err := os.Chmod(s.Host, 0o600); err != nil {
			l.Close()
			b.Close()
			return nil, err
		}
		b.listeners, b.paths = append(b.listeners, l), append(b.paths, s.Host)
		// O discador é lido aqui, uma vez: as goroutines de cada conexão não tocam na variável
		// de pacote, que um teste troca e restaura enquanto elas ainda rodam.
		guest, dial := s.Guest, dialUnix
		b.wg.Add(1)
		go func() {
			defer b.wg.Done()
			for {
				c, err := l.Accept()
				if err != nil {
					return
				}
				go func() {
					in, err := dial(context.Background(), vsockSocket, guest)
					if err != nil {
						c.Close()
						return
					}
					bridge(c, in)
				}()
			}
		}()
	}
	return b, nil
}

// Close para de escutar e apaga os sockets do host (o próximo start cria de novo).
func (b *socketBridges) Close() error {
	for _, l := range b.listeners {
		l.Close()
	}
	b.wg.Wait()
	for _, p := range b.paths {
		os.Remove(p)
	}
	return nil
}

// bridge copia bytes nos dois sentidos, com meio-fechamento (o cliente termina de mandar e
// ainda recebe a resposta inteira).
func bridge(a, b net.Conn) {
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
