package webterm

import (
	"context"
	"io"
	"net"
	"sort"
	"strconv"
	"sync"
	"time"
)

// PortLister devolve as portas TCP em LISTEN no guest (Machines.ListeningPorts com o nome fixado).
type PortLister func(ctx context.Context) ([]uint16, error)

// PortDialer abre uma conexão até 127.0.0.1:port dentro do guest (Machines.DialPort com o nome fixado).
type PortDialer func(ctx context.Context, port uint16) (net.Conn, error)

// Forward é uma porta do guest encaminhada para o host: o navegador abre http://127.0.0.1:Host/.
type Forward struct {
	Guest uint16 `json:"guest"`
	Host  int    `json:"host"`
}

// PollInterval é de quanto em quanto tempo o guest é consultado: 2 s fazem o link aparecer logo depois de o servidor
// subir, e cada consulta é um exec de milissegundos pelo vsock.
const PollInterval = 2 * time.Second

// Forwarder espelha no 127.0.0.1 do host cada porta que um programa abre no guest, enquanto o terminal web está de pé,
// como o encaminhamento automático de portas de um editor remoto. É TCP cru, não proxy HTTP: WebSocket, hot reload e
// caminhos absolutos (o /docs do FastAPI busca /openapi.json) funcionam como no guest.
//
// Segurança: o listener fica só em 127.0.0.1, como uma porta publicada com -p; quem já roda processo nesta máquina
// alcança, nenhuma outra máquina da rede alcança.
type Forwarder struct {
	list     PortLister
	dial     PortDialer
	interval time.Duration
	// listen abre o listener do host; variável para o teste forçar porta ocupada.
	listen func(port int) (net.Listener, error)

	// notify anota na aba "eventos" quando uma porta abre ou fecha (o Hub liga; nil não anota).
	notify func(format string, args ...any)

	mu   sync.Mutex
	open map[uint16]net.Listener
}

// NewForwarder monta o encaminhador; Run liga a consulta.
func NewForwarder(list PortLister, dial PortDialer, interval time.Duration) *Forwarder {
	return &Forwarder{list: list, dial: dial, interval: interval, listen: listenLoopback, open: map[uint16]net.Listener{}}
}

// listenLoopback escuta em 127.0.0.1:port (0 = qualquer livre). Nunca em 0.0.0.0: é a porta de dentro da VM.
func listenLoopback(port int) (net.Listener, error) {
	return net.Listen("tcp", net.JoinHostPort("127.0.0.1", strconv.Itoa(port)))
}

// Run consulta o guest até o contexto acabar e, no fim, fecha todos os listeners.
func (f *Forwarder) Run(ctx context.Context) {
	tick := time.NewTicker(f.interval)
	defer tick.Stop()
	defer f.closeAll()
	for {
		f.sync(ctx)
		select {
		case <-ctx.Done():
			return
		case <-tick.C:
		}
	}
}

// sync abre o que apareceu e fecha o que sumiu. Erro na consulta (máquina parando) não mexe em nada.
func (f *Forwarder) sync(ctx context.Context) {
	ports, err := f.list(ctx)
	if err != nil {
		return
	}
	want := map[uint16]bool{}
	for _, p := range ports {
		want[p] = true
	}
	f.mu.Lock()
	defer f.mu.Unlock()
	for p, ln := range f.open {
		if !want[p] {
			ln.Close()
			delete(f.open, p)
			f.event("porta %d fechou na máquina", p)
		}
	}
	for _, p := range ports {
		if _, ok := f.open[p]; ok {
			continue
		}
		// O mesmo número do guest, para o link ser o que o programa imprime; ocupado no host, qualquer livre.
		ln, err := f.listen(int(p))
		if err != nil {
			if ln, err = f.listen(0); err != nil {
				continue
			}
		}
		f.open[p] = ln
		f.event("porta %d aberta → http://127.0.0.1:%d/", p, ln.Addr().(*net.TCPAddr).Port)
		go f.serve(ctx, ln, p)
	}
}

// serve aceita conexões no listener do host e liga cada uma à porta do guest.
func (f *Forwarder) serve(ctx context.Context, ln net.Listener, guest uint16) {
	for {
		c, err := ln.Accept()
		if err != nil {
			return
		}
		go f.pipe(ctx, c, guest)
	}
}

// pipe copia nos dois sentidos até um lado fechar; porta que fechou no guest derruba só esta conexão.
func (f *Forwarder) pipe(ctx context.Context, c net.Conn, guest uint16) {
	defer c.Close()
	g, err := f.dial(ctx, guest)
	if err != nil {
		return
	}
	defer g.Close()
	done := make(chan struct{}, 2)
	go func() { io.Copy(g, c); done <- struct{}{} }()
	go func() { io.Copy(c, g); done <- struct{}{} }()
	<-done
}

// event repassa uma linha à aba "eventos", se houver quem anote.
func (f *Forwarder) event(format string, args ...any) {
	if f.notify != nil {
		f.notify(format, args...)
	}
}

// Ports devolve os encaminhamentos abertos, em ordem da porta do guest.
func (f *Forwarder) Ports() []Forward {
	f.mu.Lock()
	defer f.mu.Unlock()
	out := make([]Forward, 0, len(f.open))
	for p, ln := range f.open {
		out = append(out, Forward{Guest: p, Host: ln.Addr().(*net.TCPAddr).Port})
	}
	sort.Slice(out, func(i, j int) bool { return out[i].Guest < out[j].Guest })
	return out
}

// closeAll fecha todos os listeners (o terminal web saiu).
func (f *Forwarder) closeAll() {
	f.mu.Lock()
	defer f.mu.Unlock()
	for p, ln := range f.open {
		ln.Close()
		delete(f.open, p)
	}
}
