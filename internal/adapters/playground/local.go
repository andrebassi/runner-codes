package playground

import (
	"context"
	"fmt"
	"net"
	"net/http"
	"net/http/httputil"
	"strconv"
	"sync"

	"runner.codes/internal/adapters/webterm"
)

// HubFactory abre o hub de terminal de uma máquina (sessão principal, aba shell, portas) com o token dado.
type HubFactory func(ctx context.Context, machine, token string) *webterm.Hub

// LocalBackend é o playground com as VMs do próprio rc (app.Playground): o terminal web roda neste processo, falando
// com a VM pelo vsock, e as portas também vão pelo vsock.
type LocalBackend struct {
	Ctx      context.Context
	Secret   []byte
	EnsureVM func(ctx context.Context, visitor string) (string, error)
	TouchVM  func(machine string)
	NewHub   HubFactory
	Dial     func(ctx context.Context, machine string, port uint16) (net.Conn, error)
	mu       sync.Mutex
	hubs     map[string]*webterm.Hub
	handlers map[*webterm.Hub]http.Handler
}

// Ensure liga a VM e abre o hub dela (ou reaproveita o vivo).
func (b *LocalBackend) Ensure(ctx context.Context, visitor string) (string, error) {
	machine, err := b.EnsureVM(ctx, visitor)
	if err != nil {
		return "", err
	}
	b.hubFor(machine)
	return machine, nil
}

// Touch repassa o uso ao app.Playground.
func (b *LocalBackend) Touch(machine string) {
	if b.TouchVM != nil {
		b.TouchVM(machine)
	}
}

// hubFor devolve o hub vivo da máquina, ou abre um novo se não há ou se a sessão principal acabou (VM religada
// depois de o reaper desligar, ou o programa saiu).
func (b *LocalBackend) hubFor(machine string) *webterm.Hub {
	b.mu.Lock()
	defer b.mu.Unlock()
	if b.hubs == nil {
		b.hubs, b.handlers = map[string]*webterm.Hub{}, map[*webterm.Hub]http.Handler{}
	}
	if h, ok := b.hubs[machine]; ok && !h.MainEnded() {
		return h
	}
	h := b.NewHub(b.Ctx, machine, Token(b.Secret, machine))
	b.hubs[machine] = h
	b.handlers[h] = webterm.Handler(h)
	return h
}

// Terminal devolve o handler do hub da máquina; sem hub (o playground reiniciou), nil.
func (b *LocalBackend) Terminal(machine string) http.Handler {
	b.mu.Lock()
	defer b.mu.Unlock()
	h, ok := b.hubs[machine]
	if !ok {
		return nil
	}
	return b.handlers[h]
}

// Port leva a requisição à porta da VM do visitante, pelo vsock (WebSocket incluso).
func (b *LocalBackend) Port(machine string, port uint16) http.Handler {
	return &httputil.ReverseProxy{
		Rewrite: func(pr *httputil.ProxyRequest) {
			pr.Out.URL.Scheme = "http"
			pr.Out.URL.Host = fmt.Sprintf("127.0.0.1:%d", port)
			pr.Out.Host = pr.In.Host
			pr.SetXForwarded()
		},
		Transport: &http.Transport{
			DialContext: func(ctx context.Context, _, _ string) (net.Conn, error) { return b.Dial(ctx, machine, port) },
		},
		ErrorHandler: portDown(port),
	}
}

// portDown é a resposta quando nada atende na porta da máquina.
func portDown(port uint16) func(http.ResponseWriter, *http.Request, error) {
	return func(w http.ResponseWriter, _ *http.Request, _ error) {
		http.Error(w, "nothing is answering on port "+strconv.Itoa(int(port))+" in this machine", http.StatusBadGateway)
	}
}
