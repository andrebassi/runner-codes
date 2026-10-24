package krun

import (
	"context"
	"strconv"
	"sync"
	"time"

	"runner.codes/internal/adapters/guestproto"
	"runner.codes/internal/protocol"
)

// DisplayResizeHook é o programa que o rc chama dentro da VM quando a janela do visualizador muda de
// tamanho, com LARGURA e ALTURA como argumentos. O rc não sabe se o guest usa Xorg, Wayland ou nada:
// quem sabe trocar a resolução é a imagem. Sem o arquivo (ou sem permissão de execução), nada acontece.
const DisplayResizeHook = "/etc/rc/display-resize"

// resizeSettle é quanto o pedido espera por outro antes de ir ao guest: durante um arrasto da janela
// chegam vários; só o último interessa, e cada troca de modo custa um xrandr inteiro no guest.
const resizeSettle = 300 * time.Millisecond

// resizeTimeout limita cada chamada do gancho: um gancho travado não pode prender o próximo pedido.
const resizeTimeout = 15 * time.Second

// runGuest manda um job ao guest; trocado nos testes (sem VM de verdade).
var runGuest = guestproto.Client{}.Run

// guestResizer leva os pedidos de tamanho do visualizador ao gancho do guest, um por vez e só o último.
type guestResizer struct {
	socket string
	settle time.Duration

	mu      sync.Mutex
	timer   *time.Timer
	w, h    int
	running bool // um gancho no ar: o pedido novo espera ele acabar, nunca roda em paralelo
	dirty   bool // chegou pedido enquanto o gancho rodava
	done    func(w, h int, res protocol.Result, err error)
}

// newGuestResizer cria o resizer que fala com o guest pelo socket do vsock da máquina.
func newGuestResizer(socket string) *guestResizer {
	return &guestResizer{socket: socket, settle: resizeSettle}
}

// Resize guarda o tamanho pedido e (re)arma a espera; o gancho roda quando os pedidos param.
func (g *guestResizer) Resize(w, h int) {
	g.mu.Lock()
	defer g.mu.Unlock()
	g.w, g.h = w, h
	if g.running {
		g.dirty = true
		return
	}
	if g.timer != nil {
		g.timer.Stop()
	}
	g.timer = time.AfterFunc(g.settle, g.fire)
}

// fire roda o gancho com o último tamanho; se chegou outro pedido no meio, roda de novo com ele.
func (g *guestResizer) fire() {
	g.mu.Lock()
	if g.running {
		g.dirty = true
		g.mu.Unlock()
		return
	}
	g.running = true
	w, h := g.w, g.h
	g.mu.Unlock()

	res, err := g.apply(w, h)

	g.mu.Lock()
	g.running = false
	again := g.dirty && (g.w != w || g.h != h)
	g.dirty = false
	done := g.done
	g.mu.Unlock()
	if done != nil {
		done(w, h, res, err)
	}
	if again {
		g.fire()
	}
}

// apply chama o gancho no guest. O teste de existência fica no próprio shell do guest: sem o arquivo,
// sai 0 sem fazer nada, e uma imagem sem tela gráfica não vê erro nenhum.
func (g *guestResizer) apply(w, h int) (protocol.Result, error) {
	ctx, cancel := context.WithTimeout(context.Background(), resizeTimeout)
	defer cancel()
	script := `[ -x "$0" ] || exit 0; exec "$0" "$1" "$2"`
	return runGuest(ctx, g.socket, protocol.Job{
		TraceID: "display-resize",
		Op:      protocol.OpExec,
		Argv:    []string{"sh", "-c", script, DisplayResizeHook, strconv.Itoa(w), strconv.Itoa(h)},
		Timeout: int(resizeTimeout / time.Second),
	})
}
