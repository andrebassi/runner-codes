package viewer

import (
	"sync"
	"time"
)

// AgentHeader marca um POST /input mandado por automação (CLI, API, SDK, MCP). Só esses aparecem para a página como
// seta e clique desenhados por cima da tela: o mouse da própria pessoa já é o cursor dela, e desenhar uma seta em
// cima dele só atrapalharia.
const AgentHeader = "X-RC-Agent"

// PointerEvent é o que a página desenha: onde o agente está (x e y de 0 a 1, independentes do tamanho da tela) e o
// que ele fez ali.
type PointerEvent struct {
	Seq uint64 `json:"seq"`
	// Kind: move, click (botão apertado), key (tecla apertada) ou scroll.
	Kind string  `json:"kind"`
	X    float64 `json:"x"`
	Y    float64 `json:"y"`
}

// Códigos do evdev que o feed reconhece (linux/input-event-codes.h).
const (
	evSynType  = 0
	evKeyType  = 1
	evRelType  = 2
	evAbsType  = 3
	absXCode   = 0
	absYCode   = 1
	relWheel   = 8
	btnFirst   = 0x110 // BTN_LEFT
	btnLast    = 0x117 // BTN_TASK: a faixa dos botões do mouse
	absAxisMax = 32767
	// feedKeep é quantos eventos ficam guardados para uma página que chegou atrasada: ela só precisa do recente.
	feedKeep = 64
)

// pointerFeed guarda os eventos de agente recentes e acorda quem espera por novos.
type pointerFeed struct {
	mu     sync.Mutex
	cond   *sync.Cond
	events []PointerEvent
	seq    uint64
	x, y   float64
}

// newPointerFeed cria o feed vazio, com o ponteiro no centro.
func newPointerFeed() *pointerFeed {
	f := &pointerFeed{x: 0.5, y: 0.5}
	f.cond = sync.NewCond(&f.mu)
	return f
}

// publish lê um lote de eventos evdev de agente e guarda o que a página precisa desenhar. Posição se acumula até o
// SYN_REPORT (é quando o guest a aplica); botão e tecla contam só ao apertar, não ao soltar.
func (f *pointerFeed) publish(evs []Event) {
	f.mu.Lock()
	defer f.mu.Unlock()
	moved := false
	for _, e := range evs {
		switch {
		case e.Type == evAbsType && e.Code == absXCode:
			f.x, moved = clamp01(float64(e.Value)/absAxisMax), true
		case e.Type == evAbsType && e.Code == absYCode:
			f.y, moved = clamp01(float64(e.Value)/absAxisMax), true
		case e.Type == evSynType && moved:
			f.add("move")
			moved = false
		case e.Type == evKeyType && e.Value == 1 && e.Code >= btnFirst && e.Code <= btnLast:
			f.add("click")
		case e.Type == evKeyType && e.Value == 1:
			f.add("key")
		case e.Type == evRelType && e.Code == relWheel:
			f.add("scroll")
		}
	}
	if moved {
		f.add("move")
	}
	f.cond.Broadcast()
}

// add guarda um evento na posição atual (com o lock), descartando os mais velhos que feedKeep.
func (f *pointerFeed) add(kind string) {
	f.seq++
	f.events = append(f.events, PointerEvent{Seq: f.seq, Kind: kind, X: f.x, Y: f.y})
	if len(f.events) > feedKeep {
		f.events = append([]PointerEvent(nil), f.events[len(f.events)-feedKeep:]...)
	}
}

// since devolve os eventos depois de after, esperando até wait por algum; sem nada no prazo, devolve vazio e o
// mesmo after (a página só repete a espera).
func (f *pointerFeed) since(after uint64, wait time.Duration) ([]PointerEvent, uint64) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.seq <= after {
		t := time.AfterFunc(wait, func() { f.mu.Lock(); f.cond.Broadcast(); f.mu.Unlock() })
		defer t.Stop()
		deadline := time.Now().Add(wait)
		for f.seq <= after && time.Now().Before(deadline) {
			f.cond.Wait()
		}
	}
	var out []PointerEvent
	for _, e := range f.events {
		if e.Seq > after {
			out = append(out, e)
		}
	}
	if f.seq <= after {
		return nil, after
	}
	return out, f.seq
}

// clamp01 prende o valor entre 0 e 1: um evento fora do eixo não pode desenhar a seta fora da tela.
func clamp01(v float64) float64 {
	switch {
	case v < 0:
		return 0
	case v > 1:
		return 1
	}
	return v
}
