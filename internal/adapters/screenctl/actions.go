// Package screenctl controla a tela de uma máquina como uma pessoa faria: captura a imagem e manda mouse e teclado
// pelo visualizador da tela (GET /frame e POST /input), o mesmo caminho da página no navegador. Não depende de nada
// instalado no guest: os eventos chegam aos dispositivos virtio-input "runner.code keyboard" e "runner.code tablet".
//
// Este arquivo é a parte pura: ação (clicar, digitar, apertar atalho, rolar) vira pacotes de eventos evdev.
package screenctl

import (
	"fmt"
	"math"
	"strings"
	"time"

	"runner.codes/internal/adapters/viewer"
)

// Action é uma ação na tela, no formato da API, da CLI e do MCP. Coordenadas em pixels da tela atual, com a origem
// no canto superior esquerdo.
type Action struct {
	// Action é o nome: click, move, drag, type, key, scroll ou wait.
	Action string `json:"action"`
	X      int    `json:"x,omitempty"`
	Y      int    `json:"y,omitempty"`
	// ToX e ToY são o destino do drag.
	ToX int `json:"to_x,omitempty"`
	ToY int `json:"to_y,omitempty"`
	// Button do click: left (padrão), right ou middle.
	Button string `json:"button,omitempty"`
	// Double faz o click duas vezes.
	Double bool `json:"double,omitempty"`
	// Text do type.
	Text string `json:"text,omitempty"`
	// Keys do key: uma combinação como "ctrl+c", "Return" ou "ctrl+alt+t".
	Keys string `json:"keys,omitempty"`
	// Direction do scroll: up ou down (padrão); Amount é o número de passos da roda (padrão 3).
	Direction string `json:"direction,omitempty"`
	Amount    int    `json:"amount,omitempty"`
	// Ms é a espera do wait, em milissegundos.
	Ms int `json:"ms,omitempty"`
}

// Step é um pacote de eventos (fechado por SYN_REPORT) e a pausa antes do próximo. O visualizador entrega um pacote
// por vez ao guest; a pausa dá tempo ao app de ver o botão apertado antes de soltar, como uma pessoa.
type Step struct {
	Events []viewer.Event
	Pause  time.Duration
}

// Tipos e códigos do evdev (linux/input-event-codes.h).
const (
	evSyn     = 0
	evKey     = 1
	evRel     = 2
	evAbs     = 3
	synReport = 0
	relWheel  = 8
	absX      = 0
	absY      = 1
	// absMax é o teto dos eixos absolutos que o tablet virtual declara (0..32767).
	absMax = 32767
	// keyPause separa apertar e soltar: menos que isso e alguns apps (o xfwm4) perdem o clique.
	keyPause = 15 * time.Millisecond
	// travelPause é a espera entre levar o ponteiro ao ponto e apertar o botão (click, drag, scroll): quem assiste
	// pelo navegador vê a seta chegar e só então o clique, como uma pessoa faria. A página anima a seta em 1,1 s; a folga cobre o quadro seguinte.
	travelPause = 1300 * time.Millisecond
	// typePause separa as teclas de um type: com elas coladas, o terminal perdia letras (medido no exemplo
	// desktop-claude-agent); 5 ms digita ~100 caracteres por segundo.
	typePause = 5 * time.Millisecond
	// maxWait limita o wait de uma ação: espera longa é trabalho de quem chama, não de um pacote.
	maxWait = 60 * time.Second
	// maxScroll limita os passos de um scroll.
	maxScroll = 50
)

// Botões do mouse (BTN_LEFT, BTN_RIGHT, BTN_MIDDLE).
var buttons = map[string]uint16{"": 0x110, "left": 0x110, "right": 0x111, "middle": 0x112}

// Teclas com nome, em minúsculas (aceitas em qualquer caixa). Os nomes seguem o xdotool e o padrão de atalho do
// navegador: quem já escreveu "ctrl+shift+t" ou "Return" não precisa aprender outro dialeto.
var namedKeys = map[string]uint16{
	"esc": 1, "escape": 1, "backspace": 14, "tab": 15, "enter": 28, "return": 28, "space": 57,
	"ctrl": 29, "control": 29, "ctrl_l": 29, "ctrl_r": 97, "shift": 42, "shift_l": 42, "shift_r": 54,
	"alt": 56, "alt_l": 56, "alt_r": 100, "altgr": 100, "super": 125, "meta": 125, "win": 125, "cmd": 125,
	"capslock": 58, "home": 102, "up": 103, "pageup": 104, "page_up": 104, "prior": 104, "left": 105,
	"right": 106, "end": 107, "down": 108, "pagedown": 109, "page_down": 109, "next": 109, "insert": 110,
	"delete": 111, "del": 111, "menu": 139, "print": 99,
	"f1": 59, "f2": 60, "f3": 61, "f4": 62, "f5": 63, "f6": 64, "f7": 65, "f8": 66, "f9": 67, "f10": 68,
	"f11": 87, "f12": 88,
}

// keyShift é o keycode do Shift esquerdo, usado para os caracteres de cima do teclado.
const keyShift = 42

// charKeys leva cada caractere ASCII imprimível ao keycode do layout US e diz se ele precisa de Shift.
var charKeys = func() map[rune]struct {
	code  uint16
	shift bool
} {
	m := map[rune]struct {
		code  uint16
		shift bool
	}{' ': {57, false}, '\n': {28, false}, '\t': {15, false}}
	rows := []struct {
		first        uint16
		plain, shift string
	}{
		{2, "1234567890-=", "!@#$%^&*()_+"},
		{16, "qwertyuiop[]", "QWERTYUIOP{}"},
		{30, "asdfghjkl;'`", "ASDFGHJKL:\"~"},
		{44, "zxcvbnm,./", "ZXCVBNM<>?"},
	}
	for _, r := range rows {
		for i := range r.plain {
			m[rune(r.plain[i])] = struct {
				code  uint16
				shift bool
			}{r.first + uint16(i), false}
			m[rune(r.shift[i])] = struct {
				code  uint16
				shift bool
			}{r.first + uint16(i), true}
		}
	}
	// A contrabarra fica fora da sequência das fileiras no teclado US (keycode 43).
	m['\\'] = struct {
		code  uint16
		shift bool
	}{43, false}
	m['|'] = struct {
		code  uint16
		shift bool
	}{43, true}
	return m
}()

// ActionError é uma ação recusada antes de qualquer evento sair (nome, tecla, coordenada ou valor inválido). A API
// a responde como 400; os outros erros (visualizador fora do ar, tela sem quadro) são da máquina, não de quem pediu.
type ActionError struct{ Err error }

// Error devolve a mensagem da recusa.
func (e ActionError) Error() string { return e.Err.Error() }

// Unwrap expõe a causa.
func (e ActionError) Unwrap() error { return e.Err }

// Steps converte as ações nos pacotes de eventos para uma tela de width×height pixels. Ação inválida é erro antes
// de qualquer evento sair: um type pela metade ou um atalho sem a última tecla deixaria o guest num estado estranho.
func Steps(actions []Action, width, height int) ([]Step, error) {
	if len(actions) == 0 {
		return nil, ActionError{fmt.Errorf("no actions")}
	}
	var out []Step
	for i, a := range actions {
		s, err := steps(a, width, height)
		if err != nil {
			return nil, ActionError{fmt.Errorf("action %d (%s): %w", i, a.Action, err)}
		}
		out = append(out, s...)
	}
	return out, nil
}

// steps converte uma ação.
func steps(a Action, width, height int) ([]Step, error) {
	switch strings.ToLower(a.Action) {
	case "move":
		p, err := pointer(a.X, a.Y, width, height)
		if err != nil {
			return nil, err
		}
		return []Step{p}, nil
	case "click":
		return click(a, width, height)
	case "drag":
		return drag(a, width, height)
	case "type":
		return typeText(a.Text)
	case "key":
		return combo(a.Keys)
	case "scroll":
		return scroll(a, width, height)
	case "wait":
		if a.Ms < 0 || time.Duration(a.Ms)*time.Millisecond > maxWait {
			return nil, fmt.Errorf("ms must be between 0 and %d", maxWait.Milliseconds())
		}
		return []Step{{Pause: time.Duration(a.Ms) * time.Millisecond}}, nil
	default:
		return nil, fmt.Errorf("unknown action %q (click, move, drag, type, key, scroll, wait)", a.Action)
	}
}

// pointer leva o ponteiro ao pixel (x, y): eixos absolutos do tablet escalados ao tamanho da tela.
func pointer(x, y, width, height int) (Step, error) {
	if width <= 0 || height <= 0 {
		return Step{}, fmt.Errorf("screen size unknown (%dx%d)", width, height)
	}
	if x < 0 || y < 0 || x >= width || y >= height {
		return Step{}, fmt.Errorf("point (%d,%d) outside the %dx%d screen", x, y, width, height)
	}
	return packet(keyPause, ev(evAbs, absX, scale(x, width)), ev(evAbs, absY, scale(y, height))), nil
}

// scale leva o pixel p (0..size-1) ao eixo 0..absMax; o guest faz a conta inversa e cai no mesmo pixel.
func scale(p, size int) int32 {
	if size <= 1 {
		return 0
	}
	return int32(math.Round(float64(p) * absMax / float64(size-1)))
}

// click move até o ponto e aperta e solta o botão (duas vezes no double).
func click(a Action, width, height int) ([]Step, error) {
	btn, ok := buttons[strings.ToLower(a.Button)]
	if !ok {
		return nil, fmt.Errorf("unknown button %q (left, right, middle)", a.Button)
	}
	p, err := pointer(a.X, a.Y, width, height)
	if err != nil {
		return nil, err
	}
	p.Pause = travelPause
	out := []Step{p}
	for n := 0; n < 1+boolInt(a.Double); n++ {
		out = append(out, packet(keyPause, ev(evKey, btn, 1)), packet(keyPause, ev(evKey, btn, 0)))
	}
	return out, nil
}

// drag aperta o botão esquerdo no ponto de partida, passa por pontos intermediários e solta no destino: sem os
// intermediários, o app vê um salto e muitos não reconhecem o arrasto.
func drag(a Action, width, height int) ([]Step, error) {
	from, err := pointer(a.X, a.Y, width, height)
	if err != nil {
		return nil, err
	}
	if _, err := pointer(a.ToX, a.ToY, width, height); err != nil {
		return nil, err
	}
	from.Pause = travelPause
	out := []Step{from, packet(keyPause, ev(evKey, buttons["left"], 1))}
	const parts = 10
	for i := 1; i <= parts; i++ {
		x := a.X + (a.ToX-a.X)*i/parts
		y := a.Y + (a.ToY-a.Y)*i/parts
		p, _ := pointer(x, y, width, height)
		out = append(out, p)
	}
	return append(out, packet(keyPause, ev(evKey, buttons["left"], 0))), nil
}

// typeText digita o texto como teclado US: cada caractere vira apertar e soltar a tecla, com Shift quando precisa.
// Caractere fora do teclado (acento, emoji) é erro: o virtio-input só tem teclas, não texto.
func typeText(text string) ([]Step, error) {
	if text == "" {
		return nil, fmt.Errorf("text is empty")
	}
	var out []Step
	for _, ch := range text {
		k, ok := charKeys[ch]
		if !ok {
			return nil, fmt.Errorf("%q has no key on the US keyboard (type only covers ASCII; use exec with xdotool for other text)", ch)
		}
		if k.shift {
			out = append(out, packet(typePause, ev(evKey, keyShift, 1)))
		}
		out = append(out, packet(typePause, ev(evKey, k.code, 1)), packet(typePause, ev(evKey, k.code, 0)))
		if k.shift {
			out = append(out, packet(typePause, ev(evKey, keyShift, 0)))
		}
	}
	return out, nil
}

// combo aperta as teclas de um atalho em ordem ("ctrl+shift+t") e solta na ordem inversa.
func combo(keys string) ([]Step, error) {
	parts := strings.Split(keys, "+")
	codes := make([]uint16, 0, len(parts))
	for _, p := range parts {
		code, err := keyCode(strings.TrimSpace(p))
		if err != nil {
			return nil, err
		}
		codes = append(codes, code)
	}
	var out []Step
	for _, c := range codes {
		out = append(out, packet(keyPause, ev(evKey, c, 1)))
	}
	for i := len(codes) - 1; i >= 0; i-- {
		out = append(out, packet(keyPause, ev(evKey, codes[i], 0)))
	}
	return out, nil
}

// keyCode acha o keycode de um nome de tecla ("Return", "ctrl", "f5") ou de um caractere ("a", "1", "/"). Num
// atalho, a letra vale pela tecla: "ctrl+C" é o mesmo que "ctrl+c" (o Shift tem de ser pedido por nome).
func keyCode(name string) (uint16, error) {
	if name == "" {
		return 0, fmt.Errorf("empty key in combination")
	}
	if code, ok := namedKeys[strings.ToLower(name)]; ok {
		return code, nil
	}
	if r := []rune(name); len(r) == 1 {
		if k, ok := charKeys[r[0]]; ok {
			return k.code, nil
		}
	}
	return 0, fmt.Errorf("unknown key %q", name)
}

// scroll move até o ponto e gira a roda: up é positivo no evdev (REL_WHEEL), down negativo.
func scroll(a Action, width, height int) ([]Step, error) {
	p, err := pointer(a.X, a.Y, width, height)
	if err != nil {
		return nil, err
	}
	n := a.Amount
	if n == 0 {
		n = 3
	}
	if n < 0 || n > maxScroll {
		return nil, fmt.Errorf("amount must be between 1 and %d", maxScroll)
	}
	var v int32
	switch strings.ToLower(a.Direction) {
	case "", "down":
		v = -1
	case "up":
		v = 1
	default:
		return nil, fmt.Errorf("unknown direction %q (up, down)", a.Direction)
	}
	p.Pause = travelPause
	out := []Step{p}
	for i := 0; i < n; i++ {
		out = append(out, packet(keyPause, ev(evRel, relWheel, v)))
	}
	return out, nil
}

// packet fecha os eventos com o SYN_REPORT, que é o que faz o guest aplicá-los juntos.
func packet(pause time.Duration, evs ...viewer.Event) Step {
	return Step{Events: append(evs, ev(evSyn, synReport, 0)), Pause: pause}
}

// ev monta um evento.
func ev(typ, code uint16, value int32) viewer.Event {
	return viewer.Event{Type: typ, Code: code, Value: value}
}

// boolInt é 1 para true.
func boolInt(b bool) int {
	if b {
		return 1
	}
	return 0
}
