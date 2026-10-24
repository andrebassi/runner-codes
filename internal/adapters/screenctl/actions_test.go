package screenctl

import (
	"errors"
	"strings"
	"testing"
	"time"

	"runner.codes/internal/adapters/viewer"
)

// flat junta os eventos de todos os passos, para conferir a sequência inteira.
func flat(steps []Step) []viewer.Event {
	var out []viewer.Event
	for _, s := range steps {
		out = append(out, s.Events...)
	}
	return out
}

// keysOf devolve, na ordem, as teclas apertadas (+code) e soltas (-code), ignorando ponteiro e SYN.
func keysOf(steps []Step) []int {
	var out []int
	for _, e := range flat(steps) {
		if e.Type == evKey {
			if e.Value == 1 {
				out = append(out, int(e.Code))
			} else {
				out = append(out, -int(e.Code))
			}
		}
	}
	return out
}

// equal compara duas listas de inteiros.
func equal(a, b []int) bool {
	if len(a) != len(b) {
		return false
	}
	for i := range a {
		if a[i] != b[i] {
			return false
		}
	}
	return true
}

// TestEveryPacketEndsWithSyn: todo passo com eventos fecha com SYN_REPORT (sem ele o guest não aplica o pacote),
// e o SYN aparece só no fim.
func TestEveryPacketEndsWithSyn(t *testing.T) {
	steps, err := Steps([]Action{
		{Action: "click", X: 5, Y: 5}, {Action: "type", Text: "Ab!"}, {Action: "key", Keys: "ctrl+c"},
		{Action: "scroll", X: 1, Y: 1}, {Action: "drag", X: 1, Y: 1, ToX: 9, ToY: 9}, {Action: "move", X: 2, Y: 2},
	}, 10, 10)
	if err != nil {
		t.Fatal(err)
	}
	for i, s := range steps {
		if len(s.Events) == 0 {
			t.Fatalf("passo %d sem eventos", i)
		}
		last := s.Events[len(s.Events)-1]
		if last.Type != evSyn || last.Code != synReport {
			t.Fatalf("passo %d não fecha com SYN: %+v", i, s.Events)
		}
		for _, e := range s.Events[:len(s.Events)-1] {
			if e.Type == evSyn {
				t.Fatalf("passo %d tem SYN no meio: %+v", i, s.Events)
			}
		}
	}
}

// TestPointerScale: os cantos da tela caem nos extremos do eixo (0 e 32767) e o meio no meio, e o guest faz a conta
// inversa no mesmo pixel; ponto fora da tela e tela de tamanho desconhecido são recusados.
func TestPointerScale(t *testing.T) {
	for _, c := range []struct{ x, y, w, h, wantX, wantY int }{
		{0, 0, 1280, 800, 0, 0}, {1279, 799, 1280, 800, absMax, absMax}, {640, 400, 1281, 801, 16384, 16384},
	} {
		s, err := pointer(c.x, c.y, c.w, c.h)
		if err != nil {
			t.Fatal(err)
		}
		if int(s.Events[0].Value) != c.wantX || int(s.Events[1].Value) != c.wantY || s.Events[0].Code != absX || s.Events[1].Code != absY {
			t.Fatalf("%+v: %+v", c, s.Events)
		}
	}
	for _, c := range []struct{ x, y, w, h int }{{-1, 0, 10, 10}, {0, -1, 10, 10}, {10, 0, 10, 10}, {0, 10, 10, 10}, {0, 0, 0, 10}, {0, 0, 10, 0}} {
		if _, err := pointer(c.x, c.y, c.w, c.h); err == nil {
			t.Fatalf("%+v devia falhar", c)
		}
	}
	if scale(0, 1) != 0 {
		t.Fatal("tela de 1 pixel")
	}
}

// TestClick: botão certo, apertar antes de soltar, duas vezes no double; botão desconhecido e ponto fora recusam.
func TestClick(t *testing.T) {
	for btn, code := range map[string]int{"": 0x110, "left": 0x110, "RIGHT": 0x111, "middle": 0x112} {
		steps, err := Steps([]Action{{Action: "click", X: 1, Y: 1, Button: btn}}, 10, 10)
		if err != nil || !equal(keysOf(steps), []int{code, -code}) {
			t.Fatalf("%q: %v %v", btn, keysOf(steps), err)
		}
	}
	steps, _ := Steps([]Action{{Action: "click", X: 1, Y: 1, Double: true}}, 10, 10)
	if !equal(keysOf(steps), []int{0x110, -0x110, 0x110, -0x110}) {
		t.Fatalf("double: %v", keysOf(steps))
	}
	if steps[0].Events[0].Type != evAbs || steps[0].Pause != travelPause {
		t.Fatalf("o click leva o ponteiro e espera a seta chegar antes de apertar: %+v", steps[0])
	}
	for _, a := range []Action{{Action: "click", Button: "side"}, {Action: "click", X: 99}} {
		if _, err := Steps([]Action{a}, 10, 10); err == nil {
			t.Fatalf("%+v devia falhar", a)
		}
	}
}

// TestType: maiúscula e símbolo de cima apertam Shift em volta da tecla; espaço, Enter e Tab têm tecla; acento e
// texto vazio são recusados antes de digitar.
func TestType(t *testing.T) {
	steps, err := Steps([]Action{{Action: "type", Text: "aA1!\n\t |\\"}}, 0, 0)
	if err != nil {
		t.Fatal(err)
	}
	want := []int{30, -30, 42, 30, -30, -42, 2, -2, 42, 2, -2, -42, 28, -28, 15, -15, 57, -57, 42, 43, -43, -42, 43, -43}
	if !equal(keysOf(steps), want) {
		t.Fatalf("got %v\nwant %v", keysOf(steps), want)
	}
	if steps[0].Pause != typePause {
		t.Fatal("type sem pausa entre teclas")
	}
	for _, text := range []string{"", "ação", "🙂"} {
		if _, err := Steps([]Action{{Action: "type", Text: text}}, 0, 0); err == nil {
			t.Fatalf("%q devia falhar", text)
		}
	}
}

// TestCharKeysCoverPrintableASCII: todo ASCII imprimível tem tecla (genérico: pega o caractere esquecido no mapa).
func TestCharKeysCoverPrintableASCII(t *testing.T) {
	for r := rune(32); r < 127; r++ {
		if _, ok := charKeys[r]; !ok {
			t.Errorf("%q sem tecla", r)
		}
	}
}

// TestCombo: aperta na ordem e solta na inversa; nomes em qualquer caixa e letra pela tecla; vazio e desconhecido
// recusam.
func TestCombo(t *testing.T) {
	for keys, want := range map[string][]int{
		"ctrl+c":         {29, 46, -46, -29},
		"Ctrl+Shift+T":   {29, 42, 20, -20, -42, -29},
		"Return":         {28, -28},
		"alt+F4":         {56, 62, -62, -56},
		"super":          {125, -125},
		"ctrl + alt + t": {29, 56, 20, -20, -56, -29},
		"/":              {53, -53},
	} {
		steps, err := Steps([]Action{{Action: "key", Keys: keys}}, 0, 0)
		if err != nil || !equal(keysOf(steps), want) {
			t.Fatalf("%q: %v %v", keys, keysOf(steps), err)
		}
	}
	for _, keys := range []string{"", "ctrl+", "ctrl+hyper", "á"} {
		if _, err := Steps([]Action{{Action: "key", Keys: keys}}, 0, 0); err == nil {
			t.Fatalf("%q devia falhar", keys)
		}
	}
}

// TestScroll: down é negativo e é o padrão, up positivo, 3 passos sem amount; direção e quantidade inválidas
// recusam.
func TestScroll(t *testing.T) {
	wheel := func(steps []Step) (n int, v int32) {
		for _, e := range flat(steps) {
			if e.Type == evRel && e.Code == relWheel {
				n, v = n+1, e.Value
			}
		}
		return
	}
	for _, c := range []struct {
		a     Action
		n     int
		value int32
	}{
		{Action{Action: "scroll", X: 1, Y: 1}, 3, -1},
		{Action{Action: "scroll", X: 1, Y: 1, Direction: "UP", Amount: 5}, 5, 1},
		{Action{Action: "scroll", X: 1, Y: 1, Direction: "down", Amount: 1}, 1, -1},
	} {
		steps, err := Steps([]Action{c.a}, 10, 10)
		n, v := wheel(steps)
		if err != nil || n != c.n || v != c.value {
			t.Fatalf("%+v: %d %d %v", c.a, n, v, err)
		}
	}
	for _, a := range []Action{
		{Action: "scroll", X: 1, Y: 1, Direction: "left"}, {Action: "scroll", X: 1, Y: 1, Amount: -1},
		{Action: "scroll", X: 1, Y: 1, Amount: maxScroll + 1}, {Action: "scroll", X: 50},
	} {
		if _, err := Steps([]Action{a}, 10, 10); err == nil {
			t.Fatalf("%+v devia falhar", a)
		}
	}
}

// TestDrag: aperta no início, passa por pontos intermediários e solta no destino; destino ou origem fora recusam.
func TestDrag(t *testing.T) {
	steps, err := Steps([]Action{{Action: "drag", X: 0, Y: 0, ToX: 99, ToY: 99}}, 100, 100)
	if err != nil {
		t.Fatal(err)
	}
	if !equal(keysOf(steps), []int{0x110, -0x110}) {
		t.Fatalf("botão: %v", keysOf(steps))
	}
	moves := 0
	var lastX int32
	for _, e := range flat(steps) {
		if e.Type == evAbs && e.Code == absX {
			moves, lastX = moves+1, e.Value
		}
	}
	if moves < 5 || lastX != absMax {
		t.Fatalf("intermediários %d, último x %d", moves, lastX)
	}
	for _, a := range []Action{{Action: "drag", X: 0, Y: 0, ToX: 100}, {Action: "drag", X: -1, ToX: 5}} {
		if _, err := Steps([]Action{a}, 100, 100); err == nil {
			t.Fatalf("%+v devia falhar", a)
		}
	}
}

// TestWaitAndUnknown: wait vira só pausa, com teto; ação desconhecida diz o índice e as válidas; uma ação ruim no
// meio recusa a lista inteira (nada sai pela metade).
func TestWaitAndUnknown(t *testing.T) {
	steps, err := Steps([]Action{{Action: "wait", Ms: 250}}, 0, 0)
	if err != nil || len(steps) != 1 || steps[0].Pause != 250*time.Millisecond || len(steps[0].Events) != 0 {
		t.Fatalf("wait: %+v %v", steps, err)
	}
	for _, ms := range []int{-1, int(maxWait.Milliseconds()) + 1} {
		if _, err := Steps([]Action{{Action: "wait", Ms: ms}}, 0, 0); err == nil {
			t.Fatalf("wait %d devia falhar", ms)
		}
	}
	_, err = Steps([]Action{{Action: "type", Text: "ok"}, {Action: "jump"}}, 0, 0)
	var ae ActionError
	if !errors.As(err, &ae) || !strings.Contains(err.Error(), "action 1 (jump)") || !strings.Contains(err.Error(), "click, move") || ae.Unwrap() == nil {
		t.Fatalf("desconhecida: %v", err)
	}
	if _, err := Steps(nil, 0, 0); !errors.As(err, &ae) {
		t.Fatalf("lista vazia: %v", err)
	}
}
