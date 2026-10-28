package viewer

import (
	"strings"
	"testing"
)

// TestPageClickInOneBatch trava o clique inteiro num lote só: o aperto espera o soltar (PRESS_HOLD_MS) e os dois vão
// no mesmo send. Sem isso, num worker remoto o soltar chegava uma ida e volta depois do aperto, e o XFCE abria o menu
// de contexto em todo clique esquerdo (medido em 28/10/2026). Também trava a barra sem o "ao vivo" repetido.
func TestPageClickInOneBatch(t *testing.T) {
	p := string(page)
	for _, want := range []string{"PRESS_HOLD_MS", "function flushPress()", "send([...evs, { type: EV_SYN, code: SYN_REPORT, value: 0 }, ...up])"} {
		if !strings.Contains(p, want) {
			t.Errorf("a página da tela não tem %q", want)
		}
	}
	if strings.Contains(p, `"ao vivo (`) {
		t.Error("a barra da tela voltou a escrever \"ao vivo\"")
	}
}
