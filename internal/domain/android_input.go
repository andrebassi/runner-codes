package domain

import (
	"errors"
	"fmt"
	"regexp"
	"strings"
)

// AndroidInput é uma ação na tela do Android: o que a rota de entrada recebe e o que o operador de IA pede. Vira um
// comando do `input` do Android (pelo ADB), validado aqui para que nada fora do formato chegue ao shell do convidado.
type AndroidInput struct {
	// Kind é tap, swipe, text ou key.
	Kind string `json:"kind"`
	// X e Y são o ponto do toque, ou o começo do arrasto, em pixels da tela do Android.
	X int `json:"x,omitempty"`
	Y int `json:"y,omitempty"`
	// X2 e Y2 são o fim do arrasto; DurationMs é quanto ele dura (0 usa 300 ms, um arrasto de dedo).
	X2         int `json:"x2,omitempty"`
	Y2         int `json:"y2,omitempty"`
	DurationMs int `json:"duration_ms,omitempty"`
	// Text é o que digitar (só ASCII imprimível: o `input text` não digita acento — limite do próprio Android).
	Text string `json:"text,omitempty"`
	// Key é uma tecla: o nome do KeyEvent sem o prefixo (HOME, BACK, ENTER, DEL, APP_SWITCH...) ou o código numérico.
	Key string `json:"key,omitempty"`
}

// Limites das ações: um arrasto de até 10 s cobre o "segurar e arrastar"; 1000 caracteres por vez, acima disso o
// `input text` do Android fica lento demais (ele digita tecla a tecla).
const (
	androidMaxSwipeMs = 10000
	androidMaxText    = 1000
)

// androidKeyRe casa um nome de KeyEvent (letras maiúsculas, dígitos e _) ou um código numérico.
var androidKeyRe = regexp.MustCompile(`^([A-Z][A-Z0-9_]{0,40}|[0-9]{1,3})$`)

// Command monta o comando do shell do Android para a ação, já com as aspas; a tela (w x h) limita os pontos.
func (in AndroidInput) Command(screen Android) (string, error) {
	inside := func(x, y int) error {
		if x < 0 || y < 0 || x >= int(screen.Width) || y >= int(screen.Height) {
			return fmt.Errorf("point %d,%d is outside the %dx%d screen", x, y, screen.Width, screen.Height)
		}
		return nil
	}
	switch in.Kind {
	case "tap":
		if err := inside(in.X, in.Y); err != nil {
			return "", err
		}
		return fmt.Sprintf("input tap %d %d", in.X, in.Y), nil
	case "swipe":
		if err := inside(in.X, in.Y); err != nil {
			return "", err
		}
		if err := inside(in.X2, in.Y2); err != nil {
			return "", err
		}
		d := in.DurationMs
		if d == 0 {
			d = 300
		}
		if d < 0 || d > androidMaxSwipeMs {
			return "", fmt.Errorf("swipe duration %d ms must be between 1 and %d", d, androidMaxSwipeMs)
		}
		return fmt.Sprintf("input swipe %d %d %d %d %d", in.X, in.Y, in.X2, in.Y2, d), nil
	case "text":
		if in.Text == "" || len(in.Text) > androidMaxText {
			return "", fmt.Errorf("text must have 1 to %d characters", androidMaxText)
		}
		for _, r := range in.Text {
			if r < 0x20 || r > 0x7e {
				return "", fmt.Errorf("text %q: only printable ASCII is typed by android's input (no accents)", in.Text)
			}
		}
		// O `input text` troca %s por espaço; o espaço literal partiria o argumento no shell do Android.
		return "input text " + ShellQuote(strings.ReplaceAll(in.Text, " ", "%s")), nil
	case "key":
		if !androidKeyRe.MatchString(in.Key) {
			return "", fmt.Errorf("key %q must be a KeyEvent name (HOME, BACK, ENTER...) or a number", in.Key)
		}
		k := in.Key
		if k[0] < '0' || k[0] > '9' {
			k = "KEYCODE_" + strings.TrimPrefix(k, "KEYCODE_")
		}
		return "input keyevent " + k, nil
	}
	return "", errors.New(`kind must be tap, swipe, text or key`)
}

// ShellQuote põe s entre aspas simples para o sh (o do Android é o mksh, que segue a mesma regra): cada aspa simples
// de dentro fecha a citação, entra escapada com barra e reabre — nada que venha de fora é interpretado pelo shell do
// convidado.
func ShellQuote(s string) string {
	return "'" + strings.ReplaceAll(s, "'", `'\''`) + "'"
}
