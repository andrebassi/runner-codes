package domain

import (
	"strings"
	"testing"
)

// TestAndroidInputCommand trava cada ação no comando exato do `input` do Android.
func TestAndroidInputCommand(t *testing.T) {
	screen := DefaultAndroid
	ok := []struct {
		in   AndroidInput
		want string
	}{
		{AndroidInput{Kind: "tap", X: 360, Y: 640}, "input tap 360 640"},
		{AndroidInput{Kind: "tap", X: 0, Y: 0}, "input tap 0 0"},
		{AndroidInput{Kind: "swipe", X: 360, Y: 1000, X2: 360, Y2: 200}, "input swipe 360 1000 360 200 300"},
		{AndroidInput{Kind: "swipe", X: 1, Y: 2, X2: 3, Y2: 4, DurationMs: 900}, "input swipe 1 2 3 4 900"},
		{AndroidInput{Kind: "text", Text: "ola mundo"}, "input text 'ola%smundo'"},
		{AndroidInput{Kind: "text", Text: "it's"}, `input text 'it'\''s'`},
		{AndroidInput{Kind: "key", Key: "HOME"}, "input keyevent KEYCODE_HOME"},
		{AndroidInput{Kind: "key", Key: "KEYCODE_BACK"}, "input keyevent KEYCODE_BACK"},
		{AndroidInput{Kind: "key", Key: "66"}, "input keyevent 66"},
	}
	for _, c := range ok {
		got, err := c.in.Command(screen)
		if err != nil || got != c.want {
			t.Errorf("%+v: %q, %v; want %q", c.in, got, err, c.want)
		}
	}
}

// TestAndroidInputRejects confere que nada fora do formato vira comando: ponto fora da tela (nas duas pontas do
// arrasto e nas duas bordas), duração fora do limite, texto vazio, longo, com acento ou com controle, tecla que
// poderia carregar shell, e tipo desconhecido.
func TestAndroidInputRejects(t *testing.T) {
	screen := DefaultAndroid
	bad := map[string]AndroidInput{
		"outside the 720x1280": {Kind: "tap", X: 720, Y: 10},
		"point -1,5":           {Kind: "tap", X: -1, Y: 5},
		"point 5,1280":         {Kind: "swipe", X: 5, Y: 1280, X2: 1, Y2: 1},
		"point 9,9999":         {Kind: "swipe", X: 1, Y: 1, X2: 9, Y2: 9999},
		"duration -5":          {Kind: "swipe", X: 1, Y: 1, X2: 2, Y2: 2, DurationMs: -5},
		"duration 10001":       {Kind: "swipe", X: 1, Y: 1, X2: 2, Y2: 2, DurationMs: 10001},
		"1 to 1000":            {Kind: "text"},
		"no accents":           {Kind: "text", Text: "ação"},
		"printable ASCII":      {Kind: "text", Text: "a\nb"},
		"KeyEvent name":        {Kind: "key", Key: "HOME; reboot"},
		"must be tap":          {Kind: "pinch"},
	}
	for want, in := range bad {
		if _, err := in.Command(screen); err == nil || !strings.Contains(err.Error(), want) {
			t.Errorf("%+v: err = %v; want contendo %q", in, err, want)
		}
	}
	if _, err := (AndroidInput{Kind: "text", Text: strings.Repeat("a", 1001)}).Command(screen); err == nil {
		t.Error("texto de 1001 caracteres aceito")
	}
}

// TestShellQuote: aspas simples por fora, a de dentro escapada — nem $() nem ; escapam para o shell.
func TestShellQuote(t *testing.T) {
	cases := map[string]string{"": "''", "a b": "'a b'", "$(reboot); x": "'$(reboot); x'", "it's": `'it'\''s'`}
	for in, want := range cases {
		if got := ShellQuote(in); got != want {
			t.Errorf("ShellQuote(%q) = %q; want %q", in, got, want)
		}
	}
}
