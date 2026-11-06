package client

import (
	"context"
	"net/http"
)

// AndroidInput é uma ação na tela de uma máquina Android (POST .../android/input): tap no ponto, swipe de um ponto a
// outro, text (só ASCII imprimível) ou key (nome do KeyEvent sem o prefixo, ou o código). Coordenadas em pixels do
// aparelho.
type AndroidInput struct {
	Kind       string `json:"kind"`
	X          int    `json:"x,omitempty"`
	Y          int    `json:"y,omitempty"`
	X2         int    `json:"x2,omitempty"`
	Y2         int    `json:"y2,omitempty"`
	DurationMs int    `json:"duration_ms,omitempty"`
	Text       string `json:"text,omitempty"`
	Key        string `json:"key,omitempty"`
}

// AndroidScreenshot devolve a tela do Android em PNG (GET .../android/screenshot).
func (c *Client) AndroidScreenshot(ctx context.Context, name string) ([]byte, error) {
	var b []byte
	err := c.do(ctx, http.MethodGet, machinePath(name, "/android/screenshot"), nil, "", &b)
	return b, err
}

// AndroidInputAction manda uma ação à tela do Android; o rc confere os limites e responde 400 com o motivo.
func (c *Client) AndroidInputAction(ctx context.Context, name string, in AndroidInput) error {
	return c.doJSON(ctx, http.MethodPost, machinePath(name, "/android/input"), in, nil)
}

// AndroidShell roda uma linha no shell do Android (POST .../android/shell, `adb shell` sem PTY) e devolve a saída,
// stdout e stderr juntos. Cada chamada é um shell novo: diretório e variáveis não passam de uma para a outra.
func (c *Client) AndroidShell(ctx context.Context, name, command string) (string, error) {
	var out struct {
		Output string `json:"output"`
	}
	err := c.doJSON(ctx, http.MethodPost, machinePath(name, "/android/shell"), map[string]string{"command": command}, &out)
	return out.Output, err
}
