package client

import (
	"bytes"
	"context"
	"fmt"
	"image"
	_ "image/jpeg" // a API entrega a tela em JPEG; o DecodeConfig precisa do decodificador registrado
	"net/http"
)

// ScreenAction é uma ação na tela de uma máquina com display. Coordenadas em pixels da tela atual (a do último
// Screenshot), origem no canto superior esquerdo.
type ScreenAction struct {
	// Action: click, move, drag, type, key, scroll ou wait.
	Action string `json:"action"`
	X      int    `json:"x,omitempty"`
	Y      int    `json:"y,omitempty"`
	ToX    int    `json:"to_x,omitempty"` // destino do drag
	ToY    int    `json:"to_y,omitempty"`
	Button string `json:"button,omitempty"` // click: left (padrão), right, middle
	Double bool   `json:"double,omitempty"` // click duplo
	Text   string `json:"text,omitempty"`   // type: teclado US, só ASCII
	Keys   string `json:"keys,omitempty"`   // key: "Return", "ctrl+c", "alt+F4"
	// Direction do scroll (up ou down, o padrão) e Amount, os passos da roda (padrão 3).
	Direction string `json:"direction,omitempty"`
	Amount    int    `json:"amount,omitempty"`
	Ms        int    `json:"ms,omitempty"` // wait, em milissegundos
}

// Screenshot é a tela de uma máquina: o quadro em JPEG e o tamanho em pixels.
type Screenshot struct {
	JPEG   []byte
	Width  int
	Height int
}

// Screenshot captura a tela da máquina (GET /api/v1/machines/:name/screen).
func (c *Client) Screenshot(ctx context.Context, name string) (Screenshot, error) {
	var b []byte
	if err := c.do(ctx, http.MethodGet, machinePath(name, "/screen"), nil, "", &b); err != nil {
		return Screenshot{}, err
	}
	cfg, _, err := image.DecodeConfig(bytes.NewReader(b))
	if err != nil {
		return Screenshot{}, fmt.Errorf("screen is not an image: %w", err)
	}
	return Screenshot{JPEG: b, Width: cfg.Width, Height: cfg.Height}, nil
}

// ScreenActions executa as ações na tela da máquina, em ordem (POST /api/v1/machines/:name/screen/actions). Ação
// inválida é recusada inteira (*Error com status 400) antes de qualquer evento chegar à máquina.
func (c *Client) ScreenActions(ctx context.Context, name string, actions ...ScreenAction) error {
	return c.doJSON(ctx, http.MethodPost, machinePath(name, "/screen/actions"), map[string]any{"actions": actions}, nil)
}

// Click clica com o botão esquerdo em (x, y).
func (c *Client) Click(ctx context.Context, name string, x, y int) error {
	return c.ScreenActions(ctx, name, ScreenAction{Action: "click", X: x, Y: y})
}

// Type digita o texto na janela em foco (teclado US, só ASCII).
func (c *Client) Type(ctx context.Context, name, text string) error {
	return c.ScreenActions(ctx, name, ScreenAction{Action: "type", Text: text})
}

// Key aperta uma tecla ou um atalho ("Return", "ctrl+c").
func (c *Client) Key(ctx context.Context, name, keys string) error {
	return c.ScreenActions(ctx, name, ScreenAction{Action: "key", Keys: keys})
}
