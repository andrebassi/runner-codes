package client

import (
	"context"
	"errors"
	"io"
	"net/http"
	"strings"
	"testing"
)

// TestTerminal: abrir manda o tamanho e devolve o id; a SSE chega inteira com o token; teclado vai cru, resize em
// JSON e o DELETE fecha; id e nome saem escapados; erro da API (máquina parada) vira *Error, inclusive na SSE.
func TestTerminal(t *testing.T) {
	c, f := newFake(t, map[string]func(http.ResponseWriter){
		"POST /api/v1/machines/desk/terminal":            reply(201, `{"id":"abc"}`),
		"GET /api/v1/machines/desk/terminal/abc/stream":  reply(200, "event: replay\ndata: JCA=\n\n"),
		"POST /api/v1/machines/desk/terminal/abc/input":  reply(204, ""),
		"POST /api/v1/machines/desk/terminal/abc/resize": reply(204, ""),
		"DELETE /api/v1/machines/desk/terminal/abc":      reply(204, ""),
		"POST /api/v1/machines/off/terminal":             reply(409, `{"error":{"code":"INVALID_STATE","message":"not running"}}`),
		"GET /api/v1/machines/desk/terminal/gone/stream": reply(404, `{"error":"no such terminal"}`),
	})
	c.Token = "tok"
	ctx := context.Background()
	id, err := c.OpenTerminal(ctx, "desk", TerminalOptions{Rows: 40, Cols: 100})
	if err != nil || id != "abc" || !strings.Contains(f.calls[0].body, `"rows":40`) || strings.Contains(f.calls[0].body, "user") {
		t.Fatalf("open: %q %v %+v", id, err, f.calls[0])
	}
	body, err := c.TerminalStream(ctx, "desk", "abc")
	if err != nil {
		t.Fatal(err)
	}
	raw, _ := io.ReadAll(body)
	body.Close()
	if !strings.Contains(string(raw), "event: replay") {
		t.Fatalf("stream: %q", raw)
	}
	if err := c.TerminalInput(ctx, "desk", "abc", strings.NewReader("ls\r")); err != nil {
		t.Fatal(err)
	}
	if cl := f.calls[2]; cl.body != "ls\r" || cl.ctype != "application/octet-stream" {
		t.Fatalf("input: %+v", cl)
	}
	if err := c.TerminalResize(ctx, "desk", "abc", 50, 160); err != nil || f.calls[3].body != `{"cols":160,"rows":50}` {
		t.Fatalf("resize: %v %+v", err, f.calls[3])
	}
	if err := c.CloseTerminal(ctx, "desk", "abc"); err != nil {
		t.Fatal(err)
	}
	var apiErr *Error
	if _, err := c.OpenTerminal(ctx, "off", TerminalOptions{}); !errors.As(err, &apiErr) || apiErr.Code != "INVALID_STATE" {
		t.Fatalf("parada: %v", err)
	}
	if _, err := c.TerminalStream(ctx, "desk", "gone"); !errors.As(err, &apiErr) || apiErr.Status != 404 {
		t.Fatalf("sessão sumida: %v", err)
	}
}
