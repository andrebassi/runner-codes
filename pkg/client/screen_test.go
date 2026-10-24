package client

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"image"
	"image/jpeg"
	"net/http"
	"testing"
)

// jpegOf devolve um JPEG de w×h.
func jpegOf(w, h int) string {
	var b bytes.Buffer
	jpeg.Encode(&b, image.NewRGBA(image.Rect(0, 0, w, h)), nil)
	return b.String()
}

// TestScreen: Screenshot lê a imagem e o tamanho; Click, Type e Key mandam a ação certa à rota de ações (nome
// escapado); erro da API vira *Error; corpo que não é imagem é erro.
func TestScreen(t *testing.T) {
	c, f := newFake(t, map[string]func(http.ResponseWriter){
		"GET /api/v1/machines/desk/screen":          reply(200, jpegOf(320, 200)),
		"POST /api/v1/machines/desk/screen/actions": reply(204, ""),
		"POST /api/v1/machines/a/b/screen/actions":  reply(400, `{"code":"CONFIG_ERROR","error":{"code":"CONFIG_ERROR","message":"action 0 (fly): unknown"}}`),
		"GET /api/v1/machines/broken/screen":        reply(200, "not an image"),
		"GET /api/v1/machines/stopped/screen":       reply(409, `{"code":"INVALID_STATE","error":{"code":"INVALID_STATE","message":"not running"}}`),
	})
	ctx := context.Background()
	shot, err := c.Screenshot(ctx, "desk")
	if err != nil || shot.Width != 320 || shot.Height != 200 || len(shot.JPEG) == 0 {
		t.Fatalf("%+v %v", shot, err)
	}
	if err := c.Click(ctx, "desk", 10, 20); err != nil {
		t.Fatal(err)
	}
	if err := c.Type(ctx, "desk", "ls"); err != nil {
		t.Fatal(err)
	}
	if err := c.Key(ctx, "desk", "Return"); err != nil {
		t.Fatal(err)
	}
	want := []ScreenAction{{Action: "click", X: 10, Y: 20}, {Action: "type", Text: "ls"}, {Action: "key", Keys: "Return"}}
	for i, w := range want {
		var body struct{ Actions []ScreenAction }
		cl := f.calls[1+i]
		if err := json.Unmarshal([]byte(cl.body), &body); err != nil || len(body.Actions) != 1 || body.Actions[0] != w || cl.ctype != "application/json" {
			t.Fatalf("ação %d: %+v %v", i, cl, err)
		}
	}
	var e *Error
	if err := c.ScreenActions(ctx, "a/b", ScreenAction{Action: "fly"}); !errors.As(err, &e) || e.Status != 400 || e.Code != "CONFIG_ERROR" {
		t.Fatalf("400: %v", err)
	}
	if f.calls[len(f.calls)-1].uri != "/api/v1/machines/a%2Fb/screen/actions" {
		t.Fatalf("nome escapado: %s", f.calls[len(f.calls)-1].uri)
	}
	if _, err := c.Screenshot(ctx, "broken"); err == nil {
		t.Fatal("corpo que não é imagem")
	}
	if _, err := c.Screenshot(ctx, "stopped"); !errors.As(err, &e) || e.Code != "INVALID_STATE" {
		t.Fatalf("409: %v", err)
	}
}
