package client

import (
	"context"
	"errors"
	"net/http"
	"strings"
	"testing"
)

// TestAndroid: a tela volta em bytes (o PNG inteiro), a ação vai em JSON com os campos que vieram e os zeros fora; o
// erro do rc (máquina parada, ação recusada) vira *Error com o código.
func TestAndroid(t *testing.T) {
	c, f := newFake(t, map[string]func(http.ResponseWriter){
		"GET /api/v1/machines/phone/android/screenshot": reply(200, "\x89PNG-tela"),
		"POST /api/v1/machines/phone/android/input":     reply(204, ""),
		"GET /api/v1/machines/off/android/screenshot":   reply(409, `{"error":{"code":"INVALID_STATE","message":"not running"}}`),
		"POST /api/v1/machines/off/android/input":       reply(400, `{"error":{"code":"CONFIG","message":"text must be ASCII"}}`),
		"POST /api/v1/machines/phone/android/shell":     reply(200, `{"output":"uid=0(root)\n"}`),
		"POST /api/v1/machines/off/android/shell":       reply(409, `{"error":{"code":"INVALID_STATE","message":"not running"}}`),
	})
	ctx := context.Background()
	png, err := c.AndroidScreenshot(ctx, "phone")
	if err != nil || string(png) != "\x89PNG-tela" {
		t.Fatalf("tela: %q %v", png, err)
	}
	if err := c.AndroidInputAction(ctx, "phone", AndroidInput{Kind: "tap", X: 10, Y: 20}); err != nil {
		t.Fatal(err)
	}
	if b := f.calls[1].body; b != `{"kind":"tap","x":10,"y":20}` {
		t.Fatalf("corpo: %s", b)
	}
	var e *Error
	if _, err := c.AndroidScreenshot(ctx, "off"); !errors.As(err, &e) || !strings.Contains(err.Error(), "not running") {
		t.Fatalf("parada: %v", err)
	}
	if err := c.AndroidInputAction(ctx, "off", AndroidInput{Kind: "text", Text: "é"}); !errors.As(err, &e) {
		t.Fatalf("recusada: %v", err)
	}
	out, err := c.AndroidShell(ctx, "phone", "id")
	if err != nil || out != "uid=0(root)\n" || f.calls[len(f.calls)-1].body != `{"command":"id"}` {
		t.Fatalf("shell: %q %v %s", out, err, f.calls[len(f.calls)-1].body)
	}
	if _, err := c.AndroidShell(ctx, "off", "id"); !errors.As(err, &e) {
		t.Fatalf("shell parada: %v", err)
	}
}
