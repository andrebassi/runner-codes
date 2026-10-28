package client

import (
	"bytes"
	"context"
	"errors"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"
)

// TestExportImport: Export copia o corpo em streaming; Import manda o corpo com o tipo próprio e
// decodifica a máquina; recusa da API vira *Error com o código.
func TestExportImport(t *testing.T) {
	c, f := newFake(t, map[string]func(http.ResponseWriter){
		"GET /api/v1/machines/dev/export": reply(200, "RCMX-bytes"),
		"GET /api/v1/machines/run/export": reply(409, `{"error":{"code":"INVALID_STATE","message":"must be stopped"}}`),
		"POST /api/v1/machines/import":    reply(201, machineJSON),
	})
	ctx := context.Background()
	var buf bytes.Buffer
	if err := c.Export(ctx, "dev", &buf); err != nil || buf.String() != "RCMX-bytes" {
		t.Fatalf("export: %v %q", err, buf.String())
	}
	var e *Error
	if err := c.Export(ctx, "run", &buf); !errors.As(err, &e) || e.Code != "INVALID_STATE" {
		t.Fatalf("export ligada: %v", err)
	}
	m, err := c.Import(ctx, strings.NewReader("corpo"))
	if err != nil || m.Name != "dev" {
		t.Fatalf("import: %+v %v", m, err)
	}
	last := f.calls[len(f.calls)-1]
	if last.body != "corpo" || last.ctype != "application/x-runner-machine" {
		t.Fatalf("pedido do import: %+v", last)
	}
	if err := New("http://127.0.0.1:1").Export(ctx, "x", &buf); err == nil {
		t.Fatal("servidor fora sem erro")
	}
	if err := New("http://[::1").Export(ctx, "x", &buf); err == nil {
		t.Fatal("URL ruim sem erro")
	}
}

// TestToken: com Token, toda chamada leva o Bearer; sem ele, nenhum cabeçalho de autorização.
func TestToken(t *testing.T) {
	var got []string
	s := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		got = append(got, r.Header.Get("Authorization"))
		w.Write([]byte("[]"))
	}))
	defer s.Close()
	c := New(s.URL)
	c.Token = "segredo"
	ctx := context.Background()
	_, _ = c.Machines(ctx)
	_ = c.Export(ctx, "dev", &bytes.Buffer{})
	_, _ = New(s.URL).Machines(ctx)
	if len(got) != 3 || got[0] != "Bearer segredo" || got[1] != "Bearer segredo" || got[2] != "" {
		t.Fatalf("cabeçalhos: %q", got)
	}
}
