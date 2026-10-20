package viewer

import (
	"image/jpeg"
	"net/http"
	"net/http/httptest"
	"strings"
	"sync"
	"testing"

	"runner.code/internal/adapters/screen"
)

// fakeInput guarda os eventos recebidos.
type fakeInput struct {
	mu  sync.Mutex
	evs []Event
}

// Push guarda o lote.
func (f *fakeInput) Push(evs []Event) { f.mu.Lock(); f.evs = append(f.evs, evs...); f.mu.Unlock() }

// do faz uma requisição no handler e devolve a resposta gravada.
func do(h http.Handler, method, url, body string) *httptest.ResponseRecorder {
	rec := httptest.NewRecorder()
	h.ServeHTTP(rec, httptest.NewRequest(method, url, strings.NewReader(body)))
	return rec
}

// TestViewer: página, tamanho, quadro em JPEG com a versão no cabeçalho, entrada repassada,
// e as recusas (sem entrada = só ver; corpo inválido).
func TestViewer(t *testing.T) {
	scr := screen.New()
	in := &fakeInput{}
	h := Handler(scr, in, 640, 480)
	if r := do(h, "GET", "/", ""); r.Code != 200 || !strings.Contains(r.Body.String(), "<canvas") {
		t.Fatalf("page: %d", r.Code)
	}
	if r := do(h, "GET", "/size", ""); !strings.Contains(r.Body.String(), `"width":640`) {
		t.Fatalf("size: %s", r.Body)
	}
	scr.Update(2, 2, screen.FormatR8G8B8A8, make([]byte, 16))
	r := do(h, "GET", "/frame?after=0", "")
	if r.Code != 200 || r.Header().Get("X-Seq") != "1" || r.Header().Get("Content-Type") != "image/jpeg" {
		t.Fatalf("frame: %d %v", r.Code, r.Header())
	}
	if img, err := jpeg.Decode(r.Body); err != nil || img.Bounds().Dx() != 2 {
		t.Fatalf("jpeg: %v", err)
	}
	if r := do(h, "POST", "/input", `[{"type":1,"code":30,"value":1}]`); r.Code != 204 || len(in.evs) != 1 || in.evs[0].Code != 30 {
		t.Fatalf("input: %d %+v", r.Code, in.evs)
	}
	if r := do(h, "POST", "/input", `{`); r.Code != 400 {
		t.Fatalf("bad input: %d", r.Code)
	}
	if r := do(Handler(scr, nil, 1, 1), "POST", "/input", `[]`); r.Code != 403 {
		t.Fatalf("view-only: %d", r.Code)
	}
}
