package viewer

import (
	"image/jpeg"
	"net/http"
	"net/http/httptest"
	"strings"
	"sync"
	"testing"

	"runner.codes/internal/adapters/screen"
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

// fakeResizer guarda os tamanhos pedidos.
type fakeResizer struct{ sizes [][2]int }

// Resize guarda o pedido.
func (f *fakeResizer) Resize(w, h int) { f.sizes = append(f.sizes, [2]int{w, h}) }

// TestViewerResize: o tamanho pedido pela página chega ao resizer; fora dos limites, corpo inválido,
// tela sem resizer e tela só para ver recusam, e nada chega ao guest nesses casos.
func TestViewerResize(t *testing.T) {
	scr, in, rs := screen.New(), &fakeInput{}, &fakeResizer{}
	h := Handler(scr, in, rs, 640, 480)
	if r := do(h, "POST", "/resize", `{"width":1512,"height":794}`); r.Code != 204 || len(rs.sizes) != 1 || rs.sizes[0] != [2]int{1512, 794} {
		t.Fatalf("resize: %d %v", r.Code, rs.sizes)
	}
	for body, want := range map[string]int{
		`{"width":319,"height":794}`:  400, // abaixo do mínimo
		`{"width":1512,"height":199}`: 400,
		`{"width":7681,"height":800}`: 400, // acima do máximo
		`{"width":800,"height":4321}`: 400,
		`{`:                           400,
	} {
		if r := do(h, "POST", "/resize", body); r.Code != want {
			t.Errorf("%s: %d, quero %d", body, r.Code, want)
		}
	}
	if r := do(h, "POST", "/resize", `{"width":320,"height":200}`); r.Code != 204 {
		t.Fatalf("mínimo exato: %d", r.Code)
	}
	if r := do(h, "POST", "/resize", `{"width":7680,"height":4320}`); r.Code != 204 {
		t.Fatalf("máximo exato: %d", r.Code)
	}
	if len(rs.sizes) != 3 {
		t.Fatalf("pedido recusado chegou ao resizer: %v", rs.sizes)
	}
	if r := do(Handler(scr, in, nil, 1, 1), "POST", "/resize", `{"width":800,"height":600}`); r.Code != 404 {
		t.Fatalf("sem resizer: %d", r.Code)
	}
	if r := do(Handler(scr, nil, rs, 1, 1), "POST", "/resize", `{"width":800,"height":600}`); r.Code != 404 || len(rs.sizes) != 3 {
		t.Fatalf("só para ver: %d %v", r.Code, rs.sizes)
	}
	if !strings.Contains(do(h, "GET", "/", "").Body.String(), `fetch("resize"`) {
		t.Fatal("a página não pede o tamanho da janela")
	}
}

// TestViewer: página, tamanho, quadro em JPEG com a versão no cabeçalho, entrada repassada,
// e as recusas (sem entrada = só ver; corpo inválido).
func TestViewer(t *testing.T) {
	scr := screen.New()
	in := &fakeInput{}
	h := Handler(scr, in, nil, 640, 480)
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
	if r := do(Handler(scr, nil, nil, 1, 1), "POST", "/input", `[]`); r.Code != 403 {
		t.Fatalf("view-only: %d", r.Code)
	}
}

// TestViewerTitle: a página leva o título na barra e na aba; vazio usa o padrão; texto com marcação sai
// escapado; e nenhum marcador {{title}} sobra na página.
func TestViewerTitle(t *testing.T) {
	for title, want := range map[string]string{
		"superbot.computer": "<b>superbot.computer</b>",
		"":                  "<b>" + DefaultTitle + "</b>",
		"<i>x</i>":          "<b>&lt;i&gt;x&lt;/i&gt;</b>",
	} {
		body := do(TitledHandler(title, screen.New(), nil, nil, 100, 100), "GET", "/", "").Body.String()
		if !strings.Contains(body, want) || strings.Contains(body, "{{title}}") {
			t.Fatalf("%q: barra errada ou marcador sobrando", title)
		}
	}
	if !strings.Contains(do(Handler(screen.New(), nil, nil, 100, 100), "GET", "/", "").Body.String(), "<title>"+DefaultTitle+"</title>") {
		t.Fatal("Handler sem título não usa o padrão na aba")
	}
}
