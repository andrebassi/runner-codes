package screenctl

import (
	"bytes"
	"context"
	"encoding/json"
	"image"
	"image/jpeg"
	"net/http"
	"net/http/httptest"
	"strings"
	"sync"
	"testing"
	"time"

	"runner.codes/internal/adapters/viewer"
)

// fakeViewer imita o visualizador: GET /frame devolve um JPEG do tamanho dado (ou o status forçado) e POST /input
// guarda os pacotes recebidos.
type fakeViewer struct {
	mu          sync.Mutex
	w, h        int
	frameStatus int
	inputStatus int
	body        []byte // corpo do /frame quando não é para gerar imagem
	packets     [][]viewer.Event
	frames      int
	agent       []string // cabeçalho de agente de cada POST /input
}

// ServeHTTP atende as duas rotas.
func (f *fakeViewer) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	f.mu.Lock()
	defer f.mu.Unlock()
	switch {
	case r.Method == "GET" && r.URL.Path == "/frame":
		f.frames++
		if r.URL.Query().Get("after") != "0" {
			w.WriteHeader(400)
			return
		}
		if f.frameStatus != 0 {
			w.WriteHeader(f.frameStatus)
			return
		}
		if f.body != nil {
			w.Write(f.body)
			return
		}
		var buf bytes.Buffer
		jpeg.Encode(&buf, image.NewRGBA(image.Rect(0, 0, f.w, f.h)), nil)
		w.Header().Set("Content-Type", "image/jpeg")
		w.Write(buf.Bytes())
	case r.Method == "POST" && r.URL.Path == "/input":
		f.agent = append(f.agent, r.Header.Get(viewer.AgentHeader))
		var evs []viewer.Event
		json.NewDecoder(r.Body).Decode(&evs)
		f.packets = append(f.packets, evs)
		if f.inputStatus != 0 {
			w.WriteHeader(f.inputStatus)
			return
		}
		w.WriteHeader(http.StatusNoContent)
	default:
		w.WriteHeader(404)
	}
}

// start sobe o visualizador falso e devolve a Screen apontada para ele, sem as pausas de verdade.
func start(t *testing.T, f *fakeViewer) Screen {
	srv := httptest.NewServer(f)
	t.Cleanup(srv.Close)
	var slept time.Duration
	return Screen{Base: srv.URL, sleep: func(d time.Duration) { slept += d }}
}

// TestCapture: o quadro volta inteiro, com o tamanho lido do JPEG; tela sem quadro, status de erro, corpo que não é
// imagem e visualizador fora do ar são erros com mensagem própria.
func TestCapture(t *testing.T) {
	f := &fakeViewer{w: 64, h: 48}
	s := start(t, f)
	shot, err := s.Capture(context.Background())
	if err != nil || shot.Width != 64 || shot.Height != 48 || len(shot.JPEG) == 0 {
		t.Fatalf("%+v %v", shot, err)
	}
	for status, want := range map[int]string{204: "not drawn", 500: "returned 500"} {
		f.frameStatus = status
		if _, err := s.Capture(context.Background()); err == nil || !strings.Contains(err.Error(), want) {
			t.Fatalf("%d: %v", status, err)
		}
	}
	f.frameStatus, f.body = 0, []byte("not an image")
	if _, err := s.Capture(context.Background()); err == nil || !strings.Contains(err.Error(), "not an image") {
		t.Fatalf("corpo ruim: %v", err)
	}
	if _, err := (Screen{Base: "http://127.0.0.1:1"}).Capture(context.Background()); err == nil {
		t.Fatal("visualizador fora do ar")
	}
	if _, err := (Screen{Base: "::"}).Capture(context.Background()); err == nil {
		t.Fatal("URL inválida")
	}
}

// TestDo: o click usa o tamanho da captura e manda um pacote por passo, na ordem; type e key não capturam a tela;
// ação inválida não manda nada; erro do /input e do /frame chegam a quem chamou.
func TestDo(t *testing.T) {
	f := &fakeViewer{w: 100, h: 50}
	s := start(t, f)
	if err := s.Do(context.Background(), []Action{{Action: "click", X: 99, Y: 49}}); err != nil {
		t.Fatal(err)
	}
	if f.frames != 1 || len(f.packets) != 3 || f.packets[0][0].Value != absMax || f.packets[0][1].Value != absMax {
		t.Fatalf("frames=%d packets=%v", f.frames, f.packets)
	}
	for _, a := range f.agent {
		if a != "1" {
			t.Fatalf("POST /input sem a marca de agente (a página não desenharia a seta): %q", f.agent)
		}
	}
	f.packets, f.frames = nil, 0
	if err := s.Do(context.Background(), []Action{{Action: "type", Text: "a"}, {Action: "key", Keys: "Return"}, {Action: "wait", Ms: 1}}); err != nil {
		t.Fatal(err)
	}
	if f.frames != 0 || len(f.packets) != 4 {
		t.Fatalf("type/key: frames=%d packets=%d", f.frames, len(f.packets))
	}
	// Um move sozinho também precisa do tamanho da tela: sem ele, a coordenada não tem escala.
	f.packets, f.frames = nil, 0
	if err := s.Do(context.Background(), []Action{{Action: "move", X: 50, Y: 25}}); err != nil || f.frames != 1 || len(f.packets) != 1 {
		t.Fatalf("move: frames=%d packets=%d %v", f.frames, len(f.packets), err)
	}
	f.packets = nil
	if err := s.Do(context.Background(), []Action{{Action: "type", Text: "ok"}, {Action: "key", Keys: "nada"}}); err == nil || len(f.packets) != 0 {
		t.Fatalf("inválida mandou %d pacotes: %v", len(f.packets), err)
	}
	f.inputStatus = 403
	if err := s.Do(context.Background(), []Action{{Action: "key", Keys: "a"}}); err == nil || !strings.Contains(err.Error(), "403") {
		t.Fatalf("403: %v", err)
	}
	f.frameStatus = 500
	if err := s.Do(context.Background(), []Action{{Action: "move", X: 1, Y: 1}}); err == nil {
		t.Fatal("captura falhou e o move seguiu")
	}
	if err := (Screen{Base: "http://127.0.0.1:1"}).Do(context.Background(), []Action{{Action: "key", Keys: "a"}}); err == nil {
		t.Fatal("visualizador fora do ar")
	}
	if err := (Screen{Base: "::"}).Do(context.Background(), []Action{{Action: "key", Keys: "a"}}); err == nil {
		t.Fatal("URL inválida")
	}
}

// TestPausesAreHonored: a Screen dorme a pausa de cada passo (sem ela, o clique chega rápido demais ao app).
func TestPausesAreHonored(t *testing.T) {
	f := &fakeViewer{w: 10, h: 10}
	srv := httptest.NewServer(f)
	defer srv.Close()
	var slept time.Duration
	s := Screen{Base: srv.URL, sleep: func(d time.Duration) { slept += d }}
	if err := s.Do(context.Background(), []Action{{Action: "click", X: 1, Y: 1}, {Action: "wait", Ms: 100}}); err != nil {
		t.Fatal(err)
	}
	if slept != travelPause+2*keyPause+100*time.Millisecond {
		t.Fatalf("dormiu %s", slept)
	}
	if real := (Screen{Base: srv.URL}); real.sleep != nil {
		t.Fatal("sem sleep trocado, usa o time.Sleep")
	}
	if err := (Screen{Base: srv.URL}).Do(context.Background(), []Action{{Action: "wait", Ms: 1}}); err != nil {
		t.Fatal(err)
	}
}

// TestForPort aponta para o loopback na porta da tela, e o cliente padrão tem prazo.
func TestForPort(t *testing.T) {
	s := ForPort(6080)
	if s.Base != "http://127.0.0.1:6080" || s.client().Timeout != localTimeout {
		t.Fatalf("%+v", s)
	}
	custom := &http.Client{}
	if (Screen{HTTP: custom}).client() != custom {
		t.Fatal("cliente próprio ignorado")
	}
}
