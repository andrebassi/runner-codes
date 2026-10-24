// Package viewer serve a tela de uma máquina no navegador: uma página com um <canvas>, os
// frames por long-poll em JPEG (GET /frame?after=N, o N é a versão já vista) e o teclado e o
// mouse de volta (POST /input). HTTP simples em vez de WebSocket/RFB: no loopback o long-poll
// passa de 30 quadros por segundo e não precisa de biblioteca nem de cliente VNC.
package viewer

import (
	"bytes"
	_ "embed"
	"encoding/json"
	"html"
	"image/jpeg"
	"net/http"
	"strconv"
	"time"

	"runner.codes/internal/adapters/screen"
)

//go:embed page/index.html
var page []byte

// Event é um evento de entrada no formato do evdev do Linux (o que o virtio-input entrega
// ao guest): tipo (EV_KEY, EV_ABS, EV_SYN...), código e valor.
type Event struct {
	Type  uint16 `json:"type"`
	Code  uint16 `json:"code"`
	Value int32  `json:"value"`
}

// Input recebe os eventos do navegador; nil deixa a tela só para ver.
type Input interface {
	Push(events []Event)
}

// Resizer recebe o tamanho que o navegador tem para a tela; nil deixa o tamanho fixo no do boot.
type Resizer interface {
	Resize(width, height int)
}

// Limites de um POST /resize: os mesmos do display (domain.ParseDisplay). Fora disso o pedido é
// recusado, para uma janela minúscula ou um número absurdo não chegarem ao guest.
const (
	minWidth, minHeight = 320, 200
	maxWidth, maxHeight = 7680, 4320
)

// pollWait é quanto um GET /frame espera por quadro novo antes de devolver 204: curto o
// bastante para o navegador notar uma VM que parou, longo o bastante para não martelar.
const pollWait = 20 * time.Second

// jpegQuality equilibra nitidez de texto e tamanho do quadro (1080p fica perto de 200 KB).
const jpegQuality = 85

// maxInput limita o corpo de um POST /input: um lote de eventos de teclado e mouse tem
// poucos KB; sem teto, um cliente prenderia memória do processo da VM.
const maxInput = 64 << 10

// DefaultTitle é o nome na barra da página quando a máquina não dá outro (display_title).
const DefaultTitle = "runner.codes"

// titledPage devolve a página com o título no lugar do marcador {{title}} (barra e aba). O título vem
// escapado: é texto do runner.code, e um "<" ali viraria marcação na página.
func titledPage(title string) []byte {
	if title == "" {
		title = DefaultTitle
	}
	return bytes.ReplaceAll(page, []byte("{{title}}"), []byte(html.EscapeString(title)))
}

// Handler monta as rotas do visualizador com o título padrão; ver TitledHandler.
func Handler(s *screen.Screen, in Input, rs Resizer, width, height int) http.Handler {
	return TitledHandler(DefaultTitle, s, in, rs, width, height)
}

// TitledHandler monta as rotas do visualizador, com title na barra e na aba da página. rs recebe os
// pedidos de tamanho da página (tela que acompanha a janela do navegador); nil responde 404 ao
// POST /resize e a página para de pedir.
func TitledHandler(title string, s *screen.Screen, in Input, rs Resizer, width, height int) http.Handler {
	mux := http.NewServeMux()
	feed := newPointerFeed()
	body := titledPage(title)
	mux.HandleFunc("GET /{$}", func(w http.ResponseWriter, _ *http.Request) {
		w.Header().Set("Content-Type", "text/html; charset=utf-8")
		w.Write(body)
	})
	mux.HandleFunc("GET /size", func(w http.ResponseWriter, _ *http.Request) {
		json.NewEncoder(w).Encode(map[string]int{"width": width, "height": height})
	})
	mux.HandleFunc("GET /frame", func(w http.ResponseWriter, r *http.Request) {
		after, _ := strconv.ParseUint(r.URL.Query().Get("after"), 10, 64)
		img, seq := s.Snapshot(after, pollWait)
		if img == nil {
			w.WriteHeader(http.StatusNoContent)
			return
		}
		var buf bytes.Buffer
		jpeg.Encode(&buf, img, &jpeg.Options{Quality: jpegQuality})
		w.Header().Set("Content-Type", "image/jpeg")
		w.Header().Set("Cache-Control", "no-store")
		w.Header().Set("X-Seq", strconv.FormatUint(seq, 10))
		w.Write(buf.Bytes())
	})
	mux.HandleFunc("POST /input", func(w http.ResponseWriter, r *http.Request) {
		if in == nil {
			http.Error(w, "view-only display", http.StatusForbidden)
			return
		}
		var evs []Event
		if err := json.NewDecoder(http.MaxBytesReader(w, r.Body, maxInput)).Decode(&evs); err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}
		in.Push(evs)
		// Lote de automação: a página desenha a seta e o clique por cima da tela (o da pessoa não).
		if r.Header.Get(AgentHeader) == "1" {
			feed.publish(evs)
		}
		w.WriteHeader(http.StatusNoContent)
	})
	mux.HandleFunc("GET /pointer", func(w http.ResponseWriter, r *http.Request) {
		after, _ := strconv.ParseUint(r.URL.Query().Get("after"), 10, 64)
		evs, seq := feed.since(after, pollWait)
		w.Header().Set("Content-Type", "application/json")
		w.Header().Set("Cache-Control", "no-store")
		json.NewEncoder(w).Encode(map[string]any{"seq": seq, "events": evs})
	})
	mux.HandleFunc("POST /resize", func(w http.ResponseWriter, r *http.Request) {
		// Tela só para ver não muda de tamanho: quem só olha não decide a resolução do guest.
		if rs == nil || in == nil {
			http.Error(w, "resize not supported", http.StatusNotFound)
			return
		}
		var sz struct {
			Width  int `json:"width"`
			Height int `json:"height"`
		}
		if err := json.NewDecoder(http.MaxBytesReader(w, r.Body, 1<<10)).Decode(&sz); err != nil {
			http.Error(w, err.Error(), http.StatusBadRequest)
			return
		}
		if sz.Width < minWidth || sz.Height < minHeight || sz.Width > maxWidth || sz.Height > maxHeight {
			http.Error(w, "size out of range (320x200 to 7680x4320)", http.StatusBadRequest)
			return
		}
		rs.Resize(sz.Width, sz.Height)
		w.WriteHeader(http.StatusNoContent)
	})
	return mux
}
