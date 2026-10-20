// Package viewer serve a tela de uma máquina no navegador: uma página com um <canvas>, os
// frames por long-poll em JPEG (GET /frame?after=N, o N é a versão já vista) e o teclado e o
// mouse de volta (POST /input). HTTP simples em vez de WebSocket/RFB: no loopback o long-poll
// passa de 30 quadros por segundo e não precisa de biblioteca nem de cliente VNC.
package viewer

import (
	"bytes"
	_ "embed"
	"encoding/json"
	"image/jpeg"
	"net/http"
	"strconv"
	"time"

	"runner.code/internal/adapters/screen"
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

// pollWait é quanto um GET /frame espera por quadro novo antes de devolver 204: curto o
// bastante para o navegador notar uma VM que parou, longo o bastante para não martelar.
const pollWait = 20 * time.Second

// jpegQuality equilibra nitidez de texto e tamanho do quadro (1080p fica perto de 200 KB).
const jpegQuality = 85

// maxInput limita o corpo de um POST /input: um lote de eventos de teclado e mouse tem
// poucos KB; sem teto, um cliente prenderia memória do processo da VM.
const maxInput = 64 << 10

// Handler monta as rotas do visualizador.
func Handler(s *screen.Screen, in Input, width, height int) http.Handler {
	mux := http.NewServeMux()
	mux.HandleFunc("GET /{$}", func(w http.ResponseWriter, _ *http.Request) {
		w.Header().Set("Content-Type", "text/html; charset=utf-8")
		w.Write(page)
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
		w.WriteHeader(http.StatusNoContent)
	})
	return mux
}
