package krun

import (
	"fmt"
	"net"
	"net/http"
	"os"

	"runner.code/internal/adapters/screen"
	"runner.code/internal/adapters/viewer"
	"runner.code/internal/ports"
)

// startDisplay cria a tela e sobe o visualizador no endereço pedido, neste mesmo processo
// (vive e morre com a VM). Sem display devolve nil, e a VM sobe sem saída de vídeo.
func startDisplay(d *ports.VMDisplay) (*screen.Screen, error) {
	if d == nil {
		return nil, nil
	}
	l, err := net.Listen("tcp", d.Listen)
	if err != nil {
		return nil, fmt.Errorf("display viewer on %s: %w", d.Listen, err)
	}
	// Tela por framebuffer de CPU (Rutabaga2D, patch da nix/libkrun-darwin.nix): com o
	// virglrenderer só com Venus o recurso 2D do guest falha e a tela nunca chega aqui.
	if err := os.Setenv("KRUN_GPU_BACKEND", "2d"); err != nil {
		return nil, err
	}
	scr := screen.New()
	_, _, in, err := newInputBackends()
	if err != nil {
		return nil, fmt.Errorf("display input: %w", err)
	}
	srv := &http.Server{Handler: viewer.Handler(scr, in, int(d.Width), int(d.Height))}
	go srv.Serve(l)
	return scr, nil
}
