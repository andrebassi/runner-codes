// Package screen guarda a tela de uma máquina com display: o último frame que o guest
// apresentou, convertido para RGBA, com um número de versão para quem assiste esperar por
// mudança sem varrer a imagem. Quem escreve é o backend de display da libkrun; quem lê é o
// visualizador no navegador.
package screen

import (
	"image"
	"sync"
	"time"
)

// Formatos de pixel que o guest pode configurar (os KRUN_DISPLAY_FORMAT_* da libkrun, iguais
// aos do virtio-gpu). Só os de 32 bits interessam: é o que o fbdev e o Wayland usam.
const (
	FormatB8G8R8A8 = 1
	FormatB8G8R8X8 = 2
	FormatA8R8G8B8 = 3
	FormatX8R8G8B8 = 4
	FormatR8G8B8A8 = 67
	FormatX8B8G8R8 = 68
	FormatA8B8G8R8 = 121
	FormatR8G8B8X8 = 134
)

// Screen é a tela compartilhada.
type Screen struct {
	mu   sync.Mutex
	cond *sync.Cond
	img  *image.RGBA
	seq  uint64
}

// New cria uma tela vazia.
func New() *Screen {
	s := &Screen{}
	s.cond = sync.NewCond(&s.mu)
	return s
}

// channelOrder diz, para cada formato, de que byte do pixel vem R, G e B (o A é ignorado:
// a tela é opaca). Formato desconhecido cai em BGRX, o mais comum no virtio-gpu.
func channelOrder(format uint32) (r, g, b int) {
	switch format {
	case FormatA8R8G8B8, FormatX8R8G8B8:
		return 1, 2, 3
	case FormatR8G8B8A8, FormatR8G8B8X8:
		return 0, 1, 2
	case FormatX8B8G8R8, FormatA8B8G8R8:
		return 3, 2, 1
	default:
		return 2, 1, 0
	}
}

// Update grava um frame de w×h pixels de 32 bits no formato dado e acorda quem espera.
// Buffer menor que w×h×4 é ignorado: um frame torto não pode derrubar o processo da VM.
func (s *Screen) Update(w, h int, format uint32, data []byte) {
	if w <= 0 || h <= 0 || len(data) < w*h*4 {
		return
	}
	ri, gi, bi := channelOrder(format)
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.img == nil || s.img.Rect.Dx() != w || s.img.Rect.Dy() != h {
		s.img = image.NewRGBA(image.Rect(0, 0, w, h))
	}
	px := s.img.Pix
	for i := 0; i < w*h; i++ {
		o := i * 4
		px[o], px[o+1], px[o+2], px[o+3] = data[o+ri], data[o+gi], data[o+bi], 0xff
	}
	s.seq++
	s.cond.Broadcast()
}

// Snapshot devolve uma cópia do frame mais novo que after, esperando até timeout por ele.
// Sem frame novo no prazo devolve (nil, after): quem assiste só repete a espera.
func (s *Screen) Snapshot(after uint64, timeout time.Duration) (*image.RGBA, uint64) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.seq <= after {
		t := time.AfterFunc(timeout, func() { s.mu.Lock(); s.cond.Broadcast(); s.mu.Unlock() })
		defer t.Stop()
		deadline := time.Now().Add(timeout)
		for s.seq <= after && time.Now().Before(deadline) {
			s.cond.Wait()
		}
	}
	if s.seq <= after || s.img == nil {
		return nil, after
	}
	cp := image.NewRGBA(s.img.Rect)
	copy(cp.Pix, s.img.Pix)
	return cp, s.seq
}
