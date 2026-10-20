package screen

import (
	"testing"
	"time"
)

// pixel monta um frame de 1×1 com os 4 bytes dados.
func pixel(b0, b1, b2, b3 byte) []byte { return []byte{b0, b1, b2, b3} }

// TestChannelOrder: cada formato de 32 bits vira RGBA com o canal certo, e o desconhecido
// cai em BGRX.
func TestChannelOrder(t *testing.T) {
	for format, in := range map[uint32][]byte{
		FormatB8G8R8A8: pixel(3, 2, 1, 0), FormatB8G8R8X8: pixel(3, 2, 1, 0), FormatA8R8G8B8: pixel(0, 1, 2, 3),
		FormatX8R8G8B8: pixel(0, 1, 2, 3), FormatR8G8B8A8: pixel(1, 2, 3, 0), FormatR8G8B8X8: pixel(1, 2, 3, 0),
		FormatX8B8G8R8: pixel(0, 3, 2, 1), FormatA8B8G8R8: pixel(0, 3, 2, 1), 999: pixel(3, 2, 1, 0),
	} {
		s := New()
		s.Update(1, 1, format, in)
		img, seq := s.Snapshot(0, time.Second)
		if seq != 1 || img.Pix[0] != 1 || img.Pix[1] != 2 || img.Pix[2] != 3 || img.Pix[3] != 0xff {
			t.Fatalf("format %d: %v", format, img.Pix)
		}
	}
}

// TestSnapshot: sem quadro novo espera o prazo e devolve nil; um Update acorda quem espera;
// a cópia não muda com o próximo quadro; frame torto é ignorado; mudança de tamanho recria.
func TestSnapshot(t *testing.T) {
	s := New()
	start := time.Now()
	if img, seq := s.Snapshot(0, 50*time.Millisecond); img != nil || seq != 0 || time.Since(start) < 40*time.Millisecond {
		t.Fatal("no frame must wait and return nil")
	}
	go func() {
		time.Sleep(30 * time.Millisecond)
		s.Update(2, 1, FormatR8G8B8A8, []byte{9, 9, 9, 0, 8, 8, 8, 0})
	}()
	img, seq := s.Snapshot(0, 5*time.Second)
	if img == nil || seq != 1 || img.Rect.Dx() != 2 {
		t.Fatalf("waiter must wake: %v %d", img, seq)
	}
	s.Update(2, 1, FormatR8G8B8A8, []byte{1, 1, 1, 0, 1, 1, 1, 0})
	if img.Pix[0] != 9 {
		t.Fatal("a snapshot is a copy")
	}
	s.Update(4, 4, FormatR8G8B8A8, []byte{1})
	s.Update(0, 4, FormatR8G8B8A8, nil)
	if _, seq := s.Snapshot(2, 10*time.Millisecond); seq != 2 {
		t.Fatal("a short or empty frame must be ignored")
	}
	s.Update(1, 1, FormatR8G8B8A8, pixel(5, 5, 5, 0))
	if img, seq := s.Snapshot(2, time.Second); seq != 3 || img.Rect.Dx() != 1 {
		t.Fatalf("resize: %v %d", img.Rect, seq)
	}
}
