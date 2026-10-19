package guestproto

import (
	"bytes"
	"context"
	"errors"
	"io"
	"net"
	"strings"
	"sync"
	"testing"
	"time"

	"runner.code/internal/ports"
	"runner.code/internal/protocol"
)

// ptyGuest simula o guest de uma sessão: confere o job, devolve um eco da entrada e o
// tamanho recebido no resize, e sai com 9 quando a entrada traz "q" ou o Ctrl+D.
func ptyGuest(got *protocol.Job, mu *sync.Mutex) func(net.Conn) {
	return func(c net.Conn) {
		var j protocol.Job
		if protocol.ReadFrame(c, &j) != nil {
			return
		}
		mu.Lock()
		*got = j
		mu.Unlock()
		for {
			var ev protocol.Event
			if protocol.ReadFrame(c, &ev) != nil {
				return
			}
			switch ev.Type {
			case protocol.EventResize:
				protocol.WriteFrame(c, protocol.Event{Type: protocol.EventOutput, Bytes: []byte("size")})
			case protocol.EventInput:
				protocol.WriteFrame(c, protocol.Event{Type: protocol.EventOutput, Bytes: append([]byte("echo:"), ev.Bytes...)})
				if bytes.Contains(ev.Bytes, []byte("q")) || bytes.Equal(ev.Bytes, []byte{eot}) {
					protocol.WriteFrame(c, protocol.Event{Type: protocol.EventExit, Code: 9})
					return
				}
			}
		}
	}
}

// TestInteractive: o job sai como OpPTY com o tamanho, a entrada e o resize chegam ao
// guest, a saída chega ao out e o código volta. Com o stdin no fim, vai o Ctrl+D.
func TestInteractive(t *testing.T) {
	var got protocol.Job
	var mu sync.Mutex
	sock := fakeGuest(t, ptyGuest(&got, &mu))
	pr, pw := io.Pipe()
	resize := make(chan ports.Winsize, 1)
	var out bytes.Buffer
	done := make(chan int)
	go func() {
		code, err := Client{}.Interactive(context.Background(), sock, protocol.Job{Rows: 30, Cols: 100}, pr, &out, resize)
		if err != nil {
			t.Error(err)
		}
		done <- code
	}()
	resize <- ports.Winsize{Rows: 40, Cols: 120}
	time.Sleep(100 * time.Millisecond)
	pw.Write([]byte("q"))
	if code := <-done; code != 9 {
		t.Fatalf("code %d", code)
	}
	mu.Lock()
	job := got
	mu.Unlock() // soltar antes da 2ª sessão: o guest falso trava o mesmo mutex
	if job.Op != protocol.OpPTY || job.Rows != 30 || job.Cols != 100 {
		t.Fatalf("job: %+v", job)
	}
	if !strings.Contains(out.String(), "size") || !strings.Contains(out.String(), "echo:q") {
		t.Fatalf("output %q", out.String())
	}

	var got2 protocol.Job
	sock = fakeGuest(t, ptyGuest(&got2, &mu))
	code, err := Client{}.Interactive(context.Background(), sock, protocol.Job{}, strings.NewReader(""), io.Discard, nil)
	if err != nil || code != 9 {
		t.Fatalf("stdin EOF must send Ctrl+D: %d %v", code, err)
	}
}

// TestInteractiveErrors: sem guest, evento inesperado, conexão que cai e saída que falha.
func TestInteractiveErrors(t *testing.T) {
	if _, err := (Client{}).Interactive(context.Background(), t.TempDir()+"/none", protocol.Job{}, strings.NewReader(""), io.Discard, nil); err == nil {
		t.Fatal("no guest must fail")
	}
	weird := fakeGuest(t, func(c net.Conn) {
		var j protocol.Job
		protocol.ReadFrame(c, &j)
		protocol.WriteFrame(c, protocol.Event{Type: "progress"})
	})
	if _, err := (Client{}).Interactive(context.Background(), weird, protocol.Job{}, blockReader{}, io.Discard, nil); err == nil {
		t.Fatal("unexpected event must fail")
	}
	gone := fakeGuest(t, func(c net.Conn) {
		var j protocol.Job
		protocol.ReadFrame(c, &j)
	})
	if _, err := (Client{}).Interactive(context.Background(), gone, protocol.Job{}, blockReader{}, io.Discard, nil); err == nil {
		t.Fatal("guest closing without exit must fail")
	}
	chatty := fakeGuest(t, func(c net.Conn) {
		var j protocol.Job
		protocol.ReadFrame(c, &j)
		protocol.WriteFrame(c, protocol.Event{Type: protocol.EventOutput, Bytes: []byte("x")})
		time.Sleep(time.Second)
	})
	if _, err := (Client{}).Interactive(context.Background(), chatty, protocol.Job{}, blockReader{}, failWriter{}, nil); err == nil {
		t.Fatal("output write failure must fail")
	}
	ctx, cancel := context.WithTimeout(context.Background(), 100*time.Millisecond)
	defer cancel()
	slow := fakeGuest(t, func(c net.Conn) { time.Sleep(2 * time.Second) })
	if _, err := (Client{}).Interactive(ctx, slow, protocol.Job{}, blockReader{}, io.Discard, nil); !errors.Is(err, context.DeadlineExceeded) {
		t.Fatalf("ctx deadline: %v", err)
	}
}

// blockReader nunca entrega nada: um terminal parado.
type blockReader struct{}

// Read bloqueia "para sempre" (o teste termina antes).
func (blockReader) Read([]byte) (int, error) { time.Sleep(time.Hour); return 0, nil }

// failWriter recusa toda escrita, como um terminal que fechou.
type failWriter struct{}

// Write falha.
func (failWriter) Write([]byte) (int, error) { return 0, errors.New("terminal closed") }
