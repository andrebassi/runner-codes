package guestproto

import (
	"context"
	"errors"
	"fmt"
	"net"
	"os"
	"path/filepath"
	"sync/atomic"
	"testing"
	"time"

	"runner.code/internal/protocol"
)

// fakeGuest escuta num socket Unix e responde conforme handle; devolve o caminho.
func fakeGuest(t *testing.T, handle func(net.Conn)) string {
	t.Helper()
	sock := filepath.Join(t.TempDir(), "g.sock")
	ln, err := net.Listen("unix", sock)
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { ln.Close() })
	go func() {
		for {
			c, err := ln.Accept()
			if err != nil {
				return
			}
			go func() { defer c.Close(); handle(c) }()
		}
	}()
	return sock
}

// echoGuest responde ao job com stdout = código, como um guest que executa `echo`.
func echoGuest(c net.Conn) {
	var j protocol.Job
	if protocol.ReadFrame(c, &j) != nil {
		return
	}
	protocol.WriteFrame(c, protocol.Result{TraceID: j.TraceID, Status: "success", Stdout: j.Code})
}

// TestRunRoundTrip: o job vai e o resultado volta pelo framing do protocolo.
func TestRunRoundTrip(t *testing.T) {
	sock := fakeGuest(t, echoGuest)
	res, err := Client{}.Run(context.Background(), sock, protocol.Job{TraceID: "a", Code: "oi"})
	if err != nil || res.Stdout != "oi" || res.TraceID != "a" {
		t.Fatalf("res=%+v err=%v", res, err)
	}
}

// TestRunDeadline: guest que não responde não segura o chamador além do deadline.
func TestRunDeadline(t *testing.T) {
	sock := fakeGuest(t, func(c net.Conn) { time.Sleep(2 * time.Second) })
	ctx, cancel := context.WithTimeout(context.Background(), 100*time.Millisecond)
	defer cancel()
	start := time.Now()
	_, err := Client{}.Run(ctx, sock, protocol.Job{})
	if !errors.Is(err, context.DeadlineExceeded) {
		t.Fatalf("expected DeadlineExceeded, got %v", err)
	}
	if time.Since(start) > time.Second {
		t.Fatal("Run exceeded the deadline")
	}
}

// TestRunCancel: cancelar o ctx sem deadline também solta o chamador.
func TestRunCancel(t *testing.T) {
	sock := fakeGuest(t, func(c net.Conn) { time.Sleep(2 * time.Second) })
	ctx, cancel := context.WithCancel(context.Background())
	go func() { time.Sleep(50 * time.Millisecond); cancel() }()
	if _, err := (Client{}).Run(ctx, sock, protocol.Job{}); !errors.Is(err, context.Canceled) {
		t.Fatalf("expected Canceled, got %v", err)
	}
}

// TestRunErrors cobre socket inexistente, guest que fecha sem responder e job não serializável.
func TestRunErrors(t *testing.T) {
	if _, err := (Client{}).Run(context.Background(), filepath.Join(t.TempDir(), "x.sock"), protocol.Job{}); err == nil {
		t.Fatal("missing socket should fail")
	}
	closer := fakeGuest(t, func(c net.Conn) {})
	if _, err := (Client{}).Run(context.Background(), closer, protocol.Job{}); err == nil {
		t.Fatal("guest that closes without replying should fail")
	}
	sock := fakeGuest(t, echoGuest)
	big := protocol.Job{Code: string(make([]byte, protocol.MaxFrameBytes+1))}
	if _, err := (Client{}).Run(context.Background(), sock, big); err == nil {
		t.Fatal("job above the limit should fail on send")
	}
}

// TestWaitReadyRetriesUntilAnswer: as primeiras conexões caem (guest ainda subindo) e a
// espera continua até a primeira resposta, como acontece com a libkrun no boot.
func TestWaitReadyRetriesUntilAnswer(t *testing.T) {
	var n atomic.Int32
	sock := fakeGuest(t, func(c net.Conn) {
		if n.Add(1) <= 3 {
			return // aceita e fecha: igual à libkrun antes de o guest escutar
		}
		echoGuest(c)
	})
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	if err := (Client{}).WaitReady(ctx, sock); err != nil {
		t.Fatal(err)
	}
	if n.Load() < 4 {
		t.Fatalf("expected at least 4 attempts, got %d", n.Load())
	}
}

// TestCtxError trava a tradução que corrigiu o teste intermitente: "i/o timeout" do
// socket vira DeadlineExceeded mesmo com o ctx ainda vivo.
func TestCtxError(t *testing.T) {
	live := context.Background()
	if err := ctxError(live, fmt.Errorf("read: %w", os.ErrDeadlineExceeded)); !errors.Is(err, context.DeadlineExceeded) {
		t.Fatalf("i/o timeout should become DeadlineExceeded: %v", err)
	}
	canceled, cancel := context.WithCancel(live)
	cancel()
	if err := ctxError(canceled, errors.New("connection closed")); !errors.Is(err, context.Canceled) {
		t.Fatalf("cancellation should take precedence: %v", err)
	}
	other := errors.New("connection reset")
	if err := ctxError(live, other); err != other {
		t.Fatalf("plain error should pass through unchanged: %v", err)
	}
}

// TestWaitReadyGivesUp: sem resposta, desiste no prazo do ctx e diz a última falha.
func TestWaitReadyGivesUp(t *testing.T) {
	ctx, cancel := context.WithTimeout(context.Background(), 50*time.Millisecond)
	defer cancel()
	err := (Client{}).WaitReady(ctx, filepath.Join(t.TempDir(), "never.sock"))
	if err == nil || !errors.Is(err, context.DeadlineExceeded) {
		t.Fatalf("expected DeadlineExceeded, got %v", err)
	}
}
