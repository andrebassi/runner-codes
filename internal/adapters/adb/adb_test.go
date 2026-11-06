package adb

import (
	"bytes"
	"context"
	"encoding/binary"
	"errors"
	"net"
	"strings"
	"sync/atomic"
	"testing"
	"time"
)

// fakeAdbd é o lado do aparelho numa net.Pipe: responde ao handshake e ao OPEN como o roteiro mandar.
type fakeAdbd struct {
	auth    bool     // responde AUTH em vez de CNXN
	refuse  bool     // fecha o stream sem OKAY (serviço recusado)
	chunks  [][]byte // WRTE mandados depois do OKAY
	noClose bool     // fecha a conexão em vez de mandar CLSE
	garbage bool     // manda um pacote com magic errado depois do handshake
	hang    bool     // não responde nada depois do handshake
	opened  string   // o serviço pedido no OPEN
	acks    int      // OKAY recebidos (um por WRTE)
}

// serve roda o roteiro do lado do aparelho até a conexão fechar.
func (f *fakeAdbd) serve(conn net.Conn) {
	defer conn.Close()
	if _, err := read(conn); err != nil { // CNXN do cliente
		return
	}
	if f.auth {
		write(conn, packet{cmd: cmdAUTH, arg0: 1, data: bytes.Repeat([]byte{7}, 20)})
		return
	}
	// Um pacote qualquer antes do CNXN: o cliente tem de ignorar.
	write(conn, packet{cmd: cmdOKAY})
	write(conn, packet{cmd: cmdCNXN, arg0: version, arg1: maxPayload, data: []byte("device::ro.product.name=redroid;\x00")})
	open, err := read(conn)
	if err != nil {
		return
	}
	f.opened = strings.TrimSuffix(string(open.data), "\x00")
	switch {
	case f.hang:
		time.Sleep(time.Second)
		return
	case f.garbage:
		hdr := make([]byte, 24)
		binary.LittleEndian.PutUint32(hdr[0:], cmdWRTE)
		conn.Write(hdr)
		return
	case f.refuse:
		write(conn, packet{cmd: cmdCLSE, arg1: open.arg0})
		return
	}
	const remote = 77
	write(conn, packet{cmd: cmdOKAY, arg0: remote, arg1: open.arg0})
	for _, c := range f.chunks {
		write(conn, packet{cmd: cmdWRTE, arg0: remote, arg1: open.arg0, data: c})
		if p, err := read(conn); err == nil && p.cmd == cmdOKAY && p.arg1 == remote {
			f.acks++
		}
	}
	if f.noClose {
		return
	}
	write(conn, packet{cmd: cmdCLSE, arg0: remote, arg1: open.arg0})
	read(conn) // o CLSE de volta
}

// clientFor devolve um Client cujo Dial entrega o lado do computador de uma net.Pipe servida por f.
func clientFor(f *fakeAdbd) *Client {
	return &Client{Dial: func(context.Context, string) (net.Conn, error) {
		a, b := net.Pipe()
		go f.serve(b)
		return a, nil
	}}
}

// TestExecJoinsWrites trava o caminho feliz: o serviço vai no OPEN, cada WRTE é confirmado e a saída chega inteira.
func TestExecJoinsWrites(t *testing.T) {
	f := &fakeAdbd{chunks: [][]byte{[]byte("<hierarchy>"), []byte("<node/>"), []byte("</hierarchy>")}}
	out, err := clientFor(f).Exec(context.Background(), "x:5555", "exec:cat /data/local/tmp/ui.xml")
	if err != nil || string(out) != "<hierarchy><node/></hierarchy>" {
		t.Fatalf("Exec = %q, %v", out, err)
	}
	if f.opened != "exec:cat /data/local/tmp/ui.xml" || f.acks != 3 {
		t.Fatalf("opened=%q acks=%d", f.opened, f.acks)
	}
}

// TestShellStripsCR confere que o modo shell antigo (CRLF) chega com quebras normais.
func TestShellStripsCR(t *testing.T) {
	f := &fakeAdbd{chunks: [][]byte{[]byte("1\r\n")}}
	out, err := clientFor(f).Shell(context.Background(), "x", "getprop sys.boot_completed")
	if err != nil || out != "1\n" || f.opened != "shell:getprop sys.boot_completed" {
		t.Fatalf("Shell = %q, %v (opened %q)", out, err, f.opened)
	}
}

// TestExecFailures cobre cada resposta ruim do adbd: AUTH, serviço recusado, conexão fechada sem CLSE (a saída vale),
// pacote corrompido e o dial que falha.
func TestExecFailures(t *testing.T) {
	ctx := context.Background()
	if _, err := clientFor(&fakeAdbd{auth: true}).Exec(ctx, "x", "shell:id"); !errors.Is(err, ErrAuth) {
		t.Errorf("auth: err = %v", err)
	}
	if _, err := clientFor(&fakeAdbd{refuse: true}).Exec(ctx, "x", "nope:"); err == nil || !strings.Contains(err.Error(), "refused") {
		t.Errorf("refuse: err = %v", err)
	}
	out, err := clientFor(&fakeAdbd{noClose: true, chunks: [][]byte{[]byte("parcial")}}).Exec(ctx, "x", "shell:id")
	if err != nil || string(out) != "parcial" {
		t.Errorf("noClose: %q, %v", out, err)
	}
	if _, err := clientFor(&fakeAdbd{garbage: true}).Exec(ctx, "x", "shell:id"); err == nil || !strings.Contains(err.Error(), "magic") {
		t.Errorf("garbage: err = %v", err)
	}
	c := &Client{Dial: func(context.Context, string) (net.Conn, error) { return nil, errors.New("connection refused") }}
	if _, err := c.Exec(ctx, "127.0.0.1:1", "shell:id"); err == nil || !strings.Contains(err.Error(), "dial 127.0.0.1:1") {
		t.Errorf("dial: err = %v", err)
	}
}

// TestExecDeadline confere que um adbd mudo não segura o chamador: o prazo do Client (sem deadline no ctx) e o
// cancelamento do ctx encerram o comando com o erro do contexto.
func TestExecDeadline(t *testing.T) {
	c := clientFor(&fakeAdbd{hang: true})
	c.Timeout = 50 * time.Millisecond
	if _, err := c.Exec(context.Background(), "x", "shell:sleep 9"); err == nil || !errors.Is(err, context.DeadlineExceeded) {
		t.Errorf("timeout: err = %v", err)
	}
	ctx, cancel := context.WithCancel(context.Background())
	time.AfterFunc(30*time.Millisecond, cancel)
	if _, err := clientFor(&fakeAdbd{hang: true}).Exec(ctx, "x", "shell:sleep 9"); err == nil || !errors.Is(err, context.Canceled) {
		t.Errorf("cancel: err = %v", err)
	}
}

// TestRead recusa um tamanho de payload absurdo (lixo no fio, não um WRTE).
func TestRead(t *testing.T) {
	hdr := make([]byte, 24)
	binary.LittleEndian.PutUint32(hdr[0:], cmdWRTE)
	binary.LittleEndian.PutUint32(hdr[12:], maxPayload*8)
	binary.LittleEndian.PutUint32(hdr[20:], cmdWRTE^0xffffffff)
	if _, err := read(bytes.NewReader(hdr)); err == nil || !strings.Contains(err.Error(), "over the limit") {
		t.Fatalf("err = %v", err)
	}
	if _, err := read(bytes.NewReader(append(hdr[:12:12], make([]byte, 12)...))); err == nil {
		t.Fatal("magic errado aceito")
	}
}

// TestWaitBoot espera o "1" (tentando de novo enquanto não vem) e, no prazo, explica onde parou.
func TestWaitBoot(t *testing.T) {
	var calls atomic.Int32
	c := &Client{Dial: func(context.Context, string) (net.Conn, error) {
		a, b := net.Pipe()
		v := "0\r\n"
		if calls.Add(1) >= 3 {
			v = "1\r\n"
		}
		go (&fakeAdbd{chunks: [][]byte{[]byte(v)}}).serve(b)
		return a, nil
	}}
	if err := c.WaitBoot(context.Background(), "x", time.Millisecond); err != nil || calls.Load() != 3 {
		t.Fatalf("WaitBoot: %v (chamadas %d)", err, calls.Load())
	}
	ctx, cancel := context.WithTimeout(context.Background(), 300*time.Millisecond)
	defer cancel()
	zero := clientFor(&fakeAdbd{chunks: [][]byte{[]byte("0\n")}})
	if err := zero.WaitBoot(ctx, "x", 5*time.Millisecond); err == nil || !strings.Contains(err.Error(), `sys.boot_completed="0"`) {
		t.Fatalf("prazo: err = %v", err)
	}
	down := &Client{Dial: func(context.Context, string) (net.Conn, error) { return nil, errors.New("refused") }}
	ctx2, cancel2 := context.WithTimeout(context.Background(), 200*time.Millisecond)
	defer cancel2()
	if err := down.WaitBoot(ctx2, "x", 5*time.Millisecond); err == nil || !strings.Contains(err.Error(), "refused") {
		t.Fatalf("adbd fora: err = %v", err)
	}
}

// TestWaitBootKeepsReasonWhenDeadlineHitsMidAttempt: a 1ª tentativa lê "0"; a 2ª trava até o prazo inteiro vencer.
// O erro tem de dizer o "0" (o motivo útil), não o i/o timeout da tentativa interrompida.
func TestWaitBootKeepsReasonWhenDeadlineHitsMidAttempt(t *testing.T) {
	var calls atomic.Int32
	c := &Client{Dial: func(context.Context, string) (net.Conn, error) {
		a, b := net.Pipe()
		if calls.Add(1) == 1 {
			go (&fakeAdbd{chunks: [][]byte{[]byte("0\n")}}).serve(b)
		} else {
			go (&fakeAdbd{hang: true}).serve(b)
		}
		return a, nil
	}}
	ctx, cancel := context.WithTimeout(context.Background(), 200*time.Millisecond)
	defer cancel()
	err := c.WaitBoot(ctx, "x", time.Millisecond)
	if err == nil || !strings.Contains(err.Error(), `sys.boot_completed="0"`) || calls.Load() != 2 {
		t.Fatalf("err = %v (chamadas %d)", err, calls.Load())
	}
}
