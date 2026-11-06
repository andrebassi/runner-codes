package adb

import (
	"bytes"
	"context"
	"encoding/binary"
	"errors"
	"io"
	"net"
	"strings"
	"sync"
	"testing"
	"time"

	"runner.codes/internal/ports"
)

// fakeShell é um adbd com o shell v2: confirma cada WRTE, guarda os pacotes recebidos, ecoa o stdin como stdout e, ao
// ver "exit" no teclado, manda o código e fecha.
type fakeShell struct {
	mu       sync.Mutex
	service  string
	sizes    []string // pacotes de tamanho recebidos
	stdin    bytes.Buffer
	maxData  uint32
	code     byte
	noExit   bool // fecha sem o pacote de saída
	refuse   bool
	mute     bool // não responde ao OPEN
	cut      bool // fecha no meio de um pacote
	wrteSize []int
}

// serve roda o lado do aparelho numa net.Pipe.
func (f *fakeShell) serve(conn net.Conn) {
	defer conn.Close()
	if _, err := read(conn); err != nil {
		return
	}
	write(conn, packet{cmd: cmdCNXN, arg0: version, arg1: f.maxData, data: []byte("device::\x00")})
	open, err := read(conn)
	if err != nil {
		return
	}
	f.mu.Lock()
	f.service = strings.TrimSuffix(string(open.data), "\x00")
	f.mu.Unlock()
	if f.mute {
		time.Sleep(time.Second)
		return
	}
	if f.refuse {
		write(conn, packet{cmd: cmdCLSE, arg1: open.arg0})
		return
	}
	const remote = 9
	var wmu sync.Mutex
	send := func(p packet) {
		wmu.Lock()
		defer wmu.Unlock()
		write(conn, p)
	}
	send(packet{cmd: cmdOKAY, arg0: remote, arg1: open.arg0})
	var pending []byte
	for {
		p, err := read(conn)
		if err != nil || p.cmd == cmdCLSE {
			return
		}
		if p.cmd != cmdWRTE {
			continue
		}
		f.mu.Lock()
		f.wrteSize = append(f.wrteSize, len(p.data))
		f.mu.Unlock()
		send(packet{cmd: cmdOKAY, arg0: remote, arg1: open.arg0})
		pending = append(pending, p.data...)
		for len(pending) >= 5 {
			n := int(binary.LittleEndian.Uint32(pending[1:]))
			if len(pending) < 5+n {
				break
			}
			id, data := pending[0], append([]byte(nil), pending[5:5+n]...)
			pending = pending[5+n:]
			f.mu.Lock()
			switch id {
			case shellWindowSize:
				f.sizes = append(f.sizes, strings.TrimSuffix(string(data), "\x00"))
			case shellStdin:
				f.stdin.Write(data)
			}
			exit := strings.Contains(f.stdin.String(), "exit")
			f.mu.Unlock()
			if id == shellStdin {
				// Em goroutine: o cliente pode estar no meio de um Write esperando OKAY, e a net.Pipe não tem buffer.
				go send(packet{cmd: cmdWRTE, arg0: remote, arg1: open.arg0, data: shellPacket(shellStdout, append([]byte("eco:"), data...))})
			}
			if exit {
				time.Sleep(20 * time.Millisecond)
				if f.cut {
					send(packet{cmd: cmdWRTE, arg0: remote, arg1: open.arg0, data: []byte{shellStdout, 9, 0}})
					return
				}
				if !f.noExit {
					send(packet{cmd: cmdWRTE, arg0: remote, arg1: open.arg0, data: shellPacket(shellExit, []byte{f.code})})
				}
				send(packet{cmd: cmdCLSE, arg0: remote, arg1: open.arg0})
				return
			}
		}
	}
}

// shellClient devolve um Client cujo Dial abre uma conexão TCP de loopback servida por f. TCP e não net.Pipe: os dois
// lados escrevem ao mesmo tempo (o OKAY de um cruza com o WRTE do outro), e a net.Pipe, sem buffer, trava nisso — o
// socket de verdade tem buffer, como o adbd.
func shellClient(f *fakeShell) *Client {
	return &Client{Dial: func(ctx context.Context, _ string) (net.Conn, error) {
		l, err := net.Listen("tcp", "127.0.0.1:0")
		if err != nil {
			return nil, err
		}
		go func() {
			defer l.Close()
			if c, err := l.Accept(); err == nil {
				f.serve(c)
			}
		}()
		var d net.Dialer
		return d.DialContext(ctx, "tcp", l.Addr().String())
	}}
}

// syncBuffer é a saída do terminal, lida pelo teste enquanto o leitor do shell escreve nela.
type syncBuffer struct {
	mu sync.Mutex
	b  bytes.Buffer
}

// Write guarda o pedaço com o lock (o leitor do shell e o teste mexem ao mesmo tempo).
func (s *syncBuffer) Write(p []byte) (int, error) { s.mu.Lock(); defer s.mu.Unlock(); return s.b.Write(p) }

// String devolve tudo o que chegou até agora.
func (s *syncBuffer) String() string { s.mu.Lock(); defer s.mu.Unlock(); return s.b.String() }

// TestInteractive trava o terminal com PTY do Android: o serviço pedido é o shell v2 com PTY e TERM; o tamanho vai
// antes de qualquer tecla e cada resize depois; o teclado chega como stdin; a saída volta; o código de saída vem do
// pacote de saída; e o adbd com payload pequeno recebe o pacote em pedaços do tamanho dele.
func TestInteractive(t *testing.T) {
	f := &fakeShell{maxData: 4, code: 7}
	inR, inW := io.Pipe()
	resize := make(chan ports.Winsize, 1)
	var out syncBuffer
	done := make(chan struct{})
	var code int
	var err error
	go func() {
		code, err = shellClient(f).Interactive(context.Background(), "x", "", ports.Winsize{Rows: 30, Cols: 100}, inR, &out, resize)
		close(done)
	}()
	resize <- ports.Winsize{Rows: 40, Cols: 120}
	time.Sleep(50 * time.Millisecond)
	inW.Write([]byte("ls\r"))
	time.Sleep(20 * time.Millisecond)
	inW.Write([]byte("exit\r"))
	select {
	case <-done:
	case <-time.After(3 * time.Second):
		t.Fatal("o shell não terminou")
	}
	if err != nil || code != 7 {
		t.Fatalf("code=%d err=%v", code, err)
	}
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.service != "shell,v2,TERM=xterm-256color,pty:" {
		t.Errorf("serviço: %q", f.service)
	}
	if len(f.sizes) != 2 || f.sizes[0] != "30x100,0x0" || f.sizes[1] != "40x120,0x0" {
		t.Errorf("tamanhos: %v", f.sizes)
	}
	if f.stdin.String() != "ls\rexit\r" {
		t.Errorf("stdin: %q", f.stdin.String())
	}
	if !strings.Contains(out.String(), "eco:ls") {
		t.Errorf("saída: %q", out.String())
	}
	for _, n := range f.wrteSize {
		if n > 4 {
			t.Fatalf("WRTE de %d bytes num adbd que aceita 4", n)
		}
	}
}

// TestInteractiveEdges: fechar sem código é fim normal com 0; o comando vai no serviço; fechar no meio de um pacote é
// erro; serviço recusado, dial que falha, adbd mudo e ctx cancelado voltam com erro.
func TestInteractiveEdges(t *testing.T) {
	f := &fakeShell{noExit: true}
	code, err := shellClient(f).Interactive(context.Background(), "x", "id", ports.Winsize{Rows: 1, Cols: 1}, strings.NewReader("exit\r"), io.Discard, nil)
	if err != nil || code != 0 {
		t.Fatalf("sem código: %d %v", code, err)
	}
	if f.service != "shell,v2,TERM=xterm-256color,pty:id" {
		t.Errorf("serviço com comando: %q", f.service)
	}
	if _, err := shellClient(&fakeShell{cut: true}).Interactive(context.Background(), "x", "", ports.Winsize{}, strings.NewReader("exit"), io.Discard, nil); err == nil {
		t.Fatal("pacote cortado passou")
	}
	if _, err := shellClient(&fakeShell{refuse: true}).Interactive(context.Background(), "x", "", ports.Winsize{}, strings.NewReader(""), io.Discard, nil); err == nil ||
		!strings.Contains(err.Error(), "refused") {
		t.Fatalf("recusa: %v", err)
	}
	bad := &Client{Dial: func(context.Context, string) (net.Conn, error) { return nil, errors.New("sem rota") }}
	if _, err := bad.Interactive(context.Background(), "x", "", ports.Winsize{}, nil, io.Discard, nil); err == nil || !strings.Contains(err.Error(), "sem rota") {
		t.Fatalf("dial: %v", err)
	}
	ctx, cancel := context.WithTimeout(context.Background(), 100*time.Millisecond)
	defer cancel()
	if _, err := shellClient(&fakeShell{mute: true}).Interactive(ctx, "x", "", ports.Winsize{}, nil, io.Discard, nil); !errors.Is(err, context.DeadlineExceeded) {
		t.Fatalf("adbd mudo: %v", err)
	}
	// Teclado aberto, ctx cancelado no meio: o shell volta com o erro do ctx.
	inR, _ := io.Pipe()
	ctx2, cancel2 := context.WithCancel(context.Background())
	go func() { time.Sleep(50 * time.Millisecond); cancel2() }()
	if _, err := shellClient(&fakeShell{}).Interactive(ctx2, "x", "", ports.Winsize{}, inR, io.Discard, nil); !errors.Is(err, context.Canceled) {
		t.Fatalf("cancelado: %v", err)
	}
}

// TestStreamAuthAndClose: adbd que pede chave é recusado no Open; Close repetido não quebra; Write depois do Close
// volta com erro em vez de travar esperando um OKAY que não vem.
func TestStreamAuthAndClose(t *testing.T) {
	if _, err := clientFor(&fakeAdbd{auth: true}).Open(context.Background(), "x", "shell:"); !errors.Is(err, ErrAuth) {
		t.Fatalf("auth: %v", err)
	}
	s, err := clientFor(&fakeAdbd{noClose: true}).Open(context.Background(), "x", "shell:")
	if err != nil {
		t.Fatal(err)
	}
	s.Close()
	s.Close()
	if _, err := s.Write([]byte("x")); err == nil {
		t.Fatal("write depois do close passou")
	}
}
