package adb

import (
	"context"
	"encoding/binary"
	"errors"
	"fmt"
	"io"
	"net"
	"strconv"
	"sync"

	"runner.codes/internal/ports"
)

// Stream é um serviço do adbd aberto nos dois sentidos: o que o aparelho escreve sai no Read, o que se escreve no Write
// chega ao serviço. É o que o terminal com PTY e o vídeo pedem — o Exec só lê a saída inteira no fim.
//
// Controle de fluxo do protocolo: cada WRTE nosso espera o OKAY do adbd antes do próximo (um WRTE em voo por stream,
// como o adb do AOSP faz), e cada WRTE do adbd é confirmado com OKAY só depois que o leitor consumiu os bytes — leitor
// lento segura o aparelho em vez de encher a memória do servidor.
type Stream struct {
	conn    net.Conn
	remote  uint32
	maxData int
	// wmu deixa um Write por vez (um WRTE em voo); cmu serializa os pacotes na conexão (o OKAY do leitor e o WRTE do
	// escritor saem de goroutines diferentes e não podem se misturar no meio de um cabeçalho).
	wmu, cmu sync.Mutex
	okay     chan struct{}
	pr       *io.PipeReader
	pw       *io.PipeWriter
	done     chan struct{}
	once     sync.Once
}

// Open abre o serviço e liga o leitor. O stream vive até Close, até o ctx acabar ou até o adbd fechar.
func (c *Client) Open(ctx context.Context, addr, service string) (*Stream, error) {
	dial := c.Dial
	if dial == nil {
		dial = func(ctx context.Context, addr string) (net.Conn, error) {
			var d net.Dialer
			return d.DialContext(ctx, "tcp", addr)
		}
	}
	conn, err := dial(ctx, addr)
	if err != nil {
		return nil, fmt.Errorf("adb: dial %s: %w", addr, err)
	}
	// Até o OKAY do OPEN, o ctx também limita a espera (adbd mudo não segura o chamador para sempre).
	stop := context.AfterFunc(ctx, func() { conn.Close() })
	s, err := handshake(conn, service)
	if err != nil {
		stop()
		conn.Close()
		if ctx.Err() != nil {
			return nil, fmt.Errorf("adb: %s: %w", service, ctx.Err())
		}
		return nil, err
	}
	go s.readLoop()
	// Daqui em diante o ctx encerra o stream inteiro: o AfterFunc segue armado e fecha a conexão.
	go func() {
		<-s.done
		stop()
	}()
	return s, nil
}

// handshake faz o CNXN e o OPEN numa conexão nova e devolve o stream com o id remoto e o teto de payload do adbd.
func handshake(conn net.Conn, service string) (*Stream, error) {
	if err := write(conn, packet{cmd: cmdCNXN, arg0: version, arg1: maxPayload, data: []byte("host::\x00")}); err != nil {
		return nil, fmt.Errorf("adb: handshake: %w", err)
	}
	maxData := maxPayload
	for {
		p, err := read(conn)
		if err != nil {
			return nil, fmt.Errorf("adb: handshake: %w", err)
		}
		if p.cmd == cmdAUTH {
			return nil, ErrAuth
		}
		if p.cmd == cmdCNXN {
			// arg1 do CNXN do adbd é o maior payload que ele aceita; 0 ou maior que o nosso fica com o nosso.
			if p.arg1 > 0 && p.arg1 < maxPayload {
				maxData = int(p.arg1)
			}
			break
		}
	}
	if err := write(conn, packet{cmd: cmdOPEN, arg0: localID, data: append([]byte(service), 0)}); err != nil {
		return nil, fmt.Errorf("adb: open %s: %w", service, err)
	}
	for {
		p, err := read(conn)
		if err != nil {
			return nil, fmt.Errorf("adb: open %s: %w", service, err)
		}
		switch p.cmd {
		case cmdOKAY:
			pr, pw := io.Pipe()
			return &Stream{conn: conn, remote: p.arg0, maxData: maxData, okay: make(chan struct{}, 1), pr: pr, pw: pw,
				done: make(chan struct{})}, nil
		case cmdCLSE:
			return nil, fmt.Errorf("adb: service %q refused by adbd", service)
		}
	}
}

// send escreve um pacote inteiro na conexão, sem misturar com o de outra goroutine.
func (s *Stream) send(p packet) error {
	s.cmu.Lock()
	defer s.cmu.Unlock()
	return write(s.conn, p)
}

// readLoop entrega os WRTE do adbd ao Read (confirmando cada um depois de consumido), avisa o Write de cada OKAY e
// encerra no CLSE ou no erro da conexão.
func (s *Stream) readLoop() {
	defer s.Close()
	for {
		p, err := read(s.conn)
		if err != nil {
			if errors.Is(err, io.EOF) || errors.Is(err, net.ErrClosed) {
				s.pw.Close()
			} else {
				s.pw.CloseWithError(err)
			}
			return
		}
		switch p.cmd {
		case cmdWRTE:
			if _, err := s.pw.Write(p.data); err != nil {
				return
			}
			if s.send(packet{cmd: cmdOKAY, arg0: localID, arg1: s.remote}) != nil {
				return
			}
		case cmdOKAY:
			select {
			case s.okay <- struct{}{}:
			default:
			}
		case cmdCLSE:
			s.send(packet{cmd: cmdCLSE, arg0: localID, arg1: s.remote})
			s.pw.Close()
			return
		}
	}
}

// Read lê o que o serviço escreveu; io.EOF quando o adbd fecha o stream.
func (s *Stream) Read(p []byte) (int, error) { return s.pr.Read(p) }

// Write manda os bytes ao serviço em WRTEs do tamanho que o adbd aceita, esperando o OKAY de cada um. Uma chamada de
// Write chega inteira e em ordem, sem outro Write no meio (o shell v2 depende disso para não partir um pacote).
func (s *Stream) Write(p []byte) (int, error) {
	s.wmu.Lock()
	defer s.wmu.Unlock()
	n := 0
	for len(p) > 0 {
		chunk := p
		if len(chunk) > s.maxData {
			chunk = chunk[:s.maxData]
		}
		if err := s.send(packet{cmd: cmdWRTE, arg0: localID, arg1: s.remote, data: chunk}); err != nil {
			return n, err
		}
		select {
		case <-s.okay:
		case <-s.done:
			return n, io.ErrClosedPipe
		}
		n += len(chunk)
		p = p[len(chunk):]
	}
	return n, nil
}

// Close fecha o stream e a conexão (de qualquer lado, quantas vezes for).
func (s *Stream) Close() error {
	s.once.Do(func() {
		close(s.done)
		s.conn.Close()
		s.pw.Close()
	})
	return nil
}

// Ids dos pacotes do shell v2 do adbd (system/core/adb/shell_protocol.h): cada pacote é o id (1 byte), o tamanho
// (4 bytes little-endian) e os dados.
const (
	shellStdin      = 0
	shellStdout     = 1
	shellStderr     = 2
	shellExit       = 3
	shellCloseStdin = 4
	shellWindowSize = 5
)

// shellPacket monta um pacote do shell v2.
func shellPacket(id byte, data []byte) []byte {
	b := make([]byte, 5+len(data))
	b[0] = id
	binary.LittleEndian.PutUint32(b[1:], uint32(len(data)))
	copy(b[5:], data)
	return b
}

// windowSize é o pacote de tamanho do shell v2: "linhasxcolunas,xpixelsxypixels" (o adbd lê com sscanf; os pixels
// vão zerados, como o adb do AOSP manda quando não sabe).
func windowSize(w ports.Winsize) []byte {
	return shellPacket(shellWindowSize, []byte(strconv.Itoa(int(w.Rows))+"x"+strconv.Itoa(int(w.Cols))+",0x0\x00"))
}

// Interactive liga um shell com PTY do Android (shell v2) a in/out até o programa sair, repassando cada resize, e
// devolve o código de saída. cmd vazio é o shell de login; o PTY faz top, vi e as setas funcionarem, coisa que o
// modo shell antigo (sem pacote de tamanho) não deixa.
func (c *Client) Interactive(ctx context.Context, addr, cmd string, size ports.Winsize, in io.Reader, out io.Writer, resize <-chan ports.Winsize) (int, error) {
	s, err := c.Open(ctx, addr, "shell,v2,TERM=xterm-256color,pty:"+cmd)
	if err != nil {
		return 0, err
	}
	defer s.Close()
	if _, err := s.Write(windowSize(size)); err != nil {
		return 0, fmt.Errorf("adb: shell window size: %w", err)
	}
	// Teclado: cada pedaço lido vira um pacote stdin; o fim do teclado fecha o stdin do lado de lá.
	go func() {
		buf := make([]byte, 4096)
		for {
			n, err := in.Read(buf)
			if n > 0 {
				if _, werr := s.Write(shellPacket(shellStdin, buf[:n])); werr != nil {
					return
				}
			}
			if err != nil {
				s.Write(shellPacket(shellCloseStdin, nil))
				return
			}
		}
	}()
	go func() {
		for {
			select {
			case w, ok := <-resize:
				if !ok {
					return
				}
				if _, err := s.Write(windowSize(w)); err != nil {
					return
				}
			case <-s.done:
				return
			}
		}
	}()
	code, err := readShell(s, out)
	// Cancelado, a conexão fecha e o leitor vê um EOF que parece fim normal: quem decide é o ctx.
	if ctx.Err() != nil {
		return code, fmt.Errorf("adb: shell: %w", ctx.Err())
	}
	return code, err
}

// readShell lê os pacotes do shell v2 até o adbd fechar: stdout e stderr vão juntos para out (num PTY o stderr já chega
// misturado no stdout), e o pacote de saída traz o código. Fechar entre pacotes é fim normal (código 0 se o de saída
// não veio); fechar no meio de um pacote é erro.
func readShell(r io.Reader, out io.Writer) (int, error) {
	hdr := make([]byte, 5)
	code, exited := 0, false
	for {
		if _, err := io.ReadFull(r, hdr); err != nil {
			if exited || errors.Is(err, io.EOF) {
				return code, nil
			}
			return code, fmt.Errorf("adb: shell: %w", err)
		}
		n := binary.LittleEndian.Uint32(hdr[1:])
		if n > maxPayload*4 {
			return code, fmt.Errorf("adb: shell packet of %d bytes is over the limit", n)
		}
		data := make([]byte, n)
		if _, err := io.ReadFull(r, data); err != nil {
			return code, fmt.Errorf("adb: shell: %w", err)
		}
		switch hdr[0] {
		case shellStdout, shellStderr:
			if _, err := out.Write(data); err != nil {
				return code, err
			}
		case shellExit:
			if len(data) > 0 {
				code = int(data[0])
			}
			exited = true
		}
	}
}
