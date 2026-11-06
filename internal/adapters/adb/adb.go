// Package adb fala o protocolo do ADB com o adbd de uma máquina Android (Redroid), sem o binário adb no servidor:
// handshake (CNXN), um stream por comando (OPEN/WRTE/OKAY/CLSE) e a saída inteira de volta.
//
// Uma conexão TCP por comando: o handshake custa um ida-e-volta no loopback, e um transporte compartilhado entre
// comandos concorrentes exigiria multiplexar streams — complexidade que nada aqui pede ainda. O adbd do Redroid não
// pede autenticação (ro.adb.secure=0); um adbd que peça (AUTH) é recusado com um erro que diz isso.
package adb

import (
	"bytes"
	"context"
	"encoding/binary"
	"errors"
	"fmt"
	"io"
	"net"
	"os"
	"strings"
	"time"
)

// Comandos do protocolo (os quatro bytes ASCII lidos em little-endian, como no adb.h do AOSP).
const (
	cmdCNXN = 0x4e584e43
	cmdAUTH = 0x48545541
	cmdOPEN = 0x4e45504f
	cmdOKAY = 0x59414b4f
	cmdWRTE = 0x45545257
	cmdCLSE = 0x45534c43
)

// version é a versão do protocolo que este cliente anuncia: a 0x01000001 dispensa a soma de verificação, mas ela vai
// assim mesmo (adbd antigo confere). maxPayload é o teto de um WRTE (256 KiB, o do adb atual).
const (
	version    = 0x01000001
	maxPayload = 256 * 1024
)

// localID é o id do nosso lado do único stream de cada conexão; qualquer valor diferente de 0 serve.
const localID = 1

// ErrAuth é a resposta de um adbd que exige chave (ro.adb.secure=1): este cliente não autentica.
var ErrAuth = errors.New("adbd asked for authentication (ro.adb.secure=1); this client does not authenticate")

// Client fala com um adbd por TCP.
type Client struct {
	// Dial abre a conexão; nil usa net.Dialer. Os testes trocam por um adbd falso.
	Dial func(ctx context.Context, addr string) (net.Conn, error)
	// Timeout é o prazo de cada comando inteiro quando o ctx não traz um; 0 é 30 s.
	Timeout time.Duration
}

// packet é uma mensagem do protocolo: cabeçalho de 24 bytes e o payload.
type packet struct {
	cmd, arg0, arg1 uint32
	data            []byte
}

// write manda um pacote: comando, argumentos, tamanho, soma dos bytes e o comando invertido (o "magic").
func write(w io.Writer, p packet) error {
	var sum uint32
	for _, b := range p.data {
		sum += uint32(b)
	}
	hdr := make([]byte, 24)
	binary.LittleEndian.PutUint32(hdr[0:], p.cmd)
	binary.LittleEndian.PutUint32(hdr[4:], p.arg0)
	binary.LittleEndian.PutUint32(hdr[8:], p.arg1)
	binary.LittleEndian.PutUint32(hdr[12:], uint32(len(p.data)))
	binary.LittleEndian.PutUint32(hdr[16:], sum)
	binary.LittleEndian.PutUint32(hdr[20:], p.cmd^0xffffffff)
	_, err := w.Write(append(hdr, p.data...))
	return err
}

// read lê um pacote e confere o magic e o tamanho (um tamanho acima do teto é lixo, não um WRTE legítimo).
func read(r io.Reader) (packet, error) {
	hdr := make([]byte, 24)
	if _, err := io.ReadFull(r, hdr); err != nil {
		return packet{}, err
	}
	p := packet{cmd: binary.LittleEndian.Uint32(hdr[0:]), arg0: binary.LittleEndian.Uint32(hdr[4:]),
		arg1: binary.LittleEndian.Uint32(hdr[8:])}
	n := binary.LittleEndian.Uint32(hdr[12:])
	if binary.LittleEndian.Uint32(hdr[20:]) != p.cmd^0xffffffff {
		return packet{}, fmt.Errorf("adb: bad packet magic for command %#x", p.cmd)
	}
	if n > maxPayload*4 {
		return packet{}, fmt.Errorf("adb: payload of %d bytes is over the limit", n)
	}
	p.data = make([]byte, n)
	if _, err := io.ReadFull(r, p.data); err != nil {
		return packet{}, err
	}
	return p, nil
}

// Exec roda um serviço do adbd e devolve tudo o que ele escreveu até fechar o stream. service é o que vai no OPEN:
// "exec:<cmd>" para saída binária intacta (screencap, cat), "shell:<cmd>" para um shell com o stderr junto.
func (c *Client) Exec(ctx context.Context, addr, service string) ([]byte, error) {
	if _, ok := ctx.Deadline(); !ok {
		t := c.Timeout
		if t <= 0 {
			t = 30 * time.Second
		}
		var cancel context.CancelFunc
		ctx, cancel = context.WithTimeout(ctx, t)
		defer cancel()
	}
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
	defer conn.Close()
	if dl, ok := ctx.Deadline(); ok {
		conn.SetDeadline(dl)
	}
	// Cancelado o ctx sem prazo novo (o chamador desistiu), a conexão fecha e os reads destravam.
	stop := context.AfterFunc(ctx, func() { conn.Close() })
	defer stop()
	out, err := run(conn, service)
	if err != nil && ctx.Err() != nil {
		return out, fmt.Errorf("adb: %s: %w", service, ctx.Err())
	}
	// O prazo da conexão é o do ctx: os dois vencem juntos, e o read pode voltar com "i/o timeout" antes de o ctx se
	// marcar vencido (corrida medida no TestExecDeadline sob carga, 06/11/2026). Pelo tipo do erro, é o prazo do ctx.
	if err != nil && errors.Is(err, os.ErrDeadlineExceeded) {
		return out, fmt.Errorf("adb: %s: %w", service, context.DeadlineExceeded)
	}
	return out, err
}

// run é o diálogo de um comando numa conexão já aberta: handshake, OPEN, os WRTE (cada um confirmado com OKAY) até o
// CLSE do adbd.
func run(conn io.ReadWriter, service string) ([]byte, error) {
	if err := write(conn, packet{cmd: cmdCNXN, arg0: version, arg1: maxPayload, data: []byte("host::\x00")}); err != nil {
		return nil, fmt.Errorf("adb: handshake: %w", err)
	}
	for {
		p, err := read(conn)
		if err != nil {
			return nil, fmt.Errorf("adb: handshake: %w", err)
		}
		if p.cmd == cmdAUTH {
			return nil, ErrAuth
		}
		if p.cmd == cmdCNXN {
			break
		}
		// Qualquer outra coisa antes do CNXN é descartada (o adbd não deveria mandar nada).
	}
	if err := write(conn, packet{cmd: cmdOPEN, arg0: localID, data: append([]byte(service), 0)}); err != nil {
		return nil, fmt.Errorf("adb: open %s: %w", service, err)
	}
	var out bytes.Buffer
	var remote uint32
	for {
		p, err := read(conn)
		if err != nil {
			if remote != 0 && errors.Is(err, io.EOF) {
				// adbd que fecha a conexão em vez de mandar CLSE: a saída que chegou vale.
				return out.Bytes(), nil
			}
			return out.Bytes(), fmt.Errorf("adb: %s: %w", service, err)
		}
		switch p.cmd {
		case cmdOKAY:
			remote = p.arg0
		case cmdWRTE:
			remote = p.arg0
			out.Write(p.data)
			if err := write(conn, packet{cmd: cmdOKAY, arg0: localID, arg1: remote}); err != nil {
				return out.Bytes(), fmt.Errorf("adb: %s: %w", service, err)
			}
		case cmdCLSE:
			if remote == 0 {
				// CLSE sem OKAY antes: o adbd recusou o serviço (nome inválido, comando que não existe).
				return nil, fmt.Errorf("adb: service %q refused by adbd", service)
			}
			write(conn, packet{cmd: cmdCLSE, arg0: localID, arg1: remote})
			return out.Bytes(), nil
		}
	}
}

// Shell roda um comando no shell do Android (sh -c) e devolve stdout e stderr juntos, sem o CR que o adbd põe nas
// quebras de linha do modo shell antigo.
func (c *Client) Shell(ctx context.Context, addr, cmd string) (string, error) {
	out, err := c.Exec(ctx, addr, "shell:"+cmd)
	return strings.ReplaceAll(string(out), "\r\n", "\n"), err
}

// WaitBoot espera o Android terminar de subir (sys.boot_completed=1), tentando a cada poll até o ctx acabar. Devolve
// o último erro (ou o último valor lido) quando o prazo vence, para o chamador explicar onde parou.
func (c *Client) WaitBoot(ctx context.Context, addr string, poll time.Duration) error {
	var last error
	for {
		tryCtx, cancel := context.WithTimeout(ctx, 10*time.Second)
		v, err := c.Shell(tryCtx, addr, "getprop sys.boot_completed")
		cancel()
		if err == nil && strings.TrimSpace(v) == "1" {
			return nil
		}
		switch {
		case err == nil:
			last = fmt.Errorf("sys.boot_completed=%q", strings.TrimSpace(v))
		case timedOut(err) && last != nil:
			// A tentativa morreu por prazo (o inteiro acabou no meio dela): o "i/o timeout" não diz nada, e o motivo
			// útil é o da tentativa anterior — o valor que a propriedade tinha, ou o adbd que recusava. O prazo da
			// conexão e o do ctx vencem juntos, então a decisão é pelo tipo do erro, não pelo ctx.Err() (corrida).
		default:
			last = err
		}
		select {
		case <-ctx.Done():
			return fmt.Errorf("android did not finish booting: %w", last)
		case <-time.After(poll):
		}
	}
}

// timedOut diz se o erro é de prazo: o do contexto ou o da conexão (deadline do socket).
func timedOut(err error) bool {
	var ne net.Error
	return errors.Is(err, context.DeadlineExceeded) || errors.Is(err, context.Canceled) ||
		errors.Is(err, os.ErrDeadlineExceeded) || (errors.As(err, &ne) && ne.Timeout())
}
