package guestproto

import (
	"context"
	"errors"
	"fmt"
	"io"
	"net"
	"sync"

	"runner.codes/internal/ports"
	"runner.codes/internal/protocol"
)

// eot é o Ctrl+D: quando a entrada do host acaba (stdin por pipe), vai para o terminal do
// guest como fim de arquivo, e o shell interativo sai em vez de ficar esperando para sempre.
const eot = 0x04

// Interactive abre a sessão de terminal: manda o job OpPTY, depois repassa cada leitura de
// in como frame `input` e cada resize como frame `resize`, e escreve em out cada `output`
// que volta, até o `exit` com o código. O ctx cancelado fecha a conexão (e o guest mata o
// programa).
func (Client) Interactive(ctx context.Context, socket string, job protocol.Job, in io.Reader, out io.Writer, resize <-chan ports.Winsize) (int, error) {
	job.Op = protocol.OpPTY
	d := net.Dialer{Timeout: dialTimeout}
	conn, err := d.DialContext(ctx, "unix", socket)
	if err != nil {
		return 0, fmt.Errorf("connect to guest: %w", err)
	}
	defer conn.Close()
	stop := context.AfterFunc(ctx, func() { conn.Close() })
	defer stop()
	var mu sync.Mutex
	send := func(ev protocol.Event) error {
		mu.Lock()
		defer mu.Unlock()
		return protocol.WriteFrame(conn, ev)
	}
	if err := protocol.WriteFrame(conn, job); err != nil {
		return 0, ctxError(ctx, fmt.Errorf("send job: %w", err))
	}
	go func() {
		buf := make([]byte, 4<<10)
		for {
			n, err := in.Read(buf)
			if n > 0 && send(protocol.Event{Type: protocol.EventInput, Bytes: append([]byte(nil), buf[:n]...)}) != nil {
				return
			}
			if err != nil {
				if errors.Is(err, io.EOF) {
					send(protocol.Event{Type: protocol.EventInput, Bytes: []byte{eot}})
				}
				return
			}
		}
	}()
	go func() {
		for ws := range resize {
			if send(protocol.Event{Type: protocol.EventResize, Rows: ws.Rows, Cols: ws.Cols}) != nil {
				return
			}
		}
	}()
	for {
		var ev protocol.Event
		if err := protocol.ReadFrame(conn, &ev); err != nil {
			return 0, ctxError(ctx, fmt.Errorf("read session: %w", err))
		}
		switch ev.Type {
		case protocol.EventOutput:
			if _, err := out.Write(ev.Bytes); err != nil {
				return 0, err
			}
		case protocol.EventExit:
			return ev.Code, nil
		default:
			return 0, fmt.Errorf("read session: unexpected event %q", ev.Type)
		}
	}
}
