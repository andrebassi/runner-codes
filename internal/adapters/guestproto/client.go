// Package guestproto fala com o guest-runner pelo socket Unix que a libkrun liga à
// porta vsock do guest. O formato é o de internal/protocol.
package guestproto

import (
	"context"
	"errors"
	"fmt"
	"net"
	"os"
	"time"

	"runner.codes/internal/protocol"
)

// pollInterval é o intervalo entre tentativas de prontidão: 1 ms no
// primeiro segundo de boot. O boot medido é ~260 ms, então intervalo
// maior somaria latência direta ao primeiro job.
const pollInterval = time.Millisecond

// dialTimeout limita cada tentativa de conexão no socket local; nada legítimo demora mais.
const dialTimeout = 50 * time.Millisecond

// readyProbe é o job usado para provar prontidão: `true` em sh existe em toda imagem
// (Alpine e Debian) e não escreve nada.
var readyProbe = protocol.Job{TraceID: "ready-probe", Lang: "sh", Code: "true", Timeout: 5}

// Client conversa com um guest.
type Client struct{}

// Run envia um job e espera o resultado. O deadline vem do ctx: quando ele vence,
// a conexão é fechada e o erro volta; matar a VM é decisão de quem chamou.
func (Client) Run(ctx context.Context, socket string, job protocol.Job) (protocol.Result, error) {
	var res protocol.Result
	d := net.Dialer{Timeout: dialTimeout}
	conn, err := d.DialContext(ctx, "unix", socket)
	if err != nil {
		return res, fmt.Errorf("connect to guest: %w", err)
	}
	defer conn.Close()
	if dl, ok := ctx.Deadline(); ok {
		conn.SetDeadline(dl)
	}
	// Fecha a conexão se o ctx for cancelado antes do deadline (ex.: shutdown do serviço).
	stop := context.AfterFunc(ctx, func() { conn.Close() })
	defer stop()
	if err := protocol.WriteFrame(conn, job); err != nil {
		return res, ctxError(ctx, fmt.Errorf("send job: %w", err))
	}
	if err := protocol.ReadFrame(conn, &res); err != nil {
		return res, ctxError(ctx, fmt.Errorf("read result: %w", err))
	}
	return res, nil
}

// RunStream envia o job com streaming ligado e chama onOutput a cada pedaço de saída,
// na ordem em que o guest mandou, até o frame de resultado. Prazo e cancelamento valem
// como no Run. Frame de tipo desconhecido é erro: seria guest de outra versão.
func (Client) RunStream(ctx context.Context, socket string, job protocol.Job, onOutput func(stream, data string)) (protocol.Result, error) {
	job.Stream = true
	d := net.Dialer{Timeout: dialTimeout}
	conn, err := d.DialContext(ctx, "unix", socket)
	if err != nil {
		return protocol.Result{}, fmt.Errorf("connect to guest: %w", err)
	}
	defer conn.Close()
	if dl, ok := ctx.Deadline(); ok {
		conn.SetDeadline(dl)
	}
	stop := context.AfterFunc(ctx, func() { conn.Close() })
	defer stop()
	if err := protocol.WriteFrame(conn, job); err != nil {
		return protocol.Result{}, ctxError(ctx, fmt.Errorf("send job: %w", err))
	}
	for {
		var ev protocol.Event
		if err := protocol.ReadFrame(conn, &ev); err != nil {
			return protocol.Result{}, ctxError(ctx, fmt.Errorf("read stream: %w", err))
		}
		switch ev.Type {
		case protocol.EventStdout, protocol.EventStderr:
			onOutput(ev.Type, ev.Data)
		case protocol.EventResult:
			if ev.Result == nil {
				return protocol.Result{}, errors.New("read stream: result event without a result")
			}
			return *ev.Result, nil
		case "":
			// Frame sem tipo é o Result do guest anterior ao streaming: a imagem foi
			// construída com o guest antigo e precisa ser refeita.
			return protocol.Result{}, errors.New("read stream: the guest in this image does not support streaming; rebuild it with `rc image build`")
		default:
			return protocol.Result{}, fmt.Errorf("read stream: unknown event type %q", ev.Type)
		}
	}
}

// ctxError traduz falha de E/S causada pelo prazo ou pelo cancelamento no erro do ctx.
// O deadline do socket (SetDeadline) pode disparar um instante antes de o ctx marcar
// DeadlineExceeded; sem esta tradução o chamador recebe "i/o timeout" cru, não reconhece
// o timeout do job e devolve HTTP 500 (medido: teste intermitente em 1 de 5 rodadas).
func ctxError(ctx context.Context, err error) error {
	if errors.Is(err, os.ErrDeadlineExceeded) {
		return context.DeadlineExceeded
	}
	if ctx.Err() != nil {
		return ctx.Err()
	}
	return err
}

// WaitReady repete um job mínimo até o guest responder. Conectar não prova nada: a
// libkrun cria o socket antes de o guest escutar e aceita a conexão por ele (medido
// no spike). Só a resposta prova que o guest está pronto.
// DialPort é o DialPort do pacote como método: o que a porta GuestClient pede.
func (Client) DialPort(ctx context.Context, socket string, port uint16) (net.Conn, error) {
	return DialPort(ctx, socket, port)
}

func (c Client) WaitReady(ctx context.Context, socket string) error {
	for {
		attempt, cancel := context.WithTimeout(ctx, 2*time.Second)
		_, err := c.Run(attempt, socket, readyProbe)
		cancel()
		if err == nil {
			return nil
		}
		select {
		case <-ctx.Done():
			return fmt.Errorf("guest did not become ready: %w (last failure: %v)", ctx.Err(), err)
		case <-time.After(pollInterval):
		}
	}
}
