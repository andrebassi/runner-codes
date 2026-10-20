package guestproto

import (
	"context"
	"fmt"
	"net"

	"runner.code/internal/protocol"
)

// DialPort abre uma conexão TCP até 127.0.0.1:port dentro do guest, pelo vsock: manda o
// OpDial, espera o Result e devolve a conexão já virando tubo de bytes. É o transporte das
// portas publicadas.
func DialPort(ctx context.Context, socket string, port uint16) (net.Conn, error) {
	return dial(ctx, socket, protocol.Job{TraceID: "dial", Op: protocol.OpDial, Port: port}, fmt.Sprintf("guest port %d", port))
}

// DialUnix abre uma conexão até o socket Unix path dentro do guest, pelo vsock (a ponte do
// docker.sock: o cliente do host fala com o daemon de dentro).
func DialUnix(ctx context.Context, socket, path string) (net.Conn, error) {
	return dial(ctx, socket, protocol.Job{TraceID: "dial", Op: protocol.OpDial, Path: path}, "guest socket "+path)
}

// dial manda o OpDial e devolve a conexão já virando tubo de bytes.
func dial(ctx context.Context, socket string, job protocol.Job, what string) (net.Conn, error) {
	d := net.Dialer{Timeout: dialTimeout}
	conn, err := d.DialContext(ctx, "unix", socket)
	if err != nil {
		return nil, fmt.Errorf("connect to guest: %w", err)
	}
	if err := protocol.WriteFrame(conn, job); err != nil {
		conn.Close()
		return nil, err
	}
	var res protocol.Result
	if err := protocol.ReadFrame(conn, &res); err != nil {
		conn.Close()
		return nil, err
	}
	if res.Status != "success" {
		conn.Close()
		return nil, fmt.Errorf("%s: %s", what, res.Error)
	}
	return vsockConn{conn}, nil
}

// vsockConn é a conexão pelo vsock da libkrun, sem meio-fechamento: o shutdown de escrita no
// socket do host derruba a conexão inteira do lado do guest, e a resposta se perde (medido:
// cliente que manda e faz SHUT_WR recebia vazio; sem o SHUT_WR, a resposta inteira). Quem
// copia nos dois sentidos chama CloseWrite e segue lendo; o fechamento de verdade vem no fim.
// Custo declarado: servidor do guest que espera o EOF do cliente antes de responder não o
// recebe por aqui.
type vsockConn struct{ net.Conn }

// CloseWrite não repassa o meio-fechamento (veja vsockConn).
func (vsockConn) CloseWrite() error { return nil }
