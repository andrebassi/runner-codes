package podnet

import (
	"bytes"
	"encoding/binary"
	"io"
	"net"
	"net/netip"
	"strings"
	"testing"
	"time"
)

// TestInfoEnv: o guest recebe IP/prefixo e gateway do pod; sem gateway (rede só local), só o IP.
func TestInfoEnv(t *testing.T) {
	i := Info{Addr: netip.MustParsePrefix("10.42.0.7/24"), Gateway: netip.MustParseAddr("10.42.0.1")}
	if got := strings.Join(i.Env(), " "); got != "RUNNER_NET_ADDR=10.42.0.7/24 RUNNER_NET_GATEWAY=10.42.0.1" {
		t.Fatalf("env %q", got)
	}
	if len((Info{Addr: i.Addr}).Env()) != 1 {
		t.Fatal("sem gateway, só o endereço")
	}
}

// frame monta um quadro como a libkrun manda: tamanho big-endian e corpo.
func frame(b []byte) []byte {
	out := make([]byte, 4+len(b))
	binary.BigEndian.PutUint32(out, uint32(len(b)))
	copy(out[4:], b)
	return out
}

// TestPumpBothWays: quadro do TAP chega ao socket com o prefixo; quadro do socket chega ao TAP sem
// ele; e fechar um lado encerra a bomba com o motivo.
func TestPumpBothWays(t *testing.T) {
	tapHost, tapPump := net.Pipe()
	sockHost, sockPump := net.Pipe()
	done := make(chan error, 1)
	go func() { done <- Pump(tapPump, sockPump) }()
	for _, c := range []net.Conn{tapHost, sockHost} {
		c.SetDeadline(time.Now().Add(5 * time.Second))
	}
	go tapHost.Write([]byte("from-tap"))
	got := make([]byte, 12)
	if _, err := io.ReadFull(sockHost, got); err != nil || !bytes.Equal(got, frame([]byte("from-tap"))) {
		t.Fatalf("socket recebeu %q (%v)", got, err)
	}
	go sockHost.Write(frame([]byte("from-vm")))
	buf := make([]byte, 64)
	n, err := tapHost.Read(buf)
	if err != nil || string(buf[:n]) != "from-vm" {
		t.Fatalf("tap recebeu %q (%v)", buf[:n], err)
	}
	sockHost.Close()
	select {
	case err := <-done:
		if err == nil {
			t.Fatal("bomba terminou sem motivo")
		}
	case <-time.After(5 * time.Second):
		t.Fatal("bomba não parou com o socket fechado")
	}
}

// TestPumpBadFrame: prefixo zero ou maior que o teto é fluxo corrompido e para a bomba, e quadro
// truncado também.
func TestPumpBadFrame(t *testing.T) {
	for _, bad := range [][]byte{{0, 0, 0, 0}, {0xff, 0xff, 0xff, 0xff}, {0, 0, 0, 9, 'x'}} {
		tapHost, tapPump := net.Pipe()
		sockHost, sockPump := net.Pipe()
		done := make(chan error, 1)
		go func() { done <- Pump(tapPump, sockPump) }()
		go func() { sockHost.Write(bad); sockHost.Close() }()
		select {
		case err := <-done:
			if err == nil || !strings.Contains(err.Error(), "vm") {
				t.Fatalf("%v: %v", bad, err)
			}
		case <-time.After(5 * time.Second):
			t.Fatalf("%v: bomba não parou", bad)
		}
		tapHost.Close()
	}
}

// failWriter aceita leitura do TAP e falha toda escrita: o caminho de erro de cada sentido.
type failWriter struct{ io.Reader }

// Write sempre falha.
func (failWriter) Write([]byte) (int, error) { return 0, io.ErrClosedPipe }

// TestPumpWriteErrors: escrita que falha no socket ou no TAP para a bomba com o lado certo no erro.
func TestPumpWriteErrors(t *testing.T) {
	err := Pump(failWriter{strings.NewReader("quadro")}, failWriter{blockingReader{}})
	if err == nil || !strings.Contains(err.Error(), "vm write") {
		t.Fatalf("socket: %v", err)
	}
	err = Pump(failWriter{blockingReader{}}, failWriter{bytes.NewReader(frame([]byte("q")))})
	if err == nil || !strings.Contains(err.Error(), "tap write") {
		t.Fatalf("tap: %v", err)
	}
}

// blockingReader nunca entrega nada e devolve EOF quando acaba a paciência: o outro sentido decide.
type blockingReader struct{}

// Read espera um pouco e devolve EOF.
func (blockingReader) Read([]byte) (int, error) {
	time.Sleep(200 * time.Millisecond)
	return 0, io.EOF
}
