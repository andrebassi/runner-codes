package webterm

import (
	"context"
	"encoding/json"
	"errors"
	"io"
	"net"
	"net/http"
	"net/http/httptest"
	"strconv"
	"sync"
	"testing"
	"time"

	"runner.codes/internal/ports"
)

// echoGuest é o "guest": um servidor de eco em 127.0.0.1 numa porta livre, que o dialer falso disca.
func echoGuest(t *testing.T) (port uint16, dial PortDialer) {
	t.Helper()
	ln, err := net.Listen("tcp", "127.0.0.1:0")
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
			go func() { defer c.Close(); io.Copy(c, c) }()
		}
	}()
	addr := ln.Addr().String()
	return uint16(ln.Addr().(*net.TCPAddr).Port), func(ctx context.Context, _ uint16) (net.Conn, error) {
		return (&net.Dialer{}).DialContext(ctx, "tcp", addr)
	}
}

// freePort devolve um número de porta livre no host (para fazer de "porta do guest").
func freePort(t *testing.T) uint16 {
	t.Helper()
	ln, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	defer ln.Close()
	return uint16(ln.Addr().(*net.TCPAddr).Port)
}

// lister é um PortLister controlável pelo teste.
type lister struct {
	mu    sync.Mutex
	ports []uint16
	err   error
}

// list devolve o estado atual.
func (l *lister) list(context.Context) ([]uint16, error) {
	l.mu.Lock()
	defer l.mu.Unlock()
	return append([]uint16(nil), l.ports...), l.err
}

// set troca as portas e o erro.
func (l *lister) set(p []uint16, err error) { l.mu.Lock(); l.ports, l.err = p, err; l.mu.Unlock() }

// roundTrip conecta no host, manda "oi" e espera o eco.
func roundTrip(t *testing.T, hostPort int) error {
	t.Helper()
	c, err := net.DialTimeout("tcp", "127.0.0.1:"+strconv.Itoa(hostPort), time.Second)
	if err != nil {
		return err
	}
	defer c.Close()
	c.SetDeadline(time.Now().Add(2 * time.Second))
	if _, err := c.Write([]byte("oi")); err != nil {
		return err
	}
	buf := make([]byte, 2)
	if _, err := io.ReadFull(c, buf); err != nil {
		return err
	}
	if string(buf) != "oi" {
		return errors.New("eco errado: " + string(buf))
	}
	return nil
}

// TestForwarderLifecycle: a porta aparece com o mesmo número no host e o tráfego vai e volta; erro de consulta não
// mexe em nada; quando some do guest, o listener do host fecha; o fim do contexto fecha tudo.
func TestForwarderLifecycle(t *testing.T) {
	_, dial := echoGuest(t)
	guest := freePort(t)
	l := &lister{ports: []uint16{guest}}
	f := NewForwarder(l.list, dial, time.Hour)
	ctx, cancel := context.WithCancel(context.Background())

	f.sync(ctx)
	got := f.Ports()
	if len(got) != 1 || got[0] != (Forward{Guest: guest, Host: int(guest)}) {
		t.Fatalf("ports %+v", got)
	}
	if err := roundTrip(t, int(guest)); err != nil {
		t.Fatalf("tráfego: %v", err)
	}

	l.set(nil, errors.New("máquina parando"))
	f.sync(ctx)
	if len(f.Ports()) != 1 {
		t.Fatal("erro de consulta fechou a porta")
	}

	l.set(nil, nil)
	f.sync(ctx)
	if len(f.Ports()) != 0 {
		t.Fatal("porta que sumiu do guest continuou aberta")
	}
	if err := roundTrip(t, int(guest)); err == nil {
		t.Fatal("listener do host continuou aceitando")
	}

	l.set([]uint16{guest}, nil)
	done := make(chan struct{})
	go func() { f.Run(ctx); close(done) }()
	deadline := time.Now().Add(2 * time.Second)
	for len(f.Ports()) == 0 && time.Now().Before(deadline) {
		time.Sleep(10 * time.Millisecond)
	}
	cancel()
	<-done
	if len(f.Ports()) != 0 {
		t.Fatal("Run saiu sem fechar os listeners")
	}
}

// TestForwarderBusyHostPort: com o número ocupado no host, usa outra porta livre e mostra a troca.
func TestForwarderBusyHostPort(t *testing.T) {
	_, dial := echoGuest(t)
	busy, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	defer busy.Close()
	guest := uint16(busy.Addr().(*net.TCPAddr).Port)
	f := NewForwarder((&lister{ports: []uint16{guest}}).list, dial, time.Hour)
	f.sync(context.Background())
	defer f.closeAll()
	got := f.Ports()
	if len(got) != 1 || got[0].Guest != guest || got[0].Host == int(guest) || got[0].Host == 0 {
		t.Fatalf("ports %+v", got)
	}
	if err := roundTrip(t, got[0].Host); err != nil {
		t.Fatalf("tráfego pela porta trocada: %v", err)
	}
}

// TestForwarderNoPortAtAll: se nem a porta livre abre, a porta do guest fica de fora (e a próxima consulta tenta de novo).
func TestForwarderNoPortAtAll(t *testing.T) {
	f := NewForwarder((&lister{ports: []uint16{80}}).list, nil, time.Hour)
	f.listen = func(int) (net.Listener, error) { return nil, errors.New("sem porta") }
	f.sync(context.Background())
	if len(f.Ports()) != 0 {
		t.Fatalf("ports %+v", f.Ports())
	}
}

// TestForwarderGuestRefuses: porta que fechou no guest derruba só a conexão do host, sem travar.
func TestForwarderGuestRefuses(t *testing.T) {
	guest := freePort(t)
	refuse := func(context.Context, uint16) (net.Conn, error) { return nil, errors.New("connection refused") }
	f := NewForwarder((&lister{ports: []uint16{guest}}).list, refuse, time.Hour)
	f.sync(context.Background())
	defer f.closeAll()
	c, err := net.DialTimeout("tcp", "127.0.0.1:"+strconv.Itoa(int(guest)), time.Second)
	if err != nil {
		t.Fatal(err)
	}
	defer c.Close()
	c.SetReadDeadline(time.Now().Add(2 * time.Second))
	if _, err := c.Read(make([]byte, 1)); err == nil {
		t.Fatal("conexão continuou aberta com o guest recusando")
	}
}

// TestPortsRoute: /ports exige token e devolve os encaminhamentos; sem Forwarder, lista vazia.
func TestPortsRoute(t *testing.T) {
	_, dial := echoGuest(t)
	guest := freePort(t)
	f := NewForwarder((&lister{ports: []uint16{guest}}).list, dial, time.Hour)
	f.sync(context.Background())
	defer f.closeAll()
	block := func(ctx context.Context, _ ports.Winsize, _ io.Reader, _ io.Writer, _ <-chan ports.Winsize) (int, error) {
		<-ctx.Done()
		return 0, nil
	}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	for _, c := range []struct {
		fw   *Forwarder
		want int
	}{{f, 1}, {nil, 0}} {
		srv := httptest.NewServer(Handler(hubOf(Start(ctx, block, ports.Winsize{}), nil, c.fw)))
		if resp, _ := http.Get(srv.URL + "/ports"); resp.StatusCode != http.StatusForbidden {
			t.Fatalf("sem token: %d", resp.StatusCode)
		}
		resp, err := http.Get(srv.URL + "/ports?t=" + tok)
		if err != nil {
			t.Fatal(err)
		}
		var got []Forward
		json.NewDecoder(resp.Body).Decode(&got)
		resp.Body.Close()
		srv.Close()
		if got == nil || len(got) != c.want {
			t.Fatalf("fw=%v: %+v", c.fw != nil, got)
		}
	}
}
