package webterm

import (
	"context"
	"encoding/json"
	"io"
	"net"
	"net/http"
	"net/http/httptest"
	"strconv"
	"strings"
	"testing"
	"time"

	"runner.codes/internal/ports"
)

// exitAfter é um PTY falso que escreve msg e sai com code quando lê qualquer coisa.
func exitAfter(msg string, code int) ShellFunc {
	return func(_ context.Context, _ ports.Winsize, in io.Reader, out io.Writer, _ <-chan ports.Winsize) (int, error) {
		out.Write([]byte(msg))
		in.Read(make([]byte, 16))
		return code, nil
	}
}

// getState lê /state.
func getState(t *testing.T, srv *httptest.Server) State {
	t.Helper()
	r, err := http.Get(srv.URL + "/state?t=" + tok)
	if err != nil {
		t.Fatal(err)
	}
	defer r.Body.Close()
	var st State
	if err := json.NewDecoder(r.Body).Decode(&st); err != nil {
		t.Fatal(err)
	}
	return st
}

// TestHubShellTabs: a aba principal vem no estado; POST /sessions abre um shell que é uma sessão própria (o teclado
// dele não vai para a principal), e o fim de cada uma entra na linha do tempo.
func TestHubShellTabs(t *testing.T) {
	main := Start(context.Background(), exitAfter("principal\n", 0), ports.Winsize{})
	h := hubOf(main, exitAfter("sou o shell\n", 3), nil)
	srv := httptest.NewServer(Handler(h))
	defer srv.Close()

	st := getState(t, srv)
	if st.Machine != "m1" || len(st.Sessions) != 1 || st.Sessions[0].Label != "main" || st.Ports == nil {
		t.Fatalf("estado inicial %+v", st)
	}
	r, err := http.Post(srv.URL+"/sessions?t="+tok, "text/plain", nil)
	if err != nil {
		t.Fatal(err)
	}
	var info TabInfo
	json.NewDecoder(r.Body).Decode(&info)
	r.Body.Close()
	if info.ID != "shell-1" || info.Label != "shell" {
		t.Fatalf("nova aba %+v", info)
	}
	// A tela do shell sai pela própria sessão.
	resp, err := http.Get(srv.URL + "/stream?s=shell-1&t=" + tok)
	if err != nil {
		t.Fatal(err)
	}
	readEvents(t, resp.Body, "sou o shell")
	resp.Body.Close()
	// Teclado no shell encerra o shell (código 3) e não a principal.
	http.Post(srv.URL+"/input?s=shell-1&t="+tok, "text/plain", strings.NewReader("x"))
	deadline := time.Now().Add(3 * time.Second)
	for time.Now().Before(deadline) {
		st = getState(t, srv)
		if len(st.Sessions) == 2 && st.Sessions[1].Ended {
			break
		}
		time.Sleep(20 * time.Millisecond)
	}
	if !st.Sessions[1].Ended || st.Sessions[1].Code != 3 || st.Sessions[0].Ended {
		t.Fatalf("sessões %+v", st.Sessions)
	}
	text := ""
	for _, e := range st.Events {
		text += e.Text + "|"
	}
	if !strings.Contains(text, "shell: sessão iniciada") || !strings.Contains(text, "shell: saiu com código 3") {
		t.Fatalf("eventos %q", text)
	}
	// Uma segunda aba de shell ganha número.
	if info, err := h.OpenShell(); err != nil || info.Label != "shell 2" {
		t.Fatalf("segunda aba %+v %v", info, err)
	}
}

// TestHubLimitsAndUnknown: aba de shell desligada, teto de sessões vivas e aba que não existe.
func TestHubLimitsAndUnknown(t *testing.T) {
	block := func(ctx context.Context, _ ports.Winsize, _ io.Reader, _ io.Writer, _ <-chan ports.Winsize) (int, error) {
		<-ctx.Done()
		return 0, nil
	}
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	if _, err := NewHub(ctx, tok, "m", Start(ctx, block, ports.Winsize{}), "main", nil, nil).OpenShell(); err == nil {
		t.Fatal("sem fábrica de shell abriu aba")
	}
	h := NewHub(ctx, tok, "m", Start(ctx, block, ports.Winsize{}), "main", block, nil)
	for i := 1; i < maxSessions; i++ {
		if _, err := h.OpenShell(); err != nil {
			t.Fatalf("aba %d: %v", i, err)
		}
	}
	srv := httptest.NewServer(Handler(h))
	defer srv.Close()
	r, _ := http.Post(srv.URL+"/sessions?t="+tok, "text/plain", nil)
	r.Body.Close()
	if r.StatusCode != http.StatusTooManyRequests {
		t.Fatalf("acima do teto: %d", r.StatusCode)
	}
	r, _ = http.Get(srv.URL + "/stream?s=nao-existe&t=" + tok)
	r.Body.Close()
	if r.StatusCode != http.StatusNotFound {
		t.Fatalf("aba inexistente: %d", r.StatusCode)
	}
	r, _ = http.Get(srv.URL + "/state")
	r.Body.Close()
	if r.StatusCode != http.StatusForbidden {
		t.Fatalf("state sem token: %d", r.StatusCode)
	}
}

// TestHubEventsCapped: a linha do tempo guarda só as últimas maxEvents.
func TestHubEventsCapped(t *testing.T) {
	h := hubOf(Start(context.Background(), exitAfter("", 0), ports.Winsize{}), nil, nil)
	for i := 0; i < maxEvents+50; i++ {
		h.Logf("evento %d", i)
	}
	ev := h.Snapshot().Events
	if len(ev) != maxEvents || ev[len(ev)-1].Text != "evento 249" {
		t.Fatalf("%d eventos, último %q", len(ev), ev[len(ev)-1].Text)
	}
}

// TestForwarderEventsInHub: abrir e fechar porta aparece na linha do tempo do hub.
func TestForwarderEventsInHub(t *testing.T) {
	_, dial := echoGuest(t)
	guest := freePort(t)
	l := &lister{ports: []uint16{guest}}
	fw := NewForwarder(l.list, dial, time.Hour)
	h := hubOf(Start(context.Background(), exitAfter("", 0), ports.Winsize{}), nil, fw)
	fw.sync(context.Background())
	l.set(nil, nil)
	fw.sync(context.Background())
	text := ""
	for _, e := range h.Snapshot().Events {
		text += e.Text + "|"
	}
	if !strings.Contains(text, "aberta → http://127.0.0.1:") || !strings.Contains(text, "fechou na máquina") {
		t.Fatalf("eventos %q", text)
	}
}

// TestPublicHostAndPortURL: com PublicHost, esse nome passa e outro não; o Origin https do próprio host passa e o de
// outro site não; PortURL troca o endereço das abas de porta.
func TestPublicHostAndPortURL(t *testing.T) {
	_, dial := echoGuest(t)
	guest := freePort(t)
	fw := NewForwarder((&lister{ports: []uint16{guest}}).list, dial, time.Hour)
	fw.WatchOnly = true
	fw.sync(context.Background())
	h := hubOf(Start(context.Background(), exitAfter("", 0), ports.Winsize{}), nil, fw)
	h.PublicHost = "Demo.Example.com"
	h.PortURL = func(g uint16, _ int) string { return "https://" + strconv.Itoa(int(g)) + "-abc.example.com/" }
	srv := httptest.NewServer(Handler(h))
	defer srv.Close()

	do := func(host, origin string) int {
		req, _ := http.NewRequest("POST", srv.URL+"/input?t="+tok, strings.NewReader("x"))
		req.Host = host
		if origin != "" {
			req.Header.Set("Origin", origin)
		}
		r, err := http.DefaultClient.Do(req)
		if err != nil {
			t.Fatal(err)
		}
		r.Body.Close()
		return r.StatusCode
	}
	if c := do("demo.example.com", "https://demo.example.com"); c == http.StatusForbidden {
		t.Fatalf("host público com origem https: %d", c)
	}
	if c := do("outro.example.com", ""); c != http.StatusForbidden {
		t.Fatalf("outro host: %d", c)
	}
	if c := do("demo.example.com", "https://atacante.example.com"); c != http.StatusForbidden {
		t.Fatalf("origem de outro site: %d", c)
	}
	st := h.Snapshot()
	if len(st.Ports) != 1 || st.Ports[0].Host != 0 || st.Ports[0].URL != "https://"+strconv.Itoa(int(guest))+"-abc.example.com/" {
		t.Fatalf("portas %+v", st.Ports)
	}
	// WatchOnly não abriu nada no host: o número da porta segue livre.
	if ln, err := net.Listen("tcp", "127.0.0.1:"+strconv.Itoa(int(guest))); err != nil {
		t.Fatalf("WatchOnly abriu a porta no host: %v", err)
	} else {
		ln.Close()
	}
	fw.closeAll()
}

// TestDefaultPortURL: sem PortURL, a aba aponta para o encaminhamento local.
func TestDefaultPortURL(t *testing.T) {
	_, dial := echoGuest(t)
	guest := freePort(t)
	fw := NewForwarder((&lister{ports: []uint16{guest}}).list, dial, time.Hour)
	fw.sync(context.Background())
	defer fw.closeAll()
	h := hubOf(Start(context.Background(), exitAfter("", 0), ports.Winsize{}), nil, fw)
	if u := h.Snapshot().Ports[0].URL; u != "http://127.0.0.1:"+strconv.Itoa(int(guest))+"/" {
		t.Fatalf("url %q", u)
	}
}
