package playground

import (
	"bytes"
	"context"
	"encoding/json"
	"errors"
	"io"
	"net"
	"net/http"
	"net/http/httptest"
	"strings"
	"testing"

	"runner.code/internal/adapters/runfile"
	"runner.code/internal/adapters/webterm"
	"runner.code/internal/domain"
	"runner.code/internal/ports"
)

// errFull é o "lotado" do teste.
var errFull = errors.New("full")

// rig monta o servidor com Ensure, hub e dial falsos; o app responde "app:<máquina>:<porta>" em qualquer porta.
func rig(t *testing.T) (*Server, *httptest.Server, *[]string) {
	s, srv, touched, _ := rigLocal(t)
	return s, srv, touched
}

// rigLocal é o rig com o LocalBackend exposto.
func rigLocal(t *testing.T) (*Server, *httptest.Server, *[]string, *LocalBackend) {
	t.Helper()
	var touched []string
	app := &http.Server{Handler: http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		io.WriteString(w, "app:"+r.Host+":"+r.URL.Path)
	})}
	ln, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	go app.Serve(ln)
	t.Cleanup(func() { app.Close() })
	block := func(ctx context.Context, _ ports.Winsize, _ io.Reader, out io.Writer, _ <-chan ports.Winsize) (int, error) {
		out.Write([]byte("claude pronto"))
		<-ctx.Done()
		return 0, nil
	}
	lb := &LocalBackend{
		Ctx: context.Background(), Secret: []byte("segredo"),
		EnsureVM: func(_ context.Context, v string) (string, error) {
			if v == "cheio0000000" {
				return "", errFull
			}
			if v == "quebrado0000" {
				return "", errors.New("disco cheio")
			}
			return domain.VisitorMachineName(v)
		},
		TouchVM: func(m string) { touched = append(touched, m) },
		NewHub: func(ctx context.Context, machine, token string) *webterm.Hub {
			return webterm.NewHub(ctx, token, machine, webterm.Start(ctx, block, ports.Winsize{}), "claude", nil, nil)
		},
		Dial: func(ctx context.Context, machine string, port uint16) (net.Conn, error) {
			if port == 9 {
				return nil, errors.New("connection refused")
			}
			return (&net.Dialer{}).DialContext(ctx, "tcp", ln.Addr().String())
		},
	}
	s := New(Config{
		Host: "play.example.com", PortDomain: "example.com", Secret: []byte("segredo"), Backend: lb,
		IsFull: func(err error) bool { return errors.Is(err, errFull) },
	})
	srv := httptest.NewServer(s)
	t.Cleanup(srv.Close)
	return s, srv, &touched, lb
}

// postSession chama /api/session com o visitante e devolve status e corpo decodificado.
func postSession(t *testing.T, srv *httptest.Server, body string) (int, map[string]string) {
	t.Helper()
	r, err := http.Post(srv.URL+"/api/session", "application/json", strings.NewReader(body))
	if err != nil {
		t.Fatal(err)
	}
	defer r.Body.Close()
	var out map[string]string
	json.NewDecoder(r.Body).Decode(&out)
	return r.StatusCode, out
}

// get faz um GET com o Host dado (vazio = o do servidor de teste).
func get(t *testing.T, url, host string) (int, string) {
	t.Helper()
	req, _ := http.NewRequest("GET", url, nil)
	if host != "" {
		req.Host = host
	}
	c := &http.Client{CheckRedirect: func(*http.Request, []*http.Request) error { return http.ErrUseLastResponse }}
	r, err := c.Do(req)
	if err != nil {
		t.Fatal(err)
	}
	defer r.Body.Close()
	b, _ := io.ReadAll(r.Body)
	return r.StatusCode, string(b)
}

// TestPagesAndSession: a entrada e o FingerprintJS saem; a sessão devolve o terminal com o token derivado, e o
// terminal recusa token errado e aceita o certo.
func TestPagesAndSession(t *testing.T) {
	s, srv, touched, lb := rigLocal(t)
	if code, body := get(t, srv.URL+"/", ""); code != 200 || !strings.Contains(body, "FingerprintJS.load") {
		t.Fatalf("entrada: %d", code)
	}
	if code, body := get(t, srv.URL+"/fp.min.js", ""); code != 200 || !strings.Contains(body, "FingerprintJS") {
		t.Fatalf("fp.min.js: %d", code)
	}
	code, out := postSession(t, srv, `{"visitor":"visitorAAAAAAAAAAAAA"}`)
	machine, _ := domain.VisitorMachineName("visitorAAAAAAAAAAAAA")
	id := strings.TrimPrefix(machine, domain.VisitorPrefix)
	if code != 200 || out["url"] != "/v/"+id+"/?t="+s.Token(machine) || out["machine"] != machine {
		t.Fatalf("sessão: %d %v", code, out)
	}
	if c, _ := get(t, srv.URL+"/v/"+id+"/state?t=errado", ""); c != http.StatusForbidden {
		t.Fatalf("token errado: %d", c)
	}
	c, body := get(t, srv.URL+"/v/"+id+"/state?t="+s.Token(machine), "")
	if c != 200 || !strings.Contains(body, `"label":"claude"`) {
		t.Fatalf("estado do terminal: %d %s", c, body)
	}
	if len(*touched) == 0 || (*touched)[0] != machine {
		t.Fatalf("uso não registrado: %v", *touched)
	}
	// Mesmo visitante de novo: o mesmo hub (o programa ainda roda), não um segundo.
	h1 := lb.hubs[machine]
	postSession(t, srv, `{"visitor":"visitorAAAAAAAAAAAAA"}`)
	if lb.hubs[machine] != h1 {
		t.Fatal("hub recriado com a sessão viva")
	}
	if s.Token(machine) == s.Token("pg-0000000000") {
		t.Fatal("tokens iguais para máquinas diferentes")
	}
}

// TestSessionErrors: corpo inválido e visitante inválido são 400; lotado é 503; outro erro é 500.
func TestSessionErrors(t *testing.T) {
	_, srv, _ := rig(t)
	for body, want := range map[string]int{`{`: 400, `{"visitor":"../x"}`: 400, `{"visitor":"cheio0000000"}`: 503, `{"visitor":"quebrado0000"}`: 500} {
		if code, out := postSession(t, srv, body); code != want || out["error"] == "" {
			t.Errorf("%s: %d %v", body, code, out)
		}
	}
}

// TestTerminalWithoutHub: /v/ de uma máquina sem hub (playground reiniciado) volta para a entrada; caminho
// desconhecido é 404.
func TestTerminalWithoutHub(t *testing.T) {
	_, srv, _ := rig(t)
	if code, _ := get(t, srv.URL+"/v/0123456789/", ""); code != http.StatusFound {
		t.Fatalf("sem hub: %d", code)
	}
	if code, _ := get(t, srv.URL+"/nada", ""); code != http.StatusNotFound {
		t.Fatalf("desconhecido: %d", code)
	}
}

// TestPortProxy: <porta>-<id>.<domínio> chega à app da VM com o Host original; porta fechada é 502; porta fora de
// faixa é 400; Host que não casa com o padrão cai na página.
func TestPortProxy(t *testing.T) {
	_, srv, touched := rig(t)
	code, body := get(t, srv.URL+"/docs", "3000-0123456789.example.com")
	if code != 200 || body != "app:3000-0123456789.example.com:/docs" {
		t.Fatalf("proxy: %d %q", code, body)
	}
	if (*touched)[len(*touched)-1] != "pg-0123456789" {
		t.Fatalf("uso da porta não registrado: %v", *touched)
	}
	if code, _ := get(t, srv.URL+"/", "9-0123456789.example.com"); code != http.StatusBadGateway {
		t.Fatalf("porta fechada: %d", code)
	}
	if code, _ := get(t, srv.URL+"/", "99999-0123456789.example.com"); code != http.StatusBadRequest {
		t.Fatalf("porta fora de faixa: %d", code)
	}
	if code, body := get(t, srv.URL+"/", "3000-ZZZ.example.com"); code != 200 || !bytes.Contains([]byte(body), []byte("FingerprintJS")) {
		t.Fatalf("host fora do padrão: %d", code)
	}
}

// TestHubRecreatedAfterExit: quando o programa principal sai (VM desligada pelo reaper), a próxima sessão abre um hub
// novo em vez de devolver o que acabou.
func TestHubRecreatedAfterExit(t *testing.T) {
	_, srv, _, lb := rigLocal(t)
	exit := func(context.Context, ports.Winsize, io.Reader, io.Writer, <-chan ports.Winsize) (int, error) {
		return 0, nil
	}
	lb.NewHub = func(ctx context.Context, machine, token string) *webterm.Hub {
		return webterm.NewHub(ctx, token, machine, webterm.Start(ctx, exit, ports.Winsize{}), "claude", nil, nil)
	}
	postSession(t, srv, `{"visitor":"visitorBBBBBBBBBBBBB"}`)
	machine, _ := domain.VisitorMachineName("visitorBBBBBBBBBBBBB")
	first := lb.hubs[machine]
	for !first.MainEnded() {
	}
	postSession(t, srv, `{"visitor":"visitorBBBBBBBBBBBBB"}`)
	if lb.hubs[machine] == first {
		t.Fatal("hub com a sessão encerrada foi reaproveitado")
	}
}

// TestBaseCodeParses: o runner.code da base é válido, com rede filtrada, privados fechados e sem volume nem SSH.
func TestBaseCodeParses(t *testing.T) {
	mc, err := runfile.Parse(BaseCode)
	if err != nil {
		t.Fatal(err)
	}
	if mc.Image == "" || len(mc.Init) == 0 || !mc.Network.Egress.BlockPrivate || len(mc.Network.Egress.AllowHosts) == 0 || len(mc.Mounts) != 0 || mc.SSHAgent {
		t.Fatalf("base %+v", mc)
	}
}
