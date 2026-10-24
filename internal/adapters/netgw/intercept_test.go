package netgw

import (
	"bufio"
	"crypto/tls"
	"crypto/x509"
	"encoding/json"
	"errors"
	"io"
	"net"
	"net/http"
	"net/http/httptest"
	"strconv"
	"strings"
	"testing"
	"time"

	"runner.codes/internal/credca"
	"runner.codes/internal/domain"
)

// Placeholders das bindings de teste (o formato que o create emite).
const (
	phToken = domain.PlaceholderPrefix + "TOKEN_AAAA"
	phForm  = domain.PlaceholderPrefix + "FORM_BBBB"
	phGet   = domain.PlaceholderPrefix + "READONLY_CCCC"
)

// echoed é o que o servidor de verdade viu de uma requisição.
type echoed struct {
	Method, Path, Auth, Key, Body, ContentType string
}

// credRig é um gateway com interceptador na frente de um servidor HTTPS de verdade
// (httptest com TLS), e o guest falso que confia na CA da máquina.
type credRig struct {
	guest   *guestStack
	port    string
	caPool  *x509.CertPool
	release chan struct{}
	values  map[string]string
}

// newCredRig monta o servidor, a CA (example.com e *.example.org), as bindings e o gateway.
func newCredRig(t *testing.T) *credRig {
	t.Helper()
	r := &credRig{release: make(chan struct{}), values: map[string]string{"token": "real-token", "form": "a b&c", "readonly": "ro", "gh": "Bearer set-by-host"}}
	up := httptest.NewUnstartedServer(http.HandlerFunc(func(w http.ResponseWriter, q *http.Request) {
		if q.URL.Path == "/sse" {
			w.Header().Set("Content-Type", "text/event-stream")
			io.WriteString(w, "data: 1\n\n")
			w.(http.Flusher).Flush()
			<-r.release
			io.WriteString(w, "data: 2\n\n")
			return
		}
		b, _ := io.ReadAll(q.Body)
		json.NewEncoder(w).Encode(echoed{q.Method, q.URL.RequestURI(), q.Header.Get("Authorization"), q.Header.Get("X-Key"), string(b), q.Header.Get("Content-Type")})
	}))
	up.StartTLS()
	t.Cleanup(up.Close)
	t.Cleanup(func() {
		select {
		case <-r.release:
		default:
			close(r.release)
		}
	})
	roots := x509.NewCertPool()
	roots.AddCert(up.Certificate())
	_, r.port, _ = net.SplitHostPort(up.Listener.Addr().String())
	certPEM, keyPEM, _ := credca.New([]string{"example.com", "*.example.org"}, time.Now())
	auth, err := credca.Load(certPEM, keyPEM)
	if err != nil {
		t.Fatal(err)
	}
	r.caPool = x509.NewCertPool()
	r.caPool.AppendCertsFromPEM(certPEM)
	bindings := []domain.Credential{
		{Name: "token", Env: "TOKEN", Hosts: []string{"example.com"}, Placeholder: phToken},
		{Name: "form", Env: "FORM", Hosts: []string{"example.com"}, Body: true, Placeholder: phForm},
		{Name: "readonly", Env: "RO", Hosts: []string{"example.com"}, Methods: []string{"GET"}, Placeholder: phGet},
		{Name: "gh", Env: "GH", Hosts: []string{"*.example.com"}, SetHeader: "authorization", Placeholder: domain.PlaceholderPrefix + "GH_DDDD"},
		{Name: "org", Env: "ORG", Hosts: []string{"*.example.org"}, Placeholder: domain.PlaceholderPrefix + "ORG_EEEE"},
	}
	p, _ := domain.Network{Enabled: true, Egress: domain.Egress{AllowCIDRs: []string{"127.0.0.1/32"}}}.OutboundPolicy()
	port, _ := strconv.Atoi(r.port)
	host, guest := net.Pipe()
	gw, err := Start(host, Config{Policy: p, Upstream: "127.0.0.1:1", Intercept: &Interceptor{
		Bindings: bindings, Authority: auth, Port: uint16(port), Roots: roots,
		Resolve: func(c domain.Credential) (string, error) {
			v, ok := r.values[c.Name]
			if !ok {
				return "", errors.New("unset")
			}
			return v, nil
		}}})
	if err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { gw.Close() })
	r.guest = newGuest(t, guest)
	return r
}

// open abre TLS do guest para o servidor, pedindo sni, confiando só na CA da máquina.
func (r *credRig) open(t *testing.T, sni string) (*tls.Conn, *bufio.Reader) {
	t.Helper()
	c, err := r.guest.dial("100.96.0.1:" + r.port)
	if err != nil {
		t.Fatal(err)
	}
	tc := tls.Client(c, &tls.Config{ServerName: sni, RootCAs: r.caPool})
	tc.SetDeadline(time.Now().Add(20 * time.Second))
	if err := tc.Handshake(); err != nil {
		t.Fatalf("handshake %s: %v", sni, err)
	}
	return tc, bufio.NewReader(tc)
}

// send manda uma requisição crua e devolve status e corpo da resposta.
func send(t *testing.T, c *tls.Conn, br *bufio.Reader, raw string) (int, string) {
	t.Helper()
	if _, err := io.WriteString(c, raw); err != nil {
		t.Fatal(err)
	}
	resp, err := http.ReadResponse(br, nil)
	if err != nil {
		t.Fatalf("response: %v", err)
	}
	defer resp.Body.Close()
	b, _ := io.ReadAll(resp.Body)
	return resp.StatusCode, string(b)
}

// request monta uma requisição HTTP/1.1 para example.com com cabeçalhos e corpo.
func request(method, path string, headers []string, body string) string {
	s := method + " " + path + " HTTP/1.1\r\nHost: example.com\r\n"
	for _, h := range headers {
		s += h + "\r\n"
	}
	if body != "" {
		s += "Content-Length: " + strconv.Itoa(len(body)) + "\r\n"
	}
	return s + "\r\n" + body
}

// TestInterceptSubstitutes: o servidor recebe o valor real no lugar do placeholder (só ele
// muda, o "Bearer " fica), duas requisições na mesma conexão, o corpo de formulário com
// :body leva o valor codificado, e set_header define o cabeçalho inteiro pelo host.
func TestInterceptSubstitutes(t *testing.T) {
	r := newCredRig(t)
	c, br := r.open(t, "example.com")
	defer c.Close()
	code, body := send(t, c, br, request("GET", "/v1/me", []string{"Authorization: Bearer " + phToken}, ""))
	var e echoed
	json.Unmarshal([]byte(body), &e)
	if code != 200 || e.Auth != "Bearer real-token" || e.Path != "/v1/me" {
		t.Fatalf("header substitution: %d %+v", code, e)
	}
	code, body = send(t, c, br, request("POST", "/token", []string{"Content-Type: application/x-www-form-urlencoded"}, "grant_type=refresh&refresh_token="+phForm))
	json.Unmarshal([]byte(body), &e)
	if code != 200 || e.Body != "grant_type=refresh&refresh_token=a+b%26c" {
		t.Fatalf("form body substitution on the same connection: %d %+v", code, e)
	}
	code, body = send(t, c, br, request("GET", "/plain", []string{"X-Key: no-placeholder"}, ""))
	json.Unmarshal([]byte(body), &e)
	if code != 200 || e.Key != "no-placeholder" || e.Auth != "" {
		t.Fatalf("a request without placeholder goes as is: %d %+v", code, e)
	}
	o, obr := r.open(t, "api.example.com")
	defer o.Close()
	code, body = send(t, o, obr, strings.Replace(request("GET", "/repo", []string{"Authorization: Basic guest-sent"}, ""), "example.com", "api.example.com", 1))
	json.Unmarshal([]byte(body), &e)
	if code != 200 || e.Auth != "Bearer set-by-host" {
		t.Fatalf("set_header must replace what the guest sent: %d %+v", code, e)
	}
}

// TestInterceptRefusals: cada recusa com o seu status e um motivo que nunca traz o valor.
func TestInterceptRefusals(t *testing.T) {
	defer func(old int64) { maxCredentialBody = old }(maxCredentialBody)
	maxCredentialBody = 64 << 10
	r := newCredRig(t)
	for name, tc := range map[string]struct {
		raw  string
		code int
	}{
		"url":            {request("GET", "/x?k="+phToken, nil, ""), 403},
		"cookie":         {request("GET", "/", []string{"Cookie: s=" + phToken}, ""), 403},
		"two":            {request("GET", "/", []string{"Authorization: " + phToken, "X-Key: " + phGet}, ""), 403},
		"unknown":        {request("GET", "/", []string{"Authorization: " + domain.PlaceholderPrefix + "NOPE_1"}, ""), 403},
		"method":         {request("POST", "/", []string{"Authorization: " + phGet}, "x"), 405},
		"body no :body":  {request("POST", "/", []string{"Content-Type: application/x-www-form-urlencoded"}, "t="+phToken), 403},
		"body json":      {request("POST", "/", []string{"Content-Type: application/json"}, `{"t":"`+phForm+`"}`), 415},
		"upgrade":        {request("GET", "/", []string{"Upgrade: websocket", "Connection: Upgrade"}, ""), 403},
		"too big":        {request("POST", "/", nil, strings.Repeat("x", int(maxCredentialBody)+1)), 413},
		"bad body frame": {"POST / HTTP/1.1\r\nHost: example.com\r\nTransfer-Encoding: chunked\r\n\r\nzz\r\n", 400},
	} {
		c, br := r.open(t, "example.com")
		code, body := send(t, c, br, tc.raw)
		c.Close()
		if code != tc.code || strings.Contains(body, "real-token") {
			t.Fatalf("%s: %d %q, want %d", name, code, body, tc.code)
		}
	}
	delete(r.values, "token")
	c, br := r.open(t, "example.com")
	if code, body := send(t, c, br, request("GET", "/", []string{"Authorization: Bearer " + phToken}, "")); code != 502 || !strings.Contains(body, "credential unavailable") {
		t.Fatalf("unresolvable value: %d %q", code, body)
	}
	c.Close()
	r.values["token"] = "bad\r\nX-Injected: 1"
	c, br = r.open(t, "example.com")
	if code, _ := send(t, c, br, request("GET", "/", []string{"Authorization: Bearer " + phToken}, "")); code != 502 {
		t.Fatalf("a value with control characters must not go out: %d", code)
	}
	c.Close()
	r.values["token"] = ""
	c, br = r.open(t, "example.com")
	if code, _ := send(t, c, br, request("GET", "/", []string{"Authorization: Bearer " + phToken}, "")); code != 502 {
		t.Fatalf("an empty value: %d", code)
	}
	c.Close()
	delete(r.values, "gh")
	o, obr := r.open(t, "api.example.com")
	if code, _ := send(t, o, obr, strings.Replace(request("GET", "/", nil, ""), "example.com", "api.example.com", 1)); code != 502 {
		t.Fatalf("set_header without a value fails instead of going out bare: %d", code)
	}
	o.Close()
}

// TestInterceptStreams: a resposta sai pedaço a pedaço (server-sent events): o primeiro
// evento chega ao guest antes de o servidor terminar.
func TestInterceptStreams(t *testing.T) {
	r := newCredRig(t)
	c, br := r.open(t, "example.com")
	defer c.Close()
	io.WriteString(c, request("GET", "/sse", []string{"Authorization: Bearer " + phToken}, ""))
	resp, err := http.ReadResponse(br, nil)
	if err != nil {
		t.Fatal(err)
	}
	first := make([]byte, len("data: 1\n\n"))
	if _, err := io.ReadFull(resp.Body, first); err != nil || string(first) != "data: 1\n\n" {
		t.Fatalf("the first event must arrive while the server still holds the response: %q %v", first, err)
	}
	close(r.release)
	rest, _ := io.ReadAll(resp.Body)
	if string(rest) != "data: 2\n\n" {
		t.Fatalf("rest: %q", rest)
	}
}

// TestInterceptPassthrough: host sem binding passa intacto (o guest vê o certificado real do
// servidor, não o da CA da máquina), e o que não é TLS também passa.
func TestInterceptPassthrough(t *testing.T) {
	r := newCredRig(t)
	c, err := r.guest.dial("100.96.0.1:" + r.port)
	if err != nil {
		t.Fatal(err)
	}
	tc := tls.Client(c, &tls.Config{ServerName: "other.net", InsecureSkipVerify: true})
	if err := tc.Handshake(); err != nil {
		t.Fatal(err)
	}
	if issuer := tc.ConnectionState().PeerCertificates[0].Issuer.CommonName; strings.Contains(issuer, "runner.code") {
		t.Fatalf("a host without binding must see the real certificate, got issuer %q", issuer)
	}
	tc.Close()
	raw, _ := r.guest.dial("100.96.0.1:" + r.port)
	raw.SetDeadline(time.Now().Add(5 * time.Second))
	io.WriteString(raw, "GET / HTTP/1.0\r\n\r\n")
	b := make([]byte, 64)
	n, _ := raw.Read(b)
	raw.Close()
	if !strings.Contains(string(b[:n]), "HTTP/1.0 400") {
		t.Fatalf("plain bytes must reach the server untouched: %q", b[:n])
	}
}

// TestInterceptUpstreamTLS: servidor que não prova ser o host (certificado que não confere)
// vira 502 para o guest, sem a credencial sair.
func TestInterceptUpstreamTLS(t *testing.T) {
	r := newCredRig(t)
	c, br := r.open(t, "api.example.org")
	defer c.Close()
	// O httptest só tem certificado para example.com e *.example.com: para api.example.org a
	// verificação falha.
	if code, body := send(t, c, br, strings.Replace(request("GET", "/", nil, ""), "example.com", "api.example.org", 1)); code != 502 || !strings.Contains(body, "upstream TLS") {
		t.Fatalf("an unverifiable upstream: %d %q", code, body)
	}
}

// TestWriteResponseErrors: falha ao escrever cabeçalho ou corpo, e corpo que quebra no meio.
func TestWriteResponseErrors(t *testing.T) {
	resp := func(body io.Reader, n int64) *http.Response {
		return &http.Response{Status: "200 OK", StatusCode: 200, Header: http.Header{}, Body: io.NopCloser(body), ContentLength: n}
	}
	for limit := 0; limit < 4; limit++ {
		if err := writeResponse(&failWriter{left: limit}, resp(strings.NewReader("abc"), -1)); err == nil {
			t.Fatalf("write failure after %d writes must surface", limit)
		}
	}
	if err := writeResponse(io.Discard, resp(io.MultiReader(strings.NewReader("a"), errReader{}), -1)); err == nil {
		t.Fatal("a body that breaks must surface")
	}
	var sb strings.Builder
	if err := writeResponse(&sb, resp(strings.NewReader("abc"), 3)); err != nil || !strings.Contains(sb.String(), "Content-Length: 3") || !strings.HasSuffix(sb.String(), "abc") {
		t.Fatalf("known length: %q %v", sb.String(), err)
	}
}

// failWriter aceita left escritas e falha nas seguintes.
type failWriter struct{ left int }

// Write falha quando acabam as escritas permitidas.
func (f *failWriter) Write(p []byte) (int, error) {
	if f.left == 0 {
		return 0, errors.New("guest went away")
	}
	f.left--
	return len(p), nil
}

// errReader falha na leitura.
type errReader struct{}

// Read falha sempre.
func (errReader) Read([]byte) (int, error) { return 0, errors.New("upstream reset") }
