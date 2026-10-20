package netgw

import (
	"bufio"
	"bytes"
	"crypto/tls"
	"crypto/x509"
	"errors"
	"fmt"
	"io"
	"log"
	"mime"
	"net"
	"net/http"
	"net/http/httputil"
	"net/url"
	"regexp"
	"strconv"
	"strings"
	"time"

	"runner.code/internal/credca"
	"runner.code/internal/domain"
)

// Interceptor troca placeholders de credencial pelo valor real nas requisições HTTPS aos
// hosts das bindings. Só a porta Port (443) passa por ele; conexão a outro host segue
// intacta, com o certificado real do servidor.
type Interceptor struct {
	Bindings  []domain.Credential
	Authority *credca.Authority
	// Resolve devolve o valor real de uma binding, a cada requisição (o ambiente de quem deu
	// o start). Erro vira 502, nunca uma requisição sem a credencial.
	Resolve func(domain.Credential) (string, error)
	// Port é a porta interceptada (443); trocada nos testes.
	Port uint16
	// Roots verifica o servidor de verdade; nil usa as raízes do sistema.
	Roots *x509.CertPool
}

// maxCredentialBody é o teto do corpo bufferizado num host de credencial:
// o corpo precisa ser lido inteiro para saber se carrega um placeholder. Variável só para o
// teste do teto não precisar empurrar 8 MiB pela pilha.
var maxCredentialBody int64 = 8 << 20

// placeholderRe acha placeholders em qualquer texto da requisição.
var placeholderRe = regexp.MustCompile(domain.PlaceholderPrefix + `[A-Z0-9_]+`)

// recordingConn guarda o que foi lido, para devolver os bytes do ClientHello à conexão real.
type recordingConn struct {
	net.Conn
	buf bytes.Buffer
}

// Read lê da conexão e guarda.
func (r *recordingConn) Read(p []byte) (int, error) {
	n, err := r.Conn.Read(p)
	r.buf.Write(p[:n])
	return n, err
}

// Write descarta: o handshake de leitura é interrompido de propósito, e o alerta que o
// crypto/tls manda nessa hora não pode chegar ao guest (ele abortaria a conexão real).
func (r *recordingConn) Write(p []byte) (int, error) { return len(p), nil }

// replayConn devolve primeiro os bytes já lidos e depois segue na conexão.
type replayConn struct {
	net.Conn
	r io.Reader
}

// Read lê do que foi guardado, depois da conexão.
func (c *replayConn) Read(p []byte) (int, error) { return c.r.Read(p) }

// CloseWrite repassa o meio-fechamento à conexão de baixo: o pipe o usa para o servidor ver
// o fim do envio do guest sem cortar a resposta.
func (c *replayConn) CloseWrite() error {
	if cw, ok := c.Conn.(interface{ CloseWrite() error }); ok {
		return cw.CloseWrite()
	}
	return c.Conn.Close()
}

// errHello interrompe o handshake de leitura assim que o SNI é conhecido.
var errHello = errors.New("client hello read")

// peekSNI lê o ClientHello do guest e devolve o nome pedido e uma conexão que repete os
// bytes lidos (o handshake de verdade começa do zero nela).
func peekSNI(c net.Conn) (string, net.Conn, error) {
	rec := &recordingConn{Conn: c}
	var sni string
	c.SetReadDeadline(time.Now().Add(10 * time.Second))
	err := tls.Server(rec, &tls.Config{GetConfigForClient: func(h *tls.ClientHelloInfo) (*tls.Config, error) {
		sni = h.ServerName
		return nil, errHello
	}}).Handshake()
	c.SetReadDeadline(time.Time{})
	replay := &replayConn{Conn: c, r: io.MultiReader(bytes.NewReader(rec.buf.Bytes()), c)}
	if !errors.Is(err, errHello) {
		return "", replay, fmt.Errorf("not a TLS client hello: %w", err)
	}
	return strings.ToLower(sni), replay, nil
}

// serve atende uma conexão aprovada à porta interceptada: host sem binding é retransmitido
// intacto; host com binding tem o TLS terminado aqui e as requisições reescritas.
func (ic *Interceptor) serve(guest, upstream net.Conn) {
	sni, replay, err := peekSNI(guest)
	if err != nil || !ic.covered(sni) {
		pipe(replay, upstream)
		return
	}
	defer guest.Close()
	defer upstream.Close()
	in := tls.Server(replay, &tls.Config{NextProtos: []string{"http/1.1"},
		GetCertificate: func(*tls.ClientHelloInfo) (*tls.Certificate, error) { return ic.Authority.Issue(sni) }})
	if err := in.Handshake(); err != nil {
		return
	}
	// O servidor de verdade, no IP que o guest resolveu (a conexão TCP já aberta), com o
	// certificado verificado para o nome pedido.
	out := tls.Client(upstream, &tls.Config{ServerName: sni, RootCAs: ic.Roots, NextProtos: []string{"http/1.1"}})
	if err := out.Handshake(); err != nil {
		writeStatus(in, http.StatusBadGateway, "upstream TLS: "+err.Error())
		return
	}
	ic.relayRequests(in, out, sni)
}

// covered diz se alguma binding vale para o host (em qualquer método).
func (ic *Interceptor) covered(host string) bool {
	for _, b := range ic.Bindings {
		for _, h := range b.Hosts {
			if domain.HostMatches(h, host) {
				return true
			}
		}
	}
	return false
}

// relayRequests lê requisições do guest, reescreve, manda ao servidor e devolve a resposta,
// em keep-alive até um dos lados fechar.
func (ic *Interceptor) relayRequests(in, out net.Conn, host string) {
	gr, ur := bufio.NewReader(in), bufio.NewReader(out)
	for {
		req, err := http.ReadRequest(gr)
		if err != nil {
			return
		}
		if status, why := ic.rewrite(req, host); status != 0 {
			io.Copy(io.Discard, io.LimitReader(req.Body, maxCredentialBody))
			writeStatus(in, status, why)
			if req.Close {
				return
			}
			continue
		}
		if err := req.Write(out); err != nil {
			writeStatus(in, http.StatusBadGateway, "upstream: "+err.Error())
			return
		}
		resp, err := http.ReadResponse(ur, req)
		if err != nil {
			writeStatus(in, http.StatusBadGateway, "upstream: "+err.Error())
			return
		}
		err = writeResponse(in, resp)
		resp.Body.Close()
		if err != nil || req.Close || resp.Close {
			return
		}
	}
}

// rewrite aplica as regras ao pedido e troca o placeholder. Devolve 0 para seguir, ou o
// status e o motivo da recusa (uma linha, sem valor nenhum de credencial).
func (ic *Interceptor) rewrite(req *http.Request, host string) (int, string) {
	if req.Method == http.MethodConnect || req.Header.Get("Upgrade") != "" {
		return http.StatusForbidden, "upgrade and CONNECT are refused on credential hosts"
	}
	if placeholderRe.MatchString(req.RequestURI) {
		return http.StatusForbidden, "a credential placeholder may not travel in the URL"
	}
	found := map[string]bool{}
	for name, vs := range req.Header {
		for _, v := range vs {
			for _, p := range placeholderRe.FindAllString(v, -1) {
				if domain.ForbiddenHeader(name) {
					return http.StatusForbidden, "a credential placeholder may not travel in the " + strings.ToLower(name) + " header"
				}
				found[p] = true
			}
		}
	}
	var body []byte
	if req.Body != nil {
		var err error
		body, err = io.ReadAll(io.LimitReader(req.Body, maxCredentialBody+1))
		req.Body.Close()
		if err != nil {
			return http.StatusBadRequest, "request body: " + err.Error()
		}
		if int64(len(body)) > maxCredentialBody {
			return http.StatusRequestEntityTooLarge, "request bodies to credential hosts are limited to 8 MiB"
		}
	}
	inBody := placeholderRe.FindAllString(string(body), -1)
	for _, p := range inBody {
		found[p] = true
	}
	if len(found) > 1 {
		return http.StatusForbidden, "more than one credential placeholder in a request"
	}
	for p := range found {
		b, ok := ic.binding(p)
		if !ok {
			return http.StatusForbidden, "unknown credential placeholder"
		}
		if !b.Allows(host, req.Method) {
			return http.StatusMethodNotAllowed, "credential " + b.Name + " is not allowed for this host or method"
		}
		if len(inBody) > 0 {
			if !b.Body {
				return http.StatusForbidden, "credential " + b.Name + " may not travel in the body (no :body)"
			}
			if mt, _, _ := mime.ParseMediaType(req.Header.Get("Content-Type")); mt != "application/x-www-form-urlencoded" {
				return http.StatusUnsupportedMediaType, "credential bodies must be application/x-www-form-urlencoded"
			}
		}
		value, err := ic.value(b)
		if err != nil {
			return http.StatusBadGateway, "runner.code credentials: credential unavailable"
		}
		for name, vs := range req.Header {
			for i, v := range vs {
				req.Header[name][i] = strings.ReplaceAll(v, p, value)
			}
		}
		body = bytes.ReplaceAll(body, []byte(p), []byte(url.QueryEscape(value)))
	}
	for _, b := range ic.Bindings {
		if b.SetHeader != "" && b.Allows(host, req.Method) {
			value, err := ic.value(b)
			if err != nil {
				return http.StatusBadGateway, "runner.code credentials: credential unavailable"
			}
			req.Header.Set(b.SetHeader, value)
		}
	}
	// Corpo vazio vai como NoBody: para o Request.Write do cliente, tamanho 0 com corpo não
	// nulo é "desconhecido", e um GET sairia em chunked (há servidor que recusa).
	req.Body, req.ContentLength, req.TransferEncoding = http.NoBody, 0, nil
	if len(body) > 0 {
		req.Body, req.ContentLength = io.NopCloser(bytes.NewReader(body)), int64(len(body))
	}
	return 0, ""
}

// binding acha a binding dona de um placeholder.
func (ic *Interceptor) binding(placeholder string) (domain.Credential, bool) {
	for _, b := range ic.Bindings {
		if b.Placeholder == placeholder {
			return b, true
		}
	}
	return domain.Credential{}, false
}

// value resolve a binding e recusa valor vazio ou com caractere de controle (quebraria o
// cabeçalho, e um \r\n permitiria injetar outro).
func (ic *Interceptor) value(b domain.Credential) (string, error) {
	v, err := ic.Resolve(b)
	if err == nil && v == "" {
		err = errors.New("empty value")
	}
	if err == nil && strings.ContainsFunc(v, func(r rune) bool { return r < 0x20 || r == 0x7f }) {
		err = errors.New("control character in value")
	}
	if err != nil {
		// O detalhe vai para o log do processo da VM, nunca para a resposta ao guest.
		log.Printf("credential %s unavailable: %v", b.Name, err)
	}
	return v, err
}

// writeStatus responde ao guest com um status e uma linha de motivo.
func writeStatus(w io.Writer, status int, why string) {
	fmt.Fprintf(w, "HTTP/1.1 %d %s\r\nContent-Type: text/plain\r\nContent-Length: %d\r\n\r\n%s\n",
		status, http.StatusText(status), len(why)+1, why)
}

// writeResponse devolve a resposta ao guest sem bufferizar o corpo: cada pedaço lido do
// servidor sai na hora (server-sent events chegam ao vivo). Tamanho desconhecido vai em
// chunked.
func writeResponse(w io.Writer, resp *http.Response) error {
	chunked := resp.ContentLength < 0 && resp.StatusCode != http.StatusNoContent && resp.StatusCode != http.StatusNotModified
	resp.Header.Del("Transfer-Encoding")
	resp.Header.Del("Content-Length")
	if resp.ContentLength >= 0 {
		resp.Header.Set("Content-Length", strconv.FormatInt(resp.ContentLength, 10))
	}
	if chunked {
		resp.Header.Set("Transfer-Encoding", "chunked")
	}
	if _, err := fmt.Fprintf(w, "HTTP/1.1 %s\r\n", resp.Status); err != nil {
		return err
	}
	if err := resp.Header.Write(w); err != nil {
		return err
	}
	if _, err := io.WriteString(w, "\r\n"); err != nil {
		return err
	}
	var body io.Writer = w
	var cw io.WriteCloser
	if chunked {
		cw = httputil.NewChunkedWriter(w)
		body = cw
	}
	buf := make([]byte, 32<<10)
	for {
		n, rerr := resp.Body.Read(buf)
		if n > 0 {
			if _, err := body.Write(buf[:n]); err != nil {
				return err
			}
		}
		if rerr == io.EOF {
			break
		}
		if rerr != nil {
			return rerr
		}
	}
	if cw != nil {
		cw.Close()
		_, err := io.WriteString(w, "\r\n")
		return err
	}
	return nil
}
