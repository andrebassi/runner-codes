// Package playground é o lado HTTP do playground público (claude-in-chrome.runner.codes): a página de entrada calcula
// o visitorId do navegador (FingerprintJS, embutido), pede a máquina daquele visitante e leva para o terminal web dela
// (/v/<id>/); cada porta que um programa abre na máquina do visitante responde em <porta>-<id>.<domínio>.
//
// O isolamento entre visitantes é a VM de cada um (ver app.Playground). O fingerprint só escolhe qual VM é de quem:
// o token do terminal é derivado dele por HMAC com um segredo do servidor, então quem não manda o visitorId inteiro
// não chega ao terminal de outro visitante. O subdomínio de porta leva só 10 hexa de um hash e dá acesso à app
// publicada, não ao terminal.
package playground

import (
	"context"
	"crypto/hmac"
	"crypto/sha256"
	_ "embed"
	"encoding/hex"
	"encoding/json"
	"errors"
	"io"
	"net/http"
	"regexp"
	"strconv"
	"strings"

	"runner.code/internal/domain"
)

//go:embed page/index.html
var page []byte

//go:embed page/fp.min.js
var fingerprintJS []byte

// BaseCode é o runner.code da máquina-base de onde sai a máquina de cada visitante.
//
//go:embed base.code
var BaseCode []byte

// Backend é quem tem as máquinas: o rc local (uma VM do rc por visitante, LocalBackend) ou o Fly (uma Fly Machine
// por visitante, FlyBackend). O Server só cuida do HTTP público e do token.
type Backend interface {
	// Ensure devolve a máquina do visitante ligada (cria, liga ou recusa com o erro de lotado).
	Ensure(ctx context.Context, visitor string) (string, error)
	// Touch marca uso agora (o desligamento por ociosidade conta a partir daqui).
	Touch(machine string)
	// Terminal é o terminal web da máquina, já sem o prefixo /v/<id>; nil manda o navegador refazer a sessão.
	Terminal(machine string) http.Handler
	// Port leva a requisição a uma porta da máquina (WebSocket incluso), com o Host original.
	Port(machine string, port uint16) http.Handler
}

// Config liga o servidor ao backend.
type Config struct {
	// Host é o nome público da página (claude-in-chrome.runner.codes).
	Host string
	// PortDomain é o domínio das portas: <porta>-<id>.<PortDomain>. Precisa de um certificado que cubra *.<PortDomain>.
	PortDomain string
	// Secret assina os tokens do terminal (HMAC-SHA256 do nome da máquina).
	Secret  []byte
	Backend Backend
	// IsFull diz se o erro do Ensure é "playground lotado" (vira 503 com a mensagem).
	IsFull func(error) bool
}

// Server é o handler HTTP do playground.
type Server struct {
	cfg    Config
	portRe *regexp.Regexp
}

// maxSessionBody é o teto do corpo do POST /api/session: só cabe o visitorId.
const maxSessionBody = 1024

// New monta o servidor.
func New(cfg Config) *Server {
	// <porta>-<10 hexa>.<domínio>: a porta tem de 1 a 5 dígitos e o id é o do nome da máquina (pg-<10 hexa>).
	re := regexp.MustCompile(`^(\d{1,5})-([0-9a-f]{10})\.` + regexp.QuoteMeta(strings.ToLower(cfg.PortDomain)) + `(:\d+)?$`)
	return &Server{cfg: cfg, portRe: re}
}

// Token é o token do terminal da máquina: HMAC com o segredo do servidor, então só sai de /api/session.
func Token(secret []byte, machine string) string {
	m := hmac.New(sha256.New, secret)
	m.Write([]byte(machine))
	return hex.EncodeToString(m.Sum(nil))[:32]
}

// Token é o token do terminal da máquina com o segredo deste servidor.
func (s *Server) Token(machine string) string { return Token(s.cfg.Secret, machine) }

// ServeHTTP decide pelo Host: subdomínio de porta vai ao proxy da VM; o host da página vai à entrada e aos terminais.
func (s *Server) ServeHTTP(w http.ResponseWriter, r *http.Request) {
	host := strings.ToLower(r.Host)
	if m := s.portRe.FindStringSubmatch(host); m != nil {
		port, err := strconv.Atoi(m[1])
		if err != nil || port < 1 || port > 65535 {
			http.Error(w, "invalid port", http.StatusBadRequest)
			return
		}
		machine := domain.VisitorPrefix + m[2]
		s.cfg.Backend.Touch(machine)
		s.cfg.Backend.Port(machine, uint16(port)).ServeHTTP(w, r)
		return
	}
	switch {
	case r.URL.Path == "/" && r.Method == http.MethodGet:
		w.Header().Set("Content-Type", "text/html; charset=utf-8")
		w.Header().Set("Cache-Control", "no-store")
		w.Write(page)
	case r.URL.Path == "/fp.min.js" && r.Method == http.MethodGet:
		w.Header().Set("Content-Type", "text/javascript; charset=utf-8")
		w.Write(fingerprintJS)
	case r.URL.Path == "/api/session" && r.Method == http.MethodPost:
		s.session(w, r)
	case strings.HasPrefix(r.URL.Path, "/v/"):
		s.terminal(w, r)
	default:
		http.NotFound(w, r)
	}
}

// session recebe o visitorId, garante a máquina dele e devolve o endereço do terminal (com o token).
func (s *Server) session(w http.ResponseWriter, r *http.Request) {
	var body struct {
		Visitor string `json:"visitor"`
	}
	if err := json.NewDecoder(io.LimitReader(r.Body, maxSessionBody)).Decode(&body); err != nil {
		writeErr(w, http.StatusBadRequest, "invalid body")
		return
	}
	machine, err := s.cfg.Backend.Ensure(r.Context(), body.Visitor)
	if err != nil {
		var ve domain.ValidationError
		switch {
		case errors.As(err, &ve):
			writeErr(w, http.StatusBadRequest, err.Error())
		case s.cfg.IsFull != nil && s.cfg.IsFull(err):
			writeErr(w, http.StatusServiceUnavailable, err.Error())
		default:
			writeErr(w, http.StatusInternalServerError, err.Error())
		}
		return
	}
	id := strings.TrimPrefix(machine, domain.VisitorPrefix)
	w.Header().Set("Content-Type", "application/json")
	json.NewEncoder(w).Encode(map[string]string{"url": "/v/" + id + "/?t=" + s.Token(machine), "machine": machine})
}

// terminal serve /v/<id>/... com o terminal da máquina; sem terminal (front reiniciado, máquina desligada) manda
// voltar à entrada, que refaz a sessão com o mesmo visitorId e, portanto, a mesma máquina.
func (s *Server) terminal(w http.ResponseWriter, r *http.Request) {
	rest := strings.TrimPrefix(r.URL.Path, "/v/")
	id, _, _ := strings.Cut(rest, "/")
	machine := domain.VisitorPrefix + id
	var h http.Handler
	if len(id) == 10 {
		h = s.cfg.Backend.Terminal(machine)
	}
	if h == nil {
		http.Redirect(w, r, "/", http.StatusFound)
		return
	}
	if activity(r, id) {
		s.cfg.Backend.Touch(machine)
	}
	http.StripPrefix("/v/"+id, h).ServeHTTP(w, r)
}

// activity diz se a requisição ao terminal é da PESSOA: abrir a página (GET /v/<id>/) ou qualquer POST (tecla,
// resize, aba shell nova). O GET /state de 2 em 2 s e o stream da saída vêm sozinhos de uma aba aberta: contando,
// uma aba esquecida seguraria uma das vagas para sempre (visto em 20/10/2026, a máquina não desligava).
func activity(r *http.Request, id string) bool {
	return r.Method != http.MethodGet || r.URL.Path == "/v/"+id+"/" || r.URL.Path == "/v/"+id
}

// writeErr responde {"error": msg} com o status.
func writeErr(w http.ResponseWriter, code int, msg string) {
	w.Header().Set("Content-Type", "application/json")
	w.WriteHeader(code)
	json.NewEncoder(w).Encode(map[string]string{"error": msg})
}
