// Package webterm serve o terminal de uma máquina no navegador: uma página com xterm.js (embutido, funciona offline),
// a saída do PTY por SSE (GET /stream) e o teclado e o tamanho da janela de volta (POST /input, POST /resize).
//
// A sessão nasce com o comando, não com o navegador: o programa (o Claude Code, por exemplo) já roda enquanto a página
// abre, e um F5 ou uma segunda aba recebem de novo os últimos bytes da tela em vez de começar no escuro.
//
// Segurança: é um shell na máquina, e, com o Claude Code em --dangerously-skip-permissions, um agente sem confirmação de permissão. Por isso:
//   - escuta só em loopback (quem chama abre o listener em 127.0.0.1);
//   - toda rota que lê ou escreve na sessão exige o token aleatório da URL (comparação em tempo constante);
//   - o Host tem de ser de loopback: um domínio que resolve para 127.0.0.1 (DNS rebinding) recebe 403;
//   - POST com Origin de outro site recebe 403, e o corpo tem teto.
package webterm

import (
	"context"
	"crypto/subtle"
	_ "embed"
	"encoding/base64"
	"encoding/json"
	"fmt"
	"io"
	"net"
	"net/http"
	"strings"
	"sync"
	"time"

	"runner.codes/internal/ports"
)

//go:embed page/index.html
var page []byte

//go:embed page/xterm.js
var xtermJS []byte

//go:embed page/xterm.css
var xtermCSS []byte

//go:embed page/addon-fit.js
var fitJS []byte

// linksJS deixa as URLs da tela clicáveis: o login do Claude Code mostra um link OAuth longo, e o "c to copy" dele
// copia para a área de transferência da VM, não para a do navegador.
//
//go:embed page/addon-web-links.js
var linksJS []byte

// ShellFunc roda o programa num PTY da máquina: lê o teclado de in, escreve a tela em out, recebe cada resize e
// devolve o código de saída. É o Machines.Shell com nome e pedido já fixados.
type ShellFunc func(ctx context.Context, size ports.Winsize, in io.Reader, out io.Writer, resize <-chan ports.Winsize) (int, error)

// maxReplay é quanto da tela recente fica guardado para quem conecta depois: 256 KiB cobrem várias telas cheias de
// um TUI (o Claude Code redesenha a tela inteira) sem prender memória à toa.
const maxReplay = 256 << 10

// maxInput limita o corpo de um POST /input: uma colagem grande chega em pedaços de 32 KiB pela página.
const maxInput = 64 << 10

// subBuffer é quantos pedaços de saída um navegador pode ficar devendo; quem atrasa mais que isso é desligado e
// reconecta recebendo o replay, em vez de travar a sessão para todos.
const subBuffer = 256

// heartbeat mantém o SSE vivo atrás de proxy e faz o navegador notar logo um processo que caiu.
const heartbeat = 15 * time.Second

// Session é uma sessão de terminal compartilhada por todas as abas abertas na mesma URL.
type Session struct {
	in     *io.PipeWriter
	resize chan ports.Winsize
	done   chan struct{}

	mu   sync.Mutex
	buf  []byte
	subs map[chan []byte]struct{}
	code int
	err  error
}

// Start liga o programa em segundo plano com o tamanho inicial size. O contexto encerra a sessão.
func Start(ctx context.Context, shell ShellFunc, size ports.Winsize) *Session {
	r, w := io.Pipe()
	s := &Session{in: w, resize: make(chan ports.Winsize, 1), done: make(chan struct{}), subs: map[chan []byte]struct{}{}}
	go func() {
		code, err := shell(ctx, size, r, s, s.resize)
		r.Close()
		s.mu.Lock()
		s.code, s.err = code, err
		for ch := range s.subs {
			close(ch)
		}
		s.subs = map[chan []byte]struct{}{}
		s.mu.Unlock()
		close(s.done)
	}()
	return s
}

// Write recebe a saída do PTY: guarda o fim no replay e entrega a cada aba conectada.
func (s *Session) Write(p []byte) (int, error) {
	chunk := append([]byte(nil), p...)
	s.mu.Lock()
	defer s.mu.Unlock()
	s.buf = append(s.buf, chunk...)
	if over := len(s.buf) - maxReplay; over > 0 {
		s.buf = append([]byte(nil), s.buf[over:]...)
	}
	for ch := range s.subs {
		select {
		case ch <- chunk:
		default:
			// Aba lenta: desliga, e o EventSource dela reconecta e recebe o replay.
			close(ch)
			delete(s.subs, ch)
		}
	}
	return len(p), nil
}

// Wait espera o programa sair e devolve o código e o erro do Shell.
func (s *Session) Wait() (int, error) {
	<-s.done
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.code, s.err
}

// Done fecha quando o programa sai.
func (s *Session) Done() <-chan struct{} { return s.done }

// subscribe devolve a tela recente e um canal com o que vier depois; ended diz que o programa já saiu.
func (s *Session) subscribe() (replay []byte, ch chan []byte, ended bool) {
	s.mu.Lock()
	defer s.mu.Unlock()
	replay = append([]byte(nil), s.buf...)
	select {
	case <-s.done:
		return replay, nil, true
	default:
	}
	ch = make(chan []byte, subBuffer)
	s.subs[ch] = struct{}{}
	return replay, ch, false
}

// unsubscribe tira a aba da lista quando ela fecha a conexão.
func (s *Session) unsubscribe(ch chan []byte) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if _, ok := s.subs[ch]; ok {
		delete(s.subs, ch)
		close(ch)
	}
}

// exitCode lê o código de saída já gravado (só depois de Done).
func (s *Session) exitCode() int {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.code
}

// Handler monta as rotas do terminal web do hub, protegidas pelo token. As rotas de sessão escolhem a aba pelo
// parâmetro s (vazio é a principal).
func Handler(h *Hub) http.Handler {
	mux := http.NewServeMux()
	static := func(body []byte, ctype string) http.HandlerFunc {
		return func(w http.ResponseWriter, _ *http.Request) {
			w.Header().Set("Content-Type", ctype)
			w.Header().Set("Cache-Control", "no-store")
			w.Write(body)
		}
	}
	// A página e o xterm.js não abrem sessão sozinhos: ficam sem token, só com a checagem de Host.
	mux.HandleFunc("GET /{$}", static(page, "text/html; charset=utf-8"))
	mux.HandleFunc("GET /xterm.js", static(xtermJS, "text/javascript; charset=utf-8"))
	mux.HandleFunc("GET /xterm.css", static(xtermCSS, "text/css; charset=utf-8"))
	mux.HandleFunc("GET /addon-fit.js", static(fitJS, "text/javascript; charset=utf-8"))
	mux.HandleFunc("GET /addon-web-links.js", static(linksJS, "text/javascript; charset=utf-8"))
	token := h.token
	mux.HandleFunc("GET /stream", authorized(token, h.withSession((*Session).stream)))
	mux.HandleFunc("POST /input", authorized(token, h.withSession((*Session).input)))
	mux.HandleFunc("POST /resize", authorized(token, h.withSession((*Session).setSize)))
	mux.HandleFunc("GET /state", authorized(token, func(w http.ResponseWriter, _ *http.Request) {
		writeJSON(w, h.Snapshot())
	}))
	mux.HandleFunc("GET /ports", authorized(token, func(w http.ResponseWriter, _ *http.Request) {
		writeJSON(w, h.Snapshot().Ports)
	}))
	mux.HandleFunc("POST /sessions", authorized(token, func(w http.ResponseWriter, _ *http.Request) {
		info, err := h.OpenShell()
		if err != nil {
			http.Error(w, err.Error(), http.StatusTooManyRequests)
			return
		}
		writeJSON(w, info)
	}))
	return allowedHosts(h.PublicHost, mux)
}

// withSession entrega ao handler a sessão do parâmetro s; aba que não existe é 404.
func (h *Hub) withSession(fn func(*Session, http.ResponseWriter, *http.Request)) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		s := h.session(r.URL.Query().Get("s"))
		if s == nil {
			http.Error(w, "no such session", http.StatusNotFound)
			return
		}
		fn(s, w, r)
	}
}

// writeJSON responde JSON sem cache (a página consulta de 2 em 2 s).
func writeJSON(w http.ResponseWriter, v any) {
	w.Header().Set("Content-Type", "application/json")
	w.Header().Set("Cache-Control", "no-store")
	json.NewEncoder(w).Encode(v)
}

// allowedHosts recusa Host que não seja de loopback nem o público declarado (DNS rebinding) e POST vindo de outra
// origem (página de outro site mandando teclas para o shell). public vazio é o modo local: só loopback.
func allowedHosts(public string, next http.Handler) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		host := strings.ToLower(r.Host)
		if !isLoopbackHost(host) && (public == "" || host != strings.ToLower(public)) {
			http.Error(w, "forbidden host", http.StatusForbidden)
			return
		}
		// Atrás do túnel a página é https; no loopback, http. Qualquer outra origem é página de terceiro.
		if o := r.Header.Get("Origin"); r.Method != http.MethodGet && o != "" && o != "http://"+r.Host && o != "https://"+r.Host {
			http.Error(w, "forbidden origin", http.StatusForbidden)
			return
		}
		next.ServeHTTP(w, r)
	})
}

// isLoopbackHost aceita 127.0.0.1, ::1 e localhost, com ou sem porta.
func isLoopbackHost(hostport string) bool {
	host := hostport
	if h, _, err := net.SplitHostPort(hostport); err == nil {
		host = h
	}
	host = strings.Trim(host, "[]")
	if host == "localhost" {
		return true
	}
	ip := net.ParseIP(host)
	return ip != nil && ip.IsLoopback()
}

// authorized exige o token no parâmetro t (o EventSource não manda cabeçalho próprio).
func authorized(token string, next http.HandlerFunc) http.HandlerFunc {
	return func(w http.ResponseWriter, r *http.Request) {
		if subtle.ConstantTimeCompare([]byte(r.URL.Query().Get("t")), []byte(token)) != 1 {
			http.Error(w, "forbidden", http.StatusForbidden)
			return
		}
		next(w, r)
	}
}

// stream manda a tela por SSE: um evento replay com o recente, um data por pedaço novo (base64, porque a saída do
// PTY é binária) e um exit com o código quando o programa sai.
func (s *Session) stream(w http.ResponseWriter, r *http.Request) {
	flusher, ok := w.(http.Flusher)
	if !ok {
		http.Error(w, "streaming unsupported", http.StatusInternalServerError)
		return
	}
	w.Header().Set("Content-Type", "text/event-stream")
	w.Header().Set("Cache-Control", "no-store")
	replay, ch, ended := s.subscribe()
	fmt.Fprintf(w, "event: replay\ndata: %s\n\n", base64.StdEncoding.EncodeToString(replay))
	flusher.Flush()
	if ended {
		s.sendExit(w, flusher)
		return
	}
	defer s.unsubscribe(ch)
	tick := time.NewTicker(heartbeat)
	defer tick.Stop()
	for {
		select {
		case chunk, open := <-ch:
			if !open {
				// Fechado por Write (aba lenta) ou pelo fim do programa: no fim, avisa a saída.
				select {
				case <-s.done:
					s.sendExit(w, flusher)
				default:
				}
				return
			}
			fmt.Fprintf(w, "data: %s\n\n", base64.StdEncoding.EncodeToString(chunk))
			flusher.Flush()
		case <-tick.C:
			fmt.Fprint(w, ": ping\n\n")
			flusher.Flush()
		case <-r.Context().Done():
			return
		}
	}
}

// sendExit avisa a página que o programa saiu, com o código.
func (s *Session) sendExit(w io.Writer, f http.Flusher) {
	fmt.Fprintf(w, "event: exit\ndata: {\"code\":%d}\n\n", s.exitCode())
	f.Flush()
}

// input escreve o teclado no PTY. Depois de o programa sair, 410.
func (s *Session) input(w http.ResponseWriter, r *http.Request) {
	select {
	case <-s.done:
		http.Error(w, "session ended", http.StatusGone)
		return
	default:
	}
	body, err := io.ReadAll(http.MaxBytesReader(w, r.Body, maxInput))
	if err != nil {
		http.Error(w, "input too large", http.StatusRequestEntityTooLarge)
		return
	}
	if _, err := s.in.Write(body); err != nil {
		http.Error(w, "session ended", http.StatusGone)
		return
	}
	w.WriteHeader(http.StatusNoContent)
}

// setSize repassa o tamanho da janela do navegador ao PTY; só o último importa se vierem vários seguidos.
func (s *Session) setSize(w http.ResponseWriter, r *http.Request) {
	var size ports.Winsize
	// 1000 linhas ou colunas é muito acima de qualquer tela real; acima disso é lixo, não janela.
	if err := json.NewDecoder(http.MaxBytesReader(w, r.Body, 1024)).Decode(&size); err != nil ||
		size.Rows == 0 || size.Cols == 0 || size.Rows > 1000 || size.Cols > 1000 {
		http.Error(w, "invalid size", http.StatusBadRequest)
		return
	}
	// Nunca bloqueia: se há um tamanho ainda não lido, ele é trocado pelo novo (dois POSTs juntos não travam).
	for {
		select {
		case s.resize <- size:
			w.WriteHeader(http.StatusNoContent)
			return
		default:
			select {
			case <-s.resize:
			default:
			}
		}
	}
}
