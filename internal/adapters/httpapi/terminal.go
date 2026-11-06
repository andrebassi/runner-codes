package httpapi

import (
	"context"
	"crypto/rand"
	"encoding/hex"
	"io"
	"net/http"
	"sync"
	"time"

	"github.com/gin-gonic/gin"

	"runner.codes/internal/adapters/webterm"
	"runner.codes/internal/app"
	"runner.codes/internal/domain"
	"runner.codes/internal/ports"
)

// ShellService abre um terminal (PTY no guest) numa máquina ligada: o app.Machines.Shell. Opcional — sem ele, as rotas
// de terminal respondem 501.
type ShellService interface {
	Shell(ctx context.Context, name string, req app.ShellRequest, in io.Reader, out io.Writer, resize <-chan ports.Winsize) (int, error)
}

// Limites das sessões de terminal: 8 por máquina (um console com abas, não um servidor de shells), e a sessão que
// ninguém olha por 10 min é encerrada — quem fecha a aba do navegador sem o DELETE (aba morta, rede caída) não deixa
// um shell aberto para sempre no guest. A varredura roda a cada minuto.
const (
	maxTerminalsPerMachine = 8
	terminalIdle           = 10 * time.Minute
	terminalSweep          = time.Minute
)

// terminalSize é o PTY até o primeiro resize do navegador (que chega em milissegundos).
var terminalSize = ports.Winsize{Rows: 32, Cols: 120}

// terminal é uma sessão guardada: a máquina dela, o cancelamento e desde quando ninguém a olha.
type terminal struct {
	machine string
	sess    *webterm.Session
	cancel  context.CancelFunc
	idleAt  time.Time // zero enquanto há tela conectada
}

// terminals guarda as sessões de terminal do servidor, por id aleatório.
type terminals struct {
	mu   sync.Mutex
	byID map[string]*terminal
	now  func() time.Time
}

// table devolve a tabela de sessões, criando na primeira vez (e ligando a varredura das esquecidas).
func (s *Server) table() *terminals {
	s.termsOnce.Do(func() {
		s.terms = &terminals{byID: map[string]*terminal{}, now: time.Now}
		go func() {
			for range time.Tick(terminalSweep) {
				s.terms.sweep()
			}
		}()
	})
	return s.terms
}

// sweep encerra a sessão que acabou (programa saiu) e a que está sem tela há mais de terminalIdle.
func (t *terminals) sweep() {
	t.mu.Lock()
	defer t.mu.Unlock()
	now := t.now()
	for id, term := range t.byID {
		select {
		case <-term.sess.Done():
			delete(t.byID, id)
			continue
		default:
		}
		switch {
		case term.sess.Viewers() > 0:
			term.idleAt = time.Time{}
		case term.idleAt.IsZero():
			term.idleAt = now
		case now.Sub(term.idleAt) >= terminalIdle:
			term.cancel()
			delete(t.byID, id)
		}
	}
}

// count conta as sessões vivas de uma máquina.
func (t *terminals) count(machine string) int {
	n := 0
	for _, term := range t.byID {
		if term.machine == machine {
			n++
		}
	}
	return n
}

// terminalRoutes monta as rotas de terminal sob /machines/:name/terminal.
func (s *Server) terminalRoutes(g *gin.RouterGroup) {
	g.POST("/:name/terminal", s.openTerminal)
	g.GET("/:name/terminal/:id/stream", s.withTerminal((*webterm.Session).ServeStream))
	g.POST("/:name/terminal/:id/input", s.withTerminal((*webterm.Session).ServeInput))
	g.POST("/:name/terminal/:id/resize", s.withTerminal((*webterm.Session).ServeResize))
	g.DELETE("/:name/terminal/:id", s.closeTerminal)
}

// openTerminalBody é o pedido de um terminal novo: o usuário (vazio é o da máquina) e o tamanho inicial.
type openTerminalBody struct {
	User string `json:"user"`
	Rows uint16 `json:"rows"`
	Cols uint16 `json:"cols"`
}

// openTerminal atende POST .../terminal: liga o shell padrão da imagem num PTY e devolve o id da sessão. A sessão
// vive no servidor (não na conexão): um F5 reconecta ao mesmo shell e recebe a tela recente.
func (s *Server) openTerminal(c *gin.Context) {
	shell, ok := s.Machines.(ShellService)
	if !ok {
		c.JSON(http.StatusNotImplemented, gin.H{"error": "terminals are not available on this host"})
		return
	}
	var in openTerminalBody
	if c.Request.ContentLength != 0 {
		if err := bindStrict(c, &in); err != nil {
			invalid(c, "invalid request: "+err.Error())
			return
		}
	}
	name := c.Param("name")
	st, err := s.Machines.Status(name)
	if err != nil {
		machineError(c, err)
		return
	}
	if st.State != app.StateRunning {
		machineError(c, domain.ErrMachineState(name, "is not running"))
		return
	}
	size := terminalSize
	if in.Rows > 0 && in.Cols > 0 && in.Rows <= 1000 && in.Cols <= 1000 {
		size = ports.Winsize{Rows: in.Rows, Cols: in.Cols}
	}
	t := s.table()
	t.mu.Lock()
	defer t.mu.Unlock()
	if t.count(name) >= maxTerminalsPerMachine {
		c.JSON(http.StatusTooManyRequests, gin.H{"error": "too many open terminals on this machine"})
		return
	}
	var raw [12]byte
	_, _ = rand.Read(raw[:])
	id := hex.EncodeToString(raw[:])
	// A sessão sobrevive ao pedido que a criou: o contexto é o do servidor, encerrado pelo DELETE ou pela varredura.
	ctx, cancel := context.WithCancel(context.Background())
	env := []string{"TERM=xterm-256color", "COLORTERM=truecolor"}
	sess := webterm.Start(ctx, func(ctx context.Context, size ports.Winsize, r io.Reader, w io.Writer, resize <-chan ports.Winsize) (int, error) {
		return shell.Shell(ctx, name, app.ShellRequest{User: in.User, Env: env, Size: size}, r, w, resize)
	}, size)
	t.byID[id] = &terminal{machine: name, sess: sess, cancel: cancel}
	c.JSON(http.StatusCreated, gin.H{"id": id})
}

// withTerminal entrega ao handler a sessão do :id, conferindo que é da máquina do :name; outra máquina ou id
// desconhecido é 404 (não diz se o id existe noutra máquina).
func (s *Server) withTerminal(fn func(*webterm.Session, http.ResponseWriter, *http.Request)) gin.HandlerFunc {
	return func(c *gin.Context) {
		t := s.table()
		t.mu.Lock()
		term, ok := t.byID[c.Param("id")]
		t.mu.Unlock()
		if !ok || term.machine != c.Param("name") {
			c.JSON(http.StatusNotFound, gin.H{"error": "no such terminal"})
			return
		}
		fn(term.sess, c.Writer, c.Request)
	}
}

// closeTerminal atende DELETE .../terminal/:id: encerra o shell (o programa recebe o fim do PTY) e esquece a sessão.
func (s *Server) closeTerminal(c *gin.Context) {
	t := s.table()
	t.mu.Lock()
	term, ok := t.byID[c.Param("id")]
	if ok && term.machine == c.Param("name") {
		term.cancel()
		delete(t.byID, c.Param("id"))
	}
	t.mu.Unlock()
	if !ok || term.machine != c.Param("name") {
		c.JSON(http.StatusNotFound, gin.H{"error": "no such terminal"})
		return
	}
	c.Status(http.StatusNoContent)
}
