package httpapi

import (
	"context"
	"net/http"

	"github.com/gin-gonic/gin"

	"runner.codes/internal/domain"
)

// AndroidService são as operações de uma máquina Android (app.Machines num servidor com o kernel do Android): a
// árvore da tela, a entrada e o screenshot, todos pelo ADB. Opcional — sem ele, as rotas respondem 501.
type AndroidService interface {
	AndroidUI(ctx context.Context, name string) ([]byte, error)
	AndroidInput(ctx context.Context, name string, in domain.AndroidInput) error
	AndroidScreenshot(ctx context.Context, name string) ([]byte, error)
	AndroidShell(ctx context.Context, name, cmd string) (string, error)
}

// androidRoutes monta as rotas do Android sob /machines/:name/android.
func (s *Server) androidRoutes(g *gin.RouterGroup) {
	g.GET("/:name/android/ui", s.androidUI)
	g.POST("/:name/android/input", s.androidInput)
	g.GET("/:name/android/screenshot", s.androidScreenshot)
	g.POST("/:name/android/shell", s.androidShell)
}

// androidService devolve o serviço de Android, ou responde 501 e devolve false.
func (s *Server) androidService(c *gin.Context) (AndroidService, bool) {
	a, ok := s.Machines.(AndroidService)
	if !ok {
		c.JSON(http.StatusNotImplemented, gin.H{"error": "android machines are not available on this host"})
	}
	return a, ok
}

// androidUI atende GET .../android/ui: a árvore da tela em XML (o dump do uiautomator), o que o agente lê em vez de
// uma imagem — cada nó com o texto, o id e os limites na tela.
func (s *Server) androidUI(c *gin.Context) {
	a, ok := s.androidService(c)
	if !ok {
		return
	}
	xml, err := a.AndroidUI(c.Request.Context(), c.Param("name"))
	if err != nil {
		machineError(c, err)
		return
	}
	c.Header("Cache-Control", "no-store")
	c.Data(http.StatusOK, "application/xml", xml)
}

// androidInput atende POST .../android/input: uma ação (tap, swipe, text, key) em JSON; 204 quando o Android a aceitou.
func (s *Server) androidInput(c *gin.Context) {
	a, ok := s.androidService(c)
	if !ok {
		return
	}
	var in domain.AndroidInput
	if err := bindStrict(c, &in); err != nil {
		invalid(c, "invalid request: "+err.Error())
		return
	}
	if err := a.AndroidInput(c.Request.Context(), c.Param("name"), in); err != nil {
		machineError(c, err)
		return
	}
	c.Status(http.StatusNoContent)
}

// androidScreenshot atende GET .../android/screenshot: a tela em PNG (o screencap do Android).
func (s *Server) androidScreenshot(c *gin.Context) {
	a, ok := s.androidService(c)
	if !ok {
		return
	}
	png, err := a.AndroidScreenshot(c.Request.Context(), c.Param("name"))
	if err != nil {
		machineError(c, err)
		return
	}
	c.Header("Cache-Control", "no-store")
	c.Data(http.StatusOK, "image/png", png)
}

// androidShellBody é o pedido do terminal: a linha digitada.
type androidShellBody struct {
	Command string `json:"command"`
}

// androidShell atende POST .../android/shell: uma linha no shell do Android; a saída (stdout e stderr juntos) volta
// em JSON. O código de saída não vem: o protocolo shell: antigo do adbd não o carrega.
func (s *Server) androidShell(c *gin.Context) {
	a, ok := s.androidService(c)
	if !ok {
		return
	}
	var in androidShellBody
	if err := bindStrict(c, &in); err != nil {
		invalid(c, "invalid request: "+err.Error())
		return
	}
	out, err := a.AndroidShell(c.Request.Context(), c.Param("name"), in.Command)
	if err != nil {
		machineError(c, err)
		return
	}
	c.Header("Cache-Control", "no-store")
	c.JSON(http.StatusOK, gin.H{"output": out})
}
