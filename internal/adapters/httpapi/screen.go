package httpapi

import (
	"errors"
	"net/http"
	"strconv"

	"github.com/gin-gonic/gin"

	"runner.codes/internal/adapters/screenctl"
	"runner.codes/internal/domain"
)

// ScreenActionsRequest é o corpo de POST /api/v1/machines/:name/screen/actions: as ações em ordem, no formato do
// screenctl.Action (click, move, drag, type, key, scroll, wait).
type ScreenActionsRequest struct {
	Actions []screenctl.Action `json:"actions"`
}

// maxScreenActions limita uma chamada: um lote enorme seguraria a requisição por minutos (cada clique tem pausa).
const maxScreenActions = 500

// screen devolve o controle da tela de uma máquina pela porta do visualizador; trocado nos testes.
func (s *Server) screen(port uint16) screenctl.Screen {
	if s.ScreenFor != nil {
		return s.ScreenFor(port)
	}
	return screenctl.ForPort(port)
}

// screenshot atende GET .../screen: o quadro atual em JPEG, com o tamanho nos cabeçalhos X-Screen-Width e
// X-Screen-Height (as coordenadas das ações são nesses pixels).
func (s *Server) screenshot(c *gin.Context) {
	port, err := s.Machines.DisplayPort(c.Param("name"))
	if err != nil {
		machineError(c, err)
		return
	}
	shot, err := s.screen(port).Capture(c.Request.Context())
	if err != nil {
		screenError(c, err)
		return
	}
	c.Header("X-Screen-Width", strconv.Itoa(shot.Width))
	c.Header("X-Screen-Height", strconv.Itoa(shot.Height))
	c.Header("Cache-Control", "no-store")
	c.Data(http.StatusOK, "image/jpeg", shot.JPEG)
}

// screenActions atende POST .../screen/actions: executa as ações em ordem e responde 204 quando todas saíram.
func (s *Server) screenActions(c *gin.Context) {
	var req ScreenActionsRequest
	if err := bindStrict(c, &req); err != nil {
		invalid(c, "invalid request: "+err.Error())
		return
	}
	if len(req.Actions) > maxScreenActions {
		invalid(c, "too many actions in one request (max "+strconv.Itoa(maxScreenActions)+")")
		return
	}
	port, err := s.Machines.DisplayPort(c.Param("name"))
	if err != nil {
		machineError(c, err)
		return
	}
	if err := s.screen(port).Do(c.Request.Context(), req.Actions); err != nil {
		screenError(c, err)
		return
	}
	c.Status(http.StatusNoContent)
}

// invalid responde 400 com o código CONFIG_ERROR, como as outras recusas de pedido das rotas de máquina.
func invalid(c *gin.Context, msg string) {
	machineError(c, domain.ValidationError{Err: domain.MachineError{Code: domain.CodeConfig, Msg: msg}})
}

// screenError responde 400 para ação recusada e 502 para falha do visualizador da máquina (ele é o serviço do outro
// lado desta chamada).
func screenError(c *gin.Context, err error) {
	var ae screenctl.ActionError
	if errors.As(err, &ae) {
		invalid(c, err.Error())
		return
	}
	var body apiError
	body.Error.Code, body.Error.Message, body.Code = "SCREEN_UNAVAILABLE", err.Error(), "SCREEN_UNAVAILABLE"
	c.JSON(http.StatusBadGateway, body)
}
