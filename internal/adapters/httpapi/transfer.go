package httpapi

import (
	"crypto/subtle"
	"fmt"
	"net/http"
	"net/http/httputil"
	"net/url"
	"strings"

	"github.com/gin-gonic/gin"

	"runner.codes/internal/app"
)

// requireToken exige `Authorization: Bearer <token>` em toda rota do grupo. É o que deixa o `rc
// serve` escutar fora do loopback (o servidor da casa falando com o rc de outro servidor da rede
// interna): sem token, quem alcança a porta liga, apaga e lê o disco de qualquer máquina.
// Comparação em tempo constante: `==` vazaria, pelo tempo da resposta, o prefixo certo do token.
func requireToken(token string) gin.HandlerFunc {
	want := []byte("Bearer " + token)
	return func(c *gin.Context) {
		if subtle.ConstantTimeCompare([]byte(c.GetHeader("Authorization")), want) != 1 {
			var body apiError
			body.Error.Code, body.Error.Message, body.Code = "UNAUTHORIZED", "missing or invalid bearer token", "UNAUTHORIZED"
			c.AbortWithStatusJSON(http.StatusUnauthorized, body)
			return
		}
		c.Next()
	}
}

// exportMachine atende GET .../export: a máquina desligada no formato de transferência (gzip), em
// streaming. Erro antes do primeiro byte sai como JSON; depois dele o corpo para no meio, sem o
// marcador de fim nem o rodapé do gzip — e a importação recusa corpo truncado. (Abortar a conexão
// com http.ErrAbortHandler não serve aqui: o Recovery do gin engole o pânico.)
func (s *Server) exportMachine(c *gin.Context) {
	name := c.Param("name")
	st, err := s.Machines.Status(name)
	if err != nil {
		machineError(c, err)
		return
	}
	if st.State != app.StateStopped {
		var body apiError
		body.Error.Code, body.Error.Message, body.Code = "INVALID_STATE", "machine "+name+" must be stopped to export", "INVALID_STATE"
		c.JSON(http.StatusConflict, body)
		return
	}
	c.Header("Content-Type", "application/x-runner-machine")
	c.Header("Content-Disposition", fmt.Sprintf(`attachment; filename="%s.rcmx"`, name))
	c.Status(http.StatusOK)
	if err := s.Machines.Export(c.Request.Context(), name, c.Writer); err != nil {
		_ = c.Error(err) // vai para o log do gin; o corpo já está truncado para quem lê
	}
}

// importMachine atende POST /api/v1/machines/import: cria a máquina de uma exportação (o nome vem
// do registro dentro dela). Responde a máquina criada, como o create.
func (s *Server) importMachine(c *gin.Context) {
	mc, err := s.Machines.Import(c.Request.Context(), c.Request.Body)
	if err != nil {
		machineError(c, err)
		return
	}
	st, err := s.Machines.Status(mc.Name)
	if err != nil {
		machineError(c, err)
		return
	}
	c.JSON(http.StatusCreated, st)
}

// displayBase é a raiz do visualizador da tela de uma máquina (trocado nos testes).
func (s *Server) displayBase(port uint16) string {
	if s.DisplayBase != nil {
		return s.DisplayBase(port)
	}
	return fmt.Sprintf("http://127.0.0.1:%d", port)
}

// display atende /api/v1/machines/:name/display/*path: repassa ao visualizador da tela, que só
// escuta no loopback do host. Com o token da API na frente, o servidor da casa mostra a tela de uma
// máquina que mora em outro servidor sem que o visualizador (que não tem autenticação) saia do
// loopback. FlushInterval -1: o /frame é long-poll e o quadro não pode ficar parado num buffer.
func (s *Server) display(c *gin.Context) {
	port, err := s.Machines.DisplayPort(c.Param("name"))
	if err != nil {
		machineError(c, err)
		return
	}
	target, _ := url.Parse(s.displayBase(port)) // montado acima: sempre válido
	rest := strings.TrimPrefix(c.Param("path"), "/")
	p := &httputil.ReverseProxy{
		Rewrite: func(pr *httputil.ProxyRequest) {
			pr.SetURL(target)
			pr.Out.URL.Path = "/" + rest
			pr.Out.URL.RawPath = ""
			pr.Out.Host = target.Host
			// O token do rc não segue para o visualizador.
			pr.Out.Header.Del("Authorization")
		},
		FlushInterval: -1,
		ErrorHandler: func(w http.ResponseWriter, _ *http.Request, err error) {
			http.Error(w, "display unavailable: "+err.Error(), http.StatusBadGateway)
		},
	}
	p.ServeHTTP(plainWriter{c.Writer}, c.Request)
}

// plainWriter esconde o CloseNotify do writer do gin: o ReverseProxy usa o CloseNotifier quando o
// writer diz ter, e o do gin repassa a chamada ao writer de baixo, que pode não ter (o httptest
// não tem: pânico medido). Unwrap mantém o Flush ao alcance do http.ResponseController, de que o
// FlushInterval -1 depende.
type plainWriter struct{ http.ResponseWriter }

// Unwrap devolve o writer embrulhado para o ResponseController achar o Flush.
func (p plainWriter) Unwrap() http.ResponseWriter { return p.ResponseWriter }
