// Package httpapi expõe o runner por HTTP, mantendo o contrato de /api/v1/run e
// /api/v1/execute que existia antes da migração para libkrun.
package httpapi

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"net/http"
	"net/url"
	"runner.codes/internal/adapters/screenctl"
	"strconv"
	"strings"
	"time"

	"github.com/gin-gonic/gin"

	"runner.codes/internal/app"
	"runner.codes/internal/domain"
)

// Runner executa um job; o app.Executor implementa.
type Runner interface {
	Run(ctx context.Context, job domain.Job) (domain.Result, error)
	// RunStream entrega a saída a onOutput enquanto o programa roda; job recusado na
	// validação volta como erro sem nenhuma chamada a onOutput.
	RunStream(ctx context.Context, job domain.Job, onOutput func(stream, data string)) (domain.Result, error)
	Known(lang string) bool
}

// StreamResult é o evento `result` que fecha POST /api/v1/execute/stream. Não repete
// stdout/stderr: o cliente já os recebeu pelos eventos, e repetir dobraria os bytes.
type StreamResult struct {
	Status        string      `json:"status"`
	ExitCode      int         `json:"exitCode"`
	Exception     interface{} `json:"exception"`
	ExecutionTime int64       `json:"executionTime"`
	TimedOut      bool        `json:"timedOut"`
	Error         string      `json:"error,omitempty"`
}

// LanguageInfo é uma linha de GET /api/v1/languages.
type LanguageInfo struct {
	Name  string `json:"name"`
	Image string `json:"image"`
	Ready bool   `json:"ready"`
}

// Server guarda as dependências dos handlers.
type Server struct {
	Runner    Runner
	Languages func() []LanguageInfo
	Version   string
	// Pool devolve as VMs prontas por linguagem; nil omite o campo no /health.
	Pool func() map[string]int
	// Machines liga as rotas /api/v1/machines; nil deixa só as rotas de job.
	Machines MachineService
	// AllowNested aceita nested no create de máquina. Desligado, quem alcança a porta não
	// entrega as extensões de virtualização do host a um guest (rc serve --allow-nested-virt).
	AllowNested bool
	// ScreenFor monta o controle da tela pela porta do visualizador; nil usa o visualizador no 127.0.0.1 do host
	// (os testes apontam para um visualizador falso).
	ScreenFor func(port uint16) screenctl.Screen
	// DisplayBase é a raiz do visualizador pela porta (rotas /display); nil usa o 127.0.0.1 do host.
	DisplayBase func(port uint16) string
	// Token exige `Authorization: Bearer <Token>` em /api/v1/*; vazio deixa a API aberta (só serve no
	// loopback: o cmd recusa escutar fora dele sem token).
	Token string
	// Host mede o host (CPU, memória, disco, KVM) para o GET /api/v1/host; nil não liga a rota.
	Host func() (domain.Host, error)
	// Proc mede o processo de uma máquina ligada (CPU acumulada, RSS); nil deixa o /host sem consumo
	// por máquina.
	Proc func(pid int) (cpuNanos, rssBytes int64, err error)
	// EgressAPI é a raiz da API de leitura do proxy de saída do host (http://127.0.0.1:15081, RC_EGRESS_API): o
	// GET /api/v1/egress/events a repassa, com o token do rc na frente. Vazio responde 501.
	EgressAPI string
	// EgressClient fala com a EgressAPI; nil usa um cliente com prazo curto (o proxy está no mesmo host).
	EgressClient *http.Client
}

// FileRequest é um arquivo no corpo da requisição.
type FileRequest struct {
	Name    string `json:"name"`
	Content string `json:"content"`
}

// RunRequest é o corpo de POST /api/v1/run. Stdin string roda uma vez; lista roda uma
// vez por item, cada uma numa VM nova.
type RunRequest struct {
	Language string        `json:"language" binding:"required"`
	Stdin    interface{}   `json:"stdin,omitempty"`
	Files    []FileRequest `json:"files" binding:"required"`
}

// ExecuteRequest é o corpo de POST /api/v1/execute (formato interno, aceita code solto).
type ExecuteRequest struct {
	Language string        `json:"language" binding:"required"`
	Code     string        `json:"code"`
	Files    []FileRequest `json:"files,omitempty"`
	Stdin    interface{}   `json:"stdin,omitempty"`
	Timeout  int           `json:"timeout"`
	// GPU pede VM com virtio-gpu (Venus); stack sem GPU no catálogo responde 400 (E007).
	GPU bool `json:"gpu,omitempty"`
}

// RunResponse é a resposta de uma execução. Stderr e Exception são null quando vazios.
type RunResponse struct {
	Status        string      `json:"status"`
	Stdout        string      `json:"stdout"`
	Stderr        interface{} `json:"stderr"`
	Exception     interface{} `json:"exception"`
	ExecutionTime int64       `json:"executionTime"`
	Error         string      `json:"error,omitempty"`
	Stdin         string      `json:"stdin,omitempty"`
}

// Handler monta as rotas.
func (s *Server) Handler() http.Handler {
	gin.SetMode(gin.ReleaseMode)
	r := gin.New()
	r.Use(gin.Recovery())
	r.GET("/health", func(c *gin.Context) {
		body := gin.H{"status": "ok", "version": s.Version}
		if s.Pool != nil {
			body["pool"] = s.Pool()
		}
		c.JSON(http.StatusOK, body)
	})
	v1 := r.Group("/api/v1")
	if s.Token != "" {
		v1.Use(requireToken(s.Token))
	}
	v1.GET("/languages", func(c *gin.Context) { c.JSON(http.StatusOK, s.Languages()) })
	v1.POST("/run", s.run)
	v1.POST("/execute", s.execute)
	v1.POST("/execute/stream", s.executeStream)
	if s.Host != nil {
		v1.GET("/host", s.host)
	}
	v1.GET("/egress/events", s.egressEvents)
	s.machineRoutes(v1)
	return r
}

// egressEvents atende GET /api/v1/egress/events: repassa o cursor (after) e o tamanho (limit) à API do proxy de
// saída do host e devolve a resposta dela como veio (JSON do registro). Só esses dois parâmetros passam, já
// validados como inteiros: a query do chamador não vira URL do proxy.
func (s *Server) egressEvents(c *gin.Context) {
	if s.EgressAPI == "" {
		c.JSON(http.StatusNotImplemented, gin.H{"error": "this host has no egress proxy (RC_EGRESS_API)"})
		return
	}
	q := url.Values{}
	for _, k := range []string{"after", "limit"} {
		if v := c.Query(k); v != "" {
			if _, err := strconv.ParseUint(v, 10, 64); err != nil {
				c.JSON(http.StatusBadRequest, gin.H{"error": k + " must be a non-negative integer"})
				return
			}
			q.Set(k, v)
		}
	}
	client := s.EgressClient
	if client == nil {
		client = &http.Client{Timeout: 10 * time.Second}
	}
	req, err := http.NewRequestWithContext(c.Request.Context(), http.MethodGet, strings.TrimRight(s.EgressAPI, "/")+"/events?"+q.Encode(), nil)
	if err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
		return
	}
	resp, err := client.Do(req)
	if err != nil {
		c.JSON(http.StatusBadGateway, gin.H{"error": "egress proxy: " + err.Error()})
		return
	}
	defer resp.Body.Close()
	c.DataFromReader(resp.StatusCode, resp.ContentLength, resp.Header.Get("Content-Type"), resp.Body, nil)
}

// host atende GET /api/v1/host: o que o host tem livre e quantas máquinas tem (ligadas e no total).
// Quem distribui máquinas entre vários rc escolhe por aqui.
func (s *Server) host(c *gin.Context) {
	h, err := s.Host()
	if err != nil {
		c.JSON(http.StatusInternalServerError, gin.H{"error": err.Error()})
		return
	}
	if s.Machines != nil {
		ms, err := s.Machines.List()
		if err != nil {
			machineError(c, err)
			return
		}
		h.Machines = len(ms)
		for _, m := range ms {
			if m.State != app.StateRunning {
				continue
			}
			h.Running++
			if s.Proc == nil || m.PID == 0 {
				continue
			}
			// Processo que sumiu entre a lista e a leitura (a máquina acabou de desligar) fica de fora.
			if cpu, rss, err := s.Proc(m.PID); err == nil {
				h.Procs = append(h.Procs, domain.MachineProc{Name: m.Name, PID: m.PID, CPUNanos: cpu, RSSBytes: rss})
			}
		}
		if p, ok := s.Machines.(PersistentService); ok {
			h.PersistentDisks = p.HasPersistentDisks()
			h.Preempting, h.Drained = p.Preemption()
		}
	}
	c.JSON(http.StatusOK, h)
}

// executeStream atende POST /api/v1/execute/stream: mesmo corpo do /execute, resposta em
// Server-Sent Events. Eventos `stdout`/`stderr` trazem o pedaço como string JSON (SSE não
// aceita quebra de linha crua no data); `result` fecha. O 200 só sai no primeiro evento,
// para job recusado ainda poder responder 400 JSON igual ao /execute.
func (s *Server) executeStream(c *gin.Context) {
	var req ExecuteRequest
	if err := c.ShouldBindJSON(&req); err != nil {
		c.JSON(http.StatusBadRequest, RunResponse{Status: "failed", Error: "invalid request: " + err.Error()})
		return
	}
	if _, batch := req.Stdin.([]interface{}); batch {
		c.JSON(http.StatusBadRequest, RunResponse{Status: "failed", Error: "invalid request: stdin as a list (batch) is not supported on the stream endpoint"})
		return
	}
	job := domain.Job{Language: req.Language, Code: req.Code, Files: toFiles(req.Files),
		Stdin: stdinString(req.Stdin), Timeout: time.Duration(req.Timeout) * time.Second, GPU: req.GPU}

	w := c.Writer
	started := false
	// emit é chamado só da goroutine que lê o guest (o cliente lê os frames em sequência),
	// então não precisa de lock.
	emit := func(event string, v interface{}) {
		if !started {
			started = true
			h := w.Header()
			h.Set("Content-Type", "text/event-stream")
			h.Set("Cache-Control", "no-cache")
			// Proxy nginx acumula a resposta por padrão e mataria o streaming.
			h.Set("X-Accel-Buffering", "no")
			w.WriteHeader(http.StatusOK)
		}
		b, _ := json.Marshal(v)
		fmt.Fprintf(w, "event: %s\ndata: %s\n\n", event, b)
		w.Flush()
	}
	res, err := s.Runner.RunStream(c.Request.Context(), job, func(stream, data string) { emit(stream, data) })
	if err != nil {
		if started {
			emit("error", gin.H{"error": err.Error()})
			return
		}
		var ve domain.ValidationError
		if errors.As(err, &ve) {
			c.JSON(http.StatusBadRequest, RunResponse{Status: "failed", Error: err.Error()})
			return
		}
		c.JSON(http.StatusInternalServerError, RunResponse{Status: "failed", Exception: err.Error()})
		return
	}
	status := res.Status
	if status == "" {
		status = "failed"
	}
	emit("result", StreamResult{Status: status, ExitCode: res.ExitCode, Exception: nullable(res.Exception),
		ExecutionTime: res.Duration.Milliseconds(), TimedOut: res.TimedOut, Error: res.Error})
}

// run atende POST /api/v1/run.
func (s *Server) run(c *gin.Context) {
	var req RunRequest
	if err := c.ShouldBindJSON(&req); err != nil {
		c.JSON(http.StatusBadRequest, RunResponse{Status: "failed", Error: "invalid request: " + err.Error()})
		return
	}
	files := toFiles(req.Files)
	if batch, ok := req.Stdin.([]interface{}); ok && len(batch) > 0 {
		out := make([]RunResponse, 0, len(batch))
		for _, in := range batch {
			stdin := stdinString(in)
			resp, _ := s.exec(c.Request.Context(), domain.Job{Language: req.Language, Files: files, Stdin: stdin})
			resp.Stdin = stdin
			out = append(out, resp)
		}
		c.JSON(http.StatusOK, out)
		return
	}
	resp, code := s.exec(c.Request.Context(), domain.Job{Language: req.Language, Files: files, Stdin: stdinString(req.Stdin)})
	c.JSON(code, resp)
}

// execute atende POST /api/v1/execute.
func (s *Server) execute(c *gin.Context) {
	var req ExecuteRequest
	if err := c.ShouldBindJSON(&req); err != nil {
		c.JSON(http.StatusBadRequest, RunResponse{Status: "failed", Error: "invalid request: " + err.Error()})
		return
	}
	job := domain.Job{Language: req.Language, Code: req.Code, Files: toFiles(req.Files),
		Stdin: stdinString(req.Stdin), Timeout: time.Duration(req.Timeout) * time.Second, GPU: req.GPU}
	resp, code := s.exec(c.Request.Context(), job)
	c.JSON(code, resp)
}

// exec roda o job e escolhe o status HTTP: 400 para job recusado na validação, 500 para
// falha de infraestrutura, 200 para qualquer execução (mesmo a que o programa falhou).
func (s *Server) exec(ctx context.Context, job domain.Job) (RunResponse, int) {
	res, err := s.Runner.Run(ctx, job)
	if err != nil {
		var ve domain.ValidationError
		if errors.As(err, &ve) {
			return RunResponse{Status: "failed", Error: err.Error()}, http.StatusBadRequest
		}
		return RunResponse{Status: "failed", Exception: err.Error()}, http.StatusInternalServerError
	}
	status := res.Status
	if status == "" {
		status = "failed"
	}
	return RunResponse{
		Status: status, Stdout: res.Stdout, Stderr: nullable(res.Stderr), Exception: nullable(res.Exception),
		ExecutionTime: res.Duration.Milliseconds(), Error: res.Error,
	}, http.StatusOK
}

// nullable devolve nil para texto vazio, mantendo o null do contrato.
func nullable(s string) interface{} {
	if s == "" {
		return nil
	}
	return s
}

// stdinString aceita o stdin como string ou como qualquer valor JSON escalar.
func stdinString(v interface{}) string {
	switch t := v.(type) {
	case nil:
		return ""
	case string:
		return t
	default:
		return fmt.Sprint(t)
	}
}

// toFiles converte os arquivos da requisição para o domínio.
func toFiles(in []FileRequest) []domain.File {
	out := make([]domain.File, 0, len(in))
	for _, f := range in {
		out = append(out, domain.File{Name: f.Name, Content: f.Content})
	}
	return out
}
