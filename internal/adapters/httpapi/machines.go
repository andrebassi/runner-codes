package httpapi

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"os"
	"strconv"
	"time"

	"github.com/gin-gonic/gin"

	"runner.code/internal/adapters/runfile"
	"runner.code/internal/app"
	"runner.code/internal/domain"
)

// MachineService é o que as rotas de máquina usam; o app.Machines implementa.
type MachineService interface {
	Create(ctx context.Context, m domain.Machine) (domain.Machine, error)
	List() ([]app.MachineStatus, error)
	Status(name string) (app.MachineStatus, error)
	Start(ctx context.Context, name string) (app.MachineStatus, error)
	Stop(ctx context.Context, name string, timeout time.Duration) error
	Delete(ctx context.Context, name string, force bool) error
	Exec(ctx context.Context, name string, req app.ExecRequest, onOutput func(stream, data string)) (domain.Result, error)
	CopyIn(ctx context.Context, name, guestPath string, r io.Reader, mode os.FileMode) (int64, error)
	CopyOut(ctx context.Context, name, guestPath string, w io.Writer) (int64, error)
	Logs(name string, tail int, source string) (string, error)
}

// CreateMachineRequest é o corpo de POST /api/v1/machines.
type CreateMachineRequest struct {
	Name      string `json:"name" binding:"required"`
	Stack     string `json:"stack"`
	CPUs      uint8  `json:"cpus"`
	MemoryMiB uint32 `json:"memory_mib"`
	DiskGiB   int    `json:"disk_gib"`
	GPU       bool   `json:"gpu"`
	// Os campos do runner.code, um a um.
	Image       string   `json:"image"`
	Env         []string `json:"env"`
	Workdir     string   `json:"workdir"`
	Init        []string `json:"init"`
	Workload    []string `json:"workload"`
	StopSignal  string   `json:"stop_signal"`
	StopTimeout int      `json:"stop_timeout"`
	StopOnExit  bool     `json:"stop_on_exit"`
	GPUVRAMMiB  uint32   `json:"gpu_vram_mib"`
	// RunnerCode é o texto de um runner.code inteiro; os campos acima que vierem preenchidos
	// vencem o arquivo, como as flags na CLI.
	RunnerCode string `json:"runner_code"`
}

// machine converte o corpo na máquina pedida: o runner.code (se veio) é a base, e todo
// campo não vazio do JSON vai por cima dele.
func (r CreateMachineRequest) machine() (domain.Machine, error) {
	var base domain.Machine
	if r.RunnerCode != "" {
		m, err := runfile.Parse([]byte(r.RunnerCode))
		if err != nil {
			return base, domain.ValidationError{Err: domain.MachineError{Code: domain.CodeConfig, Msg: err.Error()}}
		}
		base = m
	}
	over := domain.Machine{Name: r.Name, Stack: r.Stack, Image: r.Image, CPUs: r.CPUs, MemoryMiB: r.MemoryMiB, DiskGiB: r.DiskGiB,
		GPU: r.GPU, GPUVRAMMiB: r.GPUVRAMMiB, Env: r.Env, Workdir: r.Workdir, Init: r.Init, Workload: r.Workload,
		StopSignal: r.StopSignal, StopTimeout: r.StopTimeout, StopOnExit: r.StopOnExit}
	// No JSON não há "flag passada": o campo conta quando não é o valor zero.
	set := map[string]bool{"stack": r.Stack != "", "image": r.Image != "", "cpus": r.CPUs != 0, "memory": r.MemoryMiB != 0,
		"disk-gib": r.DiskGiB != 0, "gpu": r.GPU, "gpu-vram": r.GPUVRAMMiB != 0, "env": len(r.Env) > 0, "workdir": r.Workdir != "",
		"init": len(r.Init) > 0, "stop-signal": r.StopSignal != "", "stop-timeout": r.StopTimeout != 0, "stop-on-exit": r.StopOnExit}
	m := runfile.Override(base, over, func(k string) bool { return set[k] })
	if m.Stack == "" && m.Image == "" {
		return m, domain.ValidationError{Err: domain.MachineError{Code: domain.CodeConfig, Msg: "stack or image is required"}}
	}
	return m, nil
}

// ExecMachineRequest é o corpo de POST .../exec e .../exec/stream: argv explícito, sem
// shell implícito (quem quer shell manda ["sh", "-c", "..."]).
type ExecMachineRequest struct {
	Command []string `json:"command" binding:"required"`
	Env     []string `json:"env"`
	Workdir string   `json:"workdir"`
	Stdin   string   `json:"stdin"`
	Timeout int      `json:"timeout"` // segundos; 0 = sem prazo
}

// ExecMachineResponse é a resposta de .../exec.
type ExecMachineResponse struct {
	ExitCode      int    `json:"exitCode"`
	Stdout        string `json:"stdout"`
	Stderr        string `json:"stderr"`
	TimedOut      bool   `json:"timedOut"`
	ExecutionTime int64  `json:"executionTime"`
}

// apiError é o corpo de erro das rotas de máquina: {"error":{"code","message"}}, o mesmo
// formato do `--error-format json` do smolvm.
type apiError struct {
	Error struct {
		Code    string `json:"code"`
		Message string `json:"message"`
	} `json:"error"`
}

// machineRoutes monta /api/v1/machines quando há serviço de máquinas.
func (s *Server) machineRoutes(v1 *gin.RouterGroup) {
	if s.Machines == nil {
		return
	}
	g := v1.Group("/machines")
	g.POST("", s.createMachine)
	g.GET("", s.listMachines)
	g.GET("/:name", s.machineStatus)
	g.DELETE("/:name", s.deleteMachine)
	g.POST("/:name/start", s.startMachine)
	g.POST("/:name/stop", s.stopMachine)
	g.POST("/:name/exec", s.execMachine)
	g.POST("/:name/exec/stream", s.execMachineStream)
	g.PUT("/:name/files", s.putMachineFile)
	g.GET("/:name/files", s.getMachineFile)
	g.GET("/:name/logs", s.machineLogs)
}

// machineError responde o erro com o status HTTP do código estável.
func machineError(c *gin.Context, err error) {
	// Erro de infraestrutura sem código próprio sai como RUNNER_ERROR: o formato é o do
	// smolvm (SMOLVM_ERROR lá), com o nome deste projeto.
	code, status := "RUNNER_ERROR", http.StatusInternalServerError
	var me domain.MachineError
	var ve domain.ValidationError
	switch {
	case errors.As(err, &me):
		code = me.Code
		switch me.Code {
		case domain.CodeNotFound:
			status = http.StatusNotFound
		case domain.CodeConflict, domain.CodeInvalidState:
			status = http.StatusConflict
		case domain.CodeConfig:
			status = http.StatusBadRequest
		}
	case errors.As(err, &ve):
		code, status = domain.CodeConfig, http.StatusBadRequest
	}
	var body apiError
	body.Error.Code, body.Error.Message = code, err.Error()
	c.JSON(status, body)
}

// createMachine atende POST /api/v1/machines.
func (s *Server) createMachine(c *gin.Context) {
	var req CreateMachineRequest
	if err := c.ShouldBindJSON(&req); err != nil {
		machineError(c, domain.ValidationError{Err: fmt.Errorf("invalid request: %w", err)})
		return
	}
	mc, err := req.machine()
	if err != nil {
		machineError(c, err)
		return
	}
	m, err := s.Machines.Create(c.Request.Context(), mc)
	if err != nil {
		machineError(c, err)
		return
	}
	c.JSON(http.StatusCreated, app.MachineStatus{Machine: m, State: app.StateStopped})
}

// listMachines atende GET /api/v1/machines.
func (s *Server) listMachines(c *gin.Context) {
	ms, err := s.Machines.List()
	if err != nil {
		machineError(c, err)
		return
	}
	if ms == nil {
		ms = []app.MachineStatus{}
	}
	c.JSON(http.StatusOK, ms)
}

// machineStatus atende GET /api/v1/machines/:name.
func (s *Server) machineStatus(c *gin.Context) {
	st, err := s.Machines.Status(c.Param("name"))
	if err != nil {
		machineError(c, err)
		return
	}
	c.JSON(http.StatusOK, st)
}

// deleteMachine atende DELETE /api/v1/machines/:name (?force=true mata se ligada).
func (s *Server) deleteMachine(c *gin.Context) {
	if err := s.Machines.Delete(c.Request.Context(), c.Param("name"), c.Query("force") == "true"); err != nil {
		machineError(c, err)
		return
	}
	c.Status(http.StatusNoContent)
}

// startMachine atende POST .../start.
func (s *Server) startMachine(c *gin.Context) {
	st, err := s.Machines.Start(c.Request.Context(), c.Param("name"))
	if err != nil {
		machineError(c, err)
		return
	}
	c.JSON(http.StatusOK, st)
}

// stopMachine atende POST .../stop; ?timeout=N em segundos (0 mata na hora). Sem timeout
// vale o stop_timeout da máquina (ou 10 s).
func (s *Server) stopMachine(c *gin.Context) {
	timeout := time.Duration(-1)
	if t := c.Query("timeout"); t != "" {
		n, err := strconv.Atoi(t)
		if err != nil || n < 0 || n > 300 {
			machineError(c, domain.ValidationError{Err: fmt.Errorf("timeout %q out of range [0, 300]", t)})
			return
		}
		timeout = time.Duration(n) * time.Second
	}
	if err := s.Machines.Stop(c.Request.Context(), c.Param("name"), timeout); err != nil {
		machineError(c, err)
		return
	}
	c.Status(http.StatusNoContent)
}

// execRequest lê e converte o corpo de exec.
func execRequest(c *gin.Context) (app.ExecRequest, bool) {
	var req ExecMachineRequest
	if err := c.ShouldBindJSON(&req); err != nil || len(req.Command) == 0 {
		machineError(c, domain.ValidationError{Err: errors.New("invalid request: command (argv list) is required")})
		return app.ExecRequest{}, false
	}
	return app.ExecRequest{Argv: req.Command, Env: req.Env, Cwd: req.Workdir, Stdin: req.Stdin,
		Timeout: time.Duration(req.Timeout) * time.Second}, true
}

// execMachine atende POST .../exec: espera o fim e devolve tudo.
func (s *Server) execMachine(c *gin.Context) {
	req, ok := execRequest(c)
	if !ok {
		return
	}
	res, err := s.Machines.Exec(c.Request.Context(), c.Param("name"), req, nil)
	if err != nil {
		machineError(c, err)
		return
	}
	c.JSON(http.StatusOK, ExecMachineResponse{ExitCode: res.ExitCode, Stdout: res.Stdout, Stderr: res.Stderr,
		TimedOut: res.TimedOut, ExecutionTime: res.Duration.Milliseconds()})
}

// execMachineStream atende POST .../exec/stream em SSE: `stdout`/`stderr` com o pedaço como
// string JSON e `exit` no fim ({"exitCode":N}), como o smolvm. Erro antes do primeiro evento
// responde JSON com o status do código; depois, vira evento `error`.
func (s *Server) execMachineStream(c *gin.Context) {
	req, ok := execRequest(c)
	if !ok {
		return
	}
	w := c.Writer
	started := false
	emit := func(event string, v interface{}) {
		if !started {
			started = true
			h := w.Header()
			h.Set("Content-Type", "text/event-stream")
			h.Set("Cache-Control", "no-cache")
			h.Set("X-Accel-Buffering", "no")
			w.WriteHeader(http.StatusOK)
		}
		b, _ := json.Marshal(v)
		fmt.Fprintf(w, "event: %s\ndata: %s\n\n", event, b)
		w.Flush()
	}
	res, err := s.Machines.Exec(c.Request.Context(), c.Param("name"), req, func(stream, data string) { emit(stream, data) })
	if err != nil {
		if started {
			emit("error", gin.H{"error": err.Error()})
			return
		}
		machineError(c, err)
		return
	}
	emit("exit", gin.H{"exitCode": res.ExitCode, "timedOut": res.TimedOut, "executionTime": res.Duration.Milliseconds()})
}

// putMachineFile atende PUT .../files?path=/abs: o corpo cru vira o arquivo na máquina.
func (s *Server) putMachineFile(c *gin.Context) {
	mode := os.FileMode(0o644)
	if m := c.Query("mode"); m != "" {
		v, err := strconv.ParseUint(m, 8, 32)
		if err != nil || v > 0o777 {
			machineError(c, domain.ValidationError{Err: fmt.Errorf("mode %q must be octal up to 777", m)})
			return
		}
		mode = os.FileMode(v)
	}
	n, err := s.Machines.CopyIn(c.Request.Context(), c.Param("name"), c.Query("path"), c.Request.Body, mode)
	if err != nil {
		machineError(c, err)
		return
	}
	c.JSON(http.StatusOK, gin.H{"path": c.Query("path"), "bytes": n})
}

// getMachineFile atende GET .../files?path=/abs: devolve o conteúdo cru. O status só é
// escrito no primeiro byte, para um erro antes disso ainda virar JSON com o código.
func (s *Server) getMachineFile(c *gin.Context) {
	w := &lazyWriter{c: c}
	if _, err := s.Machines.CopyOut(c.Request.Context(), c.Param("name"), c.Query("path"), w); err != nil {
		if !w.started {
			machineError(c, err)
		}
		return
	}
	if !w.started {
		c.Data(http.StatusOK, "application/octet-stream", nil)
	}
}

// lazyWriter só manda o cabeçalho 200 quando o primeiro pedaço chega.
type lazyWriter struct {
	c       *gin.Context
	started bool
}

// Write manda o cabeçalho na primeira vez e repassa os bytes.
func (l *lazyWriter) Write(p []byte) (int, error) {
	if !l.started {
		l.started = true
		l.c.Header("Content-Type", "application/octet-stream")
		l.c.Status(http.StatusOK)
	}
	return l.c.Writer.Write(p)
}

// machineLogs atende GET .../logs?tail=N&source=all|workload em texto puro.
func (s *Server) machineLogs(c *gin.Context) {
	tail, _ := strconv.Atoi(c.Query("tail"))
	out, err := s.Machines.Logs(c.Param("name"), tail, c.Query("source"))
	if err != nil {
		machineError(c, err)
		return
	}
	c.Data(http.StatusOK, "text/plain; charset=utf-8", []byte(out))
}
