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
	"strings"
	"time"

	"github.com/gin-gonic/gin"
	"github.com/gin-gonic/gin/binding"

	"runner.codes/internal/adapters/runfile"
	"runner.codes/internal/app"
	"runner.codes/internal/domain"
	"runner.codes/internal/protocol"
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
	Update(name string, change func(*domain.Egress)) (domain.Machine, error)
	Pause(name string) error
	Resume(name string) error
	Sync(ctx context.Context, name string) error
	Branch(ctx context.Context, from, to string) (domain.Machine, error)
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
	User        string   `json:"user"`
	GPUVRAMMiB  uint32   `json:"gpu_vram_mib"`
	// Rede: net liga a saída; ports publica no 127.0.0.1 do host; egress é a política.
	Net    bool          `json:"net"`
	Ports  []string      `json:"ports"`
	Egress domain.Egress `json:"egress"`
	// Network junta a máquina a uma rede nomeada (o mesmo que --network). Aceita também o
	// booleano `network: true` (o mesmo que --net).
	Network networkField `json:"network"`
	// MemoryMb e Cmd são nomes alternativos aceitos para memory_mib e workload: clientes
	// que usam essa forma funcionam sem mudar o corpo.
	MemoryMb uint32   `json:"memoryMb"`
	Cmd      []string `json:"cmd"`
	// AllowedHosts e AllowedCidrs são nomes alternativos aceitos da lista de saída
	// (allow_hosts e allow_cidrs) e ligam a rede.
	AllowedHosts []string `json:"allowedHosts"`
	AllowedCidrs []string `json:"allowedCidrs"`
	// Nested (ou o nome alternativo NestedVirt) só passa com o servidor em --allow-nested-virt.
	Nested     bool `json:"nested"`
	NestedVirt bool `json:"nestedVirt"`
	// Credentials são bindings de credencial; o valor vem do ambiente do `rc serve`.
	Credentials []domain.Credential `json:"credentials"`
	// Secrets resolvem no ambiente do `rc serve` (from_env) ou num arquivo dele (from_file).
	Secrets []domain.Secret `json:"secrets"`
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
		StopSignal: r.StopSignal, StopTimeout: r.StopTimeout, StopOnExit: r.StopOnExit, User: r.User, Nested: r.Nested || r.NestedVirt,
		Network: domain.Network{Enabled: r.Net || r.Network.on, Egress: r.Egress, Name: r.Network.name, Credentials: r.Credentials}, Secrets: r.Secrets}
	if over.MemoryMiB == 0 {
		over.MemoryMiB = r.MemoryMb
	}
	if len(over.Workload) == 0 {
		over.Workload = r.Cmd
	}
	if len(r.AllowedHosts)+len(r.AllowedCidrs) > 0 {
		over.Network.Enabled = true
		over.Network.Egress.AllowHosts = append(over.Network.Egress.AllowHosts, r.AllowedHosts...)
		over.Network.Egress.AllowCIDRs = append(over.Network.Egress.AllowCIDRs, r.AllowedCidrs...)
	}
	for _, s := range r.Ports {
		ps, err := domain.ParsePorts(s)
		if err != nil {
			return base, domain.ValidationError{Err: domain.MachineError{Code: domain.CodeConfig, Msg: err.Error()}}
		}
		over.Network.Ports = append(over.Network.Ports, ps...)
	}
	// No JSON não há "flag passada": o campo conta quando não é o valor zero.
	set := map[string]bool{"stack": r.Stack != "", "image": r.Image != "", "cpus": r.CPUs != 0, "memory": r.MemoryMiB != 0 || r.MemoryMb != 0,
		"disk-gib": r.DiskGiB != 0, "gpu": r.GPU, "gpu-vram": r.GPUVRAMMiB != 0, "env": len(r.Env) > 0, "workdir": r.Workdir != "",
		"init": len(r.Init) > 0, "stop-signal": r.StopSignal != "", "stop-timeout": r.StopTimeout != 0, "stop-on-exit": r.StopOnExit, "user": r.User != "",
		"net": r.Net || r.Network.on || len(r.AllowedHosts)+len(r.AllowedCidrs) > 0, "port": len(r.Ports) > 0, "block-private": r.Egress.BlockPrivate,
		"network": r.Network.name != "", "nested": r.Nested || r.NestedVirt}
	m := runfile.Override(base, over, func(k string) bool { return set[k] })
	for _, mt := range m.Mounts {
		if _, _, ok := mt.S3(); ok {
			continue // bucket não toca o disco do servidor: pode vir pela API
		}
		// Diretório do host pela API seria leitura e escrita arbitrária no disco do servidor
		// para quem alcança a porta. Só a CLI, que roda com o usuário local, monta diretório.
		return m, domain.ValidationError{Err: domain.MachineError{Code: domain.CodeConfig,
			Msg: "host directory volumes are not accepted over the API (use `rc machine create -v` on the host); s3:// volumes are"}}
	}
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
	User    string   `json:"user"`
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
// formato do `--error-format json`, e o código também no topo ("code"), forma alternativa
// aceita: scripts que leem `.code` funcionam.
type apiError struct {
	Code  string `json:"code"`
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
	// Forma alternativa aceita: o caminho no próprio URL (files/%2Ftmp%2Fa.txt ou files/tmp/a.txt).
	g.PUT("/:name/files/*path", s.putMachineFile)
	g.GET("/:name/files/*path", s.getMachineFile)
	g.GET("/:name/logs", s.machineLogs)
	g.PUT("/:name/egress", s.updateEgress)
	g.POST("/:name/pause", s.pauseMachine)
	g.POST("/:name/resume", s.resumeMachine)
	g.POST("/:name/sync", s.syncMachine)
	g.POST("/:name/branch", s.branchMachine)
}

// machineError responde o erro com o status HTTP do código estável.
func machineError(c *gin.Context, err error) {
	// Erro de infraestrutura sem código próprio sai como RUNNER_ERROR, com o nome deste
	// projeto.
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
	body.Error.Code, body.Error.Message, body.Code = code, err.Error(), code
	c.JSON(status, body)
}

// bindStrict lê o corpo JSON recusando campo desconhecido, e depois aplica as regras
// `binding` da struct. O ShouldBindJSON ignora campo a mais: um `allowedHost` com erro de
// digitação criaria a máquina sem a restrição de saída pedida, calado.
func bindStrict(c *gin.Context, v any) error {
	dec := json.NewDecoder(c.Request.Body)
	dec.DisallowUnknownFields()
	if err := dec.Decode(v); err != nil {
		return err
	}
	return binding.Validator.ValidateStruct(v)
}

// createMachine atende POST /api/v1/machines.
func (s *Server) createMachine(c *gin.Context) {
	var req CreateMachineRequest
	if err := bindStrict(c, &req); err != nil {
		machineError(c, domain.ValidationError{Err: fmt.Errorf("invalid request: %w", err)})
		return
	}
	if (req.Nested || req.NestedVirt) && !s.AllowNested {
		machineError(c, domain.ValidationError{Err: domain.MachineError{Code: domain.CodeConfig,
			Msg: "nested virtualization over the API needs the server started with `rc serve --allow-nested-virt`"}})
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
	if err := bindStrict(c, &req); err != nil || len(req.Command) == 0 {
		msg := "invalid request: command (argv list) is required"
		if err != nil && strings.Contains(err.Error(), "unknown field") {
			msg = "invalid request: " + err.Error()
		}
		machineError(c, domain.ValidationError{Err: errors.New(msg)})
		return app.ExecRequest{}, false
	}
	return app.ExecRequest{Argv: req.Command, Env: req.Env, Cwd: req.Workdir, Stdin: req.Stdin, User: req.User,
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
// string JSON e `exit` no fim ({"exitCode":N}). Erro antes do primeiro evento
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
		data, isText := v.(string)
		if !isText {
			b, _ := json.Marshal(v)
			data = string(b)
		}
		fmt.Fprintf(w, "event: %s\ndata: %s\n\n", event, data)
		w.Flush()
	}
	// Uma linha por evento, texto cru (`event: stdout` / `data: line1`). O
	// pedaço sem \n final espera o resto da linha; o que sobrar sai antes do exit.
	partial := map[string]string{}
	lines := func(stream, data string) {
		buf := partial[stream] + data
		for {
			i := strings.IndexByte(buf, '\n')
			if i < 0 {
				break
			}
			emit(stream, buf[:i])
			buf = buf[i+1:]
		}
		partial[stream] = buf
	}
	flush := func() {
		for _, stream := range []string{protocol.EventStdout, protocol.EventStderr} {
			if partial[stream] != "" {
				emit(stream, partial[stream])
				partial[stream] = ""
			}
		}
	}
	res, err := s.Machines.Exec(c.Request.Context(), c.Param("name"), req, lines)
	flush()
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
	n, err := s.Machines.CopyIn(c.Request.Context(), c.Param("name"), filePath(c), c.Request.Body, mode)
	if err != nil {
		machineError(c, err)
		return
	}
	c.JSON(http.StatusOK, gin.H{"path": filePath(c), "bytes": n})
}

// getMachineFile atende GET .../files?path=/abs: devolve o conteúdo cru. O status só é
// escrito no primeiro byte, para um erro antes disso ainda virar JSON com o código.
func (s *Server) getMachineFile(c *gin.Context) {
	w := &lazyWriter{c: c}
	if _, err := s.Machines.CopyOut(c.Request.Context(), c.Param("name"), filePath(c), w); err != nil {
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

// pauseMachine atende POST .../pause.
func (s *Server) pauseMachine(c *gin.Context) {
	if err := s.Machines.Pause(c.Param("name")); err != nil {
		machineError(c, err)
		return
	}
	c.Status(http.StatusNoContent)
}

// syncMachine atende POST .../sync: as cópias staged voltam ao host.
func (s *Server) syncMachine(c *gin.Context) {
	if err := s.Machines.Sync(c.Request.Context(), c.Param("name")); err != nil {
		machineError(c, err)
		return
	}
	c.Status(http.StatusNoContent)
}

// resumeMachine atende POST .../resume.
func (s *Server) resumeMachine(c *gin.Context) {
	if err := s.Machines.Resume(c.Param("name")); err != nil {
		machineError(c, err)
		return
	}
	c.Status(http.StatusNoContent)
}

// branchMachine atende POST .../branch com {"name": "nova"}: cópia do disco da máquina.
func (s *Server) branchMachine(c *gin.Context) {
	var req struct {
		Name string `json:"name" binding:"required"`
	}
	if err := bindStrict(c, &req); err != nil {
		machineError(c, domain.ValidationError{Err: fmt.Errorf("invalid request: %w", err)})
		return
	}
	m, err := s.Machines.Branch(c.Request.Context(), c.Param("name"), req.Name)
	if err != nil {
		machineError(c, err)
		return
	}
	c.JSON(http.StatusCreated, app.MachineStatus{Machine: m, State: app.StateStopped})
}

// updateEgress atende PUT .../egress: troca a política de saída inteira pelo corpo (JSON
// de domain.Egress), também com a máquina ligada.
func (s *Server) updateEgress(c *gin.Context) {
	var e domain.Egress
	if err := bindStrict(c, &e); err != nil {
		machineError(c, domain.ValidationError{Err: fmt.Errorf("invalid request: %w", err)})
		return
	}
	m, err := s.Machines.Update(c.Param("name"), func(cur *domain.Egress) { *cur = e })
	if err != nil {
		machineError(c, err)
		return
	}
	c.JSON(http.StatusOK, m)
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

// filePath é o caminho do arquivo no guest: a query ?path= ou o resto do URL (files/*path, a
// forma alternativa aceita). O Go já decodifica o %2F, e a barra repetida do começo sai.
func filePath(c *gin.Context) string {
	if p := c.Query("path"); p != "" {
		return p
	}
	if p := c.Param("path"); p != "" {
		return "/" + strings.TrimLeft(p, "/")
	}
	return ""
}

// networkField é o "network" do corpo: texto (rede nomeada, o nome do rc) ou booleano (o
// mesmo que --net).
type networkField struct {
	name string
	on   bool
}

// UnmarshalJSON aceita "lab" ou true/false.
func (n *networkField) UnmarshalJSON(b []byte) error {
	if err := json.Unmarshal(b, &n.on); err == nil {
		return nil
	}
	return json.Unmarshal(b, &n.name)
}
