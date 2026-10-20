// Package client é o SDK Go da API HTTP do runner.code (`rc serve`): job efêmero e o ciclo
// de vida das máquinas. Só biblioteca padrão, e tipos
// próprios em vez dos de internal/: o SDK é contrato público e não muda quando o domínio
// interno muda.
package client

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"strconv"
	"strings"
	"time"
)

// Client fala com um `rc serve`. BaseURL é a raiz (http://127.0.0.1:8080); HTTP vazio usa um
// cliente com timeout de 10 minutos, acima do teto de 300 s de um job.
type Client struct {
	BaseURL string
	HTTP    *http.Client
}

// New devolve um cliente para baseURL.
func New(baseURL string) *Client {
	return &Client{BaseURL: strings.TrimRight(baseURL, "/"), HTTP: &http.Client{Timeout: 10 * time.Minute}}
}

// Error é a recusa da API: Status é o HTTP, Code é o código estável das rotas de máquina
// (NOT_FOUND, INVALID_STATE, CONFIG...) e fica vazio nas rotas de job, que não o têm.
type Error struct {
	Status  int
	Code    string
	Message string
}

// Error junta status, código e mensagem.
func (e *Error) Error() string {
	if e.Code != "" {
		return fmt.Sprintf("runner.code: %d %s: %s", e.Status, e.Code, e.Message)
	}
	return fmt.Sprintf("runner.code: %d: %s", e.Status, e.Message)
}

// File é um arquivo de um job efêmero; o primeiro é o que roda.
type File struct {
	Name    string `json:"name"`
	Content string `json:"content"`
}

// Job é um pedido de execução efêmera (POST /api/v1/execute): VM nova, usada uma vez.
type Job struct {
	Language string `json:"language"`
	Code     string `json:"code,omitempty"`
	Files    []File `json:"files,omitempty"`
	Stdin    string `json:"stdin,omitempty"`
	// Timeout em segundos; zero usa o padrão do servidor (10 s).
	Timeout int  `json:"timeout,omitempty"`
	GPU     bool `json:"gpu,omitempty"`
}

// JobResult é a resposta do job. Status "success" diz que a API executou, não que o programa
// passou: o resultado do programa está em Exception (Timeout, Compilation failed...) e Stderr.
type JobResult struct {
	Status        string `json:"status"`
	Stdout        string `json:"stdout"`
	Stderr        string `json:"stderr"`
	Exception     string `json:"exception"`
	ExecutionTime int64  `json:"executionTime"`
	Error         string `json:"error"`
}

// UnmarshalJSON aceita stderr e exception como null: a API herdada os manda assim quando vazios.
func (r *JobResult) UnmarshalJSON(b []byte) error {
	var raw struct {
		Status        string  `json:"status"`
		Stdout        string  `json:"stdout"`
		Stderr        *string `json:"stderr"`
		Exception     *string `json:"exception"`
		ExecutionTime int64   `json:"executionTime"`
		Error         string  `json:"error"`
	}
	if err := json.Unmarshal(b, &raw); err != nil {
		return err
	}
	*r = JobResult{Status: raw.Status, Stdout: raw.Stdout, ExecutionTime: raw.ExecutionTime, Error: raw.Error}
	if raw.Stderr != nil {
		r.Stderr = *raw.Stderr
	}
	if raw.Exception != nil {
		r.Exception = *raw.Exception
	}
	return nil
}

// Egress é a política de saída de uma máquina (a mesma tabela [network] do runner.code).
type Egress struct {
	AllowHosts        []string `json:"allow_hosts,omitempty"`
	AllowHostPatterns []string `json:"allow_host_patterns,omitempty"`
	AllowCIDRs        []string `json:"allow_cidrs,omitempty"`
	DenyCIDRs         []string `json:"deny_cidrs,omitempty"`
	BlockPrivate      bool     `json:"block_private,omitempty"`
	AllowPrivate      []string `json:"allow_private,omitempty"`
}

// MachineSpec é o pedido de criação. Campo vazio fica com o padrão do servidor; com
// RunnerCode, o arquivo é a base e os campos preenchidos vencem.
type MachineSpec struct {
	Name        string   `json:"name"`
	Stack       string   `json:"stack,omitempty"`
	Image       string   `json:"image,omitempty"`
	CPUs        int      `json:"cpus,omitempty"`
	MemoryMiB   int      `json:"memory_mib,omitempty"`
	DiskGiB     int      `json:"disk_gib,omitempty"`
	GPU         bool     `json:"gpu,omitempty"`
	GPUVRAMMiB  int      `json:"gpu_vram_mib,omitempty"`
	Env         []string `json:"env,omitempty"`
	Workdir     string   `json:"workdir,omitempty"`
	Init        []string `json:"init,omitempty"`
	Workload    []string `json:"workload,omitempty"`
	StopSignal  string   `json:"stop_signal,omitempty"`
	StopTimeout int      `json:"stop_timeout,omitempty"`
	StopOnExit  bool     `json:"stop_on_exit,omitempty"`
	Net         bool     `json:"net,omitempty"`
	Ports       []string `json:"ports,omitempty"`
	Egress      *Egress  `json:"egress,omitempty"`
	Network     string   `json:"network,omitempty"`
	RunnerCode  string   `json:"runner_code,omitempty"`
}

// Machine é o estado de uma máquina como a API devolve. Network traz o IP na rede nomeada.
type Machine struct {
	Name      string    `json:"name"`
	Stack     string    `json:"stack"`
	Image     string    `json:"image"`
	CPUs      int       `json:"cpus"`
	MemoryMiB int       `json:"memory_mib"`
	DiskGiB   int       `json:"disk_gib"`
	GPU       bool      `json:"gpu"`
	CreatedAt time.Time `json:"created_at"`
	State     string    `json:"state"`
	PID       int       `json:"pid"`
	Network   struct {
		Enabled bool   `json:"enabled"`
		Name    string `json:"name"`
		Address string `json:"address"`
	} `json:"network"`
}

// ExecOptions completa um exec: ambiente, diretório, stdin e prazo em segundos.
type ExecOptions struct {
	Env     []string `json:"env,omitempty"`
	Workdir string   `json:"workdir,omitempty"`
	Stdin   string   `json:"stdin,omitempty"`
	Timeout int      `json:"timeout,omitempty"`
}

// ExecResult é o resultado de um comando numa máquina; ExitCode é o do processo.
type ExecResult struct {
	ExitCode      int    `json:"exitCode"`
	Stdout        string `json:"stdout"`
	Stderr        string `json:"stderr"`
	TimedOut      bool   `json:"timedOut"`
	ExecutionTime int64  `json:"executionTime"`
}

// do manda a requisição e decodifica a resposta em out (nil ignora o corpo). Resposta fora
// de 2xx vira *Error, com o código da API quando ela mandou um.
func (c *Client) do(ctx context.Context, method, path string, body io.Reader, contentType string, out any) error {
	req, err := http.NewRequestWithContext(ctx, method, c.BaseURL+path, body)
	if err != nil {
		return err
	}
	if contentType != "" {
		req.Header.Set("Content-Type", contentType)
	}
	hc := c.HTTP
	if hc == nil {
		hc = http.DefaultClient
	}
	resp, err := hc.Do(req)
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	data, err := io.ReadAll(resp.Body)
	if err != nil {
		return err
	}
	if resp.StatusCode/100 != 2 {
		return decodeError(resp.StatusCode, data)
	}
	switch o := out.(type) {
	case nil:
		return nil
	case *[]byte:
		*o = data
		return nil
	default:
		return json.Unmarshal(data, out)
	}
}

// decodeError lê os dois formatos de erro da API: {"error":{"code","message"}} das máquinas
// e {"error":"..."} dos jobs; corpo que não é nenhum dos dois vira a mensagem crua.
func decodeError(status int, data []byte) error {
	e := &Error{Status: status, Message: strings.TrimSpace(string(data))}
	var obj struct {
		Error json.RawMessage `json:"error"`
	}
	if json.Unmarshal(data, &obj) != nil || len(obj.Error) == 0 {
		return e
	}
	var structured struct {
		Code    string `json:"code"`
		Message string `json:"message"`
	}
	if json.Unmarshal(obj.Error, &structured) == nil {
		e.Code, e.Message = structured.Code, structured.Message
		return e
	}
	var text string
	if json.Unmarshal(obj.Error, &text) == nil {
		e.Message = text
	}
	return e
}

// doJSON manda in como JSON (nil manda sem corpo) e decodifica a resposta em out.
func (c *Client) doJSON(ctx context.Context, method, path string, in, out any) error {
	if in == nil {
		return c.do(ctx, method, path, nil, "", out)
	}
	b, err := json.Marshal(in)
	if err != nil {
		return err
	}
	return c.do(ctx, method, path, bytes.NewReader(b), "application/json", out)
}

// machinePath monta /api/v1/machines/<nome><sufixo>, com o nome escapado.
func machinePath(name, suffix string) string {
	return "/api/v1/machines/" + url.PathEscape(name) + suffix
}

// Execute roda um job numa VM efêmera e devolve o resultado.
func (c *Client) Execute(ctx context.Context, j Job) (JobResult, error) {
	var r JobResult
	err := c.doJSON(ctx, http.MethodPost, "/api/v1/execute", j, &r)
	return r, err
}

// CreateMachine cria a máquina (parada).
func (c *Client) CreateMachine(ctx context.Context, s MachineSpec) (Machine, error) {
	var m Machine
	err := c.doJSON(ctx, http.MethodPost, "/api/v1/machines", s, &m)
	return m, err
}

// Machines lista as máquinas com o estado de cada uma.
func (c *Client) Machines(ctx context.Context) ([]Machine, error) {
	var ms []Machine
	err := c.doJSON(ctx, http.MethodGet, "/api/v1/machines", nil, &ms)
	return ms, err
}

// Machine devolve uma máquina.
func (c *Client) Machine(ctx context.Context, name string) (Machine, error) {
	var m Machine
	err := c.doJSON(ctx, http.MethodGet, machinePath(name, ""), nil, &m)
	return m, err
}

// DeleteMachine apaga a máquina; force mata se estiver ligada.
func (c *Client) DeleteMachine(ctx context.Context, name string, force bool) error {
	q := ""
	if force {
		q = "?force=true"
	}
	return c.doJSON(ctx, http.MethodDelete, machinePath(name, q), nil, nil)
}

// Start liga a máquina e devolve o estado com o PID.
func (c *Client) Start(ctx context.Context, name string) (Machine, error) {
	var m Machine
	err := c.doJSON(ctx, http.MethodPost, machinePath(name, "/start"), nil, &m)
	return m, err
}

// Stop desliga a máquina. timeout negativo usa o stop_timeout dela; zero mata na hora.
func (c *Client) Stop(ctx context.Context, name string, timeout time.Duration) error {
	q := ""
	if timeout >= 0 {
		q = "?timeout=" + strconv.Itoa(int(timeout/time.Second))
	}
	return c.doJSON(ctx, http.MethodPost, machinePath(name, "/stop"+q), nil, nil)
}

// Exec roda argv na máquina ligada, sem shell implícito (para shell: "sh", "-c", ...).
func (c *Client) Exec(ctx context.Context, name string, argv []string, o ExecOptions) (ExecResult, error) {
	body := struct {
		Command []string `json:"command"`
		ExecOptions
	}{argv, o}
	var r ExecResult
	err := c.doJSON(ctx, http.MethodPost, machinePath(name, "/exec"), body, &r)
	return r, err
}

// WriteFile grava data em path (absoluto) dentro da máquina; mode 0 usa o padrão (0644).
func (c *Client) WriteFile(ctx context.Context, name, path string, data []byte, mode uint32) error {
	q := url.Values{"path": {path}}
	if mode != 0 {
		q.Set("mode", strconv.FormatUint(uint64(mode), 8))
	}
	return c.do(ctx, http.MethodPut, machinePath(name, "/files?"+q.Encode()), bytes.NewReader(data), "application/octet-stream", nil)
}

// ReadFile lê path (absoluto) de dentro da máquina.
func (c *Client) ReadFile(ctx context.Context, name, path string) ([]byte, error) {
	var b []byte
	err := c.do(ctx, http.MethodGet, machinePath(name, "/files?"+url.Values{"path": {path}}.Encode()), nil, "", &b)
	return b, err
}

// Logs devolve o console da máquina: tail 0 traz tudo; workloadOnly filtra a saída do workload.
func (c *Client) Logs(ctx context.Context, name string, tail int, workloadOnly bool) (string, error) {
	q := url.Values{}
	if tail > 0 {
		q.Set("tail", strconv.Itoa(tail))
	}
	if workloadOnly {
		q.Set("source", "workload")
	}
	var b []byte
	err := c.do(ctx, http.MethodGet, machinePath(name, "/logs?"+q.Encode()), nil, "", &b)
	return string(b), err
}

// UpdateEgress troca a política de saída inteira, também com a máquina ligada.
func (c *Client) UpdateEgress(ctx context.Context, name string, e Egress) (Machine, error) {
	var m Machine
	err := c.doJSON(ctx, http.MethodPut, machinePath(name, "/egress"), e, &m)
	return m, err
}

// Pause congela a máquina ligada.
func (c *Client) Pause(ctx context.Context, name string) error {
	return c.doJSON(ctx, http.MethodPost, machinePath(name, "/pause"), nil, nil)
}

// Resume continua a máquina pausada.
func (c *Client) Resume(ctx context.Context, name string) error {
	return c.doJSON(ctx, http.MethodPost, machinePath(name, "/resume"), nil, nil)
}

// Branch cria `to` como cópia copy-on-write do disco de `from`.
func (c *Client) Branch(ctx context.Context, from, to string) (Machine, error) {
	var m Machine
	err := c.doJSON(ctx, http.MethodPost, machinePath(from, "/branch"), map[string]string{"name": to}, &m)
	return m, err
}
