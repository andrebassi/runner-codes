// Package mcpserver é o servidor MCP (Model Context Protocol) do rc: expõe a tela e o shell de uma máquina como
// ferramentas para um agente (o Claude Code, por exemplo) operar a VM como uma pessoa — captura, clique, teclado,
// rolagem — e rodar comandos nela. Fala JSON-RPC 2.0 por stdio, uma mensagem por linha, como pede o transporte
// stdio do MCP.
package mcpserver

import (
	"bufio"
	"context"
	"encoding/base64"
	"encoding/json"
	"fmt"
	"io"
	"strings"
	"sync"

	"runner.codes/internal/adapters/screenctl"
)

// Backend é a máquina do outro lado: a tela (captura e ações) e o shell.
type Backend interface {
	Capture(ctx context.Context) (screenctl.Shot, error)
	Do(ctx context.Context, actions []screenctl.Action) error
	Exec(ctx context.Context, command string, timeoutSec int) (ExecResult, error)
}

// ExecResult é o resultado de um comando na máquina.
type ExecResult struct {
	Stdout   string
	Stderr   string
	ExitCode int
}

// Server atende as mensagens MCP de uma máquina.
type Server struct {
	Backend Backend
	// Machine é o nome da máquina, citado nas descrições das ferramentas para o agente saber onde está agindo.
	Machine string
	// Version é a versão do rc, devolvida no initialize.
	Version string
}

// protocolVersion é a versão do MCP que este servidor implementa; um cliente que pede outra recebe esta e decide.
const protocolVersion = "2025-06-18"

// Códigos de erro do JSON-RPC 2.0.
const (
	codeParse          = -32700
	codeMethodNotFound = -32601
	codeInvalidParams  = -32602
)

// request é uma mensagem recebida; sem id, é notificação e não tem resposta.
type request struct {
	JSONRPC string          `json:"jsonrpc"`
	ID      json.RawMessage `json:"id,omitempty"`
	Method  string          `json:"method"`
	Params  json.RawMessage `json:"params,omitempty"`
}

// response é a resposta a um request com id.
type response struct {
	JSONRPC string          `json:"jsonrpc"`
	ID      json.RawMessage `json:"id"`
	Result  any             `json:"result,omitempty"`
	Error   *rpcError       `json:"error,omitempty"`
}

// rpcError é o erro do JSON-RPC (método desconhecido, parâmetro inválido). Falha de uma ferramenta não é rpcError:
// volta como resultado com isError, para o agente ler e tentar outro caminho.
type rpcError struct {
	Code    int    `json:"code"`
	Message string `json:"message"`
}

// content é um bloco do resultado de uma ferramenta: texto ou imagem em base64.
type content struct {
	Type     string `json:"type"`
	Text     string `json:"text,omitempty"`
	Data     string `json:"data,omitempty"`
	MimeType string `json:"mimeType,omitempty"`
}

// toolResult é o resultado de tools/call.
type toolResult struct {
	Content []content `json:"content"`
	IsError bool      `json:"isError,omitempty"`
}

// Serve lê mensagens de r até o fim e escreve as respostas em w, uma por linha. Cada mensagem é atendida em ordem:
// ações na tela não podem se cruzar (um type no meio de um clique embaralharia os dois).
func (s *Server) Serve(ctx context.Context, r io.Reader, w io.Writer) error {
	sc := bufio.NewScanner(r)
	// Mensagens grandes (uma lista de ações longa) passam do buffer padrão de 64 KiB do Scanner.
	sc.Buffer(make([]byte, 1<<20), 16<<20)
	var mu sync.Mutex
	enc := json.NewEncoder(w)
	for sc.Scan() {
		line := strings.TrimSpace(sc.Text())
		if line == "" {
			continue
		}
		if resp := s.handle(ctx, []byte(line)); resp != nil {
			mu.Lock()
			err := enc.Encode(resp)
			mu.Unlock()
			if err != nil {
				return err
			}
		}
	}
	return sc.Err()
}

// handle atende uma mensagem; nil quando não há resposta (notificação).
func (s *Server) handle(ctx context.Context, raw []byte) *response {
	var req request
	if err := json.Unmarshal(raw, &req); err != nil {
		return &response{JSONRPC: "2.0", ID: json.RawMessage("null"), Error: &rpcError{codeParse, "parse error: " + err.Error()}}
	}
	if len(req.ID) == 0 {
		return nil
	}
	resp := &response{JSONRPC: "2.0", ID: req.ID}
	switch req.Method {
	case "initialize":
		resp.Result = map[string]any{
			"protocolVersion": protocolVersion,
			"capabilities":    map[string]any{"tools": map[string]any{}},
			"serverInfo":      map[string]any{"name": "rc", "version": s.Version},
			"instructions": "Tools to operate the screen and shell of the runner.codes machine " + s.Machine + ". " +
				"Take a screenshot first: coordinates are pixels of that image, origin at the top left.",
		}
	case "ping":
		resp.Result = map[string]any{}
	case "tools/list":
		resp.Result = map[string]any{"tools": s.tools()}
	case "tools/call":
		var p struct {
			Name      string          `json:"name"`
			Arguments json.RawMessage `json:"arguments"`
		}
		if err := json.Unmarshal(req.Params, &p); err != nil {
			resp.Error = &rpcError{codeInvalidParams, "invalid params: " + err.Error()}
			return resp
		}
		res, err := s.call(ctx, p.Name, p.Arguments)
		if err != nil {
			resp.Error = &rpcError{codeInvalidParams, err.Error()}
			return resp
		}
		resp.Result = res
	default:
		resp.Error = &rpcError{codeMethodNotFound, "method not found: " + req.Method}
	}
	return resp
}

// call executa uma ferramenta. Erro de parâmetro (ferramenta desconhecida, JSON torto) é erro do protocolo; falha da
// máquina ou ação recusada volta como resultado com isError.
func (s *Server) call(ctx context.Context, name string, args json.RawMessage) (toolResult, error) {
	if len(args) == 0 {
		args = json.RawMessage("{}")
	}
	switch name {
	case "screenshot":
		shot, err := s.Backend.Capture(ctx)
		if err != nil {
			return failed(err), nil
		}
		return toolResult{Content: []content{
			{Type: "image", Data: base64.StdEncoding.EncodeToString(shot.JPEG), MimeType: "image/jpeg"},
			{Type: "text", Text: fmt.Sprintf("screen %dx%d", shot.Width, shot.Height)},
		}}, nil
	case "click", "move", "drag", "type", "key", "scroll":
		var a screenctl.Action
		if err := strict(args, &a); err != nil {
			return toolResult{}, err
		}
		a.Action = name
		return s.do(ctx, []screenctl.Action{a}), nil
	case "actions":
		var p struct {
			Actions []screenctl.Action `json:"actions"`
		}
		if err := strict(args, &p); err != nil {
			return toolResult{}, err
		}
		return s.do(ctx, p.Actions), nil
	case "exec":
		var p struct {
			Command string `json:"command"`
			Timeout int    `json:"timeout"`
		}
		if err := strict(args, &p); err != nil {
			return toolResult{}, err
		}
		if strings.TrimSpace(p.Command) == "" {
			return failed(fmt.Errorf("command is empty")), nil
		}
		if p.Timeout <= 0 {
			p.Timeout = 60
		}
		r, err := s.Backend.Exec(ctx, p.Command, p.Timeout)
		if err != nil {
			return failed(err), nil
		}
		text := fmt.Sprintf("exit %d\n--- stdout\n%s\n--- stderr\n%s", r.ExitCode, r.Stdout, r.Stderr)
		return toolResult{Content: []content{{Type: "text", Text: text}}, IsError: r.ExitCode != 0}, nil
	default:
		return toolResult{}, fmt.Errorf("unknown tool %q", name)
	}
}

// do executa ações na tela e diz o que foi feito.
func (s *Server) do(ctx context.Context, actions []screenctl.Action) toolResult {
	if err := s.Backend.Do(ctx, actions); err != nil {
		return failed(err)
	}
	return toolResult{Content: []content{{Type: "text", Text: fmt.Sprintf("done (%d action(s)); take a screenshot to see the result", len(actions))}}}
}

// failed é o resultado de uma ferramenta que falhou, com a mensagem para o agente.
func failed(err error) toolResult {
	return toolResult{Content: []content{{Type: "text", Text: "error: " + err.Error()}}, IsError: true}
}

// strict decodifica os argumentos recusando campo desconhecido: um "botton" por engano clicaria com o padrão, calado.
func strict(raw json.RawMessage, v any) error {
	dec := json.NewDecoder(strings.NewReader(string(raw)))
	dec.DisallowUnknownFields()
	if err := dec.Decode(v); err != nil {
		return fmt.Errorf("invalid arguments: %w", err)
	}
	return nil
}
