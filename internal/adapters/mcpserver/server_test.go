package mcpserver

import (
	"bytes"
	"context"
	"encoding/base64"
	"encoding/json"
	"errors"
	"strings"
	"testing"

	"runner.codes/internal/adapters/screenctl"
)

// fakeBackend grava as ações e comandos recebidos e responde com o que foi configurado.
type fakeBackend struct {
	actions  [][]screenctl.Action
	commands []string
	timeouts []int
	err      error
	exit     int
}

// Capture devolve uma "imagem" fixa de 1512x794.
func (f *fakeBackend) Capture(context.Context) (screenctl.Shot, error) {
	if f.err != nil {
		return screenctl.Shot{}, f.err
	}
	return screenctl.Shot{JPEG: []byte("jpeg-bytes"), Width: 1512, Height: 794}, nil
}

// Do grava as ações.
func (f *fakeBackend) Do(_ context.Context, a []screenctl.Action) error {
	f.actions = append(f.actions, a)
	return f.err
}

// Exec grava o comando e devolve saída fixa.
func (f *fakeBackend) Exec(_ context.Context, cmd string, timeout int) (ExecResult, error) {
	f.commands, f.timeouts = append(f.commands, cmd), append(f.timeouts, timeout)
	if f.err != nil {
		return ExecResult{}, f.err
	}
	return ExecResult{Stdout: "out", Stderr: "err", ExitCode: f.exit}, nil
}

// session manda as linhas ao servidor e devolve as respostas decodificadas, na ordem.
func session(t *testing.T, s *Server, lines ...string) []map[string]any {
	t.Helper()
	var out bytes.Buffer
	if err := s.Serve(context.Background(), strings.NewReader(strings.Join(lines, "\n")+"\n"), &out); err != nil {
		t.Fatal(err)
	}
	var resps []map[string]any
	dec := json.NewDecoder(&out)
	for dec.More() {
		var r map[string]any
		if err := dec.Decode(&r); err != nil {
			t.Fatal(err)
		}
		resps = append(resps, r)
	}
	return resps
}

// call monta a linha de um tools/call.
func call(id int, name, args string) string {
	return `{"jsonrpc":"2.0","id":` + itoa(id) + `,"method":"tools/call","params":{"name":"` + name + `","arguments":` + args + `}}`
}

// itoa evita importar strconv só para o teste.
func itoa(n int) string { b, _ := json.Marshal(n); return string(b) }

// result pega o result de uma resposta (falha o teste se for erro).
func result(t *testing.T, r map[string]any) map[string]any {
	t.Helper()
	res, ok := r["result"].(map[string]any)
	if !ok {
		t.Fatalf("sem result: %v", r)
	}
	return res
}

// text junta os blocos de texto de um resultado de ferramenta.
func text(res map[string]any) string {
	var b strings.Builder
	for _, c := range res["content"].([]any) {
		if m := c.(map[string]any); m["type"] == "text" {
			b.WriteString(m["text"].(string))
		}
	}
	return b.String()
}

// TestProtocol: initialize diz a versão e a capacidade de ferramentas; notificação não tem resposta; ping, método
// desconhecido e JSON torto respondem como o JSON-RPC manda; linha vazia é ignorada.
func TestProtocol(t *testing.T) {
	s := &Server{Backend: &fakeBackend{}, Machine: "desk", Version: "1.2.3"}
	resps := session(t, s,
		`{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2025-03-26"}}`,
		`{"jsonrpc":"2.0","method":"notifications/initialized"}`,
		``,
		`{"jsonrpc":"2.0","id":"a","method":"ping"}`,
		`{"jsonrpc":"2.0","id":3,"method":"resources/list"}`,
		`{not json`,
	)
	if len(resps) != 4 {
		t.Fatalf("respostas: %d %v", len(resps), resps)
	}
	init := result(t, resps[0])
	if init["protocolVersion"] != protocolVersion || init["serverInfo"].(map[string]any)["version"] != "1.2.3" ||
		init["capabilities"].(map[string]any)["tools"] == nil || !strings.Contains(init["instructions"].(string), "desk") {
		t.Fatalf("initialize: %v", init)
	}
	if resps[1]["id"] != "a" || resps[1]["result"] == nil {
		t.Fatalf("ping: %v", resps[1])
	}
	if e := resps[2]["error"].(map[string]any); e["code"].(float64) != codeMethodNotFound {
		t.Fatalf("método desconhecido: %v", resps[2])
	}
	if e := resps[3]["error"].(map[string]any); e["code"].(float64) != codeParse || resps[3]["id"] != nil {
		t.Fatalf("JSON torto: %v", resps[3])
	}
}

// TestToolsList: as nove ferramentas, cada uma com descrição e esquema de objeto sem campo extra.
func TestToolsList(t *testing.T) {
	s := &Server{Backend: &fakeBackend{}, Machine: "desk"}
	tools := result(t, session(t, s, `{"jsonrpc":"2.0","id":1,"method":"tools/list"}`)[0])["tools"].([]any)
	want := []string{"screenshot", "click", "move", "drag", "type", "key", "scroll", "actions", "exec"}
	if len(tools) != len(want) {
		t.Fatalf("%d ferramentas", len(tools))
	}
	for i, raw := range tools {
		tl := raw.(map[string]any)
		schema := tl["inputSchema"].(map[string]any)
		if tl["name"] != want[i] || tl["description"] == "" || schema["type"] != "object" || schema["additionalProperties"] != false {
			t.Fatalf("ferramenta %d: %v", i, tl)
		}
	}
}

// TestScreenTools: screenshot devolve a imagem em base64 e o tamanho; cada ação vira a Action certa, com o nome da
// ferramenta; actions passa a lista inteira; falha da tela volta como isError, não como erro do protocolo.
func TestScreenTools(t *testing.T) {
	f := &fakeBackend{}
	s := &Server{Backend: f, Machine: "desk"}
	resps := session(t, s,
		call(1, "screenshot", `{}`),
		call(2, "click", `{"x":10,"y":20,"button":"right","double":true}`),
		call(3, "type", `{"text":"ls"}`),
		call(4, "key", `{"keys":"Return"}`),
		call(5, "scroll", `{"x":1,"y":2,"direction":"up","amount":5}`),
		call(6, "drag", `{"x":1,"y":2,"to_x":3,"to_y":4}`),
		call(7, "move", `{"x":7,"y":8}`),
		call(8, "actions", `{"actions":[{"action":"key","keys":"a"},{"action":"wait","ms":10}]}`),
	)
	img := result(t, resps[0])["content"].([]any)[0].(map[string]any)
	data, _ := base64.StdEncoding.DecodeString(img["data"].(string))
	if img["type"] != "image" || img["mimeType"] != "image/jpeg" || string(data) != "jpeg-bytes" || !strings.Contains(text(result(t, resps[0])), "1512x794") {
		t.Fatalf("screenshot: %v", resps[0])
	}
	want := []screenctl.Action{
		{Action: "click", X: 10, Y: 20, Button: "right", Double: true}, {Action: "type", Text: "ls"},
		{Action: "key", Keys: "Return"}, {Action: "scroll", X: 1, Y: 2, Direction: "up", Amount: 5},
		{Action: "drag", X: 1, Y: 2, ToX: 3, ToY: 4}, {Action: "move", X: 7, Y: 8},
	}
	for i, w := range want {
		if len(f.actions[i]) != 1 || f.actions[i][0] != w {
			t.Fatalf("ação %d: %+v", i, f.actions[i])
		}
	}
	if last := f.actions[len(f.actions)-1]; len(last) != 2 || last[1].Ms != 10 {
		t.Fatalf("actions: %+v", last)
	}
	for _, r := range resps[1:] {
		if res := result(t, r); res["isError"] == true || !strings.Contains(text(res), "done") {
			t.Fatalf("ação: %v", r)
		}
	}
	f.err = errors.New("viewer down")
	resps = session(t, s, call(1, "screenshot", `{}`), call(2, "key", `{"keys":"a"}`))
	for _, r := range resps {
		if res := result(t, r); res["isError"] != true || !strings.Contains(text(res), "viewer down") {
			t.Fatalf("falha da tela: %v", r)
		}
	}
}

// TestExecTool: o comando vai ao backend com o prazo pedido (60 s sem prazo); exit diferente de 0 e falha do
// backend são isError; comando vazio é recusado sem rodar.
func TestExecTool(t *testing.T) {
	f := &fakeBackend{}
	s := &Server{Backend: f, Machine: "desk"}
	resps := session(t, s, call(1, "exec", `{"command":"uname -a","timeout":5}`), call(2, "exec", `{"command":"true"}`), call(3, "exec", `{"command":"  "}`))
	res := result(t, resps[0])
	if res["isError"] == true || !strings.Contains(text(res), "exit 0") || !strings.Contains(text(res), "out") || f.commands[0] != "uname -a" || f.timeouts[0] != 5 || f.timeouts[1] != 60 {
		t.Fatalf("exec: %v %v %v", res, f.commands, f.timeouts)
	}
	if res := result(t, resps[2]); res["isError"] != true || len(f.commands) != 2 {
		t.Fatalf("vazio: %v", res)
	}
	f.exit = 2
	if res := result(t, session(t, s, call(1, "exec", `{"command":"false"}`))[0]); res["isError"] != true || !strings.Contains(text(res), "exit 2") {
		t.Fatalf("exit 2: %v", res)
	}
	f.err = errors.New("not running")
	if res := result(t, session(t, s, call(1, "exec", `{"command":"x"}`))[0]); res["isError"] != true || !strings.Contains(text(res), "not running") {
		t.Fatalf("falha: %v", res)
	}
}

// TestBadCalls: ferramenta desconhecida, argumento com campo desconhecido e params tortos são erro do protocolo
// (-32602), e nada chega à máquina; argumentos ausentes valem como objeto vazio.
func TestBadCalls(t *testing.T) {
	f := &fakeBackend{}
	s := &Server{Backend: f, Machine: "desk"}
	resps := session(t, s,
		call(1, "teleport", `{}`),
		call(2, "click", `{"x":1,"y":1,"botton":"left"}`),
		`{"jsonrpc":"2.0","id":3,"method":"tools/call","params":"torto"}`,
		call(4, "actions", `{"actions":"x"}`),
		call(5, "exec", `{"cmd":"ls"}`),
		`{"jsonrpc":"2.0","id":6,"method":"tools/call","params":{"name":"screenshot"}}`,
	)
	for i, r := range resps[:5] {
		if e, ok := r["error"].(map[string]any); !ok || e["code"].(float64) != codeInvalidParams {
			t.Fatalf("chamada %d: %v", i, r)
		}
	}
	if len(f.actions) != 0 || len(f.commands) != 0 {
		t.Fatal("chamada inválida chegou à máquina")
	}
	if res := result(t, resps[5]); res["isError"] == true {
		t.Fatalf("sem argumentos: %v", res)
	}
}

// failingWriter recusa toda escrita.
type failingWriter struct{}

// Write falha sempre.
func (failingWriter) Write([]byte) (int, error) { return 0, errors.New("closed") }

// TestServeWriteError: saída fechada encerra o Serve com o erro (o cliente foi embora).
func TestServeWriteError(t *testing.T) {
	s := &Server{Backend: &fakeBackend{}}
	if err := s.Serve(context.Background(), strings.NewReader(`{"jsonrpc":"2.0","id":1,"method":"ping"}`+"\n"), failingWriter{}); err == nil {
		t.Fatal("erro de escrita engolido")
	}
}
