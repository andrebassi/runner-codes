// Manda tarefas ao Claude Code de uma máquina do examples/desktop-claude, só pelo SDK Go, de três jeitos:
//
//   - headless: `claude -p` por Exec; a resposta volta em JSON, sem tela;
//   - gui:      abre uma janela do Claude na tela da máquina e digita a tarefa nela (xdotool, por Exec);
//   - keys:     abre a janela e digita a tarefa como teclado, pela API de tela do rc (ScreenActions do SDK).
//
// Cada modo pede ao Claude um arquivo próprio em /root/agent com um conteúdo conhecido, e o programa confere o
// arquivo pelo ReadFile: "funcionou" é o arquivo certo no disco da VM, não só o Claude ter respondido.
//
// Uso, com a máquina do examples/desktop-claude ligada e o token no ambiente do start:
//
//	go run ./examples/desktop-claude-agent                       # os três modos
//	go run ./examples/desktop-claude-agent -mode gui -task "..."  # um modo, com a tarefa dada
package main

import (
	"context"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"log"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"time"

	"runner.codes/pkg/client"
)

// options são as escolhas da linha de comando.
type options struct {
	server string
	name   string
	shots  string
	mode   string
	task   string
}

// main lê as flags e roda os modos pedidos; o primeiro que falhar encerra com rc 1.
func main() {
	var o options
	flag.StringVar(&o.server, "server", "http://127.0.0.1:8080", "endereço do rc serve")
	flag.StringVar(&o.name, "name", "desktop-claude", "máquina do examples/desktop-claude, já ligada")
	flag.StringVar(&o.shots, "shots", ".", "pasta onde salvar a tela de cada modo com janela (vazio não salva)")
	flag.StringVar(&o.mode, "mode", "all", "headless, gui, keys ou all")
	flag.StringVar(&o.task, "task", "", "tarefa livre (sem ela, cada modo pede o arquivo de prova dele)")
	flag.Parse()
	log.SetFlags(0)

	// O Claude pode levar minutos numa tarefa de verdade; o prazo é do context.
	ctx, cancel := context.WithTimeout(context.Background(), 20*time.Minute)
	defer cancel()
	c := client.New(o.server)
	c.HTTP = &http.Client{}

	if err := run(ctx, c, o); err != nil {
		log.Fatalf("erro: %v", err)
	}
}

// run prepara o Claude da máquina e roda os modos na ordem headless, gui, keys.
func run(ctx context.Context, c *client.Client, o options) error {
	m, err := c.Machine(ctx, o.name)
	if err != nil {
		return err
	}
	if m.State != "running" {
		return fmt.Errorf("a máquina %s está %s; ligue-a (com o token no ambiente do start) antes", o.name, m.State)
	}
	if err := prepare(ctx, c, o.name); err != nil {
		return err
	}
	modes := map[string]func(context.Context, *client.Client, options) error{
		"headless": headless, "gui": gui, "keys": keys,
	}
	order := []string{"headless", "gui", "keys"}
	if o.mode != "all" {
		if modes[o.mode] == nil {
			return fmt.Errorf("modo %q desconhecido (headless, gui, keys ou all)", o.mode)
		}
		order = []string{o.mode}
	}
	for _, name := range order {
		if err := modes[name](ctx, c, o); err != nil {
			return fmt.Errorf("%s: %w", name, err)
		}
	}
	return nil
}

// prepare deixa o Claude pronto para receber tarefa sem ninguém na frente: o primeiro uso interativo pergunta o
// tema e pede o aceite do modo sem permissões, e uma janela parada nessas telas engoliria a tarefa digitada.
// As duas respostas ficam no ~/.claude.json, que o Claude lê no início.
func prepare(ctx context.Context, c *client.Client, name string) error {
	script := `f=/root/.claude.json; [ -s "$f" ] || echo '{}' > "$f"
python3 - "$f" <<'PY'
import json, sys
f = sys.argv[1]
c = json.load(open(f))
c.update(hasCompletedOnboarding=True, theme="dark", bypassPermissionsModeAccepted=True)
projects = c.setdefault("projects", {})
for d in ("/root", "/root/agent"):
    projects.setdefault(d, {})["hasTrustDialogAccepted"] = True
json.dump(c, open(f, "w"), indent=2)
# O aceite do modo sem permissões é lido das configurações do usuário (a chave do ~/.claude.json só é migrada
# para cá): sem ele, a janela abre no aviso, e o Return da tarefa digitada escolhe "No, exit".
s = "/root/.claude/settings.json"
try:
    cfg = json.load(open(s))
except (FileNotFoundError, ValueError):
    cfg = {}
cfg["skipDangerousModePermissionPrompt"] = True
json.dump(cfg, open(s, "w"), indent=2)
PY
mkdir -p /root/agent`
	_, err := sh(ctx, c, name, script, 30)
	return err
}

// proofTask é a tarefa de prova de cada modo: um arquivo com um conteúdo que só existe se o Claude a cumpriu.
func proofTask(mode string) (task, path, want string) {
	path = "/root/agent/" + mode + ".txt"
	want = "feito pelo modo " + mode
	task = fmt.Sprintf("Crie o arquivo %s contendo exatamente a linha: %s", path, want)
	return task, path, want
}

// headless roda `claude -p` por Exec e lê a resposta em JSON (campos result e is_error).
func headless(ctx context.Context, c *client.Client, o options) error {
	task, path, want := proofTask("headless")
	if o.task != "" {
		task, path = o.task, ""
	}
	log.Printf("[headless] claude -p, resposta em JSON")
	// A tarefa vai pelo ambiente, não no texto do comando: aspas e acentos chegam intactos.
	r, err := c.Exec(ctx, o.name, []string{"bash", "-lc",
		`cd /root/agent && IS_SANDBOX=1 claude -p "$TASK" --dangerously-skip-permissions --output-format json`},
		client.ExecOptions{Env: []string{"TASK=" + task}, Timeout: 600})
	if err != nil {
		return err
	}
	var out struct {
		Result  string  `json:"result"`
		IsError bool    `json:"is_error"`
		Cost    float64 `json:"total_cost_usd"`
		Turns   int     `json:"num_turns"`
	}
	if err := json.Unmarshal([]byte(r.Stdout), &out); err != nil {
		return fmt.Errorf("saída não é o JSON do claude -p (exit %d): %s %s", r.ExitCode, short(r.Stdout), short(r.Stderr))
	}
	if out.IsError {
		return fmt.Errorf("o claude respondeu com erro: %s", short(out.Result))
	}
	log.Printf("[headless] %d turnos: %s", out.Turns, short(out.Result))
	if path == "" {
		return nil
	}
	return checkFile(ctx, c, o.name, path, want)
}

// gui abre uma janela do Claude na tela da máquina e digita a tarefa nela com o xdotool, como uma pessoa faria;
// dá para acompanhar ao vivo no navegador.
func gui(ctx context.Context, c *client.Client, o options) error {
	task, path, want := proofTask("gui")
	if o.task != "" {
		task, path = o.task, ""
	}
	log.Printf("[gui] abrindo a janela do Claude na tela e digitando a tarefa (acompanhe no visualizador da tela)")
	if err := openClaudeWindow(ctx, c, o.name, "Claude · gui"); err != nil {
		return err
	}
	// xdotool type manda texto Unicode (acentos inclusive) à janela ativa; o Return envia a tarefa ao Claude. Sem
	// --window: com ele o xdotool manda evento sintético (XSendEvent), que o terminal ignora (medido: o texto
	// chegava e o Enter não); sem ele, vai pelo XTEST, como tecla de verdade na janela ativa.
	if _, err := sh(ctx, c, o.name, `export DISPLAY=:0
w=$(xdotool search --name "Claude · gui" | head -1)
xdotool windowactivate --sync "$w" type --delay 15 "$TASK" && sleep 0.5 && xdotool key Return`, 60, "TASK="+task); err != nil {
		return err
	}
	if path == "" {
		log.Printf("[gui] tarefa enviada; a resposta fica na janela")
		return nil
	}
	if err := waitFile(ctx, c, o.name, path, want, 5*time.Minute); err != nil {
		return err
	}
	return saveScreen(ctx, c, o, "gui")
}

// keys abre a janela e digita a tarefa como teclado de verdade, pela API de tela do rc (ScreenActions do SDK): os
// eventos chegam aos dispositivos virtio-input da VM, o mesmo caminho da página no navegador. Não depende de nada
// instalado no guest, mas só digita ASCII (teclado US); a API recusa acento antes de digitar qualquer coisa.
func keys(ctx context.Context, c *client.Client, o options) error {
	task, path, want := proofTask("keys")
	if o.task != "" {
		task, path = o.task, ""
	}
	log.Printf("[keys] abrindo a janela do Claude e digitando a tarefa pela API de tela (ScreenActions)")
	if err := openClaudeWindow(ctx, c, o.name, "Claude · keys"); err != nil {
		return err
	}
	if err := c.ScreenActions(ctx, o.name,
		client.ScreenAction{Action: "type", Text: task},
		client.ScreenAction{Action: "wait", Ms: 300},
		client.ScreenAction{Action: "key", Keys: "Return"}); err != nil {
		return err
	}
	if path == "" {
		log.Printf("[keys] tarefa enviada; a resposta fica na janela")
		return nil
	}
	if err := waitFile(ctx, c, o.name, path, want, 5*time.Minute); err != nil {
		return err
	}
	return saveScreen(ctx, c, o, "keys")
}

// saveScreen grava a tela da máquina (Screenshot do SDK) em <shots>/agent-<modo>.jpg: a prova visual do modo.
func saveScreen(ctx context.Context, c *client.Client, o options, mode string) error {
	if o.shots == "" {
		return nil
	}
	shot, err := c.Screenshot(ctx, o.name)
	if err != nil {
		return err
	}
	file := filepath.Join(o.shots, "agent-"+mode+".jpg")
	if err := os.WriteFile(file, shot.JPEG, 0o644); err != nil {
		return err
	}
	log.Printf("      tela %dx%d salva em %s", shot.Width, shot.Height, file)
	return nil
}

// openClaudeWindow abre uma janela de terminal maximizada com o Claude interativo e espera ele desenhar o campo
// de entrada (o texto "bypass permissions" do rodapé), para a tarefa digitada não cair antes da hora.
func openClaudeWindow(ctx context.Context, c *client.Client, name, title string) error {
	open := `export DISPLAY=:0 HOME=/root
(setsid xfce4-terminal --maximize --working-directory=/root/agent --title="$TITLE" \
  -x bash -lc 'IS_SANDBOX=1 claude --dangerously-skip-permissions; exec bash -l' >/dev/null 2>&1 </dev/null &)
for i in $(seq 1 40); do w=$(xdotool search --name "$TITLE" | head -1); [ -n "$w" ] && break; sleep 0.5; done
[ -n "$w" ] || { echo "a janela não abriu"; exit 1; }`
	if _, err := sh(ctx, c, name, open, 40, "TITLE="+title); err != nil {
		return err
	}
	// O Claude leva alguns segundos para subir; sem um jeito de ler a tela do terminal, espera o processo e uma
	// folga fixa. 8 s cobrem a subida medida (~3 s) com margem.
	if _, err := sh(ctx, c, name, `for i in $(seq 1 40); do pgrep -f "^claude --dangerously" >/dev/null && exit 0; sleep 0.5; done; exit 1`, 30); err != nil {
		return errors.New("o claude não subiu na janela")
	}
	time.Sleep(8 * time.Second)
	return nil
}

// waitFile espera o Claude criar o arquivo de prova e confere o conteúdo; no estouro, diz o que havia.
func waitFile(ctx context.Context, c *client.Client, name, path, want string, limit time.Duration) error {
	deadline := time.Now().Add(limit)
	for time.Now().Before(deadline) {
		if err := checkFile(ctx, c, name, path, want); err == nil {
			return nil
		}
		time.Sleep(3 * time.Second)
	}
	return checkFile(ctx, c, name, path, want)
}

// checkFile lê o arquivo pela API e confere a linha pedida.
func checkFile(ctx context.Context, c *client.Client, name, path, want string) error {
	data, err := c.ReadFile(ctx, name, path)
	if err != nil {
		return fmt.Errorf("%s: %w", path, err)
	}
	if strings.TrimSpace(string(data)) != want {
		return fmt.Errorf("%s tem %q, esperado %q", path, strings.TrimSpace(string(data)), want)
	}
	log.Printf("      ✓ %s = %q", path, want)
	return nil
}

// sh roda um script de shell na máquina; exit diferente de 0 vira erro com o stderr.
func sh(ctx context.Context, c *client.Client, name, script string, timeout int, env ...string) (string, error) {
	r, err := c.Exec(ctx, name, []string{"sh", "-c", script}, client.ExecOptions{Env: env, Timeout: timeout})
	if err != nil {
		return "", err
	}
	if r.ExitCode != 0 {
		return "", fmt.Errorf("exit %d: %s %s", r.ExitCode, short(r.Stdout), short(r.Stderr))
	}
	return strings.TrimSpace(r.Stdout), nil
}

// short corta texto longo para caber numa linha de log.
func short(s string) string {
	s = strings.Join(strings.Fields(s), " ")
	if len(s) > 200 {
		return s[:200] + "…"
	}
	return s
}
