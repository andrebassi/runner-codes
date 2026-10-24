// Desktop Linux com Claude Code, criado só pelo SDK Go a partir do runner.code desta pasta: XFCE no Xorg,
// área de trabalho preta com os atalhos Arquivos, Claude, Navegador e Terminal, tela que acompanha a janela
// do navegador. O programa cria a máquina, liga (o init instala tudo na 1ª vez) e confere cada peça por dentro.
//
// Uso, com um `rc serve` local (a tela é servida no 127.0.0.1 do host que roda o rc):
//
//	go run ./examples/desktop-claude                  # cria, liga e confere; a máquina fica ligada
//	go run ./examples/desktop-claude -delete          # o mesmo, e apaga no fim
//	open http://127.0.0.1:6080
package main

import (
	"bytes"
	"context"
	_ "embed"
	"encoding/json"
	"errors"
	"flag"
	"fmt"
	"log"
	"net/http"
	"regexp"
	"strconv"
	"strings"
	"time"

	"runner.codes/pkg/client"
)

// runnerCode é o runner.code desta pasta, enviado inteiro no campo runner_code: a mesma máquina que
// `rc machine create` montaria lendo o arquivo.
//
//go:embed runner.code
var runnerCode string

// options são as escolhas da linha de comando.
type options struct {
	server string
	name   string
	delete bool
	port   int
}

// main lê as flags, monta o desktop e diz onde ver a tela; qualquer falha encerra com rc 1.
func main() {
	var o options
	flag.StringVar(&o.server, "server", "http://127.0.0.1:8080", "endereço do rc serve")
	flag.StringVar(&o.name, "name", "desktop-claude", "nome da máquina")
	flag.BoolVar(&o.delete, "delete", false, "apaga a máquina no fim (sem ela, a máquina fica ligada)")
	flag.IntVar(&o.port, "display-port", 0, "porta da tela no host (0 usa a do runner.code); outra porta deixa duas máquinas lado a lado")
	flag.Parse()
	log.SetFlags(0)

	// O primeiro start instala o desktop, o Firefox e o Claude Code (apt + npm). O prazo é do context, e o
	// http.Client sem Timeout próprio deixa o context decidir.
	ctx, cancel := context.WithTimeout(context.Background(), 20*time.Minute)
	defer cancel()
	c := client.New(o.server)
	c.HTTP = &http.Client{}

	if err := run(ctx, c, o); err != nil {
		log.Fatalf("erro: %v", err)
	}
}

// displayPortRe casa a linha `display_port = N` do runner.code.
var displayPortRe = regexp.MustCompile(`(?m)^display_port\s*=\s*(\d+)`)

// spec devolve o runner.code a mandar e a porta do visualizador da tela, onde o programa manda mouse e tamanho como a
// página do navegador faria. Com override > 0, a linha display_port do arquivo é trocada por ela.
func spec(override int) (string, int, error) {
	m := displayPortRe.FindStringSubmatch(runnerCode)
	if m == nil {
		return "", 0, errors.New("runner.code sem display_port")
	}
	if override > 0 {
		return displayPortRe.ReplaceAllString(runnerCode, "display_port = "+strconv.Itoa(override)), override, nil
	}
	port, err := strconv.Atoi(m[1])
	return runnerCode, port, err
}

// run é o roteiro: criar, ligar, esperar a sessão gráfica, mexer na tela como um usuário e conferir.
func run(ctx context.Context, c *client.Client, o options) error {
	code, port, err := spec(o.port)
	if err != nil {
		return err
	}
	// Máquina de uma execução anterior: começa do zero, para o init rodar de novo e provar a receita inteira.
	if _, err := c.Machine(ctx, o.name); err == nil {
		log.Printf("[1/6] apagando a máquina %s de uma execução anterior", o.name)
		if err := c.DeleteMachine(ctx, o.name, true); err != nil {
			return err
		}
	}
	log.Printf("[1/6] criando %s a partir do runner.code (node:22, 4 vCPUs, 4 GiB, GPU e tela)", o.name)
	if _, err := c.CreateMachine(ctx, client.MachineSpec{Name: o.name, RunnerCode: code}); err != nil {
		return err
	}
	if o.delete {
		defer func() {
			// Context novo: o do roteiro pode já ter vencido, e a máquina não pode sobrar.
			del, cancel := context.WithTimeout(context.Background(), time.Minute)
			defer cancel()
			if err := c.DeleteMachine(del, o.name, true); err == nil {
				log.Printf("[fim] máquina %s apagada", o.name)
			}
		}()
	}

	log.Printf("[2/6] ligando; na 1ª vez o init instala Xorg, XFCE, Firefox e Claude Code")
	start := time.Now()
	if _, err := c.Start(ctx, o.name); err != nil {
		return err
	}
	log.Printf("[2/6] ligada em %s", time.Since(start).Round(time.Second))

	log.Printf("[3/6] esperando a sessão gráfica (Xorg, XFCE e o terminal bash)")
	if err := waitFor(ctx, c, o.name, "pgrep -x xfce4-terminal", 90*time.Second); err != nil {
		return err
	}

	log.Printf("[4/6] usando a tela como o navegador: 200 movimentos de mouse e a janela em 1200x700")
	if err := moveMouse(ctx, port, 200); err != nil {
		return err
	}
	if err := post(ctx, port, "/resize", map[string]int{"width": 1200, "height": 700}); err != nil {
		return err
	}
	if err := waitFor(ctx, c, o.name, "DISPLAY=:0 timeout 5 xrandr | grep -q 'current 1200 x 700'", 30*time.Second); err != nil {
		return fmt.Errorf("a tela não acompanhou o tamanho pedido: %w", err)
	}

	log.Printf("[5/6] conferindo dentro da VM")
	checks := []struct{ what, cmd string }{
		{"claude", "claude --version"},
		{"navegador padrão", "readlink -f /etc/alternatives/x-www-browser"},
		{"atalhos", "ls /root/Desktop | tr '\\n' ' '"},
		// Com o cursor por hardware o Xorg travava no primeiro movimento de mouse; o timeout faz o
		// travamento virar falha aqui, em vez de um Exec parado até o prazo.
		{"tela responde", "DISPLAY=:0 timeout 10 xrandr | head -1"},
		{"teclado e mouse", "grep -c 'Adding input device runner.code' /var/log/Xorg.0.log"},
		{"maximizador", "pgrep -f '^/bin/sh /usr/local/bin/maximize-windows' | head -1"},
		{"terminal maximizado", "DISPLAY=:0 timeout 10 sh -c 'w=$(xwininfo -root -tree | awk \"/bash · claude/ {print \\$1; exit}\"); " +
			"xprop -id $w _NET_WM_STATE' | grep -o _NET_WM_STATE_MAXIMIZED_HORZ"},
	}
	for _, ck := range checks {
		out, err := sh(ctx, c, o.name, ck.cmd)
		if err != nil {
			return fmt.Errorf("%s: %w", ck.what, err)
		}
		log.Printf("      %-20s %s", ck.what, out)
	}

	log.Printf("[6/6] encerrando a sessão XFCE (como um logout): ela tem de voltar sozinha, no mesmo tamanho")
	if _, err := sh(ctx, c, o.name, "pkill -x xfce4-session; sleep 3; ! pgrep -x xfce4-terminal >/dev/null"); err != nil {
		return fmt.Errorf("a sessão não caiu: %w", err)
	}
	if err := waitFor(ctx, c, o.name, "pgrep -x xfce4-terminal && DISPLAY=:0 timeout 5 xrandr | grep -q 'current 1200 x 700'", 60*time.Second); err != nil {
		return fmt.Errorf("a sessão não voltou: %w", err)
	}
	log.Printf("      sessão de volta em 1200x700")

	log.Printf("pronto: abra http://127.0.0.1:%d (a tela se ajusta à janela; clique para mandar o teclado)", port)
	log.Printf("      atalho Claude: claude --dangerously-skip-permissions (pede login ou ANTHROPIC_API_KEY)")
	return nil
}

// waitFor repete um comando na máquina até ele sair 0, com teto; no estouro, traz o fim do log da sessão.
func waitFor(ctx context.Context, c *client.Client, name, cmd string, limit time.Duration) error {
	deadline := time.Now().Add(limit)
	for time.Now().Before(deadline) {
		if _, err := sh(ctx, c, name, cmd); err == nil {
			return nil
		}
		time.Sleep(2 * time.Second)
	}
	logs, _ := c.Logs(ctx, name, 15, true)
	return fmt.Errorf("%q não passou em %s; fim do log:\n%s", cmd, limit, logs)
}

// moveMouse manda n movimentos do mouse ao visualizador (POST /input), no formato da página: eventos evdev
// do tablet virtual, eixos absolutos de 0 a 32767, cada movimento fechado por um SYN_REPORT.
func moveMouse(ctx context.Context, port, n int) error {
	// Constantes do evdev (linux/input-event-codes.h): tipos e códigos que o tablet "runner.code tablet" declara.
	const evSyn, evAbs, absX, absY, synReport = 0, 3, 0, 1, 0
	type event struct {
		Type  uint16 `json:"type"`
		Code  uint16 `json:"code"`
		Value int32  `json:"value"`
	}
	for i := 0; i < n; i++ {
		p := int32(i % 100 * 327) // 0..32373 na diagonal, volta ao começo a cada 100 passos
		if err := post(ctx, port, "/input", []event{{evAbs, absX, p}, {evAbs, absY, p}, {evSyn, synReport, 0}}); err != nil {
			return err
		}
		time.Sleep(5 * time.Millisecond)
	}
	return nil
}

// post manda um JSON ao visualizador da tela e exige 204, a resposta das rotas /input e /resize.
func post(ctx context.Context, port int, path string, body any) error {
	raw, err := json.Marshal(body)
	if err != nil {
		return err
	}
	url := fmt.Sprintf("http://127.0.0.1:%d%s", port, path)
	req, err := http.NewRequestWithContext(ctx, "POST", url, bytes.NewReader(raw))
	if err != nil {
		return err
	}
	resp, err := http.DefaultClient.Do(req)
	if err != nil {
		return fmt.Errorf("visualizador da tela em %s: %w", url, err)
	}
	resp.Body.Close()
	if resp.StatusCode != http.StatusNoContent {
		return fmt.Errorf("visualizador da tela: POST %s devolveu %d", path, resp.StatusCode)
	}
	return nil
}

// sh roda um comando de shell na máquina e devolve a saída sem a quebra final; exit diferente de 0 vira erro.
func sh(ctx context.Context, c *client.Client, name, cmd string) (string, error) {
	r, err := c.Exec(ctx, name, []string{"sh", "-c", cmd}, client.ExecOptions{Timeout: 30})
	if err != nil {
		return "", err
	}
	if r.ExitCode != 0 {
		return "", fmt.Errorf("%q saiu com %d: %s", cmd, r.ExitCode, strings.TrimSpace(r.Stderr))
	}
	return strings.TrimSpace(r.Stdout), nil
}
