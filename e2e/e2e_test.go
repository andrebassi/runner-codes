//go:build e2e

// Package e2e roda o binário real contra a libkrun real. Precisa de `task build` e da
// imagem Python construída (`task image:build -- --lang python`). Cada teste confere um
// comportamento que já foi medido funcionando, para a próxima mudança não desfazê-lo.
package e2e

import (
	"bytes"
	"context"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"strings"
	"sync"
	"syscall"
	"testing"
	"time"

	"github.com/creack/pty"
)

// bin é o binário do host, assinado (task build).
func bin(t *testing.T) string {
	t.Helper()
	b := os.Getenv("RUNNER_BIN")
	if b == "" {
		t.Fatal("RUNNER_BIN not set; run via `task test:e2e`")
	}
	return b
}

// run executa `runner.code run` e devolve stdout, stderr e rc.
func run(t *testing.T, args ...string) (string, string, int) {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), 60*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, bin(t), append([]string{"run", "--stack", "python"}, args...)...)
	var out, errb bytes.Buffer
	cmd.Stdout, cmd.Stderr = &out, &errb
	err := cmd.Run()
	rc := 0
	if ee, ok := err.(*exec.ExitError); ok {
		rc = ee.ExitCode()
	} else if err != nil {
		t.Fatal(err)
	}
	return out.String(), errb.String(), rc
}

// vmmAlive diz se sobrou algum processo de VM.
func vmmAlive() bool {
	return exec.Command("pgrep", "-f", "/rc vmm").Run() == nil
}

// TestStdoutAndExitCode: saída correta, e exit≠0 do programa vira rc≠0 da CLI.
func TestStdoutAndExitCode(t *testing.T) {
	out, _, rc := run(t, "--code", "print(2**10)")
	if rc != 0 || out != "1024\n" {
		t.Fatalf("rc=%d out=%q", rc, out)
	}
	_, errOut, rc := run(t, "--code", "import sys; sys.exit(3)")
	if rc == 0 || !strings.Contains(errOut, "exit=3") {
		t.Fatalf("exit 3 should fail the CLI: rc=%d %s", rc, errOut)
	}
}

// TestTimeoutKillsVM: laço infinito estoura o timeout e nenhuma VM sobrevive.
func TestTimeoutKillsVM(t *testing.T) {
	start := time.Now()
	_, errOut, rc := run(t, "--timeout", "2s", "--code", "while True: pass")
	if rc == 0 || !strings.Contains(errOut, "timeout=true") {
		t.Fatalf("expected timeout: rc=%d %s", rc, errOut)
	}
	if time.Since(start) > 10*time.Second {
		t.Fatalf("2s timeout took %v", time.Since(start))
	}
	if vmmAlive() {
		t.Fatal("VM survived the timeout")
	}
}

// TestNoNetwork: o guest não alcança a rede (vsock sem TSI). Medido no spike nos dois
// sentidos; aqui trava o sentido que protege: sem rede.
func TestNoNetwork(t *testing.T) {
	code := `import socket
s = socket.socket(); s.settimeout(3)
try:
    s.connect(("1.1.1.1", 80)); print("CONNECTED")
except OSError as e:
    print("BLOCKED", e.errno)`
	out, _, _ := run(t, "--code", code)
	if !strings.HasPrefix(out, "BLOCKED") {
		t.Fatalf("guest reached the network: %q", out)
	}
}

// TestRootReadOnlyAndIsolation: o root não aceita escrita, e o arquivo que um job grava
// em /tmp não existe no job seguinte (VM nova por job).
func TestRootReadOnlyAndIsolation(t *testing.T) {
	code := `import os
try:
    open("/etc/intrusion", "w"); print("ROOT_WRITABLE")
except OSError:
    print("ROOT_RO")
print("MARKER_EXISTS" if os.path.exists("/tmp/marker") else "NO_MARKER")
open("/tmp/marker", "w").write("x")`
	first, _, _ := run(t, "--code", code)
	second, _, _ := run(t, "--code", code)
	if first != "ROOT_RO\nNO_MARKER\n" || second != "ROOT_RO\nNO_MARKER\n" {
		t.Fatalf("isolation broken:\n1st %q\n2nd %q", first, second)
	}
}

// TestMemoryCeiling: alocar além do teto da linguagem (512 MiB) falha dentro da VM,
// e a CLI volta com erro em vez de travar o host.
func TestMemoryCeiling(t *testing.T) {
	out, _, rc := run(t, "--timeout", "30s", "--code", "b = bytearray(2 * 1024**3); print('ALLOCATED')")
	if rc == 0 || strings.Contains(out, "ALLOCATED") {
		t.Fatalf("allocated 2 GiB in a 512 MiB VM: rc=%d out=%q", rc, out)
	}
	if vmmAlive() {
		t.Fatal("VM survived")
	}
}

// runLang executa `runner.code run` numa linguagem qualquer.
func runLang(t *testing.T, lang, code string) (string, string, int) {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), 90*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, bin(t), "run", "--stack", lang, "--code", code)
	var out, errb bytes.Buffer
	cmd.Stdout, cmd.Stderr = &out, &errb
	err := cmd.Run()
	rc := 0
	if ee, ok := err.(*exec.ExitError); ok {
		rc = ee.ExitCode()
	}
	return out.String(), errb.String(), rc
}

// TestLanguageFamilies trava as cinco famílias medidas: interpretada (node), compilada
// com cache (go), JVM (java), compilada com apk (c). Os três defeitos achados aqui foram
// PATH da imagem (go/javac "not found"), cache do Go fora do caminho do executor (3,4 s)
// e compilação falha saindo rc 0. Pula a linguagem cuja imagem não foi construída.
func TestLanguageFamilies(t *testing.T) {
	cases := []struct{ lang, code, want string }{
		{"nodejs", `console.log("node", 6*7)`, "node 42\n"},
		{"go", "package main\nimport \"fmt\"\nfunc main(){ fmt.Println(\"go\", 6*7) }", "go 42\n"},
		{"java", `public class Main { public static void main(String[] a){ System.out.println("java " + 6*7); } }`, "java 42\n"},
		{"c", "#include <stdio.h>\nint main(){ printf(\"c %d\\n\", 6*7); return 0; }", "c 42\n"},
		{"cpp", "#include <iostream>\nint main(){ std::cout << \"cpp \" << 6*7 << std::endl; }", "cpp 42\n"},
		{"rust", "fn main(){ println!(\"rust {}\", 6*7); }", "rust 42\n"},
		{"ruby", `puts "ruby #{6*7}"`, "ruby 42\n"},
		{"php", `<?php echo "php " . 6*7 . "\n";`, "php 42\n"},
		{"perl", `print "perl ", 6*7, "\n";`, "perl 42\n"},
		// [[ ]] só existe no bash: prova que é bash de verdade e não o sh do busybox.
		{"bash", `[[ 1 == 1 ]] && echo "bash $((6*7))"`, "bash 42\n"},
		{"awk", `BEGIN { print "awk", 6*7 }`, "awk 42\n"},
		{"jq", `6*7`, "42\n"},
		// No SQLite `||` tem precedência maior que `*`: sem parênteses o resultado é 0.
		{"sqlite", `select 'sqlite ' || (6*7);`, "sqlite 42\n"},
	}
	for _, c := range cases {
		t.Run(c.lang, func(t *testing.T) {
			out, errOut, rc := runLang(t, c.lang, c.code)
			if strings.Contains(errOut, "not built") {
				t.Skipf("image for %s not built", c.lang)
			}
			if rc != 0 || out != c.want {
				t.Fatalf("rc=%d out=%q stderr=%s", rc, out, errOut)
			}
			if c.lang == "go" && !strings.Contains(errOut, "exec=") {
				t.Fatal("no execution time in stderr")
			}
		})
	}
	// Compilação falha não pode sair rc 0 (exception "Compilation failed" com exit 0).
	if _, errOut, rc := runLang(t, "java", "this is not java"); rc == 0 && !strings.Contains(errOut, "not built") {
		t.Fatal("failed Java compilation exited with rc 0")
	}
}

// TestShell trava o `rc shell`: comandos pelo stdin rodam numa VM nova com root
// read-only, /tmp gravável e sem rede; na imagem bash o shell é bash de verdade
// (a imagem instala em /usr/local/bin, e procurar só /bin/bash caía no sh — medido).
func TestShell(t *testing.T) {
	shell := func(stack, input string) string {
		ctx, cancel := context.WithTimeout(context.Background(), 60*time.Second)
		defer cancel()
		cmd := exec.CommandContext(ctx, bin(t), "shell", "--stack", stack)
		cmd.Stdin = strings.NewReader(input)
		out, _ := cmd.CombinedOutput()
		return string(out)
	}
	out := shell("python", "touch /tmp/ok && echo TMP_RW\ntouch /etc/x 2>/dev/null || echo ROOT_RO\n"+
		"wget -T 3 -q -O- http://1.1.1.1 >/dev/null 2>&1 || echo NO_NET\npython3 -c 'print(6*7)'\nexit\n")
	for _, want := range []string{"read-only root", "TMP_RW", "ROOT_RO", "NO_NET", "42"} {
		if !strings.Contains(out, want) {
			t.Fatalf("shell output missing %q:\n%s", want, out)
		}
	}
	if out := shell("bash", "[[ 1 == 1 ]] && echo DOUBLE_BRACKET\nexit\n"); !strings.Contains(out, "DOUBLE_BRACKET") {
		t.Fatalf("bash stack did not open bash:\n%s", out)
	}
	if vmmAlive() {
		t.Fatal("VM survived the shell exit")
	}
}

// ptyOutput acumula a saída do pseudo-terminal para o teste esperar por um texto.
type ptyOutput struct {
	mu  sync.Mutex
	buf strings.Builder
}

// Write guarda o que o terminal imprimiu.
func (p *ptyOutput) Write(b []byte) (int, error) {
	p.mu.Lock()
	defer p.mu.Unlock()
	return p.buf.Write(b)
}

// waitFor espera o texto aparecer na saída a partir de from; devolve o índice do fim.
func (p *ptyOutput) waitFor(t *testing.T, from int, want string, timeout time.Duration) int {
	t.Helper()
	for end := time.Now().Add(timeout); time.Now().Before(end); time.Sleep(20 * time.Millisecond) {
		p.mu.Lock()
		s := p.buf.String()
		p.mu.Unlock()
		if i := strings.Index(s[from:], want); i >= 0 {
			return from + i + len(want)
		}
	}
	p.mu.Lock()
	defer p.mu.Unlock()
	t.Fatalf("terminal output never showed %q; got:\n%s", want, p.buf.String()[from:])
	return 0
}

// TestShellTTY trava o console TTY do `rc shell` com um pseudo-terminal de verdade: o
// guest enxerga um terminal (o stdin é /dev/console sobre o hvc0), o tamanho da janela chega, e Ctrl+C interrompe
// só o comando, sem derrubar a VM (antes, o Ctrl+C matava a VM inteira — medido).
func TestShellTTY(t *testing.T) {
	cmd := exec.Command(bin(t), "shell", "--stack", "bash")
	ptmx, err := pty.StartWithSize(cmd, &pty.Winsize{Rows: 40, Cols: 120})
	if err != nil {
		t.Fatal(err)
	}
	defer ptmx.Close()
	out := &ptyOutput{}
	go io.Copy(out, ptmx)
	send := func(s string) { ptmx.Write([]byte(s)) }

	pos := out.waitFor(t, 0, "bash-5.2#", 30*time.Second)
	// test -t 0: o stdin do shell é terminal. stty size: o tamanho do pty do host chegou.
	send("test -t 0 && echo IS-TTY; stty size\r")
	pos = out.waitFor(t, pos, "IS-TTY", 10*time.Second)
	pos = out.waitFor(t, pos, "40 120", 10*time.Second)

	// SL""EPT: o eco da linha digitada mostra SL""EPT; só a execução imprime SLEPT.
	send("sleep 30; echo SL\"\"EPT\r")
	time.Sleep(time.Second)
	start := time.Now()
	send("\x03") // Ctrl+C: tem de chegar ao guest como interrupção do sleep
	send("echo ALIVE-$((6*7))\r")
	pos = out.waitFor(t, pos, "ALIVE-42", 10*time.Second)
	if time.Since(start) > 5*time.Second {
		t.Fatalf("Ctrl+C did not interrupt the sleep (took %v)", time.Since(start))
	}
	out.mu.Lock()
	slept := strings.Contains(out.buf.String(), "SLEPT\r\n")
	out.mu.Unlock()
	if slept {
		t.Fatal("the sleep finished: Ctrl+C did not reach the guest")
	}

	send("exit\r")
	done := make(chan error, 1)
	go func() { done <- cmd.Wait() }()
	select {
	case <-done:
	case <-time.After(15 * time.Second):
		cmd.Process.Kill()
		t.Fatal("rc shell did not exit after `exit`")
	}
	if vmmAlive() {
		t.Fatal("VM survived the shell exit")
	}
}

// TestServePool: o serve enche o pool, atende /run e, no SIGTERM, não deixa VM viva.
func TestServePool(t *testing.T) {
	addr := "127.0.0.1:18181"
	cmd := exec.Command(bin(t), "serve", "--listen", addr, "--pool-size", "2")
	var logs bytes.Buffer
	cmd.Stderr = &logs
	if err := cmd.Start(); err != nil {
		t.Fatal(err)
	}
	defer cmd.Process.Kill()
	url := "http://" + addr
	ready := false
	for end := time.Now().Add(15 * time.Second); time.Now().Before(end); time.Sleep(100 * time.Millisecond) {
		if r, err := http.Get(url + "/health"); err == nil {
			r.Body.Close()
			ready = true
			break
		}
	}
	if !ready {
		t.Fatalf("serve did not start: %s", logs.String())
	}
	// Espera o pool do Python encher (o /health diz quantas estão prontas) em vez de um
	// tempo fixo: com 15 linguagens o serve repõe todas ao mesmo tempo.
	full := false
	for end := time.Now().Add(60 * time.Second); time.Now().Before(end); time.Sleep(100 * time.Millisecond) {
		r, err := http.Get(url + "/health")
		if err != nil {
			continue
		}
		var h struct{ Pool map[string]int }
		json.NewDecoder(r.Body).Decode(&h)
		r.Body.Close()
		// Espera TODOS os pools cheios: medir com outras stacks ainda bootando deixa a
		// disputa de CPU entrar no número (medido: 397 ms com reposição em curso, 57 ms sem).
		all := len(h.Pool) > 0
		for _, n := range h.Pool {
			if n != 2 {
				all = false
			}
		}
		if all && h.Pool["python"] == 2 {
			full = true
			break
		}
	}
	if !full {
		t.Fatalf("pools did not fill within 60 s: %s", logs.String())
	}
	body := `{"language":"python","stdin":"oi","files":[{"name":"main.py","content":"print(input().upper())"}]}`
	start := time.Now()
	r, err := http.Post(url+"/api/v1/run", "application/json", strings.NewReader(body))
	if err != nil {
		t.Fatal(err)
	}
	got, _ := io.ReadAll(r.Body)
	r.Body.Close()
	elapsed := time.Since(start)
	if r.StatusCode != 200 || !strings.Contains(string(got), `"stdout":"OI\n"`) || !strings.Contains(string(got), `"stderr":null`) {
		t.Fatalf("response: %d %s", r.StatusCode, got)
	}
	// Com o pool cheio o boot sai do caminho: medido 26–30 ms contra ~240 ms de boot frio.
	if elapsed > 200*time.Millisecond {
		t.Errorf("job with a full pool took %v; the pool is not serving", elapsed)
	}
	fmt.Printf("job with pool: %v\n", elapsed)
	cmd.Process.Signal(syscall.SIGTERM)
	cmd.Wait()
	for end := time.Now().Add(5 * time.Second); time.Now().Before(end) && vmmAlive(); time.Sleep(100 * time.Millisecond) {
	}
	if vmmAlive() {
		t.Fatal("pool VM survived the serve SIGTERM")
	}
}
