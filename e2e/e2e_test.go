//go:build e2e

// Package e2e roda o binário real contra a libkrun real. Precisa de `task build` e da
// imagem Python construída (`task image:build -- --lang python`). Cada teste confere um
// comportamento que já foi medido funcionando, para a próxima mudança não desfazê-lo.
package e2e

import (
	"bytes"
	"context"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"strings"
	"syscall"
	"testing"
	"time"
)

// bin é o binário do host, assinado (task build).
func bin(t *testing.T) string {
	t.Helper()
	b := os.Getenv("RUNNER_BIN")
	if b == "" {
		t.Fatal("RUNNER_BIN não definido; rode via `task test:e2e`")
	}
	return b
}

// run executa `runner.code run` e devolve stdout, stderr e rc.
func run(t *testing.T, args ...string) (string, string, int) {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), 60*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, bin(t), append([]string{"run", "--lang", "python"}, args...)...)
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
	return exec.Command("pgrep", "-f", "runner.code vmm").Run() == nil
}

// TestStdoutAndExitCode: saída correta, e exit≠0 do programa vira rc≠0 da CLI.
func TestStdoutAndExitCode(t *testing.T) {
	out, _, rc := run(t, "--code", "print(2**10)")
	if rc != 0 || out != "1024\n" {
		t.Fatalf("rc=%d out=%q", rc, out)
	}
	_, errOut, rc := run(t, "--code", "import sys; sys.exit(3)")
	if rc == 0 || !strings.Contains(errOut, "exit=3") {
		t.Fatalf("exit 3 deveria falhar a CLI: rc=%d %s", rc, errOut)
	}
}

// TestTimeoutKillsVM: laço infinito estoura o timeout e nenhuma VM sobrevive.
func TestTimeoutKillsVM(t *testing.T) {
	start := time.Now()
	_, errOut, rc := run(t, "--timeout", "2s", "--code", "while True: pass")
	if rc == 0 || !strings.Contains(errOut, "timeout=true") {
		t.Fatalf("esperava timeout: rc=%d %s", rc, errOut)
	}
	if time.Since(start) > 10*time.Second {
		t.Fatalf("timeout de 2s levou %v", time.Since(start))
	}
	if vmmAlive() {
		t.Fatal("VM sobreviveu ao timeout")
	}
}

// TestNoNetwork: o guest não alcança a rede (vsock sem TSI). Medido no spike nos dois
// sentidos; aqui trava o sentido que protege: sem rede.
func TestNoNetwork(t *testing.T) {
	code := `import socket
s = socket.socket(); s.settimeout(3)
try:
    s.connect(("1.1.1.1", 80)); print("CONECTOU")
except OSError as e:
    print("BLOQUEADO", e.errno)`
	out, _, _ := run(t, "--code", code)
	if !strings.HasPrefix(out, "BLOQUEADO") {
		t.Fatalf("guest alcançou a rede: %q", out)
	}
}

// TestRootReadOnlyAndIsolation: o root não aceita escrita, e o arquivo que um job grava
// em /tmp não existe no job seguinte (VM nova por job).
func TestRootReadOnlyAndIsolation(t *testing.T) {
	code := `import os
try:
    open("/etc/invasao", "w"); print("ROOT_GRAVAVEL")
except OSError:
    print("ROOT_RO")
print("MARCA_EXISTE" if os.path.exists("/tmp/marca") else "SEM_MARCA")
open("/tmp/marca", "w").write("x")`
	first, _, _ := run(t, "--code", code)
	second, _, _ := run(t, "--code", code)
	if first != "ROOT_RO\nSEM_MARCA\n" || second != "ROOT_RO\nSEM_MARCA\n" {
		t.Fatalf("isolamento quebrado:\n1º %q\n2º %q", first, second)
	}
}

// TestMemoryCeiling: alocar além do teto da linguagem (512 MiB) falha dentro da VM,
// e a CLI volta com erro em vez de travar o host.
func TestMemoryCeiling(t *testing.T) {
	out, _, rc := run(t, "--timeout", "30s", "--code", "b = bytearray(2 * 1024**3); print('ALOCOU')")
	if rc == 0 || strings.Contains(out, "ALOCOU") {
		t.Fatalf("alocou 2 GiB numa VM de 512 MiB: rc=%d out=%q", rc, out)
	}
	if vmmAlive() {
		t.Fatal("VM sobreviveu")
	}
}

// runLang executa `runner.code run` numa linguagem qualquer.
func runLang(t *testing.T, lang, code string) (string, string, int) {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), 90*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, bin(t), "run", "--lang", lang, "--code", code)
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
	}
	for _, c := range cases {
		t.Run(c.lang, func(t *testing.T) {
			out, errOut, rc := runLang(t, c.lang, c.code)
			if strings.Contains(errOut, "não construída") {
				t.Skipf("imagem de %s não construída", c.lang)
			}
			if rc != 0 || out != c.want {
				t.Fatalf("rc=%d out=%q stderr=%s", rc, out, errOut)
			}
			if c.lang == "go" && !strings.Contains(errOut, "exec=") {
				t.Fatal("sem tempo de execução no stderr")
			}
		})
	}
	// Compilação falha não pode sair rc 0 (exception "Compilation failed" com exit 0).
	if _, errOut, rc := runLang(t, "java", "isto não é java"); rc == 0 && !strings.Contains(errOut, "não construída") {
		t.Fatal("compilação Java falha saiu rc 0")
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
		t.Fatalf("serve não subiu: %s", logs.String())
	}
	time.Sleep(2 * time.Second) // pool enche (boot medido ~260 ms)
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
		t.Fatalf("resposta: %d %s", r.StatusCode, got)
	}
	// Com o pool cheio o boot sai do caminho: medido 26–30 ms contra ~240 ms de boot frio.
	if elapsed > 200*time.Millisecond {
		t.Errorf("job com pool cheio levou %v; o pool não está servindo", elapsed)
	}
	fmt.Printf("job com pool: %v\n", elapsed)
	cmd.Process.Signal(syscall.SIGTERM)
	cmd.Wait()
	for end := time.Now().Add(5 * time.Second); time.Now().Before(end) && vmmAlive(); time.Sleep(100 * time.Millisecond) {
	}
	if vmmAlive() {
		t.Fatal("VM do pool sobreviveu ao SIGTERM do serve")
	}
}
