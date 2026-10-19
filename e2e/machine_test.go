//go:build e2e

package e2e

import (
	"bufio"
	"bytes"
	"context"
	"crypto/rand"
	"crypto/sha256"
	"encoding/hex"
	"encoding/json"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"syscall"
	"testing"
	"time"

	"github.com/creack/pty"
)

// rcMachine roda `rc machine ...` e devolve stdout, stderr e rc.
func rcMachine(t *testing.T, args ...string) (string, string, int) {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
	defer cancel()
	cmd := exec.CommandContext(ctx, bin(t), append([]string{"machine"}, args...)...)
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

// mustMachine roda e reprova se o rc não for 0.
func mustMachine(t *testing.T, args ...string) string {
	t.Helper()
	out, errOut, rc := rcMachine(t, args...)
	if rc != 0 {
		t.Fatalf("rc machine %v: rc=%d\n%s%s", args, rc, out, errOut)
	}
	return out
}

// uniqueName dá um nome por rodada: duas rodadas seguidas (-count=2) não colidem.
func uniqueName(prefix string) string {
	return fmt.Sprintf("%s-%d", prefix, time.Now().UnixNano()%1_000_000_000)
}

// TestMachineLifecycle trava o que torna a máquina persistente: o que se grava fora de
// /tmp (na raiz, por cima da imagem read-only, e em /workspace) sobrevive a outro exec e
// a stop + start; /tmp volta vazio; o rc do exec é o do comando; nada sobra após o delete.
func TestMachineLifecycle(t *testing.T) {
	name := uniqueName("e2e-life")
	defer rcMachine(t, "delete", "--name", name, "-f")
	mustMachine(t, "create", "--name", name, "--stack", "python", "--disk-gib", "2")
	if _, errOut, rc := rcMachine(t, "create", "--name", name, "--stack", "python"); rc == 0 || !strings.Contains(errOut, "CONFLICT") {
		t.Fatalf("duplicate create must be CONFLICT: rc=%d %s", rc, errOut)
	}
	if _, errOut, rc := rcMachine(t, "exec", "--name", name, "--", "true"); rc == 0 || !strings.Contains(errOut, "INVALID_STATE") {
		t.Fatalf("exec on a stopped machine must be INVALID_STATE: rc=%d %s", rc, errOut)
	}
	start := time.Now()
	mustMachine(t, "start", "--name", name)
	fmt.Printf("machine start: %v\n", time.Since(start).Round(time.Millisecond))

	mustMachine(t, "exec", "--name", name, "--", "sh", "-c",
		"echo etc > /etc/rc-e2e && mkdir -p /opt/e2e && echo opt > /opt/e2e/v && echo ws > /workspace/w && echo tmp > /tmp/t")
	if _, _, rc := rcMachine(t, "exec", "--name", name, "--", "sh", "-c", "exit 7"); rc != 7 {
		t.Fatalf("exit code must pass through: %d", rc)
	}
	if out := mustMachine(t, "exec", "--name", name, "--", "python3", "-c", "print(6*7)"); out != "42\n" {
		t.Fatalf("stack runtime inside the machine: %q", out)
	}
	if out := mustMachine(t, "status", "--name", name, "--json"); !strings.Contains(out, `"state": "running"`) {
		t.Fatalf("status: %s", out)
	}

	mustMachine(t, "stop", "--name", name)
	if out := mustMachine(t, "status", "--name", name, "--json"); !strings.Contains(out, `"state": "stopped"`) {
		t.Fatalf("status after stop: %s", out)
	}
	if out := mustMachine(t, "logs", "--name", name, "--tail", "5"); out == "" {
		t.Fatal("logs must be readable while stopped")
	}
	mustMachine(t, "start", "--name", name)
	out := mustMachine(t, "exec", "--name", name, "--", "sh", "-c", "cat /etc/rc-e2e /opt/e2e/v /workspace/w; ls /tmp/t 2>/dev/null || echo tmp-empty")
	if out != "etc\nopt\nws\ntmp-empty\n" {
		t.Fatalf("persistence after stop+start: %q", out)
	}

	mustMachine(t, "delete", "--name", name, "-f")
	if _, errOut, rc := rcMachine(t, "status", "--name", name); rc == 0 || !strings.Contains(errOut, "NOT_FOUND") {
		t.Fatalf("deleted machine: rc=%d %s", rc, errOut)
	}
	for end := time.Now().Add(5 * time.Second); time.Now().Before(end) && vmmAlive(); time.Sleep(100 * time.Millisecond) {
	}
	if vmmAlive() {
		t.Fatal("machine VM survived the delete")
	}
}

// TestMachineCopy: 5 MiB (vários pedaços) sobem e descem idênticos, conferidos pelo
// sha256 dentro e fora da máquina.
func TestMachineCopy(t *testing.T) {
	name := uniqueName("e2e-cp")
	defer rcMachine(t, "delete", "--name", name, "-f")
	mustMachine(t, "create", "--name", name, "--stack", "python", "--disk-gib", "2")
	mustMachine(t, "start", "--name", name)
	dir := t.TempDir()
	data := make([]byte, 5<<20+12345)
	rand.Read(data)
	src, dst := filepath.Join(dir, "up.bin"), filepath.Join(dir, "down.bin")
	os.WriteFile(src, data, 0o600)
	sum := sha256.Sum256(data)
	want := hex.EncodeToString(sum[:])

	mustMachine(t, "cp", src, name+":/workspace/up.bin")
	if out := mustMachine(t, "exec", "--name", name, "--", "sha256sum", "/workspace/up.bin"); !strings.HasPrefix(out, want) {
		t.Fatalf("sha inside the machine %q, want %s", out, want)
	}
	mustMachine(t, "cp", name+":/workspace/up.bin", dst)
	got, _ := os.ReadFile(dst)
	if !bytes.Equal(got, data) {
		t.Fatal("downloaded file differs")
	}
	if _, _, rc := rcMachine(t, "cp", src, dst); rc == 0 {
		t.Fatal("cp with no machine side must fail")
	}
}

// TestMachineAPI percorre as rotas HTTP com a VM real: criar, ligar, exec, exec em SSE,
// subir e baixar arquivo, parar e apagar, com os códigos de erro estáveis.
func TestMachineAPI(t *testing.T) {
	addr := "127.0.0.1:18183"
	srv := exec.Command(bin(t), "serve", "--listen", addr, "--pool-size", "0")
	var logs bytes.Buffer
	srv.Stderr = &logs
	if err := srv.Start(); err != nil {
		t.Fatal(err)
	}
	defer srv.Process.Kill()
	url := "http://" + addr + "/api/v1/machines"
	for end := time.Now().Add(15 * time.Second); time.Now().Before(end); time.Sleep(100 * time.Millisecond) {
		if r, err := http.Get(url); err == nil {
			r.Body.Close()
			break
		}
	}
	name := uniqueName("e2e-api")
	defer rcMachine(t, "delete", "--name", name, "-f")
	call := func(method, path, body string) (int, string) {
		t.Helper()
		req, _ := http.NewRequest(method, url+path, strings.NewReader(body))
		req.Header.Set("Content-Type", "application/json")
		r, err := http.DefaultClient.Do(req)
		if err != nil {
			t.Fatalf("%s %s: %v\n%s", method, path, err, logs.String())
		}
		defer r.Body.Close()
		b, _ := io.ReadAll(r.Body)
		return r.StatusCode, string(b)
	}
	if code, body := call("POST", "", `{"name":"`+name+`","stack":"python","disk_gib":2}`); code != 201 {
		t.Fatalf("create: %d %s", code, body)
	}
	if code, body := call("POST", "", `{"name":"`+name+`","stack":"python"}`); code != 409 || !strings.Contains(body, `"CONFLICT"`) {
		t.Fatalf("duplicate: %d %s", code, body)
	}
	if code, body := call("POST", "/"+name+"/start", ""); code != 200 || !strings.Contains(body, `"state":"running"`) {
		t.Fatalf("start: %d %s", code, body)
	}
	code, body := call("POST", "/"+name+"/exec", `{"command":["sh","-c","echo hi; exit 4"]}`)
	var ex struct {
		ExitCode int
		Stdout   string
	}
	json.Unmarshal([]byte(body), &ex)
	if code != 200 || ex.ExitCode != 4 || ex.Stdout != "hi\n" {
		t.Fatalf("exec: %d %s", code, body)
	}
	req, _ := http.NewRequest("POST", url+"/"+name+"/exec/stream", strings.NewReader(`{"command":["python3","-c","import time\nprint('early', flush=True)\ntime.sleep(1.5)\nprint('late')"]}`))
	req.Header.Set("Content-Type", "application/json")
	r, err := http.DefaultClient.Do(req)
	if err != nil {
		t.Fatal(err)
	}
	var early, late time.Time
	var exitData string
	sc := bufio.NewScanner(r.Body)
	event := ""
	for sc.Scan() {
		line := sc.Text()
		switch {
		case strings.HasPrefix(line, "event: "):
			event = strings.TrimPrefix(line, "event: ")
		case strings.HasPrefix(line, "data: "):
			data := strings.TrimPrefix(line, "data: ")
			if event == "stdout" && strings.Contains(data, "early") {
				early = time.Now()
			}
			if event == "stdout" && strings.Contains(data, "late") {
				late = time.Now()
			}
			if event == "exit" {
				exitData = data
			}
		}
	}
	r.Body.Close()
	if early.IsZero() || late.Sub(early) < time.Second || !strings.Contains(exitData, `"exitCode":0`) {
		t.Fatalf("exec/stream: early=%v gap=%v exit=%q", early, late.Sub(early), exitData)
	}
	payload := strings.Repeat("runner.codes\n", 200000) // 2,6 MB: três pedaços
	if code, body := call("PUT", "/"+name+"/files?path=/workspace/p.txt", payload); code != 200 {
		t.Fatalf("put: %d %s", code, body)
	}
	if code, body := call("GET", "/"+name+"/files?path=/workspace/p.txt", ""); code != 200 || body != payload {
		t.Fatalf("get: %d len=%d", code, len(body))
	}
	if code, body := call("GET", "/"+name+"/files?path=/nope", ""); code == 200 || !strings.Contains(body, `"error"`) {
		t.Fatalf("missing file: %d %s", code, body)
	}
	// O stop limpo leva milissegundos. Medido 10 s quando a VM desligada virava zumbi no
	// `rc serve` (o pid ainda respondia, e o stop esperava o prazo inteiro).
	stopStart := time.Now()
	if code, body := call("POST", "/"+name+"/stop", ""); code != 204 || time.Since(stopStart) > 5*time.Second {
		t.Fatalf("stop: %d %s after %v", code, body, time.Since(stopStart))
	}
	if code, body := call("POST", "/"+name+"/exec", `{"command":["true"]}`); code != 409 || !strings.Contains(body, `"INVALID_STATE"`) {
		t.Fatalf("exec stopped: %d %s", code, body)
	}
	if code, _ := call("DELETE", "/"+name, ""); code != 204 {
		t.Fatalf("delete: %d", code)
	}
	if code, body := call("GET", "/"+name, ""); code != 404 || !strings.Contains(body, `"NOT_FOUND"`) {
		t.Fatalf("deleted: %d %s", code, body)
	}
	srv.Process.Signal(syscall.SIGTERM)
	srv.Wait()
}

// TestMachineShellTTY trava o `rc machine shell` com um pseudo-terminal de verdade, numa
// máquina já ligada: o shell enxerga um terminal com o tamanho do host, Ctrl+C interrompe só
// o comando (o shell e a máquina continuam), o que se grava no shell persiste para o exec, e
// o rc do `rc` é o do `exit`. Antes do devpts no machine.sh o shell saía com 127 na hora
// (`open /dev/ptmx: no such device`, medido).
func TestMachineShellTTY(t *testing.T) {
	name := uniqueName("e2e-tty")
	defer rcMachine(t, "delete", "--name", name, "-f")
	mustMachine(t, "create", "--name", name, "--stack", "python", "--disk-gib", "2")
	mustMachine(t, "start", "--name", name)

	cmd := exec.Command(bin(t), "machine", "shell", "--name", name)
	ptmx, err := pty.StartWithSize(cmd, &pty.Winsize{Rows: 40, Cols: 120})
	if err != nil {
		t.Fatal(err)
	}
	defer ptmx.Close()
	out := &ptyOutput{}
	go io.Copy(out, ptmx)
	send := func(s string) { ptmx.Write([]byte(s)) }

	pos := out.waitFor(t, 0, "# ", 30*time.Second)
	// IS-T""TY e SL""EPT: o eco da linha digitada mostra as aspas; só a execução imprime junto.
	send("test -t 0 && echo IS-T\"\"TY; stty size\r")
	pos = out.waitFor(t, pos, "IS-TTY", 10*time.Second)
	pos = out.waitFor(t, pos, "40 120", 10*time.Second)
	send("sleep 30; echo SL\"\"EPT\r")
	time.Sleep(time.Second)
	start := time.Now()
	send("\x03")
	send("echo shell-wrote > /workspace/from-shell; echo ALIVE-$((6*7))\r")
	out.waitFor(t, pos, "ALIVE-42", 10*time.Second)
	if time.Since(start) > 5*time.Second {
		t.Fatalf("Ctrl+C did not interrupt the sleep (took %v)", time.Since(start))
	}
	out.mu.Lock()
	slept := strings.Contains(out.buf.String(), "SLEPT\r\n")
	out.mu.Unlock()
	if slept {
		t.Fatal("the sleep finished: Ctrl+C did not reach the guest")
	}
	send("exit 4\r")
	done := make(chan error, 1)
	go func() { done <- cmd.Wait() }()
	select {
	case err := <-done:
		if ee, ok := err.(*exec.ExitError); !ok || ee.ExitCode() != 4 {
			t.Fatalf("exit code must pass through: %v", err)
		}
	case <-time.After(15 * time.Second):
		cmd.Process.Kill()
		t.Fatal("rc machine shell did not exit after `exit 4`")
	}
	if got := mustMachine(t, "exec", "--name", name, "--", "cat", "/workspace/from-shell"); got != "shell-wrote\n" {
		t.Fatalf("what the shell wrote is not in the machine: %q", got)
	}
	if st := mustMachine(t, "status", "--name", name, "--json"); !strings.Contains(st, `"state": "running"`) {
		t.Fatal("the machine must keep running after the shell exits")
	}
}
