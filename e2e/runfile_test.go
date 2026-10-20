//go:build e2e

package e2e

import (
	"encoding/json"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// statusOf lê o `rc machine status --json`.
func statusOf(t *testing.T, name string) (state string, cpus int) {
	t.Helper()
	// O status --json sai no mesmo formato do ls: uma lista, aqui com uma máquina.
	var st []struct {
		State string `json:"state"`
		CPUs  int    `json:"cpus"`
	}
	if err := json.Unmarshal([]byte(mustMachine(t, "status", "--name", name, "--json")), &st); err != nil || len(st) != 1 {
		t.Fatalf("status: %v %+v", err, st)
	}
	return st[0].State, st[0].CPUs
}

// waitFor repete check até ele dar certo ou o prazo vencer.
func waitFor(t *testing.T, what string, d time.Duration, check func() bool) {
	t.Helper()
	for end := time.Now().Add(d); time.Now().Before(end); time.Sleep(200 * time.Millisecond) {
		if check() {
			return
		}
	}
	t.Fatalf("timed out waiting for %s", what)
}

// TestRunnerCode trava o runner.code de ponta a ponta numa VM real:
//   - a flag vence o arquivo (--cpus);
//   - o workdir nasce sozinho; env e workdir chegam ao init, ao workload e ao exec;
//   - o init roda uma vez por disco, não a cada start;
//   - o workload sobe no start, aparece no `logs --source workload`, e recebe o
//     stop_signal (SIGINT) no stop com tempo de gravar no disco antes do sync.
func TestRunnerCode(t *testing.T) {
	name := uniqueName("rcfile")
	dir := t.TempDir()
	file := filepath.Join(dir, "runner.code")
	os.WriteFile(file, []byte(`
stack = "python"
cpus = 2
env = ["GREETING=hello"]
workdir = "/app"
init = ["echo $(( $(cat count 2>/dev/null || echo 0) + 1 )) > count"]
entrypoint = ["sh", "-c"]
cmd = ["trap 'echo saved > saved; exit 0' INT; echo \"up $GREETING in $(pwd)\"; while :; do sleep 0.2; done"]
stop_signal = "SIGINT"
stop_timeout = 5
`), 0o600)
	defer rcMachine(t, "delete", "--name", name, "--force")

	mustMachine(t, "create", "-f", file, "--name", name, "--cpus", "1")
	if _, cpus := statusOf(t, name); cpus != 1 {
		t.Fatalf("--cpus must win over the file: %d", cpus)
	}
	mustMachine(t, "start", "--name", name)
	waitFor(t, "workload output", 15*time.Second, func() bool {
		out, _, _ := rcMachine(t, "logs", "--name", name, "--source", "workload")
		return strings.Contains(out, "up hello in /app\n")
	})
	if out := mustMachine(t, "exec", "--name", name, "--", "sh", "-c", "cat count; echo $GREETING; pwd"); out != "1\nhello\n/app\n" {
		t.Fatalf("init ran once, exec inherits env and workdir: %q", out)
	}
	start := time.Now()
	mustMachine(t, "stop", "--name", name)
	if d := time.Since(start); d > 8*time.Second {
		t.Fatalf("a workload that honours SIGINT must stop fast: %v", d)
	}
	mustMachine(t, "start", "--name", name)
	if out := mustMachine(t, "exec", "--name", name, "--", "sh", "-c", "cat count saved"); out != "1\nsaved\n" {
		t.Fatalf("init must not rerun and the SIGINT handler must have written before the sync: %q", out)
	}
	mustMachine(t, "stop", "--name", name)
}

// TestRunnerCodeStopOnExit: com stop_on_exit, o fim do workload desliga a máquina sozinha,
// e a saída dele fica no log com o código.
func TestRunnerCodeStopOnExit(t *testing.T) {
	name := uniqueName("rconce")
	dir := t.TempDir()
	os.WriteFile(filepath.Join(dir, "runner.code"), []byte(`
stack = "python"
workdir = "/job"
cmd = ["python3", "-c", "open('result','w').write('42'); print('done')"]
stop_on_exit = true
`), 0o600)
	defer rcMachine(t, "delete", "--name", name, "--force")

	mustMachine(t, "create", "-f", filepath.Join(dir, "runner.code"), "--name", name)
	mustMachine(t, "start", "--name", name)
	waitFor(t, "the machine to stop by itself", 20*time.Second, func() bool {
		state, _ := statusOf(t, name)
		return state == "stopped"
	})
	if out := mustMachine(t, "logs", "--name", name, "--source", "workload"); !strings.Contains(out, "done\nexited with code 0\n") {
		t.Fatalf("workload log: %q", out)
	}
}

// TestRunnerCodeRefusals: chave do runner.code ainda não suportada ([secrets], Fase B) sai
// com o motivo e não cria nada; sem stack nem image é erro.
func TestRunnerCodeRefusals(t *testing.T) {
	dir := t.TempDir()
	bad := filepath.Join(dir, "runner.code")
	// [health] segue sem suporte ([secrets] passou a valer com a story 7.7).
	os.WriteFile(bad, []byte("stack = \"python\"\n[health]\nexec = [\"true\"]\n"), 0o600)
	name := uniqueName("rcbad")
	if _, errOut, rc := rcMachine(t, "create", "-f", bad, "--name", name); rc == 0 || !strings.Contains(errOut, `"health" is not supported yet`) {
		t.Fatalf("health must be refused: rc=%d %s", rc, errOut)
	}
	if _, _, rc := rcMachine(t, "status", "--name", name); rc == 0 {
		t.Fatal("a refused file must not create the machine")
	}
	if _, errOut, rc := rcMachine(t, "create", "--name", name); rc == 0 || !strings.Contains(errOut, "--stack or --image") {
		t.Fatalf("no base: rc=%d %s", rc, errOut)
	}
}

// TestRunnerCodeImage: `image` aceita uma imagem OCI qualquer (alpine aqui), construída
// na primeira vez; a máquina liga e roda comando nela.
func TestRunnerCodeImage(t *testing.T) {
	name := uniqueName("rcimg")
	defer rcMachine(t, "delete", "--name", name, "--force")
	mustMachine(t, "create", "--name", name, "--image", "alpine:3.20")
	mustMachine(t, "start", "--name", name)
	if out := mustMachine(t, "exec", "--name", name, "--", "cat", "/etc/alpine-release"); !strings.HasPrefix(out, "3.20") {
		t.Fatalf("alpine release: %q", out)
	}
	mustMachine(t, "stop", "--name", name)
}
