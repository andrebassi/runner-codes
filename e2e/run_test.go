//go:build e2e

package e2e

import (
	"context"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
	"time"

	"github.com/creack/pty"
)

// rcRun roda `rc machine run ...` com stdin opcional e devolve saída e rc.
func rcRun(t *testing.T, stdin string, args ...string) (string, int) {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), 3*time.Minute)
	defer cancel()
	cmd := exec.CommandContext(ctx, bin(t), append([]string{"machine", "run"}, args...)...)
	if stdin != "" {
		cmd.Stdin = strings.NewReader(stdin)
	}
	out, err := cmd.CombinedOutput()
	rc := 0
	if ee, ok := err.(*exec.ExitError); ok {
		rc = ee.ExitCode()
	} else if err != nil {
		t.Fatal(err)
	}
	return string(out), rc
}

// noRunMachines confere que nenhuma máquina efêmera ficou registrada.
func noRunMachines(t *testing.T) {
	t.Helper()
	if out := mustMachine(t, "ls"); strings.Contains(out, "run-") {
		t.Fatalf("ephemeral machine left behind:\n%s", out)
	}
}

// TestMachineRun: o comando roda numa máquina que some no fim; o rc é o do comando; -i
// repassa o stdin; sem -i o stdin não é lido (não trava); runner.code com cmd vira o
// comando de primeiro plano; sem nada para rodar é erro antes de criar.
func TestMachineRun(t *testing.T) {
	out, rc := rcRun(t, "", "--stack", "python", "--", "python3", "-c", "print('ola'); raise SystemExit(3)")
	if rc != 3 || !strings.Contains(out, "ola") {
		t.Fatalf("rc and output: %d %q", rc, out)
	}
	if out, rc := rcRun(t, "dados\n", "-i", "--stack", "python", "--", "sh", "-c", "read x; echo leu:$x"); rc != 0 || !strings.Contains(out, "leu:dados") {
		t.Fatalf("-i: %d %q", rc, out)
	}
	dir := t.TempDir()
	os.WriteFile(filepath.Join(dir, "runner.code"), []byte("stack = \"python\"\nenv = [\"W=from-file\"]\ncmd = [\"sh\", \"-c\", \"echo $W\"]\n"), 0o600)
	if out, rc := rcRun(t, "", "-s", filepath.Join(dir, "runner.code")); rc != 0 || !strings.Contains(out, "from-file") {
		t.Fatalf("runner.code cmd as foreground: %d %q", rc, out)
	}
	if out, rc := rcRun(t, "", "--stack", "python"); rc == 0 || !strings.Contains(out, "nothing to run") {
		t.Fatalf("nothing to run: %d %q", rc, out)
	}
	noRunMachines(t)
}

// TestMachineRunTTY: -it sob um pseudo-terminal real abre o shell com TTY, e o exit dele
// vira o rc; a máquina some depois.
func TestMachineRunTTY(t *testing.T) {
	cmd := exec.Command(bin(t), "machine", "run", "-it", "--stack", "python", "--", "/bin/sh")
	f, err := pty.StartWithSize(cmd, &pty.Winsize{Rows: 30, Cols: 100})
	if err != nil {
		t.Fatal(err)
	}
	defer f.Close()
	var buf strings.Builder
	done := make(chan struct{})
	go func() {
		b := make([]byte, 4096)
		for {
			n, err := f.Read(b)
			buf.Write(b[:n])
			if err != nil {
				close(done)
				return
			}
		}
	}()
	time.Sleep(time.Second)
	f.Write([]byte("[ -t 0 ] && echo IS-TTY; stty size; exit 9\n"))
	err = cmd.Wait()
	<-done
	ee, _ := err.(*exec.ExitError)
	if ee == nil || ee.ExitCode() != 9 || !strings.Contains(buf.String(), "IS-TTY") || !strings.Contains(buf.String(), "30 100") {
		t.Fatalf("tty run: %v %q", err, buf.String())
	}
	noRunMachines(t)
}

// TestMachineRunDetached: -d deixa a máquina ligada com o workload sob o nome pedido.
func TestMachineRunDetached(t *testing.T) {
	name := uniqueName("runbg")
	defer rcMachine(t, "delete", "--name", name, "--force")
	if out, rc := rcRun(t, "", "-d", "--name", name, "--stack", "python", "--", "sh", "-c", "echo up; sleep 300"); rc != 0 || !strings.Contains(out, "started "+name) {
		t.Fatalf("detach: %d %q", rc, out)
	}
	state, _ := statusOf(t, name)
	if state != "running" {
		t.Fatalf("state %s", state)
	}
	waitFor(t, "the workload output", 10*time.Second, func() bool {
		out, _, _ := rcMachine(t, "logs", "--name", name, "--source", "workload")
		return strings.Contains(out, "up")
	})
	mustMachine(t, "stop", "--name", name, "--timeout", "0s")
}

// stateOf devolve o estado da máquina pelo `machine ls`, ou "absent": no começo de um run a
// máquina ainda não existe, e isso não é falha (o `status` sairia com erro).
func stateOf(t *testing.T, name string) string {
	out, _, _ := rcMachine(t, "ls")
	for _, line := range strings.Split(out, "\n") {
		if f := strings.Fields(line); len(f) > 2 && f[0] == name {
			return f[2]
		}
	}
	return "absent"
}

// TestMachineRunCallerKilled trava o teardown de um run morto: um `machine run` cujo CLI
// leva SIGKILL não deixa a VM rodando sem dono (medido antes da correção: ficava viva, com o
// workload rodando sem prazo). O registro fica parado e sai com delete --force.
func TestMachineRunCallerKilled(t *testing.T) {
	name := uniqueName("killed")
	defer rcMachine(t, "delete", "--name", name, "--force")
	cmd := exec.Command(bin(t), "machine", "run", "--name", name, "--stack", "python", "--", "sleep", "300")
	if err := cmd.Start(); err != nil {
		t.Fatal(err)
	}
	waitFor(t, "the run to start", 30*time.Second, func() bool { return stateOf(t, name) == "running" })
	cmd.Process.Kill()
	cmd.Wait()
	waitFor(t, "the VM to die with its caller", 10*time.Second, func() bool { return stateOf(t, name) != "running" })
}

// TestDevStdinLinks trava os links que o init de uma distro cria e o nosso start.sh passou a
// criar: sem /dev/stdin, `grep -f /dev/stdin` falhava, e uma busca de valor vazado dava 0 por
// estar cega. Vale na VM de job e na máquina (chroot com /dev ligado).
func TestDevStdinLinks(t *testing.T) {
	job, _, code := run(t, "--code", `import subprocess; print(subprocess.run(["sh", "-c", "echo pelo-stdin | cat /dev/stdin; test -e /dev/fd/0 && echo fd-ok"], capture_output=True, text=True).stdout, end="")`)
	if code != 0 || !strings.Contains(job, "pelo-stdin\nfd-ok") {
		t.Fatalf("job VM: rc=%d %q", code, job)
	}
	out, code := rcRun(t, "", "--stack", "python", "--", "sh", "-c", "echo pelo-stdin | cat /dev/stdin; test -e /dev/fd/0 && echo fd-ok")
	if code != 0 || out != "pelo-stdin\nfd-ok\n" {
		t.Fatalf("ephemeral machine: rc=%d %q", code, out)
	}
	name := uniqueName("devfd")
	defer rcMachine(t, "delete", "--name", name, "--force")
	mustMachine(t, "create", "--name", name, "--stack", "python")
	mustMachine(t, "start", "--name", name)
	if out := mustMachine(t, "exec", "--name", name, "--", "sh", "-c", "echo na-maquina | grep -f /dev/stdin -c /dev/stdin; ls /dev/stdout /dev/stderr"); !strings.Contains(out, "/dev/stdout") {
		t.Fatalf("machine: %q", out)
	}
}
