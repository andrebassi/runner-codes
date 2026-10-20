//go:build e2e

package e2e

import (
	"strings"
	"testing"
)

// TestMachineBranchAndPause trava o branch e a pausa numa VM real:
//   - pausada, a máquina recusa exec com o motivo e o status diz paused;
//   - o branch de uma mãe pausada leva o que ela acabou de gravar (o sync acontece antes do
//     clone — sem ele o arquivo recém-gravado ficava de fora, medido);
//   - depois do branch, mãe e filha seguem independentes (o que uma grava a outra não vê);
//   - a mãe continua pausada depois do branch e volta com resume.
func TestMachineBranchAndPause(t *testing.T) {
	mom, kid := uniqueName("mom"), uniqueName("kid")
	defer rcMachine(t, "delete", "--name", kid, "--force")
	defer rcMachine(t, "delete", "--name", mom, "--force")
	mustMachine(t, "create", "--name", mom, "--stack", "python")
	mustMachine(t, "start", "--name", mom)
	mustMachine(t, "exec", "--name", mom, "--", "sh", "-c", "echo antes > /root/marca")
	mustMachine(t, "pause", "--name", mom)
	if state, _ := statusOf(t, mom); state != "paused" {
		t.Fatalf("state %s", state)
	}
	if _, errOut, rc := rcMachine(t, "exec", "--name", mom, "--", "true"); rc == 0 || !strings.Contains(errOut, "paused") {
		t.Fatalf("exec on a paused machine: rc=%d %s", rc, errOut)
	}
	mustMachine(t, "branch", "--from", mom, "--name", kid)
	if state, _ := statusOf(t, mom); state != "paused" {
		t.Fatalf("the parent must stay paused: %s", state)
	}
	mustMachine(t, "resume", "--name", mom)
	mustMachine(t, "exec", "--name", mom, "--", "sh", "-c", "echo depois >> /root/marca")
	mustMachine(t, "start", "--name", kid)
	if out := mustMachine(t, "exec", "--name", kid, "--", "cat", "/root/marca"); out != "antes\n" {
		t.Fatalf("the child must have what the parent wrote before the branch, and nothing after: %q", out)
	}
	mustMachine(t, "exec", "--name", kid, "--", "sh", "-c", "echo da-filha >> /root/marca")
	if out := mustMachine(t, "exec", "--name", mom, "--", "cat", "/root/marca"); out != "antes\ndepois\n" {
		t.Fatalf("the parent must not see the child's writes: %q", out)
	}
	mustMachine(t, "stop", "--name", kid)
	mustMachine(t, "stop", "--name", mom)
}
