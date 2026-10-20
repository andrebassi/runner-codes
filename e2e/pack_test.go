//go:build e2e

package e2e

import (
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"testing"
)

// artifact roda um artefato de pack e devolve saída, erro e código.
func artifact(t *testing.T, path string, args ...string) (string, string, int) {
	t.Helper()
	cmd := exec.Command(path, args...)
	var out, errOut strings.Builder
	cmd.Stdout, cmd.Stderr = &out, &errOut
	err := cmd.Run()
	code := 0
	if ee, ok := err.(*exec.ExitError); ok {
		code = ee.ExitCode()
	} else if err != nil {
		t.Fatalf("%s: %v", path, err)
	}
	return out.String(), errOut.String(), code
}

// TestPack trava o artefato portátil em VMs reais, conferindo um VALOR e não só o boot (um
// pack que perdeu o disco ainda liga e sai 0):
//   - stack em stub + sidecar: o run executa e o código de saída do comando volta;
//   - máquina com marcador e pacote instalado, em arquivo único: com a origem apagada, o run
//     do artefato lê o marcador e importa o pacote;
//   - modo persistente (start/exec/stop): o que se grava sobrevive a stop e start;
//   - machine create --from o payload e pack inspect;
//   - recusas: máquina ligada e volume de diretório do host.
func TestPack(t *testing.T) {
	dir := t.TempDir()
	py := filepath.Join(dir, "py")
	rcPack := func(args ...string) (string, string, int) {
		return artifact(t, bin(t), append([]string{"pack"}, args...)...)
	}
	mustPack := func(args ...string) string {
		out, errOut, code := rcPack(args...)
		if code != 0 {
			t.Fatalf("rc pack %v: rc=%d %s", args, code, errOut)
		}
		return out
	}
	mustPack("create", "--stack", "python", "--output", py)
	if _, err := os.Stat(py + ".rcmachine"); err != nil {
		t.Fatal("the sidecar must exist next to the stub")
	}
	if out, _, code := artifact(t, py, "run", "--", "python3", "-c", "import sys; print('do pack'); sys.exit(3)"); out != "do pack\n" || code != 3 {
		t.Fatalf("run from stub + sidecar: %q rc=%d", out, code)
	}

	src, kid := uniqueName("pksrc"), uniqueName("pkkid")
	defer rcMachine(t, "delete", "--name", src, "--force")
	defer rcMachine(t, "delete", "--name", kid, "--force")
	mustMachine(t, "create", "--name", src, "--stack", "python")
	mustMachine(t, "start", "--name", src)
	mustMachine(t, "exec", "--name", src, "--", "sh", "-c", "echo marca-e2e > /root/marca && mkdir -p /root/lib && echo 'VALOR = 42' > /root/lib/mod.py")
	if _, errOut, rc := rcPack("create", "--from-vm", src, "--output", filepath.Join(dir, "x")); rc == 0 || !strings.Contains(errOut, "stop it first") {
		t.Fatalf("a running machine must not be packed: rc=%d %q", rc, errOut)
	}
	mustMachine(t, "stop", "--name", src)
	one := filepath.Join(dir, "one")
	mustPack("create", "--from-vm", src, "--output", one, "--single-file")
	if _, err := os.Stat(one + ".rcmachine"); !os.IsNotExist(err) {
		t.Fatal("--single-file must not write a sidecar")
	}
	mustMachine(t, "delete", "--name", src)
	out, errOut, code := artifact(t, one, "run", "--", "sh", "-c", "cat /root/marca; cd /root/lib && python3 -c 'import mod; print(mod.VALOR)'")
	if out != "marca-e2e\n42\n" || code != 0 {
		t.Fatalf("the packed machine's state must come with it: %q %q rc=%d", out, errOut, code)
	}

	name := "one" // o nome da máquina persistente é o do arquivo
	defer rcMachine(t, "delete", "--name", name, "--force")
	rcMachine(t, "delete", "--name", name, "--force")
	if _, errOut, code := artifact(t, one, "start"); code != 0 {
		t.Fatalf("start: %q", errOut)
	}
	artifact(t, one, "exec", "--", "sh", "-c", "echo persistente >> /root/marca")
	artifact(t, one, "stop")
	artifact(t, one, "start")
	if out, _, _ := artifact(t, one, "exec", "--", "cat", "/root/marca"); out != "marca-e2e\npersistente\n" {
		t.Fatalf("persistent mode must keep writes across stop/start: %q", out)
	}
	artifact(t, one, "stop")

	mustMachine(t, "create", "--name", kid, "--from", py+".rcmachine")
	mustMachine(t, "start", "--name", kid)
	if out := mustMachine(t, "exec", "--name", kid, "--", "python3", "-c", "print(6*7)"); out != "42\n" {
		t.Fatalf("machine create --from the payload: %q", out)
	}
	mustMachine(t, "stop", "--name", kid)
	if out := mustPack("inspect", one); !strings.Contains(out, `"source": "machine:`+src+`"`) || !strings.Contains(out, `"data.ext4"`) {
		t.Fatalf("inspect: %s", out)
	}
	if _, errOut, rc := rcPack("create", "--stack", "python", "-v", dir+":/host", "--output", filepath.Join(dir, "v")); rc == 0 || !strings.Contains(errOut, "does not travel") {
		t.Fatalf("a host directory volume must be refused: rc=%d %q", rc, errOut)
	}
}
