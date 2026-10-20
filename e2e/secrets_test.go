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
)

// TestSecretsAndSSHAgent trava o fluxo de agente de código numa VM real:
//   - --secret-env e --secret-file: o valor (lido no start) está no ambiente do exec, volta
//     depois de stop/start e não aparece no registro da máquina no host;
//   - variável ausente no start: o start falha com o motivo (env_unset);
//   - --ssh-agent: o guest lista a chave de um agente do host (próprio do teste, com chave
//     descartável) e a chave privada não entra na VM.
func TestSecretsAndSSHAgent(t *testing.T) {
	dir, _ := os.MkdirTemp("/tmp", "rcag")
	defer os.RemoveAll(dir)
	key := filepath.Join(dir, "id")
	if out, err := exec.Command("ssh-keygen", "-q", "-t", "ed25519", "-N", "", "-C", "rc-e2e", "-f", key).CombinedOutput(); err != nil {
		t.Fatalf("ssh-keygen: %v %s", err, out)
	}
	sock := filepath.Join(dir, "agent.sock")
	ctx, cancel := context.WithCancel(context.Background())
	agent := exec.CommandContext(ctx, "ssh-agent", "-D", "-a", sock)
	if err := agent.Start(); err != nil {
		t.Fatal(err)
	}
	// Um defer só, na ordem certa: dois defers rodariam o Wait antes do cancel e o teste
	// esperaria para sempre um agente que nunca sai (medido).
	defer func() { cancel(); agent.Wait() }()
	waitFor(t, "the test ssh-agent", 10*time.Second, func() bool { _, err := os.Stat(sock); return err == nil })
	add := exec.Command("ssh-add", key)
	add.Env = append(os.Environ(), "SSH_AUTH_SOCK="+sock)
	if out, err := add.CombinedOutput(); err != nil {
		t.Fatalf("ssh-add: %v %s", err, out)
	}
	fp, _ := exec.Command("ssh-keygen", "-lf", key+".pub").Output()
	fingerprint := strings.Fields(string(fp))[1]
	t.Setenv("SSH_AUTH_SOCK", sock)
	t.Setenv("RC_E2E_API", "valor-do-ambiente-e2e")
	secretFile := filepath.Join(dir, "token")
	os.WriteFile(secretFile, []byte("valor-do-arquivo-e2e\n"), 0o600)

	name := uniqueName("agent")
	defer rcMachine(t, "delete", "--name", name, "--force")
	mustMachine(t, "create", "--name", name, "--image", "alpine", "--net", "--ssh-agent",
		"--secret-env", "API=RC_E2E_API", "--secret-file", "TOKEN="+secretFile,
		"--init", "apk add -q --no-cache openssh-client")
	mustMachine(t, "start", "--name", name)
	if out := mustMachine(t, "exec", "--name", name, "--", "sh", "-c", "echo $API; echo $TOKEN"); out != "valor-do-ambiente-e2e\nvalor-do-arquivo-e2e\n" {
		t.Fatalf("secrets in exec: %q", out)
	}
	if out := mustMachine(t, "exec", "--name", name, "--", "ssh-add", "-l"); !strings.Contains(out, fingerprint) {
		t.Fatalf("the guest must list the host key %s through the agent: %q", fingerprint, out)
	}
	priv, _ := os.ReadFile(key)
	needle := strings.Split(strings.TrimSpace(string(priv)), "\n")[2] // uma linha do meio da chave
	// Árvores de arquivo de verdade: um grep em / fica
	// preso lendo arquivos especiais de /sys e /dev (medido: o teste travou ali).
	if out, _, _ := rcMachine(t, "exec", "--name", name, "--", "sh", "-c", "grep -rlF '"+needle+"' /root /home /etc /tmp /var /workspace /opt 2>/dev/null | head -1"); strings.TrimSpace(out) != "" {
		t.Fatalf("the private key must not be in the guest: %q", out)
	}
	mdir := strings.TrimSpace(mustMachine(t, "data-dir", "--name", name))
	if out, _ := exec.Command("grep", "-rlF", "--exclude=*.ext4", "valor-do-", mdir).Output(); len(out) != 0 {
		t.Fatalf("no secret value in the machine directory: %s", out)
	}
	mustMachine(t, "stop", "--name", name)
	mustMachine(t, "start", "--name", name)
	if out := mustMachine(t, "exec", "--name", name, "--", "printenv", "API"); out != "valor-do-ambiente-e2e\n" {
		t.Fatalf("secrets are sent again on every start: %q", out)
	}
	mustMachine(t, "stop", "--name", name)
	os.Unsetenv("RC_E2E_API")
	if _, errOut, rc := rcMachine(t, "start", "--name", name); rc == 0 || !strings.Contains(errOut, "env_unset") {
		t.Fatalf("an unset host variable must fail the start: rc=%d %q", rc, errOut)
	}
}
