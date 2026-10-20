//go:build linux

package krun

import (
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strings"
	"testing"
)

// threadHelperEnv faz o binário de teste virar o processo confinado: o Landlock não se desfaz, então
// a prova roda num filho e o processo de teste segue livre.
const threadHelperEnv = "KRUN_THREAD_SANDBOX_HELPER"

// TestRestrictThreadHelper é o filho: confina a thread pela ABI do kernel e tenta ler dois arquivos,
// um liberado e um fora da política. Só roda quando chamado pelo teste abaixo.
func TestRestrictThreadHelper(t *testing.T) {
	dir := os.Getenv(threadHelperEnv)
	if dir == "" {
		t.Skip("só como filho de TestRestrictThreadBlocks")
	}
	runtime.LockOSThread()
	attr, rules := legacyRuleset(SandboxPolicy{ReadOnlyFiles: []string{filepath.Join(dir, "allowed")}}, LandlockABI())
	if err := restrictThread(attr, rules); err != nil {
		os.Stdout.WriteString("restrict-error " + err.Error() + "\n")
		os.Exit(0)
	}
	_, errOK := os.ReadFile(filepath.Join(dir, "allowed"))
	_, errDenied := os.ReadFile(filepath.Join(dir, "denied"))
	os.Stdout.WriteString("allowed=" + errString(errOK) + " denied=" + errString(errDenied) + "\n")
	os.Exit(0)
}

// errString resume o erro de leitura: "ok" ou a mensagem.
func errString(err error) string {
	if err == nil {
		return "ok"
	}
	return err.Error()
}

// TestRestrictThreadBlocks prova o caminho legado no kernel desta máquina: o arquivo da política
// abre, o de fora devolve permissão negada. Sem Landlock (ABI 0) o teste é pulado.
func TestRestrictThreadBlocks(t *testing.T) {
	if LandlockABI() == 0 {
		t.Skip("kernel sem Landlock")
	}
	dir := t.TempDir()
	for _, name := range []string{"allowed", "denied"} {
		if err := os.WriteFile(filepath.Join(dir, name), []byte("x"), 0o644); err != nil {
			t.Fatal(err)
		}
	}
	cmd := exec.Command(os.Args[0], "-test.run=^TestRestrictThreadHelper$", "-test.v")
	cmd.Env = append(os.Environ(), threadHelperEnv+"="+dir)
	out, _ := cmd.CombinedOutput()
	got := string(out)
	if !strings.Contains(got, "allowed=ok") || !strings.Contains(got, "denied=") || !strings.Contains(got, "permission denied") {
		t.Fatalf("o confinamento por thread não barrou o arquivo de fora:\n%s", got)
	}
}
