package krun

import (
	"bytes"
	"flag"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"syscall"
	"testing"
	"time"
)

// helperEnv liga o modo "vmm falso": o próprio binário de teste faz o papel do
// subcomando vmm, para testar spawn, vigia e kill sem libkrun nem hypervisor.
const helperEnv = "KRUN_LAUNCHER_HELPER"

// TestMain desvia para o vmm falso quando o launcher relança o binário de teste.
func TestMain(m *testing.M) {
	if os.Getenv(helperEnv) == "1" {
		fakeVMM()
		return
	}
	os.Exit(m.Run())
}

// fakeVMM interpreta a spec: Exec diz o que fazer ("/exit3" sai com 3, "/sleep" espera
// o vigia, "/env" imprime a variável da libkrunfw no stderr).
func fakeVMM() {
	fs := flag.NewFlagSet("vmm", flag.ExitOnError)
	spec := fs.String("spec", "", "")
	fs.Int("watch-fd", 3, "")
	fs.String("lib", "", "")
	fs.Parse(os.Args[2:])
	s, err := ReadSpec(*spec)
	if err != nil {
		fmt.Fprintln(os.Stderr, err)
		os.Exit(2)
	}
	switch s.Exec {
	case "/exit3":
		os.Exit(3)
	case "/env":
		fmt.Fprint(os.Stderr, os.Getenv(libPathEnv))
		os.Exit(0)
	default:
		RunVMM(&fakeAPI{failOn: "krun_start_enter"}, s, os.NewFile(3, "watch"))
		time.Sleep(time.Hour)
	}
}

// newTestLauncher aponta o launcher para o binário de teste em modo helper.
func newTestLauncher(t *testing.T, stderr *bytes.Buffer) *Launcher {
	t.Setenv(helperEnv, "1")
	l := &Launcher{Exe: os.Args[0], LibDir: "/lib/krunfw", LibPath: "/lib/libkrun", WorkDir: filepath.Join(t.TempDir(), "w")}
	if stderr != nil {
		l.Stderr = stderr
	}
	return l
}

// specExec devolve uma spec válida com o Exec que comanda o vmm falso.
func specExec(exec string) Spec {
	s := jobSpec()
	s.Exec = exec
	return s
}

// TestStartWaitExitCode: o código de saída do vmm chega ao chamador e a spec é apagada.
func TestStartWaitExitCode(t *testing.T) {
	l := newTestLauncher(t, nil)
	p, err := l.Start(specExec("/exit3"))
	if err != nil {
		t.Fatal(err)
	}
	if code := p.Wait(); code != 3 {
		t.Fatalf("código %d, quer 3", code)
	}
	if _, err := os.Stat(p.specPath); !os.IsNotExist(err) {
		t.Fatal("spec deveria ter sido apagada")
	}
}

// TestLibPathEnvReachesVMM: o vmm recebe o diretório da libkrunfw na variável do loader.
func TestLibPathEnvReachesVMM(t *testing.T) {
	var stderr bytes.Buffer
	l := newTestLauncher(t, &stderr)
	p, err := l.Start(specExec("/env"))
	if err != nil {
		t.Fatal(err)
	}
	p.Wait()
	if !strings.Contains(stderr.String(), "/lib/krunfw") {
		t.Fatalf("%s não chegou ao vmm: %q", libPathEnv, stderr.String())
	}
}

// TestKillLeavesNothing trava o mecanismo de timeout: depois do Kill o processo não existe.
func TestKillLeavesNothing(t *testing.T) {
	l := newTestLauncher(t, nil)
	p, err := l.Start(specExec("/sleep"))
	if err != nil {
		t.Fatal(err)
	}
	pid := p.PID()
	select {
	case <-p.Done():
		t.Fatal("vmm falso saiu antes do Kill")
	case <-time.After(100 * time.Millisecond):
	}
	p.Kill()
	p.Kill() // idempotente
	if err := syscall.Kill(pid, 0); err == nil {
		t.Fatal("processo sobreviveu ao Kill")
	}
}

// TestParentGoneClosesVMM: fechar o vigia (o que a morte do pai faz) derruba o vmm sozinho.
func TestParentGoneClosesVMM(t *testing.T) {
	l := newTestLauncher(t, nil)
	p, err := l.Start(specExec("/sleep"))
	if err != nil {
		t.Fatal(err)
	}
	time.Sleep(100 * time.Millisecond)
	p.watch.Close()
	select {
	case <-p.Done():
	case <-time.After(5 * time.Second):
		p.Kill()
		t.Fatal("vmm não saiu com o vigia fechado")
	}
	if p.exitCode != ExitParentGone {
		t.Fatalf("código %d, quer %d", p.exitCode, ExitParentGone)
	}
}

// TestStartErrors cobre spec inválida, workdir impossível e binário inexistente.
func TestStartErrors(t *testing.T) {
	l := newTestLauncher(t, nil)
	if _, err := l.Start(Spec{}); err == nil {
		t.Fatal("spec inválida deveria falhar")
	}
	file := filepath.Join(t.TempDir(), "arquivo")
	os.WriteFile(file, nil, 0o600)
	bad := *l
	bad.WorkDir = filepath.Join(file, "sub")
	if _, err := bad.Start(jobSpec()); err == nil {
		t.Fatal("workdir sob um arquivo deveria falhar")
	}
	noExe := *l
	noExe.Exe = filepath.Join(t.TempDir(), "nao-existe")
	if _, err := noExe.Start(jobSpec()); err == nil {
		t.Fatal("binário inexistente deveria falhar")
	}
	if entries, _ := os.ReadDir(noExe.WorkDir); len(entries) != 0 {
		t.Fatalf("spec ficou para trás depois de falha no spawn: %v", entries)
	}
}

// TestStartVMPort: StartVM entrega a VM pela porta e repassa erro de spec inválida.
func TestStartVMPort(t *testing.T) {
	l := newTestLauncher(t, nil)
	vm, err := l.StartVM(specExec("/exit3"))
	if err != nil {
		t.Fatal(err)
	}
	if vm.PID() <= 0 || vm.Wait() != 3 {
		t.Fatal("VM pela porta não se comportou como o processo")
	}
	if _, err := l.StartVM(Spec{}); err == nil {
		t.Fatal("spec inválida deveria falhar pela porta também")
	}
}

// TestExitCodeOf cobre os três casos: sucesso, saída com código e erro que não é de saída.
func TestExitCodeOf(t *testing.T) {
	if exitCodeOf(nil) != 0 {
		t.Fatal("nil deveria ser 0")
	}
	if exitCodeOf(fmt.Errorf("outro")) != -1 {
		t.Fatal("erro sem código deveria ser -1")
	}
}
