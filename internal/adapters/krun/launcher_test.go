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
	case "/sid":
		// Prova do Setsid: o processo lidera a própria sessão (sid == pid).
		sid, _ := syscall.Getsid(0)
		fmt.Fprintf(os.Stdout, "sid-is-pid=%v lib=%s\n", sid == os.Getpid(), os.Getenv(libPathEnv))
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
		t.Fatalf("exit code %d, want 3", code)
	}
	if _, err := os.Stat(p.specPath); !os.IsNotExist(err) {
		t.Fatal("spec should have been removed")
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
		t.Fatalf("%s did not reach the vmm: %q", libPathEnv, stderr.String())
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
		t.Fatal("fake vmm exited before Kill")
	case <-time.After(100 * time.Millisecond):
	}
	p.Kill()
	p.Kill() // idempotente
	if err := syscall.Kill(pid, 0); err == nil {
		t.Fatal("process survived Kill")
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
		t.Fatal("vmm did not exit with the watch pipe closed")
	}
	if p.exitCode != ExitParentGone {
		t.Fatalf("exit code %d, want %d", p.exitCode, ExitParentGone)
	}
}

// TestStartErrors cobre spec inválida, workdir impossível e binário inexistente.
func TestStartErrors(t *testing.T) {
	l := newTestLauncher(t, nil)
	if _, err := l.Start(Spec{}); err == nil {
		t.Fatal("invalid spec should fail")
	}
	file := filepath.Join(t.TempDir(), "file")
	os.WriteFile(file, nil, 0o600)
	bad := l.clone()
	bad.WorkDir = filepath.Join(file, "sub")
	if _, err := bad.Start(jobSpec()); err == nil {
		t.Fatal("workdir under a file should fail")
	}
	noExe := l.clone()
	noExe.Exe = filepath.Join(t.TempDir(), "does-not-exist")
	if _, err := noExe.Start(jobSpec()); err == nil {
		t.Fatal("missing binary should fail")
	}
	if entries, _ := os.ReadDir(noExe.WorkDir); len(entries) != 0 {
		t.Fatalf("spec left behind after a spawn failure: %v", entries)
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
		t.Fatal("VM through the port did not behave like the process")
	}
	if _, err := l.StartVM(Spec{}); err == nil {
		t.Fatal("invalid spec should fail through the port too")
	}
}

// TestExitCodeOf cobre os três casos: sucesso, saída com código e erro que não é de saída.
func TestExitCodeOf(t *testing.T) {
	if exitCodeOf(nil) != 0 {
		t.Fatal("nil should be 0")
	}
	if exitCodeOf(fmt.Errorf("other")) != -1 {
		t.Fatal("error without an exit code should be -1")
	}
}

// TestStartDetachedTied: a VM presa ao chamador (o run em primeiro plano) morre quando o
// processo que a ligou some — aqui, fechando o lado de escrita do pipe, que é o que o kernel
// faz quando o rc leva um SIGKILL. Sem o pipe ela seguiria viva (a sessão é própria).
func TestStartDetachedTied(t *testing.T) {
	l := newTestLauncher(t, nil)
	dir := t.TempDir()
	s := specExec("/sleep")
	s.TiedToCaller = true
	pid, err := l.StartDetached(s, filepath.Join(dir, "spec.json"), filepath.Join(dir, "vmm.log"))
	if err != nil {
		t.Fatal(err)
	}
	time.Sleep(300 * time.Millisecond)
	if syscall.Kill(pid, 0) != nil {
		t.Fatal("the tied VM must be running while the caller lives")
	}
	if len(l.tied) != 1 {
		t.Fatalf("the caller must hold one watch pipe, got %d", len(l.tied))
	}
	l.tied[0].Close()
	for end := time.Now().Add(5 * time.Second); time.Now().Before(end); time.Sleep(50 * time.Millisecond) {
		if syscall.Kill(pid, 0) != nil {
			return
		}
	}
	syscall.Kill(pid, syscall.SIGKILL)
	t.Fatal("the tied VM must die when its caller goes away")
}

// TestStartDetached: a máquina sobe em sessão própria (sobrevive ao rc), com a spec no
// caminho pedido (fica lá para o próximo start) e stdout/stderr no log da máquina.
func TestStartDetached(t *testing.T) {
	l := newTestLauncher(t, nil)
	dir := t.TempDir()
	spec, logPath := filepath.Join(dir, "spec.json"), filepath.Join(dir, "vmm.log")
	pid, err := l.StartDetached(specExec("/sid"), spec, logPath)
	if err != nil || pid <= 0 {
		t.Fatalf("pid=%d err=%v", pid, err)
	}
	var out []byte
	for end := time.Now().Add(5 * time.Second); time.Now().Before(end); time.Sleep(20 * time.Millisecond) {
		if out, _ = os.ReadFile(logPath); bytes.Contains(out, []byte("sid-is-pid")) {
			break
		}
	}
	if !strings.Contains(string(out), "sid-is-pid=true lib=/lib/krunfw") {
		t.Fatalf("detached vmm log: %q", out)
	}
	if _, err := os.Stat(spec); err != nil {
		t.Fatal("the spec must stay for the next start")
	}
	if _, err := l.StartDetached(Spec{}, spec, logPath); err == nil {
		t.Fatal("invalid spec must be refused")
	}
	if _, err := l.StartDetached(specExec("/sid"), filepath.Join(dir, "no", "spec.json"), logPath); err == nil {
		t.Fatal("unwritable spec path must fail")
	}
	if _, err := l.StartDetached(specExec("/sid"), spec, filepath.Join(dir, "no", "vmm.log")); err == nil {
		t.Fatal("unwritable log path must fail")
	}
	bad := l.clone()
	bad.Exe = filepath.Join(dir, "missing-binary")
	if _, err := bad.StartDetached(specExec("/sid"), spec, logPath); err == nil {
		t.Fatal("missing binary must fail")
	}
}

// clone copia a configuração de um Launcher sem o estado (o mutex e os pipes de vigia, que
// não se copiam): cada teste de falha mexe num campo da cópia.
func (l *Launcher) clone() *Launcher {
	return &Launcher{Exe: l.Exe, LibDir: l.LibDir, LibPath: l.LibPath, WorkDir: l.WorkDir, Stderr: l.Stderr}
}
