package machinestore

import (
	"bytes"
	"crypto/rand"
	"errors"
	"fmt"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"
	"syscall"
	"testing"

	"runner.codes/internal/domain"
)

// template grava um "disco" com dados no começo e no fim e um buraco no meio.
func template(t *testing.T) (string, []byte) {
	t.Helper()
	data := make([]byte, 3*sparseBlock+123)
	rand.Read(data[:1000])
	rand.Read(data[len(data)-100:]) // só o último bloco (123 bytes) tem dado no fim
	p := filepath.Join(t.TempDir(), "tpl.ext4")
	if err := os.WriteFile(p, data, 0o600); err != nil {
		t.Fatal(err)
	}
	return p, data
}

// code devolve o código estável de um erro de máquina ("" se não for um).
func code(err error) string {
	var me domain.MachineError
	if errors.As(err, &me) {
		return me.Code
	}
	return ""
}

// TestStoreLifecycle: criar (com o disco idêntico ao modelo), carregar, listar em ordem,
// nome repetido é CONFLICT, ausente é NOT_FOUND, pid gravado e limpo, e apagar.
func TestStoreLifecycle(t *testing.T) {
	tpl, data := template(t)
	s := New(filepath.Join(t.TempDir(), "machines"))
	if ms, err := s.List(); err != nil || len(ms) != 0 {
		t.Fatalf("empty store: %v %v", ms, err)
	}
	for _, n := range []string{"zeta", "alpha"} {
		if err := s.Create(domain.Machine{Name: n, Stack: "python", DiskGiB: 1}, tpl); err != nil {
			t.Fatal(err)
		}
	}
	got, _ := os.ReadFile(filepath.Join(s.Dir("alpha"), DiskFile))
	if !bytes.Equal(got, data) {
		t.Fatal("machine disk differs from the template")
	}
	if err := s.Create(domain.Machine{Name: "alpha", Stack: "go"}, tpl); code(err) != domain.CodeConflict {
		t.Fatalf("duplicate: %v", err)
	}
	// Diretório que já existe sem registro é o ponto de montagem do disco próprio recém-criado (só o lost+found do
	// ext4 dentro): a máquina nasce ali, em vez de CONFLICT.
	os.MkdirAll(filepath.Join(s.Dir("mounted"), "lost+found"), 0o700)
	if err := s.Create(domain.Machine{Name: "mounted", Stack: "python", DiskGiB: 1}, tpl); err != nil {
		t.Fatalf("ponto de montagem: %v", err)
	}
	if _, err := s.Load("mounted"); err != nil {
		t.Fatalf("registro no ponto de montagem: %v", err)
	}
	s.Delete("mounted")
	ms, _ := s.List()
	if len(ms) != 2 || ms[0].Name != "alpha" || ms[1].Name != "zeta" || ms[0].Stack != "python" {
		t.Fatalf("list: %+v", ms)
	}
	if _, err := s.Load("nope"); code(err) != domain.CodeNotFound {
		t.Fatalf("missing: %v", err)
	}
	if s.PID("alpha") != 0 {
		t.Fatal("new machine has no pid")
	}
	if err := s.SetPID("alpha", 4242); err != nil || s.PID("alpha") != 4242 {
		t.Fatalf("set pid: %v %d", err, s.PID("alpha"))
	}
	if err := s.SetPID("alpha", 0); err != nil || s.PID("alpha") != 0 {
		t.Fatal("clear pid")
	}
	if err := s.SetPID("alpha", 0); err != nil {
		t.Fatal("clearing twice is not an error")
	}
	os.WriteFile(filepath.Join(s.Dir("alpha"), pidFile), []byte("garbage"), 0o600)
	if s.PID("alpha") != 0 {
		t.Fatal("unreadable pid counts as none")
	}
	upd, _ := s.Load("alpha")
	upd.Network.Enabled = true
	if err := s.Save(upd); err != nil {
		t.Fatal(err)
	}
	if again, _ := s.Load("alpha"); !again.Network.Enabled {
		t.Fatal("save must persist")
	}
	if _, err := os.Stat(filepath.Join(s.Dir("alpha"), recordFile+".tmp")); !os.IsNotExist(err) {
		t.Fatal("temporary record left behind")
	}
	if err := s.Save(domain.Machine{Name: "ghost"}); code(err) != domain.CodeNotFound {
		t.Fatalf("save of an unknown machine: %v", err)
	}
	os.Chmod(s.Dir("alpha"), 0o500)
	if err := s.Save(upd); err == nil {
		t.Fatal("unwritable machine dir must fail")
	}
	os.Chmod(s.Dir("alpha"), 0o700)
	if err := s.Delete("alpha"); err != nil {
		t.Fatal(err)
	}
	if _, err := os.Stat(s.Dir("alpha")); !os.IsNotExist(err) {
		t.Fatal("directory left behind")
	}
	if err := s.Delete("alpha"); code(err) != domain.CodeNotFound {
		t.Fatalf("delete twice: %v", err)
	}
}

// TestStoreFailures: clone que falha não deixa diretório (o nome fica livre), registro
// corrompido é erro claro, e diretório sem registro não aparece na lista.
func TestStoreFailures(t *testing.T) {
	tpl, _ := template(t)
	s := New(t.TempDir())
	s.clone = func(string, string) error { return errors.New("disk full") }
	if err := s.Create(domain.Machine{Name: "a", Stack: "python"}, tpl); err == nil {
		t.Fatal("clone failure must fail")
	}
	if _, err := os.Stat(s.Dir("a")); !os.IsNotExist(err) {
		t.Fatal("half-created machine left behind")
	}
	s.clone = cloneFile
	if err := s.Create(domain.Machine{Name: "a", Stack: "python"}, filepath.Join(t.TempDir(), "missing")); err == nil {
		t.Fatal("missing template must fail")
	}
	os.MkdirAll(s.Dir("orphan"), 0o700)
	os.MkdirAll(s.Dir("bad"), 0o700)
	os.WriteFile(filepath.Join(s.Dir("bad"), recordFile), []byte("{"), 0o600)
	if _, err := s.Load("bad"); err == nil || code(err) != "" {
		t.Fatalf("corrupt record: %v", err)
	}
	if ms, _ := s.List(); len(ms) != 0 {
		t.Fatalf("orphan dirs must not be listed: %+v", ms)
	}
	blocked := New(filepath.Join(tpl, "not-a-dir"))
	if err := blocked.Create(domain.Machine{Name: "a", Stack: "python"}, tpl); err == nil {
		t.Fatal("root under a file must fail")
	}
	if _, err := blocked.List(); err == nil {
		t.Fatal("list under a file must fail")
	}
}

// TestCopySparse: o conteúdo volta igual e, no Linux, o meio de zeros não ocupa disco (buraco).
func TestCopySparse(t *testing.T) {
	tpl, data := template(t)
	dst := filepath.Join(t.TempDir(), "d")
	if err := copySparse(tpl, dst); err != nil {
		t.Fatal(err)
	}
	got, _ := os.ReadFile(dst)
	if !bytes.Equal(got, data) {
		t.Fatal("sparse copy changed the content")
	}
	// Só no Linux: o APFS aloca o trecho pulado por Seek mesmo assim (medido: 3 MiB pulados
	// ocuparam 3 MiB). No macOS o caminho principal é o clonefile, que não copia nada.
	if out, err := exec.Command("du", "-k", dst).Output(); runtime.GOOS == "linux" && err == nil && len(out) > 0 {
		var kib int
		if _, err := fmt.Sscan(string(out), &kib); err == nil && kib > 2*1024 {
			t.Fatalf("zeros were written instead of holes: %d KiB on disk", kib)
		}
	}
	if err := copySparse(filepath.Join(t.TempDir(), "none"), dst); err == nil {
		t.Fatal("missing source must fail")
	}
	if err := copySparse(tpl, filepath.Join(t.TempDir(), "x", "y")); err == nil {
		t.Fatal("unwritable destination must fail")
	}
}

// TestProcesses: o próprio processo está vivo, pid 0 e um pid improvável não estão.
func TestProcesses(t *testing.T) {
	var p Processes
	if !p.Alive(os.Getpid()) || p.Alive(0) || p.Alive(1<<30) {
		t.Fatal("alive check")
	}
	cmd := exec.Command("sleep", "30")
	cmd.SysProcAttr = &syscall.SysProcAttr{Setsid: true}
	if err := cmd.Start(); err != nil {
		t.Fatal(err)
	}
	// Pause/Resume mexem no grupo inteiro; o estado do processo (T = parado) prova o sinal.
	stat := func() string {
		out, _ := exec.Command("ps", "-o", "stat=", "-p", strconv.Itoa(cmd.Process.Pid)).Output()
		return strings.TrimSpace(string(out))
	}
	if err := p.Pause(cmd.Process.Pid); err != nil || !strings.HasPrefix(stat(), "T") {
		t.Fatalf("pause: %v state %q", err, stat())
	}
	if err := p.Resume(cmd.Process.Pid); err != nil || strings.HasPrefix(stat(), "T") {
		t.Fatalf("resume: %v state %q", err, stat())
	}
	if p.Pause(0) == nil || p.Resume(-1) == nil {
		t.Fatal("pid <= 0 must be refused (it would signal the test's own group)")
	}
	p.Kill(cmd.Process.Pid)
	if err := cmd.Wait(); err == nil {
		t.Fatal("killed process exited cleanly")
	}
	p.Kill(0) // não pode matar o próprio grupo do teste
}
