package krun

import (
	"encoding/json"
	"io"
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// TestRunVMMStartEnterFailure: se krun_start_enter volta, é erro e vem com o nome.
func TestRunVMMStartEnterFailure(t *testing.T) {
	t.Setenv(sandboxDisabledEnv, "off") // confinamento tem teste próprio (sandbox_test.go)
	f := &fakeAPI{failOn: "krun_start_enter"}
	err := RunVMM(f, jobSpec(), nil)
	if err == nil || !strings.Contains(err.Error(), "krun_start_enter") {
		t.Fatalf("expected a start_enter error, got %v", err)
	}
}

// TestRunVMMConfigureFailure: falha de configuração para antes de start_enter.
func TestRunVMMConfigureFailure(t *testing.T) {
	t.Setenv(sandboxDisabledEnv, "off") // confinamento tem teste próprio (sandbox_test.go)
	f := &fakeAPI{failOn: "krun_set_exec"}
	if err := RunVMM(f, jobSpec(), nil); err == nil {
		t.Fatal("expected an error")
	}
	for _, c := range f.calls {
		if c == "krun_start_enter" {
			t.Fatal("start_enter must not be called after a configuration failure")
		}
	}
}

// TestRunVMMWatchdogExitsWhenParentGone: EOF no pipe de vigia encerra com ExitParentGone.
func TestRunVMMWatchdogExitsWhenParentGone(t *testing.T) {
	t.Setenv(sandboxDisabledEnv, "off") // confinamento tem teste próprio (sandbox_test.go)
	got := make(chan int, 1)
	exitFunc = func(code int) { got <- code }
	defer func() { exitFunc = os.Exit }()
	r, w := io.Pipe()
	RunVMM(&fakeAPI{failOn: "krun_start_enter"}, jobSpec(), r)
	select {
	case <-got:
		t.Fatal("exited before the parent closed the pipe")
	case <-time.After(20 * time.Millisecond):
	}
	w.Close()
	select {
	case code := <-got:
		if code != ExitParentGone {
			t.Fatalf("exit code %d, want %d", code, ExitParentGone)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("watcher did not react to EOF")
	}
}

// TestRunVMMWithoutWatchdog trava o defeito do vmm rodado à mão: sem vigia (nil de verdade)
// o processo não pode sair com ExitParentGone.
func TestRunVMMWithoutWatchdog(t *testing.T) {
	t.Setenv(sandboxDisabledEnv, "off") // confinamento tem teste próprio (sandbox_test.go)
	got := make(chan int, 1)
	exitFunc = func(code int) { got <- code }
	defer func() { exitFunc = os.Exit }()
	RunVMM(&fakeAPI{failOn: "krun_start_enter"}, jobSpec(), nil)
	select {
	case code := <-got:
		t.Fatalf("exited with %d without a watcher", code)
	case <-time.After(50 * time.Millisecond):
	}
}

// TestReadSpec cobre leitura válida, arquivo ausente, JSON inválido e spec inválida.
func TestReadSpec(t *testing.T) {
	dir := t.TempDir()
	good := filepath.Join(dir, "good.json")
	raw, _ := json.Marshal(jobSpec())
	os.WriteFile(good, raw, 0o600)
	s, err := ReadSpec(good)
	if err != nil || s.RootDisk != "/img/python.ext4" {
		t.Fatalf("valid spec: %v %+v", err, s)
	}
	if _, err := ReadSpec(filepath.Join(dir, "missing.json")); err == nil {
		t.Fatal("missing file should fail")
	}
	bad := filepath.Join(dir, "bad.json")
	os.WriteFile(bad, []byte("{"), 0o600)
	if _, err := ReadSpec(bad); err == nil {
		t.Fatal("invalid JSON should fail")
	}
	invalid := filepath.Join(dir, "invalid.json")
	os.WriteFile(invalid, []byte(`{"cpus":1}`), 0o600)
	if _, err := ReadSpec(invalid); err == nil {
		t.Fatal("invalid spec should fail")
	}
}
