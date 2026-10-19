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
	f := &fakeAPI{failOn: "krun_start_enter"}
	err := RunVMM(f, jobSpec(), nil)
	if err == nil || !strings.Contains(err.Error(), "krun_start_enter") {
		t.Fatalf("esperava erro de start_enter, veio %v", err)
	}
}

// TestRunVMMConfigureFailure: falha de configuração para antes de start_enter.
func TestRunVMMConfigureFailure(t *testing.T) {
	f := &fakeAPI{failOn: "krun_set_exec"}
	if err := RunVMM(f, jobSpec(), nil); err == nil {
		t.Fatal("esperava erro")
	}
	for _, c := range f.calls {
		if c == "krun_start_enter" {
			t.Fatal("start_enter não pode ser chamado depois de falha de configuração")
		}
	}
}

// TestRunVMMWatchdogExitsWhenParentGone: EOF no pipe de vigia encerra com ExitParentGone.
func TestRunVMMWatchdogExitsWhenParentGone(t *testing.T) {
	got := make(chan int, 1)
	exitFunc = func(code int) { got <- code }
	defer func() { exitFunc = os.Exit }()
	r, w := io.Pipe()
	RunVMM(&fakeAPI{failOn: "krun_start_enter"}, jobSpec(), r)
	select {
	case <-got:
		t.Fatal("saiu antes de o pai fechar o pipe")
	case <-time.After(20 * time.Millisecond):
	}
	w.Close()
	select {
	case code := <-got:
		if code != ExitParentGone {
			t.Fatalf("código %d, quer %d", code, ExitParentGone)
		}
	case <-time.After(2 * time.Second):
		t.Fatal("vigia não reagiu ao EOF")
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
		t.Fatalf("spec válida: %v %+v", err, s)
	}
	if _, err := ReadSpec(filepath.Join(dir, "nada.json")); err == nil {
		t.Fatal("arquivo ausente deveria falhar")
	}
	bad := filepath.Join(dir, "bad.json")
	os.WriteFile(bad, []byte("{"), 0o600)
	if _, err := ReadSpec(bad); err == nil {
		t.Fatal("JSON inválido deveria falhar")
	}
	invalid := filepath.Join(dir, "invalid.json")
	os.WriteFile(invalid, []byte(`{"cpus":1}`), 0o600)
	if _, err := ReadSpec(invalid); err == nil {
		t.Fatal("spec inválida deveria falhar")
	}
}
