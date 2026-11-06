package krun

import (
	"errors"
	"os"
	"syscall"
	"testing"
)

// fakeRlimit troca getrlimit/setrlimit por um limite em memória, para o teste não depender do processo.
func fakeRlimit(t *testing.T, cur, max uint64, getErr, setErr error) *syscall.Rlimit {
	t.Helper()
	state := &syscall.Rlimit{Cur: cur, Max: max}
	g, s := getrlimit, setrlimit
	t.Cleanup(func() { getrlimit, setrlimit = g, s })
	getrlimit = func(_ int, r *syscall.Rlimit) error { *r = *state; return getErr }
	setrlimit = func(_ int, r *syscall.Rlimit) error {
		if setErr != nil {
			return setErr
		}
		*state = *r
		return nil
	}
	return state
}

// TestRaiseNofile trava a correção do boot do Android: o limite sobe até o teto rígido, fica quieto quando já está
// lá, e uma recusa do kernel devolve o limite de antes com o erro (a VM segue mesmo assim).
func TestRaiseNofile(t *testing.T) {
	state := fakeRlimit(t, 1024, 524288, nil, nil)
	before, after, err := raiseNofile()
	if err != nil || before != 1024 || after != 524288 || state.Cur != 524288 {
		t.Fatalf("subir: before=%d after=%d err=%v cur=%d", before, after, err, state.Cur)
	}
	fakeRlimit(t, 4096, 4096, nil, nil)
	if before, after, err := raiseNofile(); err != nil || before != 4096 || after != 4096 {
		t.Fatalf("já no teto: before=%d after=%d err=%v", before, after, err)
	}
	fakeRlimit(t, 1024, 2048, nil, errors.New("EPERM"))
	if before, after, err := raiseNofile(); err == nil || before != 1024 || after != 1024 {
		t.Fatalf("recusado: before=%d after=%d err=%v", before, after, err)
	}
	fakeRlimit(t, 0, 0, errors.New("EFAULT"), nil)
	if _, _, err := raiseNofile(); err == nil {
		t.Fatal("erro do getrlimit sumiu")
	}
}

// TestLibDirFor trava a escolha do kernel por VM: a libkrunfw própria vai NA FRENTE (o dlopen pelo nome pega o
// primeiro diretório), e sem ela fica o diretório do servidor.
func TestLibDirFor(t *testing.T) {
	sep := string(os.PathListSeparator)
	cases := []struct{ lib, own, want string }{
		{"/srv/lib", "", "/srv/lib"},
		{"/srv/lib", "/nix/android/lib", "/nix/android/lib" + sep + "/srv/lib"},
		{"", "/nix/android/lib", "/nix/android/lib"},
	}
	for _, c := range cases {
		l := &Launcher{LibDir: c.lib}
		if got := l.libDirFor(Spec{KrunfwDir: c.own}); got != c.want {
			t.Errorf("libDirFor(lib=%q, own=%q) = %q; want %q", c.lib, c.own, got, c.want)
		}
	}
}
