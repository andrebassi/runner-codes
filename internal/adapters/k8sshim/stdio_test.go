package k8sshim

import (
	"context"
	"io"
	"os"
	"path/filepath"
	"syscall"
	"testing"
	"time"

	"github.com/containerd/containerd/api/types"
)

// TestOpenFifos: com as três FIFOs, o stdin lê o que o "containerd" escreve, e stdout e stderr chegam
// ao leitor dele; o Close fecha tudo.
func TestOpenFifos(t *testing.T) {
	dir := t.TempDir()
	in, out, errp := filepath.Join(dir, "in"), filepath.Join(dir, "out"), filepath.Join(dir, "err")
	for _, p := range []string{in, out, errp} {
		if err := syscall.Mkfifo(p, 0o600); err != nil {
			t.Fatal(err)
		}
	}
	// O lado do containerd: leitores de stdout/stderr e escritor do stdin, abertos em paralelo.
	got := make(chan string, 2)
	for _, p := range []string{out, errp} {
		go func(p string) {
			f, _ := os.Open(p)
			b, _ := io.ReadAll(f)
			got <- string(b)
		}(p)
	}
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	st, closer, err := OpenFifos(ctx, in, out, errp, false)
	if err != nil {
		t.Fatal(err)
	}
	w, _ := os.OpenFile(in, os.O_WRONLY, 0)
	w.Write([]byte("hi"))
	w.Close()
	buf := make([]byte, 2)
	time.Sleep(100 * time.Millisecond)
	if n, _ := io.ReadFull(st.Stdin, buf); n != 2 || string(buf) != "hi" {
		t.Fatalf("stdin %q", buf)
	}
	st.Stdout.Write([]byte("o"))
	st.Stderr.Write([]byte("e"))
	closer.Close()
	a, b := <-got, <-got
	if a+b != "oe" && a+b != "eo" {
		t.Fatalf("saídas %q %q", a, b)
	}
}

// TestOpenFifosTerminalAndFile: com TTY o stderr não abre; file:// do ctr vira arquivo; vazio descarta.
func TestOpenFifosTerminalAndFile(t *testing.T) {
	dir := t.TempDir()
	logf := filepath.Join(dir, "sub", "out.log")
	st, closer, err := OpenFifos(context.Background(), "file:///x", "file://"+logf, "/never/opened", true)
	if err != nil {
		t.Fatal(err)
	}
	if st.Stdin != nil || st.Stderr != nil {
		t.Fatalf("%+v", st)
	}
	st.Stdout.Write([]byte("log"))
	closer.Close()
	if b, _ := os.ReadFile(logf); string(b) != "log" {
		t.Fatalf("arquivo %q", b)
	}
	if st, _, err := OpenFifos(context.Background(), "", "", "", false); err != nil || st.Stdout != nil {
		t.Fatal("vazio")
	}
}

// TestOpenFifosErrors: stdin, stdout ou stderr que não abrem falham sem deixar descritor aberto.
func TestOpenFifosErrors(t *testing.T) {
	ctx, cancel := context.WithTimeout(context.Background(), 300*time.Millisecond)
	defer cancel()
	dir := t.TempDir()
	if _, _, err := OpenFifos(ctx, filepath.Join(dir, "missing"), "", "", false); err == nil {
		t.Fatal("stdin")
	}
	if _, _, err := OpenFifos(ctx, "", "file:///dev/null/x/y", "", false); err == nil {
		t.Fatal("stdout")
	}
	if _, _, err := OpenFifos(ctx, "", "", "file:///dev/null/x/y", false); err == nil {
		t.Fatal("stderr")
	}
}

// TestMountRootfs: sem montagens, o root da spec (relativo ao bundle ou absoluto); com montagens num
// macOS, o mount falha com o motivo e nada fica montado.
func TestMountRootfs(t *testing.T) {
	dir, un, err := MountRootfs("/b", nil, "rootfs")
	if err != nil || dir != "/b/rootfs" || un() != nil {
		t.Fatal(dir, err)
	}
	if dir, _, _ := MountRootfs("/b", nil, "/abs"); dir != "/abs" {
		t.Fatal(dir)
	}
	// Com camadas: bundle onde não se cria o rootfs, e montagem que o sistema recusa (overlay
	// inexistente / macOS), falham com o motivo.
	layer := []*types.Mount{{Type: "overlay", Source: "overlay", Options: []string{"lowerdir=/nope"}}}
	if _, _, err := MountRootfs("/dev/null/b", layer, ""); err == nil {
		t.Fatal("mkdir do rootfs")
	}
	if _, _, err := MountRootfs(t.TempDir(), layer, ""); err == nil {
		t.Fatal("montagem recusada passou")
	}
}
