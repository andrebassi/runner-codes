//go:build e2e

package e2e

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestStagedVolume: o `-v HOST:GUEST:staged`, numa VM de verdade.
// A cópia mora no ext4 da máquina (não no virtio-fs); o host não muda até o sync; o sync
// espelha (apaga do host o que sumiu da cópia, inclusive o que o host criou depois) e um
// script novo chega executável ao host; um restart não semeia de novo; o stop gracioso
// sincroniza; a filha de um branch não devolve nada.
func TestStagedVolume(t *testing.T) {
	host := t.TempDir()
	write := func(p, s string) { os.MkdirAll(filepath.Dir(p), 0o755); os.WriteFile(p, []byte(s), 0o644) }
	read := func(p string) string { b, _ := os.ReadFile(p); return string(b) }
	write(filepath.Join(host, "keep.txt"), "original")
	write(filepath.Join(host, "gone.txt"), "x")
	write(filepath.Join(host, "sub", "x.txt"), "x")
	name, kid := uniqueName("staged"), uniqueName("stagedkid")
	defer rcMachine(t, "delete", "--name", name, "--force")
	defer rcMachine(t, "delete", "--name", kid, "--force")
	mustMachine(t, "create", "--name", name, "--stack", "python", "-v", host+":/app:staged")
	mustMachine(t, "start", "--name", name)
	if out := mustMachine(t, "exec", "--name", name, "--", "sh", "-c", "cat /app/keep.txt; echo; stat -f -c %T /app"); out != "original\next2/ext3\n" {
		t.Fatalf("seeded copy on the machine disk: %q", out)
	}
	mustMachine(t, "exec", "--name", name, "--", "sh", "-c",
		"echo mudou > /app/keep.txt && rm /app/gone.txt && printf '#!/bin/sh\\n' > /app/new.sh && chmod 755 /app/new.sh && mkdir /app/novo")
	if read(filepath.Join(host, "keep.txt")) != "original" || read(filepath.Join(host, "gone.txt")) != "x" {
		t.Fatal("the host must not change before a sync")
	}
	write(filepath.Join(host, "hostonly.txt"), "feito no host depois do boot")
	mustMachine(t, "sync", "--name", name)
	if read(filepath.Join(host, "keep.txt")) != "mudou\n" {
		t.Fatalf("keep.txt after sync: %q", read(filepath.Join(host, "keep.txt")))
	}
	for _, f := range []string{"gone.txt", "hostonly.txt"} {
		if _, err := os.Stat(filepath.Join(host, f)); err == nil {
			t.Fatalf("%s must be gone after the mirror", f)
		}
	}
	if fi, err := os.Stat(filepath.Join(host, "new.sh")); err != nil || fi.Mode().Perm() != 0o755 {
		t.Fatalf("new.sh on the host: %v %v", fi, err)
	}
	if fi, err := os.Stat(filepath.Join(host, "novo")); err != nil || !fi.IsDir() || fi.Mode().Perm() != 0o755 {
		t.Fatalf("novo/ on the host: %v %v", fi, err)
	}
	mustMachine(t, "exec", "--name", name, "--", "sh", "-c", "echo tarde > /app/late.txt")
	mustMachine(t, "stop", "--name", name)
	if read(filepath.Join(host, "late.txt")) != "tarde\n" {
		t.Fatal("a graceful stop syncs")
	}
	write(filepath.Join(host, "keep.txt"), "editado no host com a maquina parada")
	mustMachine(t, "start", "--name", name)
	if out := mustMachine(t, "exec", "--name", name, "--", "cat", "/app/keep.txt"); out != "mudou\n" {
		t.Fatalf("a restart must not seed again: %q", out)
	}
	mustMachine(t, "branch", "--from", name, "--name", kid)
	mustMachine(t, "start", "--name", kid)
	mustMachine(t, "exec", "--name", kid, "--", "sh", "-c", "test -f /app/late.txt && echo filha > /app/child.txt")
	mustMachine(t, "stop", "--name", kid)
	if _, err := os.Stat(filepath.Join(host, "child.txt")); err == nil {
		t.Fatal("a branch child must not sync back")
	}
	if !strings.Contains(read(filepath.Join(host, "keep.txt")), "editado no host") {
		t.Fatal("nothing synced the child or the parent since the host edit")
	}
	mustMachine(t, "stop", "--name", name, "--timeout", "0")
}
