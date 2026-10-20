package hostfs

import (
	"os"
	"path/filepath"
	"testing"

	"golang.org/x/sys/unix"
)

// TestApplyGuestModes: arquivo e diretório com o xattr ganham as permissões do guest; o que
// não tem xattr, o link simbólico e o xattr malformado ficam como estão; diretório inexistente
// é erro.
func TestApplyGuestModes(t *testing.T) {
	dir := t.TempDir()
	script := filepath.Join(dir, "run.sh")
	sub := filepath.Join(dir, "sub")
	plain := filepath.Join(dir, "host.txt")
	bad := filepath.Join(dir, "bad.txt")
	os.WriteFile(script, []byte("#!/bin/sh\n"), 0o600)
	os.Mkdir(sub, 0o700)
	os.WriteFile(plain, nil, 0o640)
	os.WriteFile(bad, nil, 0o600)
	os.Symlink(script, filepath.Join(dir, "link"))
	set := func(p, v string) {
		if err := unix.Setxattr(p, overrideStat, []byte(v), 0); err != nil {
			t.Skipf("user xattrs unavailable here: %v", err)
		}
	}
	set(script, "0:0:0104755") // com setuid: só as permissões passam
	set(sub, "0:0:040755")
	set(bad, "0:0:nove")
	if err := ApplyGuestModes(dir); err != nil {
		t.Fatal(err)
	}
	for p, want := range map[string]os.FileMode{script: 0o755, sub: 0o755, plain: 0o640, bad: 0o600} {
		fi, _ := os.Stat(p)
		if fi.Mode().Perm() != want || fi.Mode()&os.ModeSetuid != 0 {
			t.Errorf("%s: %v, want %v", filepath.Base(p), fi.Mode(), want)
		}
	}
	if err := ApplyGuestModes(filepath.Join(dir, "nope")); err == nil {
		t.Fatal("a missing directory is an error")
	}
}

// TestGuestPerm: o formato do xattr, com e sem NUL no fim; malformado é recusado.
func TestGuestPerm(t *testing.T) {
	if p, ok := guestPerm("0:0:0100644\x00"); !ok || p != 0o644 {
		t.Fatalf("with NUL: %v %v", p, ok)
	}
	for _, bad := range []string{"0:0", "0:0:9", "a:b:c:d"} {
		if _, ok := guestPerm(bad); ok {
			t.Errorf("%q must be refused", bad)
		}
	}
}
