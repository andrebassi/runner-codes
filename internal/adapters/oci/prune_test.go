package oci

import (
	"os"
	"path/filepath"
	"sort"
	"strings"
	"testing"
)

// TestPrune: o ext4 que nenhum link aponta sai sempre (build velho); o atual fica. Com all,
// a imagem de referência sem máquina sai junto com o arquivo dela; a usada por uma máquina e
// as stacks do catálogo ficam. Diretório ausente não é erro.
func TestPrune(t *testing.T) {
	b := &Builder{CacheDir: t.TempDir()}
	if r, _, err := b.Prune(nil, true); err != nil || len(r) != 0 {
		t.Fatalf("no cache: %v %v", r, err)
	}
	dir := b.imagesDir()
	os.MkdirAll(dir, 0o755)
	for _, f := range []string{"python-old.ext4", "python-new.ext4", "img-aaa-1.ext4", "img-bbb-1.ext4", "notes.txt"} {
		os.WriteFile(filepath.Join(dir, f), []byte("x"), 0o644)
	}
	os.Symlink("python-new.ext4", filepath.Join(dir, "python.ext4"))
	os.Symlink("img-aaa-1.ext4", filepath.Join(dir, "img-aaa.ext4"))
	os.Symlink("img-bbb-1.ext4", filepath.Join(dir, "img-bbb.ext4"))
	removed, freed, err := b.Prune(map[string]bool{"img-aaa": true}, false)
	if err != nil || strings.Join(removed, " ") != "python-old.ext4" || freed < 0 {
		t.Fatalf("plain prune: %v %d %v", removed, freed, err)
	}
	removed, _, _ = b.Prune(map[string]bool{"img-aaa": true}, true)
	sort.Strings(removed)
	if strings.Join(removed, " ") != "img-bbb-1.ext4 img-bbb.ext4" {
		t.Fatalf("all: %v", removed)
	}
	left, _ := os.ReadDir(dir)
	var names []string
	for _, e := range left {
		names = append(names, e.Name())
	}
	if strings.Join(names, " ") != "img-aaa-1.ext4 img-aaa.ext4 notes.txt python-new.ext4 python.ext4" {
		t.Fatalf("left: %v", names)
	}
	if allocated(filepath.Join(dir, "missing")) != 0 {
		t.Fatal("missing file allocates nothing")
	}
}
