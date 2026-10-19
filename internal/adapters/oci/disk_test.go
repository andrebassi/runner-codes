package oci

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"runner.code/internal/ports"
)

// diskOK simula a VM de build do mkdisk: grava o "ext4" pedido e a linha RESULT.
func diskOK(s ports.VMSpec, vm *fakeVM) int {
	if s.Exec == "/work/mkdisk.sh" {
		os.WriteFile(filepath.Join(s.RootDir, s.Args[0]), []byte("ext4"), 0o644)
		os.WriteFile(s.Console, []byte("RESULT ok\n"), 0o644)
	}
	return buildOK(s, vm)
}

// TestDiskTemplate: o modelo é feito uma vez na VM de build (com o script certo e o
// tamanho no argv) e reaproveitado depois; tamanho fora da faixa e falha da VM são erro.
func TestDiskTemplate(t *testing.T) {
	l := &fakeLauncher{handle: diskOK}
	b := newBuilder(t, l, nil)
	p, err := b.DiskTemplate(context.Background(), 10)
	if err != nil {
		t.Fatal(err)
	}
	if !strings.HasSuffix(p, filepath.Join("disks", "data-10g.ext4")) {
		t.Fatalf("template path %s", p)
	}
	last := l.specs[len(l.specs)-1]
	if last.Exec != "/work/mkdisk.sh" || last.Args[1] != "10" {
		t.Fatalf("mkdisk spec: %+v", last)
	}
	script, _ := os.ReadFile(filepath.Join(b.builderRoot(), "work", "mkdisk.sh"))
	if !strings.Contains(string(script), "mkfs.ext4") {
		t.Fatal("mkdisk.sh not written for the build VM")
	}
	calls := l.calls()
	if p2, err := b.DiskTemplate(context.Background(), 10); err != nil || p2 != p || l.calls() != calls {
		t.Fatalf("second call must reuse the template: %v %d", err, l.calls())
	}
	for _, bad := range []int{0, 2048} {
		if _, err := b.DiskTemplate(context.Background(), bad); err == nil {
			t.Fatalf("size %d must be refused", bad)
		}
	}
	fail := newBuilder(t, &fakeLauncher{handle: func(s ports.VMSpec, vm *fakeVM) int {
		if s.Exec == "/work/mkdisk.sh" {
			return 1
		}
		return buildOK(s, vm)
	}}, nil)
	if _, err := fail.DiskTemplate(context.Background(), 5); err == nil {
		t.Fatal("a failed build VM must fail the template")
	}
	broken := newBuilder(t, &fakeLauncher{fail: errors.New("no hypervisor")}, nil)
	if _, err := broken.DiskTemplate(context.Background(), 5); err == nil {
		t.Fatal("no hypervisor must fail")
	}
}
