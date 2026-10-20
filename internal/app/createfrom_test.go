package app

import (
	"context"
	"io"
	"testing"

	"runner.code/internal/domain"
)

// TestCreateFrom: o disco dado (o de um pack de máquina) vira o modelo do clone, no create e
// no run; vazio continua usando o modelo novo do tamanho pedido.
func TestCreateFrom(t *testing.T) {
	m, st, _, _, _ := machinesFixture(t)
	ctx := context.Background()
	if _, err := m.CreateFrom(ctx, domain.Machine{Name: "p", Stack: "python"}, "/packs/abc/data.ext4"); err != nil {
		t.Fatal(err)
	}
	if got := st.tpls[len(st.tpls)-1]; got != "/packs/abc/data.ext4" {
		t.Fatalf("the pack disk must be the clone source: %s", got)
	}
	m.Create(ctx, domain.Machine{Name: "q", Stack: "python"})
	if got := st.tpls[len(st.tpls)-1]; got == "/packs/abc/data.ext4" {
		t.Fatal("without a disk, a new template")
	}
	_, code, err := m.Run(ctx, RunRequest{Machine: domain.Machine{Stack: "python"}, Command: []string{"true"}, DiskFrom: "/packs/def/data.ext4"}, RunIO{Stdout: io.Discard, Stderr: io.Discard})
	if err != nil || code != 0 {
		t.Fatalf("run: %d %v", code, err)
	}
	found := false
	for _, tpl := range st.tpls {
		found = found || tpl == "/packs/def/data.ext4"
	}
	if !found {
		t.Fatalf("run must clone from DiskFrom: %v", st.tpls)
	}
}
