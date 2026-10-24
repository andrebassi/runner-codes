package app

import (
	"context"
	"testing"

	"runner.codes/internal/domain"
)

// TestDisplayPort: máquina ligada com tela devolve a porta do visualizador; sem tela é CONFIG, desligada é
// INVALID_STATE e desconhecida é NOT_FOUND (os códigos que a API transforma em 400, 409 e 404).
func TestDisplayPort(t *testing.T) {
	m, _, _, _, _ := machinesFixture(t)
	ctx := context.Background()
	if _, err := m.Create(ctx, domain.Machine{Name: "tela", Stack: "llama", GPU: true,
		Display: &domain.Display{Width: 1280, Height: 800, Port: 6099}}); err != nil {
		t.Fatal(err)
	}
	if _, err := m.Create(ctx, domain.Machine{Name: "cega", Stack: "llama", GPU: true}); err != nil {
		t.Fatal(err)
	}
	if _, err := m.DisplayPort("tela"); errCode(err) != domain.CodeInvalidState {
		t.Fatalf("desligada: %v", err)
	}
	for _, n := range []string{"tela", "cega"} {
		if _, err := m.Start(ctx, n); err != nil {
			t.Fatal(err)
		}
	}
	if port, err := m.DisplayPort("tela"); err != nil || port != 6099 {
		t.Fatalf("ligada: %d %v", port, err)
	}
	if _, err := m.DisplayPort("cega"); errCode(err) != domain.CodeConfig {
		t.Fatalf("sem tela: %v", err)
	}
	if _, err := m.DisplayPort("nada"); errCode(err) != domain.CodeNotFound {
		t.Fatalf("desconhecida: %v", err)
	}
}
