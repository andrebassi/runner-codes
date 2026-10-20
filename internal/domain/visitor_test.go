package domain

import (
	"errors"
	"strings"
	"testing"
)

// TestVisitorMachineName: o mesmo visitante cai sempre na mesma máquina, visitantes diferentes em máquinas
// diferentes, e o nome não carrega o fingerprint cru.
func TestVisitorMachineName(t *testing.T) {
	a, err := VisitorMachineName("a1B2c3D4e5F6g7H8i9J0")
	if err != nil {
		t.Fatal(err)
	}
	again, _ := VisitorMachineName("a1B2c3D4e5F6g7H8i9J0")
	other, _ := VisitorMachineName("zzzzzzzzzzzzzzzzzzzz")
	if a != again || a == other || !strings.HasPrefix(a, VisitorPrefix) || len(a) != len(VisitorPrefix)+10 {
		t.Fatalf("nomes %q %q %q", a, again, other)
	}
	if strings.Contains(a, "a1B2") {
		t.Fatalf("fingerprint cru no nome: %q", a)
	}
}

// TestVisitorMachineNameRejects: curto, longo, com barra, ponto, espaço ou vazio é ValidationError.
func TestVisitorMachineNameRejects(t *testing.T) {
	for _, bad := range []string{"", "abc", strings.Repeat("a", 65), "../../etc", "a b c d e f g h", "abcdefgh.ij"} {
		_, err := VisitorMachineName(bad)
		var ve ValidationError
		if !errors.As(err, &ve) {
			t.Errorf("%q: %v", bad, err)
		}
	}
}
