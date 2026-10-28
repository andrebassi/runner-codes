package client

import (
	"strings"
	"testing"
)

// TestPersistentDiskName: nome válido vira prefixo + nome; maiúscula, sublinhado e nome longo são saneados com o hash
// do original (dois nomes que se saneiam igual não colidem), e o resultado cabe nos 63 do GCE.
func TestPersistentDiskName(t *testing.T) {
	if got := PersistentDiskName("sbot-6fa2c9b8eca2"); got != "rc-pd-sbot-6fa2c9b8eca2" {
		t.Fatalf("simples: %s", got)
	}
	a, b := PersistentDiskName("Big_Name"), PersistentDiskName("big-name")
	if a == b || !strings.HasPrefix(a, "rc-pd-big-name-") || b != "rc-pd-big-name" {
		t.Fatalf("saneado: %s %s", a, b)
	}
	if long := PersistentDiskName(strings.Repeat("a", 80)); len(long) > 63 || !strings.HasPrefix(long, "rc-pd-aaa") {
		t.Fatalf("longo (%d): %s", len(long), long)
	}
}
