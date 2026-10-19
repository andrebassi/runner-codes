package catalog

import (
	"strings"
	"testing"

	"runner.code/internal/guest"
)

// TestDefaultLoads trava o catálogo embutido: carrega, e o Python está nele.
func TestDefaultLoads(t *testing.T) {
	c, err := Default()
	if err != nil {
		t.Fatal(err)
	}
	if l, ok := c.Get("python"); !ok || l.Image == "" {
		t.Fatalf("python ausente ou sem imagem: %+v", l)
	}
	if _, ok := c.Get("cobol"); ok {
		t.Fatal("cobol não está no catálogo")
	}
	names := c.Names()
	if len(names) < 10 || !strings.HasPrefix(strings.Join(names, ","), "awk,bash,c,") {
		t.Fatalf("lista inesperada: %v", names)
	}
}

// TestEveryLanguageExistsInGuest varre o catálogo: toda linguagem declarada tem de existir
// no executor do guest, senão a imagem sobe e o job falha com E006 dentro da VM.
func TestEveryLanguageExistsInGuest(t *testing.T) {
	c, _ := Default()
	supported := map[string]bool{}
	for _, n := range guest.NewExecutor().SupportedLanguages() {
		supported[n] = true
	}
	for _, n := range c.Names() {
		if !supported[n] {
			t.Errorf("linguagem %s do catálogo não existe no executor do guest", n)
		}
	}
}

// TestParseRejects cobre cada recusa do loader.
func TestParseRejects(t *testing.T) {
	cases := map[string]string{
		"yaml quebrado":      "languages: [",
		"campo desconhecido": "languages:\n  - {name: a, image: x, cpus: 1, memory_mib: 256, memoria: 1}\n",
		"vazio":              "languages: []\n",
		"inválida":           "languages:\n  - {name: a, image: x, cpus: 0, memory_mib: 256}\n",
		"repetida":           "languages:\n  - {name: a, image: x, cpus: 1, memory_mib: 256}\n  - {name: a, image: y, cpus: 1, memory_mib: 256}\n",
		"warmup multilinha":  "languages:\n  - name: a\n    image: x\n    cpus: 1\n    memory_mib: 256\n    warmup: \"a\\nb\"\n",
	}
	for name, raw := range cases {
		t.Run(name, func(t *testing.T) {
			if _, err := Parse([]byte(raw)); err == nil {
				t.Fatal("esperava recusa")
			}
		})
	}
}
