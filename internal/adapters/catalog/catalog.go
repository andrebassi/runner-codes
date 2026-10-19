// Package catalog carrega o catálogo de linguagens embutido no binário (languages.yaml).
package catalog

import (
	"bytes"
	_ "embed"
	"fmt"
	"sort"
	"strings"

	"gopkg.in/yaml.v3"

	"runner.code/internal/domain"
)

//go:embed languages.yaml
var embedded []byte

// Catalog indexa as linguagens por nome.
type Catalog struct {
	byName map[string]domain.Language
}

// file é o formato do YAML.
type file struct {
	Languages []domain.Language `yaml:"languages"`
}

// Default carrega o catálogo embutido. Erro aqui é defeito de build, não de runtime.
func Default() (*Catalog, error) { return Parse(embedded) }

// Parse lê o YAML recusando campo desconhecido (erro de digitação no catálogo não pode
// virar default silencioso), linguagem inválida, nome repetido e warmup multilinha.
func Parse(raw []byte) (*Catalog, error) {
	dec := yaml.NewDecoder(bytes.NewReader(raw))
	dec.KnownFields(true)
	var f file
	if err := dec.Decode(&f); err != nil {
		return nil, fmt.Errorf("catálogo: %w", err)
	}
	if len(f.Languages) == 0 {
		return nil, fmt.Errorf("catálogo vazio")
	}
	c := &Catalog{byName: map[string]domain.Language{}}
	for _, l := range f.Languages {
		if err := l.ValidateLanguage(); err != nil {
			return nil, fmt.Errorf("catálogo: %w", err)
		}
		if strings.Contains(l.Warmup, "\n") {
			return nil, fmt.Errorf("catálogo: warmup de %s tem mais de uma linha", l.Name)
		}
		if _, dup := c.byName[l.Name]; dup {
			return nil, fmt.Errorf("catálogo: linguagem %s repetida", l.Name)
		}
		c.byName[l.Name] = l
	}
	return c, nil
}

// Get devolve a linguagem pelo nome.
func (c *Catalog) Get(name string) (domain.Language, bool) {
	l, ok := c.byName[name]
	return l, ok
}

// Names devolve os nomes em ordem alfabética, para listagem estável.
func (c *Catalog) Names() []string {
	out := make([]string, 0, len(c.byName))
	for n := range c.byName {
		out = append(out, n)
	}
	sort.Strings(out)
	return out
}
