package oci

import (
	"os"
	"path/filepath"
	"strings"
	"syscall"
)

// Prune libera o cache de imagens, o `machine prune`. Sempre sai o ext4 que nenhum
// nome aponta mais: o build de uma versão anterior (guest ou start.sh mudaram, a chave mudou e
// o link passou a apontar para o novo). Com all, saem também as imagens de referência OCI e
// de pack ("img-...") que nenhuma máquina usa; as stacks do catálogo ficam (são o que o `run`
// usa). inUse são os nomes de stack das máquinas existentes. Devolve o que saiu e os bytes
// ocupados de fato (o ext4 é esparso).
func (b *Builder) Prune(inUse map[string]bool, all bool) ([]string, int64, error) {
	dir := b.imagesDir()
	entries, err := os.ReadDir(dir)
	if os.IsNotExist(err) {
		return nil, 0, nil
	}
	if err != nil {
		return nil, 0, err
	}
	var removed []string
	var freed int64
	targets := map[string]bool{}
	for _, e := range entries {
		if e.Type()&os.ModeSymlink == 0 {
			continue
		}
		link := filepath.Join(dir, e.Name())
		stack := strings.TrimSuffix(e.Name(), ".ext4")
		if all && strings.HasPrefix(stack, "img-") && !inUse[stack] {
			os.Remove(link)
			removed = append(removed, e.Name())
			continue
		}
		if t, err := os.Readlink(link); err == nil {
			targets[filepath.Clean(filepath.Join(dir, t))] = true
		}
	}
	for _, e := range entries {
		p := filepath.Join(dir, e.Name())
		if !e.Type().IsRegular() || targets[p] || !strings.HasSuffix(e.Name(), ".ext4") {
			continue
		}
		freed += allocated(p)
		if err := os.Remove(p); err != nil {
			return removed, freed, err
		}
		removed = append(removed, e.Name())
	}
	return removed, freed, nil
}

// allocated é o espaço ocupado de fato pelo arquivo (blocos de 512 bytes), não o tamanho
// lógico: um ext4 de 10 GiB esparso ocupa o que tem de dado.
func allocated(p string) int64 {
	var st syscall.Stat_t
	if syscall.Stat(p, &st) != nil {
		return 0
	}
	return st.Blocks * 512
}
