package pack

import (
	"os"
	"path/filepath"
	"sort"
	"strings"
	"time"
)

// KeepRecent é quantas extrações o `pack prune` mantém sem --all: as 5 usadas por último.
const KeepRecent = 5

// Touch marca a extração como usada agora (o prune ordena por isso).
func Touch(dir string) {
	now := time.Now()
	os.Chtimes(dir, now, now)
}

// Prune apaga as extrações de packs em root (uma pasta por chave). Fica a que alguma máquina
// usa (inUse, pela chave) e, sem all, as keep usadas por último. Sobra de extração
// interrompida (".tmp") sai sempre. Devolve as chaves apagadas.
func Prune(root string, keep int, inUse map[string]bool, all bool) ([]string, error) {
	entries, err := os.ReadDir(root)
	if os.IsNotExist(err) {
		return nil, nil
	}
	if err != nil {
		return nil, err
	}
	type ext struct {
		key string
		mod time.Time
	}
	var exts []ext
	var removed []string
	for _, e := range entries {
		if !e.IsDir() {
			continue
		}
		if strings.HasSuffix(e.Name(), ".tmp") {
			os.RemoveAll(filepath.Join(root, e.Name()))
			removed = append(removed, e.Name())
			continue
		}
		info, err := e.Info()
		if err != nil {
			continue
		}
		exts = append(exts, ext{e.Name(), info.ModTime()})
	}
	sort.Slice(exts, func(i, j int) bool { return exts[i].mod.After(exts[j].mod) })
	kept := 0
	for _, x := range exts {
		if inUse[x.key] {
			continue
		}
		if !all && kept < keep {
			kept++
			continue
		}
		if err := os.RemoveAll(filepath.Join(root, x.key)); err != nil {
			return removed, err
		}
		removed = append(removed, x.key)
	}
	return removed, nil
}
