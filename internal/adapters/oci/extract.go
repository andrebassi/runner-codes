package oci

import (
	"archive/tar"
	"compress/gzip"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
)

// extractTarGz extrai o minirootfs da VM de build em dest. Recusa entrada que escape de
// dest (path traversal num tar vindo da rede) e ignora nó de dispositivo, que exige root
// no host e que o init da libkrun recria em /dev de qualquer forma.
func extractTarGz(r io.Reader, dest string) error {
	gz, err := gzip.NewReader(r)
	if err != nil {
		return fmt.Errorf("open gzip: %w", err)
	}
	defer gz.Close()
	tr := tar.NewReader(gz)
	for {
		h, err := tr.Next()
		if errors.Is(err, io.EOF) {
			return nil
		}
		if err != nil {
			return fmt.Errorf("read tar: %w", err)
		}
		target, err := safeJoin(dest, h.Name)
		if err != nil {
			return err
		}
		mode := os.FileMode(h.Mode).Perm()
		switch h.Typeflag {
		case tar.TypeDir:
			if err := os.MkdirAll(target, mode|0o700); err != nil {
				return err
			}
		case tar.TypeReg:
			if err := writeFile(target, tr, mode); err != nil {
				return err
			}
		case tar.TypeSymlink:
			// O alvo do link é interpretado dentro da VM, não no host: não se resolve aqui.
			os.Remove(target)
			if err := os.Symlink(h.Linkname, target); err != nil {
				return err
			}
		case tar.TypeLink:
			src, err := safeJoin(dest, h.Linkname)
			if err != nil {
				return err
			}
			os.Remove(target)
			if err := os.Link(src, target); err != nil {
				return err
			}
		}
	}
}

// writeFile cria o arquivo (e o diretório pai) com o conteúdo vindo do tar.
func writeFile(target string, r io.Reader, mode os.FileMode) error {
	if err := os.MkdirAll(filepath.Dir(target), 0o755); err != nil {
		return err
	}
	f, err := os.OpenFile(target, os.O_CREATE|os.O_TRUNC|os.O_WRONLY, mode|0o200)
	if err != nil {
		return err
	}
	_, err = io.Copy(f, r)
	if cerr := f.Close(); err == nil {
		err = cerr
	}
	return err
}

// safeJoin junta name em dest e recusa o que sair de dest depois de limpar o caminho.
func safeJoin(dest, name string) (string, error) {
	clean := filepath.Clean("/" + name)
	target := filepath.Join(dest, clean)
	if target != dest && !strings.HasPrefix(target, dest+string(os.PathSeparator)) {
		return "", fmt.Errorf("entry %q escapes the destination", name)
	}
	return target, nil
}
