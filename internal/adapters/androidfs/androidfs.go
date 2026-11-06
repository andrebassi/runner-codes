// Package androidfs prepara o rootfs de uma máquina Android: a imagem do Redroid extraída num diretório (cache por
// digest, uma vez por servidor), a cópia de cada máquina (o /data dela mora ali) e os ajustes que fazem o Redroid
// funcionar como PID 1 de uma microVM libkrun atrás do gateway do rc, em vez de num contêiner.
//
// Os ajustes vêm da prova de 05-06/11/2026 (scripts/android/ do superbot.computer); cada um tem o motivo no texto
// que ele grava.
package androidfs

import (
	"archive/tar"
	"context"
	"errors"
	"fmt"
	"io"
	"net/netip"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"time"

	v1 "github.com/google/go-containerregistry/pkg/v1"
	"github.com/google/go-containerregistry/pkg/v1/mutate"
)

// Fetcher busca a imagem para a arquitetura (o oci.RemoteFetcher em produção; um registry local nos testes).
type Fetcher func(ctx context.Context, ref, arch string) (v1.Image, error)

// Net é o enlace que o gateway do rc dá à máquina (o endereço do convidado com o prefixo e o do gateway) e a porta em
// que o adbd escuta no convidado.
type Net struct {
	Guest   netip.Prefix
	Gateway netip.Addr
	ADBPort int
}

// Rootfs prepara os diretórios. CacheDir guarda as imagens extraídas (uma por digest).
type Rootfs struct {
	CacheDir string
	Arch     string
	Fetch    Fetcher
	// Copy copia a imagem extraída para o diretório da máquina preservando dono, modo e links; nil usa `cp -a` (com
	// reflink quando o sistema de arquivos deixa: a cópia de 1,6 GB vira só metadados).
	Copy func(src, dst string) error
	// Chown troca o dono de cada entrada extraída; nil usa os.Lchown. Os testes trocam (sem root, chown falha).
	Chown func(path string, uid, gid int) error
}

// rootfsName é o nome do diretório do rootfs dentro do diretório da máquina.
const rootfsName = "android-rootfs"

// Prepare devolve o rootfs da máquina (machineDir/android-rootfs), criando-o a partir da imagem na primeira vez. Os
// ajustes são regravados a cada chamada: uma versão nova do rc corrige uma máquina antiga no próximo start.
func (r *Rootfs) Prepare(ctx context.Context, image, machineDir string, n Net) (string, error) {
	dst := filepath.Join(machineDir, rootfsName)
	if _, err := os.Stat(filepath.Join(dst, "system", "bin", "init")); err != nil {
		base, err := r.base(ctx, image)
		if err != nil {
			return "", err
		}
		tmp := dst + ".partial"
		os.RemoveAll(tmp)
		copyFn := r.Copy
		if copyFn == nil {
			copyFn = cpArchive
		}
		if err := copyFn(base, tmp); err != nil {
			os.RemoveAll(tmp)
			return "", fmt.Errorf("android: copy rootfs to the machine: %w", err)
		}
		os.RemoveAll(dst)
		if err := os.Rename(tmp, dst); err != nil {
			return "", fmt.Errorf("android: rootfs: %w", err)
		}
	}
	if err := Tweak(dst, n); err != nil {
		return "", err
	}
	return dst, nil
}

// base devolve a imagem extraída no cache, baixando e extraindo na primeira vez. A extração vai para um diretório
// temporário e só vira o definitivo inteira: um rc morto no meio não deixa um cache pela metade que pareça bom.
func (r *Rootfs) base(ctx context.Context, image string) (string, error) {
	img, err := r.Fetch(ctx, image, r.Arch)
	if err != nil {
		return "", fmt.Errorf("android: fetch %s: %w", image, err)
	}
	digest, err := img.Digest()
	if err != nil {
		return "", fmt.Errorf("android: digest of %s: %w", image, err)
	}
	dir := filepath.Join(r.CacheDir, "android", digest.Hex)
	root := filepath.Join(dir, "rootfs")
	if _, err := os.Stat(filepath.Join(root, "system", "bin", "init")); err == nil {
		return root, nil
	}
	tmp := root + ".partial"
	os.RemoveAll(tmp)
	if err := os.MkdirAll(tmp, 0o755); err != nil {
		return "", fmt.Errorf("android: cache: %w", err)
	}
	rc := mutate.Extract(img)
	defer rc.Close()
	chown := r.Chown
	if chown == nil {
		chown = os.Lchown
	}
	if err := Untar(rc, tmp, chown); err != nil {
		os.RemoveAll(tmp)
		return "", fmt.Errorf("android: extract %s: %w", image, err)
	}
	if _, err := os.Stat(filepath.Join(tmp, "system", "bin", "init")); err != nil {
		os.RemoveAll(tmp)
		return "", fmt.Errorf("android: %s has no /system/bin/init: not an Android (Redroid) image", image)
	}
	os.RemoveAll(root)
	if err := os.Rename(tmp, root); err != nil {
		return "", fmt.Errorf("android: cache: %w", err)
	}
	return root, nil
}

// cpArchive copia preservando tudo (dono, modo, links, xattrs) e tenta o reflink.
func cpArchive(src, dst string) error {
	out, err := exec.Command("cp", "-a", "--reflink=auto", src, dst).CombinedOutput()
	if err != nil {
		return fmt.Errorf("%w: %s", err, strings.TrimSpace(string(out)))
	}
	return nil
}

// Untar extrai um tar (a imagem já achatada, com os whiteouts resolvidos) em root. Dono, modo (com setuid/setgid,
// que o Android usa em binários do /system) e mtime são preservados; dispositivos e FIFOs são pulados (o init do
// Android cria os que precisa). Nenhuma entrada escapa de root: nem por "..", nem por um symlink que a própria
// imagem criou num diretório pai.
func Untar(r io.Reader, root string, chown func(string, int, int) error) error {
	tr := tar.NewReader(r)
	realRoot, err := filepath.EvalSymlinks(root)
	if err != nil {
		return err
	}
	for {
		h, err := tr.Next()
		if errors.Is(err, io.EOF) {
			return nil
		}
		if err != nil {
			return err
		}
		rel := filepath.Clean("/" + h.Name)
		if rel == "/" {
			continue
		}
		path := filepath.Join(realRoot, rel)
		parent := filepath.Dir(path)
		if err := os.MkdirAll(parent, 0o755); err != nil {
			return err
		}
		if real, err := filepath.EvalSymlinks(parent); err != nil || !within(realRoot, real) {
			return fmt.Errorf("entry %q escapes the rootfs", h.Name)
		}
		mode := os.FileMode(h.Mode & 0o777)
		if h.Mode&04000 != 0 {
			mode |= os.ModeSetuid
		}
		if h.Mode&02000 != 0 {
			mode |= os.ModeSetgid
		}
		if h.Mode&01000 != 0 {
			mode |= os.ModeSticky
		}
		switch h.Typeflag {
		case tar.TypeDir:
			if err := os.MkdirAll(path, 0o755); err != nil {
				return err
			}
		case tar.TypeReg:
			os.Remove(path)
			f, err := os.OpenFile(path, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0o600)
			if err != nil {
				return err
			}
			_, err = io.Copy(f, tr)
			if cerr := f.Close(); err == nil {
				err = cerr
			}
			if err != nil {
				return err
			}
		case tar.TypeSymlink:
			os.Remove(path)
			if err := os.Symlink(h.Linkname, path); err != nil {
				return err
			}
		case tar.TypeLink:
			target := filepath.Join(realRoot, filepath.Clean("/"+h.Linkname))
			os.Remove(path)
			if err := os.Link(target, path); err != nil {
				return err
			}
			// O hard link divide o inode com o alvo: dono e modo já são os dele. Aplicar os do cabeçalho (o tar
			// grava 0) zerava as permissões do próprio alvo — o /system/bin/init ficava ilegível (pego no teste).
			continue
		default:
			continue // dispositivo, FIFO: o init do Android cria
		}
		if err := chown(path, h.Uid, h.Gid); err != nil {
			return fmt.Errorf("chown %s: %w", h.Name, err)
		}
		if h.Typeflag == tar.TypeSymlink {
			continue // modo e data de symlink não se trocam no Linux
		}
		if err := os.Chmod(path, mode); err != nil {
			return err
		}
		os.Chtimes(path, time.Now(), h.ModTime)
	}
}

// within diz se path está dentro de root (ou é ele).
func within(root, path string) bool {
	return path == root || strings.HasPrefix(path, root+string(filepath.Separator))
}
