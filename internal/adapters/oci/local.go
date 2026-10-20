package oci

import (
	"context"
	"crypto/sha256"
	"encoding/hex"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"

	v1 "github.com/google/go-containerregistry/pkg/v1"
	"github.com/google/go-containerregistry/pkg/v1/layout"
	"github.com/google/go-containerregistry/pkg/v1/tarball"
)

// IsLocalRef diz se a referência é uma imagem no disco (o que o `docker save` grava, ou um
// diretório OCI layout) em vez de um nome de registry: começa com /, ./ ou ../.
func IsLocalRef(ref string) bool {
	return strings.HasPrefix(ref, "/") || strings.HasPrefix(ref, "./") || strings.HasPrefix(ref, "../")
}

// Fetch busca a imagem no disco (IsLocalRef) ou no registry. É o Fetcher padrão do rc.
func Fetch(ctx context.Context, ref, arch string) (v1.Image, error) {
	if !IsLocalRef(ref) {
		return RemoteFetcher(ctx, ref, arch)
	}
	return LocalFetcher(ref, arch)
}

// LocalFetcher abre um tar do `docker save` (ou tar OCI de uma imagem) ou um diretório OCI
// layout, e confere a arquitetura: imagem amd64 numa VM arm64 só falharia lá dentro, com
// "exec format error" no primeiro comando.
func LocalFetcher(path, arch string) (v1.Image, error) {
	fi, err := os.Stat(path)
	if err != nil {
		return nil, err
	}
	var img v1.Image
	if fi.IsDir() {
		idx, err := layout.ImageIndexFromPath(path)
		if err != nil {
			return nil, fmt.Errorf("OCI layout %s: %w", path, err)
		}
		man, err := idx.IndexManifest()
		if err != nil {
			return nil, err
		}
		for _, d := range man.Manifests {
			if d.Platform == nil || (d.Platform.OS == "linux" && d.Platform.Architecture == arch) {
				if img, err = idx.Image(d.Digest); err != nil {
					return nil, err
				}
				break
			}
		}
		if img == nil {
			return nil, fmt.Errorf("OCI layout %s has no linux/%s image", path, arch)
		}
	} else if img, err = tarball.ImageFromPath(path, nil); err != nil {
		return nil, fmt.Errorf("image tar %s (docker save or OCI tar of one image): %w", path, err)
	}
	cfg, err := img.ConfigFile()
	if err != nil {
		return nil, err
	}
	if cfg.Architecture != "" && cfg.Architecture != arch {
		return nil, fmt.Errorf("image %s is linux/%s; this host runs linux/%s guests", path, cfg.Architecture, arch)
	}
	return img, nil
}

// ImportStream grava a imagem que chega por r (o `--image -`) em dir com o nome do sha256
// do conteúdo e devolve o caminho: a mesma imagem importada de novo cai no mesmo arquivo, e
// portanto no mesmo ext4 do cache, em vez de reconstruir.
func ImportStream(r io.Reader, dir string) (string, error) {
	if err := os.MkdirAll(dir, 0o700); err != nil {
		return "", err
	}
	tmp, err := os.CreateTemp(dir, "import-*.tar")
	if err != nil {
		return "", err
	}
	defer os.Remove(tmp.Name())
	h := sha256.New()
	_, err = io.Copy(io.MultiWriter(tmp, h), r)
	if cerr := tmp.Close(); err == nil {
		err = cerr
	}
	if err != nil {
		return "", fmt.Errorf("read image from stdin: %w", err)
	}
	final := filepath.Join(dir, "sha256-"+hex.EncodeToString(h.Sum(nil))+".tar")
	if err := os.Rename(tmp.Name(), final); err != nil {
		return "", err
	}
	return final, nil
}
