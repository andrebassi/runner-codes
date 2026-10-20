package oci

import (
	"bytes"
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"github.com/google/go-containerregistry/pkg/name"
	v1 "github.com/google/go-containerregistry/pkg/v1"
	"github.com/google/go-containerregistry/pkg/v1/empty"
	"github.com/google/go-containerregistry/pkg/v1/layout"
	"github.com/google/go-containerregistry/pkg/v1/mutate"
	"github.com/google/go-containerregistry/pkg/v1/random"
	"github.com/google/go-containerregistry/pkg/v1/tarball"
)

// archImage é uma imagem aleatória com a arquitetura declarada na config.
func archImage(t *testing.T, arch string) v1.Image {
	img, err := random.Image(64, 1)
	if err != nil {
		t.Fatal(err)
	}
	cfg, _ := img.ConfigFile()
	cfg = cfg.DeepCopy()
	cfg.OS, cfg.Architecture = "linux", arch
	img, err = mutate.ConfigFile(img, cfg)
	if err != nil {
		t.Fatal(err)
	}
	return img
}

// TestIsLocalRef: só caminho explícito é local; nome de registry, mesmo com barra, não.
func TestIsLocalRef(t *testing.T) {
	for ref, want := range map[string]bool{"/x/img.tar": true, "./img.tar": true, "../a.tar": true,
		"alpine": false, "ghcr.io/x/y:1": false, "img.tar": false} {
		if IsLocalRef(ref) != want {
			t.Fatalf("%s: want %v", ref, want)
		}
	}
}

// TestLocalTar: o tar do `docker save` abre com o mesmo digest; arquitetura diferente da do
// host é recusada com as duas; arquivo ausente ou que não é imagem é erro.
func TestLocalTar(t *testing.T) {
	dir := t.TempDir()
	img := archImage(t, "arm64")
	p := filepath.Join(dir, "app.tar")
	tag, _ := name.NewTag("myapp:latest")
	if err := tarball.WriteToFile(p, tag, img); err != nil {
		t.Fatal(err)
	}
	got, err := Fetch(context.Background(), p, "arm64")
	if err != nil {
		t.Fatal(err)
	}
	want, _ := img.Digest()
	if d, _ := got.Digest(); d != want {
		t.Fatalf("digest %s, want %s", d, want)
	}
	if _, err := LocalFetcher(p, "amd64"); err == nil || !strings.Contains(err.Error(), "linux/arm64") || !strings.Contains(err.Error(), "linux/amd64") {
		t.Fatalf("arch mismatch: %v", err)
	}
	if _, err := LocalFetcher(filepath.Join(dir, "none.tar"), "arm64"); err == nil {
		t.Fatal("missing file must fail")
	}
	junk := filepath.Join(dir, "junk.tar")
	os.WriteFile(junk, []byte("not a tar"), 0o600)
	if _, err := LocalFetcher(junk, "arm64"); err == nil || !strings.Contains(err.Error(), "docker save") {
		t.Fatalf("junk: %v", err)
	}
}

// TestLocalLayout: diretório OCI layout acha a imagem da arquitetura; sem ela é erro;
// diretório que não é layout é erro.
func TestLocalLayout(t *testing.T) {
	dir := filepath.Join(t.TempDir(), "layout")
	idx := mutate.AppendManifests(empty.Index,
		mutate.IndexAddendum{Add: archImage(t, "amd64"), Descriptor: v1.Descriptor{Platform: &v1.Platform{OS: "linux", Architecture: "amd64"}}},
		mutate.IndexAddendum{Add: archImage(t, "arm64"), Descriptor: v1.Descriptor{Platform: &v1.Platform{OS: "linux", Architecture: "arm64"}}})
	if _, err := layout.Write(dir, idx); err != nil {
		t.Fatal(err)
	}
	img, err := LocalFetcher(dir, "arm64")
	if err != nil {
		t.Fatal(err)
	}
	if cfg, _ := img.ConfigFile(); cfg.Architecture != "arm64" {
		t.Fatalf("picked %s", cfg.Architecture)
	}
	if _, err := LocalFetcher(dir, "riscv64"); err == nil || !strings.Contains(err.Error(), "no linux/riscv64") {
		t.Fatalf("no matching platform: %v", err)
	}
	if _, err := LocalFetcher(t.TempDir(), "arm64"); err == nil {
		t.Fatal("a plain directory is not a layout")
	}
}

// errAfter falha a leitura no meio, como um `docker save` interrompido.
type errAfter struct{ n int }

// Read entrega n bytes e depois falha.
func (e *errAfter) Read(p []byte) (int, error) {
	if e.n <= 0 {
		return 0, errors.New("pipe broke")
	}
	e.n--
	p[0] = 'x'
	return 1, nil
}

// TestImportStream: o mesmo conteúdo cai no mesmo arquivo (sha256 no nome) e nada de
// temporário sobra; leitura interrompida é erro sem deixar arquivo pela metade.
func TestImportStream(t *testing.T) {
	dir := filepath.Join(t.TempDir(), "imports")
	a, err := ImportStream(bytes.NewReader([]byte("same")), dir)
	if err != nil {
		t.Fatal(err)
	}
	b, _ := ImportStream(bytes.NewReader([]byte("same")), dir)
	if a != b || !strings.HasPrefix(filepath.Base(a), "sha256-") {
		t.Fatalf("stable name: %s %s", a, b)
	}
	if _, err := ImportStream(&errAfter{n: 3}, dir); err == nil || !strings.Contains(err.Error(), "stdin") {
		t.Fatalf("broken stream: %v", err)
	}
	entries, _ := os.ReadDir(dir)
	if len(entries) != 1 {
		t.Fatalf("only the imported image may remain: %v", entries)
	}
	if _, err := ImportStream(bytes.NewReader(nil), filepath.Join(a, "sub")); err == nil {
		t.Fatal("unusable directory must fail")
	}
}
