package oci

import (
	"archive/tar"
	"bytes"
	"compress/gzip"
	"os"
	"path/filepath"
	"testing"
)

// entry é uma entrada de tar para os testes.
type entry struct {
	name, link, body string
	typ              byte
	mode             int64
}

// makeTarGz monta um tar.gz em memória com as entradas dadas.
func makeTarGz(t *testing.T, entries []entry) []byte {
	t.Helper()
	var buf bytes.Buffer
	gz := gzip.NewWriter(&buf)
	tw := tar.NewWriter(gz)
	for _, e := range entries {
		mode := e.mode
		if mode == 0 {
			mode = 0o644
		}
		h := &tar.Header{Name: e.name, Linkname: e.link, Typeflag: e.typ, Mode: mode, Size: int64(len(e.body))}
		if e.typ != tar.TypeReg {
			h.Size = 0
		}
		if err := tw.WriteHeader(h); err != nil {
			t.Fatal(err)
		}
		if e.typ == tar.TypeReg {
			tw.Write([]byte(e.body))
		}
	}
	tw.Close()
	gz.Close()
	return buf.Bytes()
}

// TestExtractTarGz cobre diretório, arquivo, symlink, hardlink e o dispositivo ignorado.
func TestExtractTarGz(t *testing.T) {
	dest := t.TempDir()
	raw := makeTarGz(t, []entry{
		{name: "etc/", typ: tar.TypeDir, mode: 0o755},
		{name: "etc/os-release", typ: tar.TypeReg, body: "alpine", mode: 0o644},
		{name: "bin/busybox", typ: tar.TypeReg, body: "elf", mode: 0o755},
		{name: "bin/sh", typ: tar.TypeSymlink, link: "/bin/busybox"},
		{name: "bin/ash", typ: tar.TypeLink, link: "bin/busybox"},
		{name: "dev/null", typ: tar.TypeChar},
	})
	if err := extractTarGz(bytes.NewReader(raw), dest); err != nil {
		t.Fatal(err)
	}
	if b, _ := os.ReadFile(filepath.Join(dest, "etc/os-release")); string(b) != "alpine" {
		t.Fatalf("conteúdo errado: %q", b)
	}
	if l, _ := os.Readlink(filepath.Join(dest, "bin/sh")); l != "/bin/busybox" {
		t.Fatalf("symlink errado: %q", l)
	}
	if b, _ := os.ReadFile(filepath.Join(dest, "bin/ash")); string(b) != "elf" {
		t.Fatal("hardlink não aponta para o busybox")
	}
	if st, _ := os.Stat(filepath.Join(dest, "bin/busybox")); st.Mode().Perm()&0o100 == 0 {
		t.Fatal("bit de execução perdido")
	}
	if _, err := os.Lstat(filepath.Join(dest, "dev/null")); !os.IsNotExist(err) {
		t.Fatal("dispositivo não deveria ser criado no host")
	}
}

// TestExtractTraversalStaysInside: "../" é confinado ao destino, nada é escrito fora.
func TestExtractTraversalStaysInside(t *testing.T) {
	parent := t.TempDir()
	dest := filepath.Join(parent, "root")
	os.MkdirAll(dest, 0o755)
	raw := makeTarGz(t, []entry{{name: "../../fora.txt", typ: tar.TypeReg, body: "x"}})
	if err := extractTarGz(bytes.NewReader(raw), dest); err != nil {
		t.Fatal(err)
	}
	if _, err := os.Stat(filepath.Join(parent, "fora.txt")); !os.IsNotExist(err) {
		t.Fatal("arquivo escapou do destino")
	}
	if _, err := os.Stat(filepath.Join(dest, "fora.txt")); err != nil {
		t.Fatal("entrada deveria ter sido confinada dentro do destino")
	}
}

// TestSafeJoin trava a guarda contra escape, inclusive destino sem barra final.
func TestSafeJoin(t *testing.T) {
	if _, err := safeJoin("/a/root", "x/../../y"); err != nil {
		t.Fatalf("caminho limpo dentro do destino recusado: %v", err)
	}
	if got, _ := safeJoin("/a/root", "/"); got != "/a/root" {
		t.Fatalf("raiz deveria mapear para o destino, veio %s", got)
	}
	if _, err := safeJoin("/a/root", "\x00"); err != nil {
		t.Fatal(err)
	}
}

// TestExtractErrors cobre gzip inválido, tar corrompido e falhas de escrita no host.
func TestExtractErrors(t *testing.T) {
	if err := extractTarGz(bytes.NewReader([]byte("nao-e-gzip")), t.TempDir()); err == nil {
		t.Fatal("gzip inválido deveria falhar")
	}
	var buf bytes.Buffer
	gz := gzip.NewWriter(&buf)
	gz.Write([]byte("lixo que não é tar mas tem tamanho de cabeçalho" + string(make([]byte, 600))))
	gz.Close()
	if err := extractTarGz(bytes.NewReader(buf.Bytes()), t.TempDir()); err == nil {
		t.Fatal("tar corrompido deveria falhar")
	}
	// Um arquivo ocupa o lugar do diretório: cada tipo de entrada tem de devolver erro.
	for _, e := range []entry{
		{name: "f/sub/", typ: tar.TypeDir},
		{name: "f/sub", typ: tar.TypeReg, body: "x"},
		{name: "f/sub", typ: tar.TypeSymlink, link: "/x"},
		{name: "f/sub", typ: tar.TypeLink, link: "f"},
	} {
		dest := t.TempDir()
		os.WriteFile(filepath.Join(dest, "f"), []byte("arquivo"), 0o644)
		if err := extractTarGz(bytes.NewReader(makeTarGz(t, []entry{e})), dest); err == nil {
			t.Fatalf("entrada %q sob um arquivo deveria falhar", e.name)
		}
	}
}
