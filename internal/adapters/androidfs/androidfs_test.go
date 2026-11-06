package androidfs

import (
	"archive/tar"
	"bytes"
	"context"
	"errors"
	"io"
	"net/netip"
	"os"
	"path/filepath"
	"strings"
	"sync/atomic"
	"testing"
	"time"

	v1 "github.com/google/go-containerregistry/pkg/v1"
	"github.com/google/go-containerregistry/pkg/v1/empty"
	"github.com/google/go-containerregistry/pkg/v1/mutate"
	"github.com/google/go-containerregistry/pkg/v1/tarball"
)

// entry é uma linha do tar de teste.
type entry struct {
	name, link string
	typ        byte
	mode       int64
	body       string
	uid, gid   int
}

// mkTar monta um tar com as entradas, na ordem.
func mkTar(t *testing.T, es ...entry) []byte {
	t.Helper()
	var buf bytes.Buffer
	tw := tar.NewWriter(&buf)
	for _, e := range es {
		h := &tar.Header{Name: e.name, Linkname: e.link, Typeflag: e.typ, Mode: e.mode, Size: int64(len(e.body)),
			Uid: e.uid, Gid: e.gid, ModTime: time.Unix(1700000000, 0)}
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
	return buf.Bytes()
}

// owners registra os chown pedidos (sem root, o de verdade falharia).
type owners map[string][2]int

// chown guarda o dono pedido para o caminho, no lugar do os.Lchown.
func (o owners) chown(p string, uid, gid int) error { o[p] = [2]int{uid, gid}; return nil }

// TestUntar trava o que o Android precisa da extração: setuid preservado, links, dono por entrada, dispositivo pulado
// e a entrada da raiz ignorada.
func TestUntar(t *testing.T) {
	root := t.TempDir()
	own := owners{}
	data := mkTar(t,
		entry{name: "./", typ: tar.TypeDir, mode: 0o755},
		entry{name: "system/bin/", typ: tar.TypeDir, mode: 0o755},
		entry{name: "system/bin/init", typ: tar.TypeReg, mode: 0o750, body: "ELF", uid: 0, gid: 2000},
		entry{name: "system/bin/su", typ: tar.TypeReg, mode: 0o4750, body: "su"},
		entry{name: "system/bin/run-as", typ: tar.TypeReg, mode: 0o2750, body: "ra"},
		entry{name: "data/", typ: tar.TypeDir, mode: 0o1771, uid: 1000, gid: 1000},
		entry{name: "init", typ: tar.TypeSymlink, link: "/system/bin/init"},
		entry{name: "system/bin/init2", typ: tar.TypeLink, link: "system/bin/init"},
		entry{name: "dev/null", typ: tar.TypeChar, mode: 0o666},
	)
	if err := Untar(bytes.NewReader(data), root, own.chown); err != nil {
		t.Fatal(err)
	}
	real, _ := filepath.EvalSymlinks(root)
	fi, err := os.Stat(filepath.Join(root, "system/bin/su"))
	if err != nil || fi.Mode()&os.ModeSetuid == 0 {
		t.Fatalf("setuid perdido: %v %v", fi.Mode(), err)
	}
	if fi, _ := os.Stat(filepath.Join(root, "system/bin/run-as")); fi.Mode()&os.ModeSetgid == 0 {
		t.Fatal("setgid perdido")
	}
	if fi, _ := os.Stat(filepath.Join(root, "data")); fi.Mode()&os.ModeSticky == 0 {
		t.Fatal("sticky perdido")
	}
	if l, _ := os.Readlink(filepath.Join(root, "init")); l != "/system/bin/init" {
		t.Fatalf("symlink = %q", l)
	}
	if b, _ := os.ReadFile(filepath.Join(root, "system/bin/init2")); string(b) != "ELF" {
		t.Fatal("hard link sem o conteúdo")
	}
	if _, err := os.Lstat(filepath.Join(root, "dev/null")); err == nil {
		t.Fatal("dispositivo extraído")
	}
	if own[filepath.Join(real, "system/bin/init")] != [2]int{0, 2000} || own[filepath.Join(real, "data")] != [2]int{1000, 1000} {
		t.Fatalf("donos = %v", own)
	}
}

// TestUntarStaysInsideRoot: nem ".." nem um symlink que a imagem plantou num diretório pai levam a escrita para fora.
func TestUntarStaysInsideRoot(t *testing.T) {
	outside := t.TempDir()
	root := t.TempDir()
	dotdot := mkTar(t, entry{name: "../../etc/evilfile", typ: tar.TypeReg, mode: 0o644, body: "x"})
	if err := Untar(bytes.NewReader(dotdot), root, owners{}.chown); err != nil {
		t.Fatal(err) // ".." é cortado no Clean: a entrada cai dentro do rootfs
	}
	if _, err := os.Stat(filepath.Join(root, "etc/evilfile")); err != nil {
		t.Fatal(`".." não ficou dentro do rootfs`)
	}
	root2 := t.TempDir()
	viaLink := mkTar(t,
		entry{name: "esc", typ: tar.TypeSymlink, link: outside},
		entry{name: "esc/pwned", typ: tar.TypeReg, mode: 0o644, body: "x"})
	if err := Untar(bytes.NewReader(viaLink), root2, owners{}.chown); err == nil || !strings.Contains(err.Error(), "escapes") {
		t.Fatalf("symlink para fora aceito: %v", err)
	}
	if _, err := os.Stat(filepath.Join(outside, "pwned")); err == nil {
		t.Fatal("escreveu fora do rootfs")
	}
}

// TestUntarErrors cobre o tar quebrado, o chown recusado e a raiz que não existe.
func TestUntarErrors(t *testing.T) {
	if err := Untar(strings.NewReader("lixo que não é tar"+strings.Repeat(".", 600)), t.TempDir(), owners{}.chown); err == nil {
		t.Error("tar quebrado aceito")
	}
	data := mkTar(t, entry{name: "a", typ: tar.TypeReg, mode: 0o644, body: "x"})
	err := Untar(bytes.NewReader(data), t.TempDir(), func(string, int, int) error { return errors.New("EPERM") })
	if err == nil || !strings.Contains(err.Error(), "chown a") {
		t.Errorf("chown recusado: %v", err)
	}
	if err := Untar(bytes.NewReader(data), filepath.Join(t.TempDir(), "nao-existe"), owners{}.chown); err == nil {
		t.Error("raiz inexistente aceita")
	}
}

// imageOf monta uma imagem OCI de uma camada com o tar dado.
func imageOf(t *testing.T, layerTar []byte) v1.Image {
	t.Helper()
	layer, err := tarball.LayerFromOpener(func() (io.ReadCloser, error) { return io.NopCloser(bytes.NewReader(layerTar)), nil })
	if err != nil {
		t.Fatal(err)
	}
	img, err := mutate.AppendLayers(empty.Image, layer)
	if err != nil {
		t.Fatal(err)
	}
	return img
}

// goCopy é a cópia dos testes (o cp -a --reflink não existe no macOS): conteúdo e modo, recursivo.
func goCopy(src, dst string) error {
	return filepath.Walk(src, func(p string, fi os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		rel, _ := filepath.Rel(src, p)
		to := filepath.Join(dst, rel)
		switch {
		case fi.IsDir():
			return os.MkdirAll(to, fi.Mode().Perm()|0o700)
		case fi.Mode()&os.ModeSymlink != 0:
			l, _ := os.Readlink(p)
			return os.Symlink(l, to)
		}
		b, err := os.ReadFile(p)
		if err != nil {
			return err
		}
		return os.WriteFile(to, b, fi.Mode().Perm())
	})
}

// testNet é o enlace do gateway do rc nos testes.
var testNet = Net{Guest: netip.MustParsePrefix("100.96.0.2/30"), Gateway: netip.MustParseAddr("100.96.0.1"), ADBPort: 5555}

// TestPrepare: a primeira máquina baixa e extrai; a segunda reaproveita o cache (busca só o digest); a mesma
// máquina de novo não copia outra vez, mas regrava os ajustes.
func TestPrepare(t *testing.T) {
	img := imageOf(t, mkTar(t,
		entry{name: "system/bin/init", typ: tar.TypeReg, mode: 0o755, body: "ELF"},
		entry{name: "init", typ: tar.TypeSymlink, link: "/system/bin/init"}))
	var fetches, copies atomic.Int32
	r := &Rootfs{CacheDir: t.TempDir(), Arch: "amd64",
		Fetch: func(_ context.Context, ref, arch string) (v1.Image, error) {
			fetches.Add(1)
			if ref != "redroid/redroid:14" || arch != "amd64" {
				t.Errorf("fetch %s %s", ref, arch)
			}
			return img, nil
		},
		Copy:  func(s, d string) error { copies.Add(1); return goCopy(s, d) },
		Chown: owners{}.chown,
	}
	m1, m2 := t.TempDir(), t.TempDir()
	root, err := r.Prepare(context.Background(), "redroid/redroid:14", m1, testNet)
	if err != nil || root != filepath.Join(m1, rootfsName) {
		t.Fatalf("Prepare = %q, %v", root, err)
	}
	if b, _ := os.ReadFile(filepath.Join(root, tweakRC)); !strings.Contains(string(b), "service.adb.listen_addrs tcp:5555") {
		t.Fatalf("ajuste do adb ausente: %s", b)
	}
	if _, err := r.Prepare(context.Background(), "redroid/redroid:14", m2, testNet); err != nil {
		t.Fatal(err)
	}
	os.Remove(filepath.Join(root, tweakRC))
	if _, err := r.Prepare(context.Background(), "redroid/redroid:14", m1, testNet); err != nil {
		t.Fatal(err)
	}
	if fetches.Load() != 2 || copies.Load() != 2 {
		t.Fatalf("fetches=%d copies=%d; want 2 e 2 (a 2ª máquina busca o digest, o cache não reextrai)", fetches.Load(), copies.Load())
	}
	if _, err := os.Stat(filepath.Join(root, tweakRC)); err != nil {
		t.Fatal("ajuste apagado não foi regravado no start seguinte")
	}
}

// TestPrepareFailures: imagem que não é Android, busca que falha, cópia que falha.
func TestPrepareFailures(t *testing.T) {
	notAndroid := imageOf(t, mkTar(t, entry{name: "bin/sh", typ: tar.TypeReg, mode: 0o755, body: "x"}))
	r := &Rootfs{CacheDir: t.TempDir(), Chown: owners{}.chown, Copy: goCopy,
		Fetch: func(context.Context, string, string) (v1.Image, error) { return notAndroid, nil }}
	if _, err := r.Prepare(context.Background(), "alpine", t.TempDir(), testNet); err == nil || !strings.Contains(err.Error(), "not an Android") {
		t.Errorf("imagem comum aceita: %v", err)
	}
	r.Fetch = func(context.Context, string, string) (v1.Image, error) { return nil, errors.New("401") }
	if _, err := r.Prepare(context.Background(), "x", t.TempDir(), testNet); err == nil || !strings.Contains(err.Error(), "fetch x: 401") {
		t.Errorf("fetch: %v", err)
	}
	android := imageOf(t, mkTar(t, entry{name: "system/bin/init", typ: tar.TypeReg, mode: 0o755, body: "ELF"}))
	r.Fetch = func(context.Context, string, string) (v1.Image, error) { return android, nil }
	r.Copy = func(string, string) error { return errors.New("disco cheio") }
	if _, err := r.Prepare(context.Background(), "x", t.TempDir(), testNet); err == nil || !strings.Contains(err.Error(), "disco cheio") {
		t.Errorf("copy: %v", err)
	}
}

// TestTweak trava o conteúdo dos ajustes (cada um corrige um defeito medido na prova) e o modo, inclusive sobre um
// arquivo que já existia com outro modo.
func TestTweak(t *testing.T) {
	root := t.TempDir()
	os.MkdirAll(filepath.Join(root, "system/bin"), 0o755)
	os.WriteFile(filepath.Join(root, tweakNet), []byte("velho"), 0o600)
	if err := Tweak(root, testNet); err != nil {
		t.Fatal(err)
	}
	net, _ := os.ReadFile(filepath.Join(root, tweakNet))
	for _, want := range []string{"ip addr add 100.96.0.2/30 dev eth0", "ip link set eth0 up", "default via 100.96.0.1 dev eth0"} {
		if !strings.Contains(string(net), want) {
			t.Errorf("rede sem %q:\n%s", want, net)
		}
	}
	// A eth0 fica com o nome: renomeada, o ipconfigstore do Redroid não a acha e o Android nasce "sem rede".
	if strings.Contains(string(net), "name vnet0") {
		t.Errorf("a eth0 voltou a ser renomeada:\n%s", net)
	}
	routes, _ := os.ReadFile(filepath.Join(root, tweakRoutes))
	if !strings.Contains(string(routes), "ip rule add pref 31000 from all lookup main") {
		t.Errorf("rota de volta ausente:\n%s", routes)
	}
	rc, _ := os.ReadFile(filepath.Join(root, tweakRC))
	for _, want := range []string{"on early-init", "setprop service.adb.listen_addrs tcp:5555", "on post-fs-data", "/" + tweakNet, "sys.boot_completed=1", "/" + tweakRoutes} {
		if !strings.Contains(string(rc), want) {
			t.Errorf("rc sem %q:\n%s", want, rc)
		}
	}
	if fi, _ := os.Stat(filepath.Join(root, tweakNet)); fi.Mode().Perm() != 0o755 {
		t.Errorf("modo do script = %v", fi.Mode())
	}
	if err := Tweak(root, Net{}); err == nil {
		t.Error("enlace vazio aceito")
	}
}

// badDigest é uma imagem cujo digest falha (manifesto ilegível no registry).
type badDigest struct{ v1.Image }

// Digest devolve o erro do manifesto.
func (badDigest) Digest() (v1.Hash, error) { return v1.Hash{}, errors.New("manifest unknown") }

// TestUntarCollisions: cada entrada que colide com o que já foi extraído (arquivo onde há diretório cheio, symlink
// por cima de diretório cheio, hard link para alvo que não existe, diretório onde há arquivo) volta com erro em vez
// de deixar o rootfs pela metade calado.
func TestUntarCollisions(t *testing.T) {
	fill := entry{name: "d/x", typ: tar.TypeReg, mode: 0o644, body: "x"}
	cases := map[string][]entry{
		"arquivo sobre diretório": {fill, {name: "d", typ: tar.TypeReg, mode: 0o644, body: "y"}},
		"symlink sobre diretório": {fill, {name: "d", typ: tar.TypeSymlink, link: "/x"}},
		"link para o vazio":       {{name: "l", typ: tar.TypeLink, link: "nao/existe"}},
		"diretório sobre arquivo": {{name: "f", typ: tar.TypeReg, mode: 0o644, body: "y"}, {name: "f/", typ: tar.TypeDir, mode: 0o755}},
	}
	for name, es := range cases {
		if err := Untar(bytes.NewReader(mkTar(t, es...)), t.TempDir(), owners{}.chown); err == nil {
			t.Errorf("%s: aceito", name)
		}
	}
}

// TestPrepareDigestAndDefaultCopy: digest ilegível para a preparação, e sem Copy o Prepare usa o cp -a de produção
// (no macOS ele falha pelo --reflink, e o erro chega com o contexto).
func TestPrepareDigestAndDefaultCopy(t *testing.T) {
	android := imageOf(t, mkTar(t, entry{name: "system/bin/init", typ: tar.TypeReg, mode: 0o755, body: "ELF"}))
	r := &Rootfs{CacheDir: t.TempDir(), Chown: owners{}.chown,
		Fetch: func(context.Context, string, string) (v1.Image, error) { return badDigest{android}, nil }}
	if _, err := r.Prepare(context.Background(), "x", t.TempDir(), testNet); err == nil || !strings.Contains(err.Error(), "digest of x") {
		t.Errorf("digest: %v", err)
	}
	r.Fetch = func(context.Context, string, string) (v1.Image, error) { return android, nil }
	if root, err := r.Prepare(context.Background(), "x", t.TempDir(), testNet); err != nil && !strings.Contains(err.Error(), "copy rootfs") {
		t.Errorf("cp padrão: %q, %v", root, err)
	}
}

// TestDiskErrorPaths cobre o que falha no disco: cache que não dá para criar, ajuste que não dá para gravar, e o
// cp -a de produção (no macOS o --reflink não existe e a cópia volta com o erro do cp; no Linux, copia).
func TestDiskErrorPaths(t *testing.T) {
	android := imageOf(t, mkTar(t, entry{name: "system/bin/init", typ: tar.TypeReg, mode: 0o755, body: "ELF"}))
	blocker := filepath.Join(t.TempDir(), "arquivo")
	os.WriteFile(blocker, []byte("x"), 0o600)
	r := &Rootfs{CacheDir: blocker, Chown: owners{}.chown, Copy: goCopy,
		Fetch: func(context.Context, string, string) (v1.Image, error) { return android, nil }}
	if _, err := r.Prepare(context.Background(), "x", t.TempDir(), testNet); err == nil || !strings.Contains(err.Error(), "cache") {
		t.Errorf("cache num arquivo: %v", err)
	}
	if err := Tweak(blocker, testNet); err == nil {
		t.Error("ajuste gravado dentro de um arquivo")
	}
	src := t.TempDir()
	os.WriteFile(filepath.Join(src, "a"), []byte("x"), 0o644)
	dst := filepath.Join(t.TempDir(), "copia")
	if err := cpArchive(src, dst); err == nil {
		if b, _ := os.ReadFile(filepath.Join(dst, "a")); string(b) != "x" {
			t.Error("cp -a copiou errado")
		}
	} else if !strings.Contains(err.Error(), "exit status") {
		t.Errorf("erro do cp sem o status: %v", err)
	}
	if err := Untar(bytes.NewReader(mkTar(t, entry{name: "x/y", typ: tar.TypeReg, mode: 0o644, body: "z"})), blocker, owners{}.chown); err == nil {
		t.Error("extração dentro de um arquivo aceita")
	}
}
