package pack

import (
	"bytes"
	"compress/gzip"
	"encoding/binary"
	"encoding/json"
	"io"
	"os"
	"path/filepath"
	"sort"
	"strings"
	"syscall"
	"testing"
	"time"

	"runner.codes/internal/domain"
)

// sparseFile cria um arquivo de size bytes com dado só em alguns pontos (o resto é buraco).
func sparseFile(t *testing.T, dir, name string, size int64, marks map[int64]string) string {
	t.Helper()
	p := filepath.Join(dir, name)
	f, _ := os.Create(p)
	f.Truncate(size)
	for off, s := range marks {
		f.WriteAt([]byte(s), off)
	}
	f.Close()
	return p
}

// allocated é quanto o arquivo ocupa de fato no disco (blocos de 512 bytes).
func allocated(t *testing.T, p string) int64 {
	var st syscall.Stat_t
	if err := syscall.Stat(p, &st); err != nil {
		t.Fatal(err)
	}
	return st.Blocks * 512
}

// TestRoundTrip: um disco de 1 GiB com três marcas vira um payload pequeno (só os extents),
// e volta com o mesmo tamanho lógico, as marcas no lugar e o resto como buraco; o manifesto
// chega igual.
func TestRoundTrip(t *testing.T) {
	src := t.TempDir()
	disk := sparseFile(t, src, "data.ext4", 1<<30, map[int64]string{0: "inicio", 512 << 20: "meio", 1<<30 - 3: "fim"})
	root := sparseFile(t, src, "root.ext4", 4096, map[int64]string{0: strings.Repeat("r", 4096)})
	m := Manifest{Name: "app", Source: "stack:python", Machine: domain.Machine{Name: "app", Stack: "python", CPUs: 2, Env: []string{"A=1"}}}
	var buf bytes.Buffer
	if err := Write(&buf, m, []string{root, disk}); err != nil {
		t.Fatal(err)
	}
	if buf.Len() > 1<<20 {
		t.Fatalf("a 1 GiB sparse disk must pack small, got %d bytes", buf.Len())
	}
	out := t.TempDir()
	got, err := Extract(bytes.NewReader(buf.Bytes()), out)
	if err != nil {
		t.Fatal(err)
	}
	if got.Version != Version || got.Name != "app" || got.Machine.CPUs != 2 || got.Machine.Env[0] != "A=1" || len(got.Files) != 2 {
		t.Fatalf("manifest: %+v", got)
	}
	f, _ := os.Open(filepath.Join(out, "data.ext4"))
	defer f.Close()
	fi, _ := f.Stat()
	if fi.Size() != 1<<30 {
		t.Fatalf("logical size: %d", fi.Size())
	}
	for off, want := range map[int64]string{0: "inicio", 512 << 20: "meio", 1<<30 - 3: "fim", 100 << 20: "\x00\x00\x00"} {
		b := make([]byte, len(want))
		f.ReadAt(b, off)
		if string(b) != want {
			t.Fatalf("at %d: %q", off, b)
		}
	}
	if a := allocated(t, filepath.Join(out, "data.ext4")); a > 64<<20 {
		t.Fatalf("the extracted disk must stay sparse, allocated %d", a)
	}
	if b, _ := os.ReadFile(filepath.Join(out, "root.ext4")); string(b) != strings.Repeat("r", 4096) {
		t.Fatal("dense file")
	}
	empty := sparseFile(t, src, "empty", 1<<20, nil)
	if ext, err := extents(empty, 1<<20); err != nil || len(ext) != 0 {
		t.Fatalf("an all-hole file has no extents: %v %v", ext, err)
	}
}

// TestSingleFile: o payload anexado ao stub com trailer é achado; o sidecar vence; o rc comum
// (sem nada) não é pack; trailer que aponta para fora é erro.
func TestSingleFile(t *testing.T) {
	dir := t.TempDir()
	stub := filepath.Join(dir, "app")
	os.WriteFile(stub, []byte("#!stub binary bytes"), 0o755)
	if _, ok, err := Locate(stub); ok || err != nil {
		t.Fatalf("a plain binary is not a pack: %v %v", ok, err)
	}
	// Maior que o buffer de leitura: com poucos bytes o fim do gzip cai numa leitura que
	// ninguém faz, e o defeito do leitor multistream não aparece (medido com 10 bytes).
	content := strings.Repeat("0123456789", 50_000)
	root := sparseFile(t, dir, "root.ext4", int64(len(content)), map[int64]string{0: content})
	var buf bytes.Buffer
	Write(&buf, Manifest{Name: "one"}, []string{root})
	if err := AppendTo(stub, bytes.NewReader(buf.Bytes())); err != nil {
		t.Fatal(err)
	}
	f, ok, err := Locate(stub)
	if err != nil || !ok {
		t.Fatalf("locate appended: %v %v", ok, err)
	}
	m, _, err := ReadManifest(f)
	f.Close()
	if err != nil || m.Name != "one" {
		t.Fatalf("manifest from the single file: %+v %v", m, err)
	}
	if b, _ := os.ReadFile(stub); !bytes.HasPrefix(b, []byte("#!stub binary bytes")) {
		t.Fatal("the stub must stay intact at the start")
	}
	// Extração inteira do arquivo único: o trailer vem depois do gzip e não pode ser lido
	// como outro membro (era o "gzip: invalid header" no último arquivo).
	f, _, _ = Locate(stub)
	out := t.TempDir()
	if _, err := Extract(f, out); err != nil {
		t.Fatalf("extract from the single file: %v", err)
	}
	f.Close()
	if b, _ := os.ReadFile(filepath.Join(out, "root.ext4")); string(b) != content {
		t.Fatalf("content from the single file: %d bytes", len(b))
	}
	os.WriteFile(stub+SidecarExt, buf.Bytes(), 0o644)
	f, ok, _ = Locate(stub)
	if fi, _ := f.Stat(); !ok || fi.Name() != "app"+SidecarExt {
		t.Fatal("the sidecar wins")
	}
	f.Close()
	bad := filepath.Join(dir, "bad")
	var tr [16]byte
	binary.BigEndian.PutUint64(tr[:8], 1<<40)
	copy(tr[8:], trailerMagic)
	os.WriteFile(bad, append([]byte("xxxxxxxxxxxxxxxxxxxx"), tr[:]...), 0o755)
	if _, _, err := Locate(bad); err == nil {
		t.Fatal("a trailer outside the file must fail")
	}
	if _, _, err := Locate(filepath.Join(dir, "missing")); err == nil {
		t.Fatal("missing executable")
	}
	tiny := filepath.Join(dir, "tiny")
	os.WriteFile(tiny, []byte("x"), 0o755)
	if _, ok, _ := Locate(tiny); ok {
		t.Fatal("a file shorter than the trailer is not a pack")
	}
	if err := AppendTo(filepath.Join(dir, "nope", "x"), &buf); err == nil {
		t.Fatal("append to a missing stub")
	}
}

// gzPayload monta um payload com manifesto cru (para os casos hostis).
func gzPayload(manifest []byte, data []byte) []byte {
	var b bytes.Buffer
	b.WriteString(Magic)
	zw := gzip.NewWriter(&b)
	var n [4]byte
	binary.BigEndian.PutUint32(n[:], uint32(len(manifest)))
	zw.Write(n[:])
	zw.Write(manifest)
	zw.Write(data)
	zw.Close()
	return b.Bytes()
}

// TestHostilePayloads: o payload vem de fora — nome com barra ou "..", extent fora do
// arquivo, dado cortado, manifesto gigante, versão futura, gzip ou JSON quebrados e o que não
// é pack são recusados com o motivo, sem escrever fora do diretório.
func TestHostilePayloads(t *testing.T) {
	file := func(name string, size int64, ext ...Extent) []byte {
		raw, _ := json.Marshal(Manifest{Version: 1, Files: []File{{Name: name, Size: size, Extents: ext}}})
		return raw
	}
	for name, payload := range map[string][]byte{
		"traversal": gzPayload(file("../atacante", 1, Extent{0, 1}), []byte("x")),
		"slash":     gzPayload(file("a/b", 1, Extent{0, 1}), []byte("x")),
		"dotdot":    gzPayload(file("..", 1), nil),
		"outside":   gzPayload(file("a", 1, Extent{0, 5}), []byte("xxxxx")),
		"negative":  gzPayload(file("a", 1, Extent{-1, 1}), []byte("x")),
		"cut":       gzPayload(file("a", 10, Extent{0, 10}), []byte("abc")),
		"future":    gzPayload([]byte(`{"version":99}`), nil),
		"bad json":  gzPayload([]byte(`{`), nil),
		"not pack":  []byte("hello world"),
		"bad gzip":  []byte(Magic + "not gzip"),
	} {
		dir := t.TempDir()
		if _, err := Extract(bytes.NewReader(payload), filepath.Join(dir, "out")); err == nil {
			t.Fatalf("%s must be refused", name)
		}
		if _, err := os.Stat(filepath.Join(dir, "atacante")); err == nil {
			t.Fatal("nothing may be written outside the target directory")
		}
	}
	var huge bytes.Buffer
	huge.WriteString(Magic)
	zw := gzip.NewWriter(&huge)
	zw.Write([]byte{0xff, 0xff, 0xff, 0xff})
	zw.Close()
	if _, _, err := ReadManifest(&huge); err == nil || !strings.Contains(err.Error(), "too large") {
		t.Fatalf("huge manifest: %v", err)
	}
	var short bytes.Buffer
	short.WriteString(Magic)
	zw = gzip.NewWriter(&short)
	zw.Write([]byte{0, 0, 0, 10, '{'})
	zw.Close()
	if _, _, err := ReadManifest(&short); err == nil {
		t.Fatal("a manifest shorter than declared")
	}
	var nolen bytes.Buffer
	nolen.WriteString(Magic)
	zw = gzip.NewWriter(&nolen)
	zw.Close()
	if _, _, err := ReadManifest(&nolen); err == nil {
		t.Fatal("no manifest length")
	}
	if err := Write(io.Discard, Manifest{}, []string{"/nonexistent"}); err == nil {
		t.Fatal("missing input file")
	}
	if _, err := Extract(bytes.NewReader(gzPayload(file("a", 1, Extent{0, 1}), []byte("x"))), "/dev/null/x"); err == nil {
		t.Fatal("an unusable target directory")
	}
}

// TestKey: o mesmo manifesto dá a mesma chave; mudar qualquer coisa (a data do pack, um
// extent) dá outra — dois packs nunca dividem o diretório do cache.
func TestKey(t *testing.T) {
	m := Manifest{Name: "a", Files: []File{{Name: "root.ext4", Size: 10, Extents: []Extent{{0, 10}}}}}
	k := Key(m)
	if len(k) != 12 || Key(m) != k {
		t.Fatalf("key: %q", k)
	}
	m.Files[0].Extents[0].Len = 9
	if Key(m) == k {
		t.Fatal("a different payload must have a different key")
	}
}

// TestPortable: diretório do host e credencial são recusados com o motivo; volume s3 viaja;
// rede nomeada e data de criação saem.
func TestPortable(t *testing.T) {
	mc := domain.Machine{Name: "a", Mounts: []domain.Mount{{Host: "s3://bkt", Guest: "/d"}},
		Network: domain.Network{Name: "lab", Address: "100.98.0.10", Enabled: true}, CreatedAt: time.Now()}
	got, err := Portable(mc)
	if err != nil || got.Network.Name != "" || got.Network.Address != "" || !got.Network.Enabled || !got.CreatedAt.IsZero() || len(got.Mounts) != 1 {
		t.Fatalf("portable: %+v %v", got, err)
	}
	mc.Mounts = append(mc.Mounts, domain.Mount{Host: "/home/x", Guest: "/x"})
	if _, err := Portable(mc); err == nil || !strings.Contains(err.Error(), "does not travel") {
		t.Fatalf("host directory: %v", err)
	}
	mc.Mounts = nil
	mc.Network.Credentials = []domain.Credential{{Name: "c"}}
	if _, err := Portable(mc); err == nil || !strings.Contains(err.Error(), "credentials") {
		t.Fatalf("credentials: %v", err)
	}
}

// TestRoute: cada subcomando do artefato vira o comando certo do rc, com o nome da máquina
// do pack; sem subcomando, o erro é o uso com o que o pack carrega.
func TestRoute(t *testing.T) {
	m := Manifest{Name: "app", Source: "stack:python", Platform: "linux/arm64"}
	for args, want := range map[string]string{
		"run -- python3 -V": "machine run --from /x/app -- python3 -V",
		"start":             "machine start --from /x/app --name app",
		"exec -- ls":        "machine exec --name app -- ls",
		"stop":              "machine stop --name app",
		"logs --tail 5":     "machine logs --name app --tail 5",
		"inspect":           "pack inspect /x/app",
	} {
		got, err := Route(m, "/x/app", strings.Fields(args))
		if err != nil || strings.Join(got, " ") != want {
			t.Fatalf("%s: %v %v", args, got, err)
		}
	}
	for _, args := range [][]string{nil, {"--help"}} {
		if _, err := Route(m, "/x/app", args); err == nil || !strings.Contains(err.Error(), "usage: app run") || !strings.Contains(err.Error(), "stack:python") {
			t.Fatalf("usage: %v", err)
		}
	}
}

// TestPrune: sem --all ficam as N usadas por último e a que uma máquina usa; com --all só a
// usada; sobra de extração interrompida sai sempre; raiz ausente não é erro.
func TestPrune(t *testing.T) {
	if r, err := Prune(filepath.Join(t.TempDir(), "none"), 1, nil, false); err != nil || r != nil {
		t.Fatalf("missing root: %v %v", r, err)
	}
	root := t.TempDir()
	base := time.Now().Add(-time.Hour)
	for i, k := range []string{"old", "mid", "new", "used"} {
		d := filepath.Join(root, k)
		os.MkdirAll(d, 0o755)
		os.Chtimes(d, base.Add(time.Duration(i)*time.Minute), base.Add(time.Duration(i)*time.Minute))
	}
	os.MkdirAll(filepath.Join(root, "half.tmp"), 0o755)
	os.WriteFile(filepath.Join(root, "file"), nil, 0o644)
	Touch(filepath.Join(root, "old")) // usado agora: vira o mais recente
	removed, err := Prune(root, 2, map[string]bool{"used": true}, false)
	if err != nil || strings.Join(removed, " ") != "half.tmp mid" {
		t.Fatalf("keep 2: %v %v", removed, err)
	}
	removed, _ = Prune(root, 2, map[string]bool{"used": true}, true)
	sort.Strings(removed)
	if strings.Join(removed, " ") != "new old" {
		t.Fatalf("all: %v", removed)
	}
	if _, err := os.Stat(filepath.Join(root, "used")); err != nil {
		t.Fatal("an extraction in use must stay")
	}
}

// failAfter aceita n bytes e falha depois (destino que some no meio da escrita).
type failAfter struct{ n int }

// Write falha quando o limite acaba.
func (f *failAfter) Write(p []byte) (int, error) {
	if f.n <= 0 {
		return 0, io.ErrClosedPipe
	}
	f.n -= len(p)
	return len(p), nil
}

// TestWriteFailures: destino que falha no magic e no meio dos dados; arquivo de saída que não
// dá para criar na extração; extents e cópia de arquivo que não abre; stub só de leitura.
func TestWriteFailures(t *testing.T) {
	dir := t.TempDir()
	big := sparseFile(t, dir, "big", 1<<20, map[int64]string{0: strings.Repeat("z", 1<<20)})
	for _, n := range []int{0, 64} {
		if err := Write(&failAfter{n: n}, Manifest{}, []string{big}); err == nil {
			t.Fatalf("a destination failing after %d bytes must surface", n)
		}
	}
	var buf bytes.Buffer
	Write(&buf, Manifest{}, []string{big})
	out := filepath.Join(dir, "out")
	os.MkdirAll(filepath.Join(out, "big"), 0o755) // um diretório no lugar do arquivo
	if _, err := Extract(bytes.NewReader(buf.Bytes()), out); err == nil {
		t.Fatal("an output that cannot be created must fail")
	}
	if _, err := extents(filepath.Join(dir, "missing"), 1); err == nil {
		t.Fatal("extents of a missing file")
	}
	if err := copyExtents(io.Discard, filepath.Join(dir, "missing"), nil); err == nil {
		t.Fatal("copy from a missing file")
	}
	ro := filepath.Join(dir, "ro")
	os.WriteFile(ro, []byte("x"), 0o444)
	if err := AppendTo(ro, &buf); err == nil {
		t.Fatal("append to a read-only stub")
	}
}
