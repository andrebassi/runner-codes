package app

import (
	"bytes"
	"compress/gzip"
	"context"
	"encoding/binary"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"syscall"
	"testing"
	"time"

	"runner.codes/internal/adapters/machinestore"
	"runner.codes/internal/domain"
)

// transferRig monta duas instalações (origem e destino) com o store de verdade em disco e uma
// máquina "dev" desligada na origem, com disco esparso (64 MiB declarados, 1 MiB gravado no meio).
func transferRig(t *testing.T) (*Machines, *Machines, string) {
	t.Helper()
	tpl := filepath.Join(t.TempDir(), "tpl.ext4")
	if err := os.WriteFile(tpl, nil, 0o600); err != nil {
		t.Fatal(err)
	}
	src := &Machines{Store: machinestore.New(filepath.Join(t.TempDir(), "machines")), Procs: &procs{alive: map[int]bool{}}}
	dst := &Machines{Store: machinestore.New(filepath.Join(t.TempDir(), "machines")), Procs: &procs{alive: map[int]bool{}}}
	mc := domain.Machine{Name: "dev", Stack: "python", CPUs: 2, MemoryMiB: 512, DiskGiB: 1, CreatedAt: time.Unix(1800000000, 0).UTC()}
	if err := src.Store.Create(mc, tpl); err != nil {
		t.Fatal(err)
	}
	dir := src.Store.Dir("dev")
	disk, err := os.OpenFile(filepath.Join(dir, "disk.ext4"), os.O_WRONLY, 0o600)
	if err != nil {
		t.Fatal(err)
	}
	if err := disk.Truncate(64 << 20); err != nil {
		t.Fatal(err)
	}
	if _, err := disk.WriteAt(bytes.Repeat([]byte("superbot"), 1<<17), 10<<20); err != nil {
		t.Fatal(err)
	}
	disk.Close()
	for name, body := range map[string]string{"cred-ca.pem": "CA", "vmm.pid": "42", "egress.sock": "", "console.log": "boot"} {
		if err := os.WriteFile(filepath.Join(dir, name), []byte(body), 0o600); err != nil {
			t.Fatal(err)
		}
	}
	return src, dst, dir
}

// TestTransferRoundTrip trava o que a mudança de servidor depende: a máquina chega igual (registro,
// credencial, disco byte a byte), o disco volta ESPARSO, e pid, socket e log ficam na origem.
func TestTransferRoundTrip(t *testing.T) {
	ctx := context.Background()
	src, dst, srcDir := transferRig(t)
	var buf bytes.Buffer
	if err := src.Export(ctx, "dev", &buf); err != nil {
		t.Fatal(err)
	}
	if buf.Len() > 1<<20 {
		t.Fatalf("exportação de %d bytes: os zeros não viraram bloco Z", buf.Len())
	}
	mc, err := dst.Import(ctx, &buf)
	if err != nil || mc.Name != "dev" || mc.Stack != "python" {
		t.Fatalf("importou %+v %v", mc, err)
	}
	dstDir := dst.Store.Dir("dev")
	for _, f := range []string{"disk.ext4", "cred-ca.pem", "machine.json"} {
		a, _ := os.ReadFile(filepath.Join(srcDir, f))
		b, err := os.ReadFile(filepath.Join(dstDir, f))
		if err != nil || !bytes.Equal(a, b) {
			t.Fatalf("%s diferente na chegada: %v", f, err)
		}
	}
	for _, f := range []string{"vmm.pid", "egress.sock", "console.log"} {
		if _, err := os.Stat(filepath.Join(dstDir, f)); err == nil {
			t.Errorf("%s viajou", f)
		}
	}
	info, _ := os.Stat(filepath.Join(dstDir, "disk.ext4"))
	if st, ok := info.Sys().(*syscall.Stat_t); ok && st.Blocks*512 >= info.Size() {
		t.Fatalf("disco chegou cheio: %d de %d bytes ocupados", st.Blocks*512, info.Size())
	}
	entries, _ := os.ReadDir(filepath.Dir(dstDir))
	for _, e := range entries {
		if strings.HasPrefix(e.Name(), ".import-") {
			t.Fatalf("temporário ficou para trás: %s", e.Name())
		}
	}
	// Importar de novo o mesmo nome é CONFLICT e não toca na que existe.
	var again bytes.Buffer
	_ = src.Export(ctx, "dev", &again)
	if _, err := dst.Import(ctx, &again); !isCode(err, domain.CodeConflict) {
		t.Fatalf("repetida: %v", err)
	}
}

// dirPersist é o disco próprio de mentira que cria o diretório de verdade (o import copia arquivos para dentro dele).
type dirPersist struct {
	existing            bool // o disco já existe (outra máquina com o mesmo nome)
	noDir               bool // o Ensure "monta" sem criar o diretório (a cópia falha)
	ensureErr           error
	released, destroyed int
}

// Ensure cria o diretório (o ponto de montagem) e diz se o disco nasceu agora.
func (d *dirPersist) Ensure(_ context.Context, _, dir string, _ int) (bool, error) {
	if d.ensureErr != nil {
		return false, d.ensureErr
	}
	if !d.noDir {
		os.MkdirAll(dir, 0o700)
	}
	return !d.existing, nil
}

// Mounted é sempre falso: o import não pergunta.
func (d *dirPersist) Mounted(string) bool { return false }

// Release conta os soltos.
func (d *dirPersist) Release(context.Context, string, string) error { d.released++; return nil }

// Destroy conta os apagados e tira o diretório.
func (d *dirPersist) Destroy(_ context.Context, _, dir string) error {
	d.destroyed++
	return os.RemoveAll(dir)
}

// TestImportIntoPersistentDisk: no host com disco próprio, a máquina que chega por cópia nasce num disco só dela — os
// arquivos iguais byte a byte, o disco ESPARSO (os zeros não viram bloco gravado no disco de rede); disco que já existe
// é CONFLICT soltando o que adotou; cópia que falha apaga o disco novo; erro do disco sobe.
func TestImportIntoPersistentDisk(t *testing.T) {
	ctx := context.Background()
	src, dst, srcDir := transferRig(t)
	p := &dirPersist{}
	dst.Persist = p
	export := func() *bytes.Buffer {
		var buf bytes.Buffer
		if err := src.Export(ctx, "dev", &buf); err != nil {
			t.Fatal(err)
		}
		return &buf
	}
	mc, err := dst.Import(ctx, export())
	if err != nil || mc.Name != "dev" {
		t.Fatalf("importou %+v %v", mc, err)
	}
	dstDir := dst.Store.Dir("dev")
	for _, f := range []string{"disk.ext4", "cred-ca.pem", "machine.json"} {
		a, _ := os.ReadFile(filepath.Join(srcDir, f))
		b, err := os.ReadFile(filepath.Join(dstDir, f))
		if err != nil || !bytes.Equal(a, b) {
			t.Fatalf("%s diferente na chegada: %v", f, err)
		}
	}
	info, _ := os.Stat(filepath.Join(dstDir, "disk.ext4"))
	if st, ok := info.Sys().(*syscall.Stat_t); ok && st.Blocks*512 >= info.Size() {
		t.Fatalf("disco chegou cheio: %d de %d bytes ocupados", st.Blocks*512, info.Size())
	}
	os.RemoveAll(dstDir)
	p.existing = true
	if _, err := dst.Import(ctx, export()); !isCode(err, domain.CodeConflict) || p.released != 1 {
		t.Fatalf("disco existente: %v (soltos %d)", err, p.released)
	}
	os.RemoveAll(dstDir)
	p.existing, p.noDir = false, true
	if _, err := dst.Import(ctx, export()); err == nil || p.destroyed != 1 {
		t.Fatalf("cópia falhando: %v (apagados %d)", err, p.destroyed)
	}
	p.noDir, p.ensureErr = false, errors.New("quota")
	if _, err := dst.Import(ctx, export()); err == nil || !strings.Contains(err.Error(), "quota") {
		t.Fatalf("erro do disco: %v", err)
	}
}

// TestCopySparse: cópia com buraco no fim mantém o tamanho; origem e destino inválidos devolvem erro.
func TestCopySparse(t *testing.T) {
	dir := t.TempDir()
	src := filepath.Join(dir, "a")
	f, _ := os.Create(src)
	f.Write([]byte("x"))
	f.Truncate(5 << 20) // termina em buraco
	f.Close()
	dst := filepath.Join(dir, "b")
	if err := copySparse(src, dst); err != nil {
		t.Fatal(err)
	}
	if fi, _ := os.Stat(dst); fi.Size() != 5<<20 {
		t.Fatalf("tamanho %d", fi.Size())
	}
	if copySparse(filepath.Join(dir, "nada"), dst) == nil {
		t.Fatal("origem inexistente")
	}
	if copySparse(src, filepath.Join(dir, "nada", "b")) == nil {
		t.Fatal("destino impossível")
	}
	if copySparse(dir, filepath.Join(dir, "c")) == nil {
		t.Fatal("origem diretório")
	}
}

// isCode diz se o erro é a recusa com aquele código estável.
func isCode(err error, code string) bool {
	var me domain.MachineError
	return errors.As(err, &me) && me.Code == code
}

// TestExportRefuses: ligada é INVALID_STATE (o disco mudaria no meio); inexistente é NOT_FOUND;
// contexto cancelado para; escritor que falha devolve o erro.
func TestExportRefuses(t *testing.T) {
	ctx := context.Background()
	src, _, _ := transferRig(t)
	_ = src.Store.SetPID("dev", 7)
	src.Procs.(*procs).alive[7] = true
	if err := src.Export(ctx, "dev", &bytes.Buffer{}); !isCode(err, domain.CodeInvalidState) {
		t.Fatalf("ligada: %v", err)
	}
	src.Procs.(*procs).alive[7] = false
	if err := src.Export(ctx, "nada", &bytes.Buffer{}); !isCode(err, domain.CodeNotFound) {
		t.Fatalf("inexistente: %v", err)
	}
	cancelled, cancel := context.WithCancel(ctx)
	cancel()
	if err := src.Export(cancelled, "dev", &bytes.Buffer{}); !errors.Is(err, context.Canceled) {
		t.Fatalf("cancelado: %v", err)
	}
	if err := src.Export(ctx, "dev", failWriter{}); err == nil {
		t.Fatal("escritor quebrado sem erro")
	}
}

// failWriter é o destino da exportação que recusa toda escrita (conexão caída no meio).
type failWriter struct{}

// Write recusa sempre, como um socket fechado do outro lado.
func (failWriter) Write([]byte) (int, error) { return 0, errors.New("disco cheio") }

// stream monta uma exportação à mão (para os corpos defeituosos).
func stream(parts ...[]byte) *bytes.Buffer {
	var raw bytes.Buffer
	for _, p := range parts {
		raw.Write(p)
	}
	var out bytes.Buffer
	zw := gzip.NewWriter(&out)
	zw.Write(raw.Bytes())
	zw.Close()
	return &out
}

// entry monta o cabeçalho de um arquivo.
func entry(name string, size uint64) []byte {
	b := []byte{'F'}
	b = binary.BigEndian.AppendUint16(b, uint16(len(name)))
	b = append(b, name...)
	return binary.BigEndian.AppendUint64(b, size)
}

// block monta um bloco D (com os dados) ou Z (só o tamanho).
func block(kind byte, data []byte, n uint32) []byte {
	b := []byte{kind}
	b = binary.BigEndian.AppendUint32(b, n)
	if kind == 'D' {
		b = append(b, data...)
	}
	return b
}

// TestImportRefusesBadBodies: cada defeito do corpo é recusado, e nada fica no disco do destino.
func TestImportRefusesBadBodies(t *testing.T) {
	ctx := context.Background()
	_, dst, _ := transferRig(t)
	magic := []byte(transferMagic)
	record := func(name string) []byte {
		j := `{"name":"` + name + `","stack":"python"}`
		return append(entry("machine.json", uint64(len(j))), block('D', []byte(j), uint32(len(j)))...)
	}
	cases := map[string]*bytes.Buffer{
		"não é gzip":                bytes.NewBufferString("texto"),
		"sem cabeçalho":             stream([]byte("XXXX")),
		"cortado":                   stream(magic),
		"entrada desconhecida":      stream(magic, []byte{'Q'}),
		"nome com barra":            stream(magic, entry("../x", 0)),
		"nome com ponto":            stream(magic, entry(".x", 0)),
		"nome vazio":                stream(magic, entry("", 0)),
		"grande demais":             stream(magic, entry("a", 1<<62)),
		"bloco maior que o arquivo": stream(magic, entry("a", 1), block('Z', nil, 2)),
		"bloco desconhecido":        stream(magic, entry("a", 1), []byte{'X', 0, 0, 0, 1}),
		"bloco zerado":              stream(magic, entry("a", 1), block('Z', nil, 0)),
		"dado cortado":              stream(magic, entry("a", 4), []byte{'D', 0, 0, 0, 4, 'x'}),
		"arquivo repetido":          stream(magic, entry("a", 0), entry("a", 0), []byte{'E'}),
		"sem machine.json":          stream(magic, entry("a", 0), []byte{'E'}),
		"registro inválido":         stream(magic, entry("machine.json", 1), block('D', []byte("{"), 1), []byte{'E'}),
		"nome inválido":             stream(magic, record("Inválido"), []byte{'E'}),
		"cabeçalho cortado":         stream(magic, []byte{'F', 0}),
		"nome cortado":              stream(magic, []byte{'F', 0, 5, 'a'}),
		"tamanho cortado":           stream(magic, []byte{'F', 0, 1, 'a', 0}),
		"bloco cortado":             stream(magic, entry("a", 1), []byte{'D', 0}),
	}
	for name, body := range cases {
		if _, err := dst.Import(ctx, body); err == nil {
			t.Errorf("%s: aceito", name)
		}
	}
	entries, _ := os.ReadDir(filepath.Dir(dst.Store.Dir("x")))
	if len(entries) != 0 {
		t.Fatalf("sobrou coisa no disco: %v", entries)
	}
	// Registro certo, arquivo terminando em buraco: o tamanho fecha pelo Truncate.
	ok := stream(magic, entry("disk.ext4", 2<<20), block('Z', nil, 1<<20), block('D', []byte("x"), 1), block('Z', nil, 1<<20-1), record("novo"), []byte{'E'})
	mc, err := dst.Import(ctx, ok)
	if err != nil || mc.Name != "novo" {
		t.Fatalf("válida: %+v %v", mc, err)
	}
	if info, _ := os.Stat(filepath.Join(dst.Store.Dir("novo"), "disk.ext4")); info.Size() != 2<<20 {
		t.Fatalf("tamanho: %d", info.Size())
	}
	cancelled, cancel := context.WithCancel(ctx)
	cancel()
	if _, err := dst.Import(cancelled, stream(magic, []byte{'E'})); !errors.Is(err, context.Canceled) {
		t.Fatalf("cancelado: %v", err)
	}
}
