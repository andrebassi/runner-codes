package s3fs

import (
	"context"
	"errors"
	"io"
	"sort"
	"strings"
	"sync"
	"syscall"
	"testing"
	"time"

	"runner.codes/internal/objectstore"
)

// memBucket é um bucket em memória; fail[op] faz a operação falhar (EIO no FS).
type memBucket struct {
	mu    sync.Mutex
	objs  map[string]string
	fail  map[string]error
	calls []string
}

// newBucket cria o bucket com os objetos dados.
func newBucket(objs map[string]string) *memBucket {
	return &memBucket{objs: objs, fail: map[string]error{}}
}

// failed registra a chamada e devolve o erro injetado para op, se houver.
func (b *memBucket) failed(op string) error {
	b.calls = append(b.calls, op)
	return b.fail[op]
}

// List agrupa no "/" como o ListObjectsV2.
func (b *memBucket) List(_ context.Context, prefix string) ([]objectstore.Entry, error) {
	b.mu.Lock()
	defer b.mu.Unlock()
	if err := b.failed("list"); err != nil {
		return nil, err
	}
	seen := map[string]bool{}
	var out []objectstore.Entry
	for k, v := range b.objs {
		if !strings.HasPrefix(k, prefix) {
			continue
		}
		rest := strings.TrimPrefix(k, prefix)
		if i := strings.Index(rest, "/"); i >= 0 && i < len(rest)-1 {
			if p := prefix + rest[:i+1]; !seen[p] {
				seen[p] = true
				out = append(out, objectstore.Entry{Key: p, IsDir: true})
			}
			continue
		}
		out = append(out, objectstore.Entry{Key: k, Size: int64(len(v)), ModTime: time.Unix(1, 0)})
	}
	sort.Slice(out, func(i, j int) bool { return out[i].Key < out[j].Key })
	return out, nil
}

// Head acha o objeto exato.
func (b *memBucket) Head(_ context.Context, key string) (objectstore.Entry, error) {
	b.mu.Lock()
	defer b.mu.Unlock()
	if err := b.failed("head"); err != nil {
		return objectstore.Entry{}, err
	}
	v, ok := b.objs[key]
	if !ok {
		return objectstore.Entry{}, &objectstore.Error{Status: 404, Code: "NotFound"}
	}
	return objectstore.Entry{Key: key, Size: int64(len(v)), ModTime: time.Unix(2, 0)}, nil
}

// Get lê a faixa; offset além do fim responde 416, como o S3.
func (b *memBucket) Get(_ context.Context, key string, off, n int64) (io.ReadCloser, error) {
	b.mu.Lock()
	defer b.mu.Unlock()
	if err := b.failed("get"); err != nil {
		return nil, err
	}
	v, ok := b.objs[key]
	if !ok {
		return nil, &objectstore.Error{Status: 404, Code: "NoSuchKey"}
	}
	if off > 0 && off >= int64(len(v)) {
		return nil, &objectstore.Error{Status: 416, Code: "InvalidRange"}
	}
	v = v[off:]
	if n > 0 && n < int64(len(v)) {
		v = v[:n]
	}
	return io.NopCloser(strings.NewReader(v)), nil
}

// Put grava o corpo inteiro.
func (b *memBucket) Put(_ context.Context, key string, body io.ReadSeeker) error {
	data, _ := io.ReadAll(body)
	b.mu.Lock()
	defer b.mu.Unlock()
	if err := b.failed("put"); err != nil {
		return err
	}
	b.objs[key] = string(data)
	return nil
}

// Delete apaga (ausente não é erro).
func (b *memBucket) Delete(_ context.Context, key string) error {
	b.mu.Lock()
	defer b.mu.Unlock()
	if err := b.failed("delete"); err != nil {
		return err
	}
	delete(b.objs, key)
	return nil
}

// Copy copia no "servidor".
func (b *memBucket) Copy(_ context.Context, src, dst string) error {
	b.mu.Lock()
	defer b.mu.Unlock()
	if err := b.failed("copy"); err != nil {
		return err
	}
	v, ok := b.objs[src]
	if !ok {
		return &objectstore.Error{Status: 404}
	}
	b.objs[dst] = v
	return nil
}

var ctx = context.Background()

// names devolve os nomes de uma listagem, com "/" no fim dos diretórios, em ordem.
func names(is []Info) string {
	var out []string
	for _, i := range is {
		n := i.Name
		if i.Dir {
			n += "/"
		}
		out = append(out, n)
	}
	sort.Strings(out)
	return strings.Join(out, " ")
}

// TestReadSide: o prefixo do volume é a raiz; arquivo, diretório (implícito ou por marcador)
// e ausente; listagem sem o marcador; leitura inteira, por faixa e além do fim.
func TestReadSide(t *testing.T) {
	b := newBucket(map[string]string{"data/a.txt": "0123456789", "data/sub/c.txt": "c", "data/empty/": "", "other": "x"})
	f := New(b, "data", false, t.TempDir())
	if fi, err := f.Stat(ctx, "/"); err != nil || !fi.Dir {
		t.Fatal("root is a directory")
	}
	if fi, err := f.Stat(ctx, "a.txt"); err != nil || fi.Dir || fi.Size != 10 || fi.Name != "a.txt" {
		t.Fatalf("file: %+v %v", fi, err)
	}
	for _, d := range []string{"sub", "empty"} {
		if fi, err := f.Stat(ctx, d); err != nil || !fi.Dir {
			t.Fatalf("%s must be a directory: %+v %v", d, fi, err)
		}
	}
	if _, err := f.Stat(ctx, "ghost"); err != syscall.ENOENT {
		t.Fatalf("missing: %v", err)
	}
	if got, _ := f.ReadDir(ctx, ""); names(got) != "a.txt empty/ sub/" {
		t.Fatalf("root listing: %q", names(got))
	}
	if got, _ := f.ReadDir(ctx, "empty"); len(got) != 0 {
		t.Fatalf("the directory marker must not list itself: %v", got)
	}
	buf := make([]byte, 4)
	if n, err := f.ReadAt(ctx, "a.txt", buf, 3); err != nil || string(buf[:n]) != "3456" {
		t.Fatalf("range: %q %v", buf[:n], err)
	}
	if n, err := f.ReadAt(ctx, "a.txt", buf, 8); err != nil || string(buf[:n]) != "89" {
		t.Fatalf("short read at the end: %q %v", buf[:n], err)
	}
	if n, err := f.ReadAt(ctx, "a.txt", buf, 50); err != nil || n != 0 {
		t.Fatalf("past the end is EOF: %d %v", n, err)
	}
	if _, err := f.ReadAt(ctx, "ghost", buf, 0); err != syscall.ENOENT {
		t.Fatalf("read missing: %v", err)
	}
	if New(b, "", true, "").ReadOnly() != true || New(b, "data/", false, "").Prefix() != "data/" {
		t.Fatal("prefix and read-only flag")
	}
}

// TestWriteSide: arquivo novo vazio sobe no release; escrita só sobe no flush e quem lê
// antes vê a cópia local; dois handles dividem a cópia e só o último descarta; abrir sem
// truncar baixa o conteúdo atual; truncate de arquivo fechado sobe na hora.
func TestWriteSide(t *testing.T) {
	b := newBucket(map[string]string{"old.txt": "antigo"})
	f := New(b, "", false, t.TempDir())
	if err := f.OpenWrite(ctx, "new.txt", false, true); err != nil {
		t.Fatal(err)
	}
	if got, _ := f.ReadDir(ctx, ""); names(got) != "new.txt old.txt" {
		t.Fatalf("a file being created lists before the upload: %q", names(got))
	}
	if err := f.Release(ctx, "new.txt"); err != nil {
		t.Fatal(err)
	}
	if v, ok := b.objs["new.txt"]; !ok || v != "" {
		t.Fatal("an empty new file must reach the bucket")
	}
	f.OpenWrite(ctx, "old.txt", false, false)
	f.OpenWrite(ctx, "old.txt", false, false) // segundo handle, mesma cópia
	f.WriteAt("old.txt", []byte("NOVO"), 0)
	if b.objs["old.txt"] != "antigo" {
		t.Fatal("nothing goes up before a flush")
	}
	buf := make([]byte, 10)
	if n, _ := f.ReadAt(ctx, "old.txt", buf, 0); string(buf[:n]) != "NOVOgo" {
		t.Fatalf("readers see pending writes: %q", buf[:n])
	}
	if fi, _ := f.Stat(ctx, "old.txt"); fi.Size != 6 {
		t.Fatalf("stat answers from the local copy: %+v", fi)
	}
	f.Release(ctx, "old.txt")
	if b.objs["old.txt"] != "antigo" {
		t.Fatal("the first release of two handles does not upload")
	}
	if err := f.Flush(ctx, "old.txt"); err != nil || b.objs["old.txt"] != "NOVOgo" {
		t.Fatalf("flush uploads: %v %q", err, b.objs["old.txt"])
	}
	puts := strings.Count(strings.Join(b.calls, " "), "put")
	f.Flush(ctx, "old.txt")
	f.Release(ctx, "old.txt")
	if strings.Count(strings.Join(b.calls, " "), "put") != puts {
		t.Fatal("a clean copy is not uploaded again")
	}
	if _, err := f.staged("old.txt"); err == nil {
		t.Fatal("the last release drops the local copy")
	}
	f.OpenWrite(ctx, "old.txt", true, false)
	f.OpenWrite(ctx, "old.txt", true, false) // truncar de novo com a cópia aberta
	f.WriteAt("old.txt", []byte("t"), 0)
	f.Release(ctx, "old.txt")
	f.Release(ctx, "old.txt")
	if b.objs["old.txt"] != "t" {
		t.Fatalf("O_TRUNC: %q", b.objs["old.txt"])
	}
	if err := f.Truncate(ctx, "old.txt", 0); err != nil || b.objs["old.txt"] != "" {
		t.Fatalf("truncate of a closed file: %v %q", err, b.objs["old.txt"])
	}
	b.objs["k.txt"] = "keep"
	if err := f.Truncate(ctx, "k.txt", 2); err != nil || b.objs["k.txt"] != "ke" {
		t.Fatalf("partial truncate keeps the head: %q", b.objs["k.txt"])
	}
	f.OpenWrite(ctx, "k.txt", false, false)
	if err := f.Truncate(ctx, "k.txt", 1); err != nil || b.objs["k.txt"] != "k" {
		t.Fatalf("truncate of an open file: %q", b.objs["k.txt"])
	}
	f.Release(ctx, "k.txt")
	if _, err := f.WriteAt("closed", []byte("x"), 0); err != syscall.EBADF {
		t.Fatalf("write without open: %v", err)
	}
	if f.Flush(ctx, "closed") != nil || f.Release(ctx, "closed") != nil {
		t.Fatal("flush and release of a read-only handle are no-ops")
	}
	if err := f.OpenWrite(ctx, "ghost", false, false); err != syscall.ENOENT {
		t.Fatalf("open for write without create: %v", err)
	}
}

// TestNamespace: mkdir grava o marcador, rmdir recusa diretório com filho e apaga o vazio,
// remove apaga o objeto, rename copia e só então apaga a origem (e leva a cópia local junto),
// diretório não renomeia.
func TestNamespace(t *testing.T) {
	b := newBucket(map[string]string{"d/x": "1", "a": "A"})
	f := New(b, "", false, t.TempDir())
	if err := f.Mkdir(ctx, "nova"); err != nil {
		t.Fatal(err)
	}
	if _, ok := b.objs["nova/"]; ok {
		t.Fatal("mkdir must not write a marker: some S3-compatible backends store it as a file")
	}
	if fi, _ := f.Stat(ctx, "nova"); !fi.Dir {
		t.Fatal("an empty directory exists in the mount")
	}
	if got, _ := f.ReadDir(ctx, ""); !strings.Contains(names(got), "nova/") {
		t.Fatalf("the new directory lists: %q", names(got))
	}
	if err := f.Mkdir(ctx, "nova"); err != syscall.EEXIST {
		t.Fatalf("mkdir twice: %v", err)
	}
	if err := f.Mkdir(ctx, "d"); err != syscall.EEXIST {
		t.Fatalf("mkdir over a bucket directory: %v", err)
	}
	f.Mkdir(ctx, "nova/funda")
	if got, _ := f.ReadDir(ctx, "nova"); names(got) != "funda/" {
		t.Fatalf("nested mkdir: %q", names(got))
	}
	f.Rmdir(ctx, "nova/funda")
	if err := f.Rmdir(ctx, "d"); err != syscall.ENOTEMPTY {
		t.Fatalf("rmdir with a child: %v", err)
	}
	if err := f.Rmdir(ctx, "nova"); err != nil {
		t.Fatal(err)
	}
	if _, ok := b.objs["nova/"]; ok {
		t.Fatal("rmdir removes the marker")
	}
	if err := f.Remove(ctx, "d/x"); err != nil || b.objs["d/x"] != "" {
		t.Fatal("remove")
	}
	f.OpenWrite(ctx, "a", false, false)
	f.WriteAt("a", []byte("B"), 0)
	if err := f.Rename(ctx, "a", "b"); err != nil || b.objs["b"] != "B" {
		t.Fatalf("rename uploads pending writes first: %v %q", err, b.objs["b"])
	}
	if _, ok := b.objs["a"]; ok {
		t.Fatal("rename deletes the source")
	}
	if _, err := f.staged("b"); err != nil {
		t.Fatal("the open handle follows the rename")
	}
	f.Release(ctx, "b")
	b.objs["d/y"] = "y"
	if err := f.Rename(ctx, "d", "e"); err != syscall.ENOTSUP {
		t.Fatalf("directory rename: %v", err)
	}
	if err := f.Rename(ctx, "ghost", "x"); err != syscall.ENOENT {
		t.Fatalf("rename missing: %v", err)
	}
}

// TestReadOnly: no :ro, toda escrita é EROFS sem tocar no bucket.
func TestReadOnly(t *testing.T) {
	b := newBucket(map[string]string{"a": "1"})
	f := New(b, "", true, t.TempDir())
	for name, err := range map[string]error{
		"open":   f.OpenWrite(ctx, "a", false, false),
		"remove": f.Remove(ctx, "a"),
		"mkdir":  f.Mkdir(ctx, "d"),
		"rmdir":  f.Rmdir(ctx, "d"),
		"rename": f.Rename(ctx, "a", "b"),
	} {
		if err != syscall.EROFS {
			t.Fatalf("%s: %v", name, err)
		}
	}
	if len(b.calls) != 0 {
		t.Fatalf("read-only must not reach the bucket: %v", b.calls)
	}
}

// TestRemoteFailures: cada falha do bucket vira EIO (ou ENOENT quando é ausência), e o
// rename que falha na cópia não apaga a origem; envio que falha mantém a cópia suja.
func TestRemoteFailures(t *testing.T) {
	boom := errors.New("network down")
	b := newBucket(map[string]string{"a": "1", "d/x": "2"})
	f := New(b, "", false, t.TempDir())
	check := func(op string, run func() error, want error) {
		t.Helper()
		b.fail = map[string]error{op: boom}
		if err := run(); err != want {
			t.Fatalf("%s failing: %v, want %v", op, err, want)
		}
		b.fail = map[string]error{}
	}
	check("head", func() error { _, err := f.Stat(ctx, "a"); return err }, syscall.EIO)
	check("list", func() error { _, err := f.Stat(ctx, "zz"); return err }, syscall.EIO)
	check("list", func() error { _, err := f.ReadDir(ctx, ""); return err }, syscall.EIO)
	check("list", func() error { return f.Rmdir(ctx, "d") }, syscall.EIO)
	check("get", func() error { _, err := f.ReadAt(ctx, "a", make([]byte, 1), 0); return err }, syscall.EIO)
	check("get", func() error { return f.OpenWrite(ctx, "a", false, false) }, syscall.EIO)
	check("delete", func() error { return f.Remove(ctx, "a") }, syscall.EIO)
	check("head", func() error { return f.Mkdir(ctx, "m") }, syscall.EIO)
	check("copy", func() error { return f.Rename(ctx, "a", "b") }, syscall.EIO)
	if b.objs["a"] != "1" {
		t.Fatal("a failed copy must keep the source")
	}
	check("delete", func() error { return f.Rename(ctx, "a", "b") }, syscall.EIO)
	b.objs["e/"] = ""
	check("delete", func() error { return f.Rmdir(ctx, "e") }, syscall.EIO)
	f.OpenWrite(ctx, "w", false, true)
	f.WriteAt("w", []byte("x"), 0)
	check("put", func() error { return f.Flush(ctx, "w") }, syscall.EIO)
	if err := f.Flush(ctx, "w"); err != nil || b.objs["w"] != "x" {
		t.Fatal("a failed upload keeps the copy dirty for the next flush")
	}
	f.Release(ctx, "w")
	check("put", func() error { return f.Truncate(ctx, "a", 0) }, syscall.EIO)
	check("get", func() error { return f.Truncate(ctx, "a", 1) }, syscall.EIO)
	check("put", func() error { return f.Rename(ctx, "a", "b") }, nil)
	bad := New(b, "", false, "/nonexistent/dir")
	if err := bad.OpenWrite(ctx, "a", false, false); err != syscall.EIO {
		t.Fatalf("an unusable stage dir: %v", err)
	}
}
