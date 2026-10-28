package oci

import (
	"archive/tar"
	"context"
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"testing"
	"time"

	"github.com/google/go-containerregistry/pkg/name"
	"github.com/google/go-containerregistry/pkg/registry"
	v1 "github.com/google/go-containerregistry/pkg/v1"
	"github.com/google/go-containerregistry/pkg/v1/mutate"
	"github.com/google/go-containerregistry/pkg/v1/random"
	"github.com/google/go-containerregistry/pkg/v1/remote"

	"runner.codes/internal/domain"
	"runner.codes/internal/ports"
)

// fakeVM executa o comportamento escolhido na hora do Start e guarda o código.
type fakeVM struct {
	code   int
	done   chan struct{}
	killed chan struct{}
	once   sync.Once
}

// PID devolve um pid fictício.
func (v *fakeVM) PID() int { return 1 }

// Done fecha quando a VM falsa termina.
func (v *fakeVM) Done() <-chan struct{} { return v.done }

// Wait espera a VM falsa e devolve o código.
func (v *fakeVM) Wait() int { <-v.done; return v.code }

// Kill encerra a VM falsa (libera quem bloqueou esperando o kill).
func (v *fakeVM) Kill() { v.once.Do(func() { close(v.killed) }) }

// fakeLauncher registra as specs e delega o comportamento para handle.
type fakeLauncher struct {
	mu     sync.Mutex
	specs  []ports.VMSpec
	handle func(s ports.VMSpec, vm *fakeVM) int
	fail   error
}

// StartVM roda handle num goroutine, como uma VM que termina sozinha.
func (l *fakeLauncher) StartVM(s ports.VMSpec) (ports.VM, error) {
	if l.fail != nil {
		return nil, l.fail
	}
	l.mu.Lock()
	l.specs = append(l.specs, s)
	l.mu.Unlock()
	vm := &fakeVM{done: make(chan struct{}), killed: make(chan struct{})}
	go func() { vm.code = l.handle(s, vm); close(vm.done) }()
	return vm, nil
}

// calls conta as VMs lançadas.
func (l *fakeLauncher) calls() int { l.mu.Lock(); defer l.mu.Unlock(); return len(l.specs) }

// buildOK simula a VM de build bem-sucedida: grava o ext4 e a linha RESULT no console.
func buildOK(s ports.VMSpec, _ *fakeVM) int {
	if s.Exec == "/work/build.sh" {
		os.WriteFile(filepath.Join(s.RootDir, s.Args[1]), []byte("ext4"), 0o644)
		os.WriteFile(s.Console, []byte("log\nRESULT extract_ms=1 used_mib=2\n"), 0o644)
	}
	return 0
}

// minirootfs serve um tar.gz pequeno e devolve a Source correspondente (sha correto).
func minirootfs(t *testing.T) (Source, *httptest.Server) {
	raw := makeTarGz(t, []entry{{name: "etc/", typ: tar.TypeDir, mode: 0o755}, {name: "bin/sh", typ: tar.TypeReg, body: "x", mode: 0o755}})
	srv := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if r.URL.Path != "/mini.tar.gz" {
			http.NotFound(w, r)
			return
		}
		w.Write(raw)
	}))
	t.Cleanup(srv.Close)
	sum := sha256.Sum256(raw)
	return Source{URL: srv.URL + "/mini.tar.gz", SHA256: hex.EncodeToString(sum[:])}, srv
}

// newBuilder monta um Builder com dependências falsas e um guest qualquer.
func newBuilder(t *testing.T, l *fakeLauncher, img v1.Image) *Builder {
	src, _ := minirootfs(t)
	guest := filepath.Join(t.TempDir(), "guest")
	os.WriteFile(guest, []byte("guest-binary"), 0o755)
	return &Builder{
		CacheDir: t.TempDir(), Arch: "arm64", GuestBinary: guest, Launcher: l, Source: src,
		Fetch: func(context.Context, string, string) (v1.Image, error) { return img, nil },
		Log:   &strings.Builder{},
	}
}

// pyLang é a linguagem usada nos testes de build.
var pyLang = domain.Language{Name: "python", Image: "python:3.12-alpine", CPUs: 1, MemoryMiB: 512, Warmup: "true"}

// TestEnsureBuilderOnce: bootstrap com rede uma vez, marcador gravado, segunda chamada não sobe VM.
func TestEnsureBuilderOnce(t *testing.T) {
	l := &fakeLauncher{handle: buildOK}
	b := newBuilder(t, l, nil)
	if err := b.EnsureBuilder(context.Background()); err != nil {
		t.Fatal(err)
	}
	if l.calls() != 1 || !l.specs[0].Network || l.specs[0].Exec != "/work/bootstrap.sh" || l.specs[0].RootDir != b.builderRoot() {
		t.Fatalf("bootstrap with wrong spec: %+v", l.specs)
	}
	if b, _ := os.ReadFile(filepath.Join(b.builderRoot(), "etc", "resolv.conf")); !strings.Contains(string(b), "nameserver") {
		t.Fatal("build VM resolv.conf missing")
	}
	if err := b.EnsureBuilder(context.Background()); err != nil || l.calls() != 1 {
		t.Fatalf("second call should not boot a VM: err=%v calls=%d", err, l.calls())
	}
}

// TestEnsureBuilderRefuses cobre fonte ausente, sha errado, HTTP 404 e bootstrap que falha.
func TestEnsureBuilderRefuses(t *testing.T) {
	ctx := context.Background()
	b := newBuilder(t, &fakeLauncher{handle: buildOK}, nil)
	b.Source = Source{}
	if err := b.EnsureBuilder(ctx); err == nil {
		t.Fatal("no source should fail")
	}
	b = newBuilder(t, &fakeLauncher{handle: buildOK}, nil)
	b.Source.SHA256 = strings.Repeat("0", 64)
	if err := b.EnsureBuilder(ctx); err == nil || !strings.Contains(err.Error(), "does not match") {
		t.Fatalf("wrong sha should fail before extracting: %v", err)
	}
	if _, err := os.Stat(filepath.Join(b.builderRoot(), "bin", "sh")); !os.IsNotExist(err) {
		t.Fatal("nothing may be extracted with a wrong sha")
	}
	b = newBuilder(t, &fakeLauncher{handle: buildOK}, nil)
	b.Source.URL += "-404"
	if err := b.EnsureBuilder(ctx); err == nil {
		t.Fatal("HTTP 404 should fail")
	}
	b = newBuilder(t, &fakeLauncher{handle: buildOK}, nil)
	b.Source.URL = "http://127.0.0.1:1/x"
	if err := b.EnsureBuilder(ctx); err == nil {
		t.Fatal("connection refused should fail")
	}
	b = newBuilder(t, &fakeLauncher{handle: buildOK}, nil)
	b.Source.URL = "::invalid-url"
	if err := b.EnsureBuilder(ctx); err == nil {
		t.Fatal("invalid URL should fail")
	}
	fail := &fakeLauncher{handle: func(s ports.VMSpec, _ *fakeVM) int {
		os.WriteFile(s.Console, []byte("ERROR: unable to select packages"), 0o644)
		return 1
	}}
	b = newBuilder(t, fail, nil)
	err := b.EnsureBuilder(ctx)
	if err == nil || !strings.Contains(err.Error(), "unable to select packages") {
		t.Fatalf("apk failure should include the console: %v", err)
	}
	if _, err := os.Stat(filepath.Join(b.builderRoot(), ".runner-ready")); !os.IsNotExist(err) {
		t.Fatal("marker must not exist after a failed bootstrap")
	}
	b = newBuilder(t, &fakeLauncher{fail: errors.New("no hypervisor")}, nil)
	if err := b.EnsureBuilder(ctx); err == nil {
		t.Fatal("failing launcher should fail")
	}
}

// TestBuildAndCache: primeiro build sobe a VM, grava ext4 e symlink; o segundo é cache.
func TestBuildAndCache(t *testing.T) {
	img, _ := random.Image(256, 2)
	l := &fakeLauncher{handle: buildOK}
	b := newBuilder(t, l, img)
	res, err := b.Build(context.Background(), pyLang)
	if err != nil {
		t.Fatal(err)
	}
	if res.Cached || !strings.Contains(res.Metrics, "extract_ms=1") || !strings.HasPrefix(res.Digest, "sha256:") {
		t.Fatalf("unexpected result: %+v", res)
	}
	spec := l.specs[len(l.specs)-1]
	if spec.Exec != "/work/build.sh" || spec.Args[0] != "/work/python.tar" || spec.Args[2] != "/work/python.warmup.sh" {
		t.Fatalf("wrong build spec: %+v", spec)
	}
	// Trava o defeito medido: sem PATH com /usr/local/bin o warmup do Python não roda.
	if !strings.Contains(strings.Join(spec.Env, " "), "/usr/local/bin") {
		t.Fatalf("build VM without /usr/local/bin in PATH: %v", spec.Env)
	}
	if p, ok := b.ImagePath("python"); !ok || p != res.Path {
		t.Fatalf("symlink does not point to the build: %s %v", p, ok)
	}
	if _, err := os.Stat(filepath.Join(b.builderRoot(), "work", "python.tar")); !os.IsNotExist(err) {
		t.Fatal("intermediate tar should be removed")
	}
	if g, _ := os.ReadFile(filepath.Join(b.builderRoot(), "work", "inject", "guest")); string(g) != "guest-binary" {
		t.Fatal("guest was not injected")
	}
	before := l.calls()
	again, err := b.Build(context.Background(), pyLang)
	if err != nil || !again.Cached || again.Path != res.Path || l.calls() != before {
		t.Fatalf("second build should be a cache hit without a VM: %+v err=%v", again, err)
	}
	if _, ok := b.ImagePath("ruby"); ok {
		t.Fatal("ruby was never built")
	}
}

// TestRebuildRemovesPreviousImage: um build novo da mesma linguagem apaga o ext4 anterior,
// e só ele (o cache crescia uma imagem inteira por rebuild).
func TestRebuildRemovesPreviousImage(t *testing.T) {
	img, _ := random.Image(64, 1)
	b := newBuilder(t, &fakeLauncher{handle: buildOK}, img)
	first, err := b.Build(context.Background(), pyLang)
	if err != nil {
		t.Fatal(err)
	}
	other := filepath.Join(b.imagesDir(), "ruby-aaaaaaaaaaaa.ext4")
	os.WriteFile(other, []byte("x"), 0o644)
	changed := pyLang
	changed.Warmup = "true # changed"
	second, err := b.Build(context.Background(), changed)
	if err != nil {
		t.Fatal(err)
	}
	if second.Path == first.Path {
		t.Fatal("new warmup should produce another image")
	}
	if _, err := os.Stat(first.Path); !os.IsNotExist(err) {
		t.Fatal("previous image should have been removed")
	}
	if _, err := os.Stat(second.Path); err != nil {
		t.Fatal("current image is gone")
	}
	if _, err := os.Stat(other); err != nil {
		t.Fatal("another language image must not be removed")
	}
}

// TestCacheKeyInvalidation: warmup, guest ou imagem diferentes geram chave diferente; a chave é "<base>-<injeção>", e
// o guest muda só a injeção (é o que deixa um rc novo reaproveitar a imagem da mesma base).
func TestCacheKeyInvalidation(t *testing.T) {
	base := cacheKey("sha256:a", "w", []byte("g"))
	if g2 := cacheKey("sha256:a", "w", []byte("g2")); g2[:13] != base[:13] || g2 == base {
		t.Fatalf("guest novo devia manter a base e trocar a injeção: %s %s", base, g2)
	}
	if w2 := cacheKey("sha256:a", "w2", []byte("g")); w2[:12] == base[:12] {
		t.Fatalf("warmup novo devia trocar a base: %s %s", base, w2)
	}
	for name, k := range map[string]string{
		"warmup": cacheKey("sha256:a", "w2", []byte("g")),
		"guest":  cacheKey("sha256:a", "w", []byte("g2")),
		"image":  cacheKey("sha256:b", "w", []byte("g")),
	} {
		if k == base {
			t.Errorf("changing %s did not change the key", name)
		}
	}
	if len(base) != 25 || base[12] != '-' {
		t.Fatalf("key %q: want 12 hex, '-', 12 hex", base)
	}
}

// TestBuildFailures cobre guest ausente, fetch falho, VM de build falha e ctx cancelado.
func TestBuildFailures(t *testing.T) {
	img, _ := random.Image(64, 1)
	ctx := context.Background()

	b := newBuilder(t, &fakeLauncher{handle: buildOK}, img)
	b.GuestBinary = "/does/not/exist"
	if _, err := b.Build(ctx, pyLang); err == nil {
		t.Fatal("missing guest should fail")
	}

	b = newBuilder(t, &fakeLauncher{handle: buildOK}, img)
	b.Fetch = func(context.Context, string, string) (v1.Image, error) { return nil, errors.New("rate limit") }
	if _, err := b.Build(ctx, pyLang); err == nil || !strings.Contains(err.Error(), "rate limit") {
		t.Fatalf("fetch error should propagate: %v", err)
	}

	b = newBuilder(t, &fakeLauncher{handle: func(s ports.VMSpec, _ *fakeVM) int {
		if s.Exec == "/work/build.sh" {
			os.WriteFile(s.Console, []byte("mkfs.ext4: No space left on device"), 0o644)
			return 1
		}
		return 0
	}}, img)
	if _, err := b.Build(ctx, pyLang); err == nil || !strings.Contains(err.Error(), "No space left") {
		t.Fatalf("mkfs failure should include the console: %v", err)
	}

	b = newBuilder(t, &fakeLauncher{handle: func(s ports.VMSpec, _ *fakeVM) int { return 0 }}, img)
	if _, err := b.Build(ctx, pyLang); err == nil {
		t.Fatal("VM that exits 0 without producing the ext4 should fail on rename")
	}

	b = newBuilder(t, &fakeLauncher{handle: func(s ports.VMSpec, vm *fakeVM) int {
		if s.Exec == "/work/build.sh" {
			<-vm.killed
			return -1
		}
		return 0
	}}, img)
	cctx, cancel := context.WithTimeout(ctx, 100*time.Millisecond)
	defer cancel()
	if _, err := b.Build(cctx, pyLang); !errors.Is(err, context.DeadlineExceeded) {
		t.Fatalf("cancellation should kill the build VM and return the ctx error: %v", err)
	}

	b = newBuilder(t, &fakeLauncher{handle: buildOK}, img)
	b.Source = Source{}
	if _, err := b.Build(ctx, pyLang); err == nil {
		t.Fatal("no build VM means no build")
	}
}

// TestRemoteFetcher busca de um registry OCI local, sem rede externa nem daemon.
func TestRemoteFetcher(t *testing.T) {
	srv := httptest.NewServer(registry.New())
	defer srv.Close()
	host := strings.TrimPrefix(srv.URL, "http://")
	ref, _ := name.ParseReference(host + "/lang/python:test")
	img, _ := random.Image(128, 1)
	if err := remote.Write(ref, img); err != nil {
		t.Fatal(err)
	}
	got, err := RemoteFetcher(context.Background(), ref.String(), "arm64")
	if err != nil {
		t.Fatal(err)
	}
	want, _ := img.Digest()
	if d, _ := got.Digest(); d != want {
		t.Fatalf("digest %s, want %s", d, want)
	}
	if _, err := RemoteFetcher(context.Background(), "::ref::invalid", "arm64"); err == nil {
		t.Fatal("invalid reference should fail")
	}
	tarPath := filepath.Join(t.TempDir(), "flat.tar")
	if err := writeFlat(got, tarPath); err != nil {
		t.Fatal(err)
	}
	if err := writeFlat(got, filepath.Join(t.TempDir(), "missing", "dir.tar")); err == nil {
		t.Fatal("missing destination should fail")
	}
}

// TestImageEnvAndWarmupScript trava o conserto do `go`/`javac` não encontrados: o Env da
// config OCI chega à imagem, e um valor hostil não vira comando no warmup.
func TestImageEnvAndWarmupScript(t *testing.T) {
	img, _ := random.Image(64, 1)
	cfg, _ := img.ConfigFile()
	cfg.Config.Env = []string{"PATH=/usr/local/go/bin:/usr/bin", "JAVA_HOME=/opt/java", "BAD='; rm -rf /", "BREAK=a\nb", "NOEQUALS"}
	img, _ = mutate.ConfigFile(img, cfg)
	env, err := imageEnv(img)
	if err != nil {
		t.Fatal(err)
	}
	if strings.Join(env, "|") != "PATH=/usr/local/go/bin:/usr/bin|JAVA_HOME=/opt/java|BAD='; rm -rf /" {
		t.Fatalf("wrongly filtered env: %q", env)
	}
	script := warmupScript(env, "go version")
	want := "export 'PATH=/usr/local/go/bin:/usr/bin'\nexport 'JAVA_HOME=/opt/java'\nexport 'BAD='\\''; rm -rf /'\ngo version\n"
	if script != want {
		t.Fatalf("script:\n%s\nwant:\n%s", script, want)
	}
}

// TestBuildWritesImageEnv: o arquivo env injetado na imagem carrega o PATH da imagem.
func TestBuildWritesImageEnv(t *testing.T) {
	img, _ := random.Image(64, 1)
	cfg, _ := img.ConfigFile()
	cfg.Config.Env = []string{"PATH=/opt/java/openjdk/bin:/usr/bin"}
	img, _ = mutate.ConfigFile(img, cfg)
	b := newBuilder(t, &fakeLauncher{handle: buildOK}, img)
	if _, err := b.Build(context.Background(), pyLang); err != nil {
		t.Fatal(err)
	}
	got, _ := os.ReadFile(filepath.Join(b.builderRoot(), "work", "inject", "env"))
	if string(got) != "PATH=/opt/java/openjdk/bin:/usr/bin\n" {
		t.Fatalf("injected env: %q", got)
	}
}

// TestScanConsoleLongLine trava o conserto do build do C: linha de 1 MiB não esconde o RESULT.
func TestScanConsoleLongLine(t *testing.T) {
	p := filepath.Join(t.TempDir(), "c.log")
	os.WriteFile(p, []byte(strings.Repeat("x", 1<<20)+"\nRESULT used_mib=9\n"), 0o644)
	if r, _ := scanConsole(p); r != "used_mib=9" {
		t.Fatalf("RESULT lost after a long line: %q", r)
	}
	// Caso medido no build do C: RESULT colado no fim da barra de progresso do apk, com \r.
	os.WriteFile(p, []byte("\x1b7 99% ███\x1b8\x1b[0KRESULT used_mib=240 disk_mib=332\r\n"), 0o644)
	if r, _ := scanConsole(p); r != "used_mib=240 disk_mib=332" {
		t.Fatalf("RESULT glued to the progress bar was not found: %q", r)
	}
}

// TestHelpers cobre console ausente, cópia com origem/destino inválidos e link sem diretório.
func TestHelpers(t *testing.T) {
	if r, tail := scanConsole("/does/not/exist"); r != "" || tail != "(no console)" {
		t.Fatalf("missing console: %q %q", r, tail)
	}
	long := filepath.Join(t.TempDir(), "c.log")
	var sb strings.Builder
	for i := 0; i < 30; i++ {
		sb.WriteString("line\n")
	}
	os.WriteFile(long, []byte(sb.String()), 0o644)
	if _, tail := scanConsole(long); strings.Count(tail, "\n") != 19 {
		t.Fatalf("tail should have 20 lines, has %d", strings.Count(tail, "\n")+1)
	}
	if err := copyFile("/does/not/exist", filepath.Join(t.TempDir(), "x"), 0o644); err == nil {
		t.Fatal("missing source should fail")
	}
	src := filepath.Join(t.TempDir(), "s")
	os.WriteFile(src, []byte("x"), 0o644)
	if err := copyFile(src, "/does/not/exist/x", 0o644); err == nil {
		t.Fatal("invalid destination should fail")
	}
	if _, err := fileSum("/does/not/exist"); err == nil {
		t.Fatal("fileSum of a missing file should fail")
	}
	b := &Builder{CacheDir: "/does/not/exist"}
	if err := b.link("python", "/x"); err == nil {
		t.Fatal("link without a directory should fail")
	}
	b.logf("missing log destination must not panic")
}

// fakeShared é o cache compartilhado de mentira: has diz quais chaves estão "no bucket"; put avisa cada upload.
type fakeShared struct {
	mu      sync.Mutex
	has     map[string][]byte
	gets    []string
	put     chan string
	putErr  error
	findErr error
}

// Get copia o conteúdo plantado para dst, ou falha como chave ausente.
func (f *fakeShared) Get(_ context.Context, key, dst string) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.gets = append(f.gets, key)
	data, ok := f.has[key]
	if !ok {
		return errors.New("not in the shared cache")
	}
	return os.WriteFile(dst, data, 0o644)
}

// Find devolve a primeira chave plantada com o prefixo (ou a falha plantada).
func (f *fakeShared) Find(_ context.Context, prefix string) (string, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.findErr != nil {
		return "", f.findErr
	}
	for k := range f.has {
		if strings.HasPrefix(k, prefix) {
			return k, nil
		}
	}
	return "", errors.New("none")
}

// Put guarda o que subiu e avisa pelo canal.
func (f *fakeShared) Put(_ context.Context, key, src string) error {
	data, err := os.ReadFile(src)
	f.mu.Lock()
	if err == nil && f.putErr == nil {
		f.has[key] = data
	}
	f.mu.Unlock()
	f.put <- key
	if err != nil {
		return err
	}
	return f.putErr
}

// syncLog é um log seguro para goroutines (o upload em segundo plano escreve nele enquanto o teste lê).
type syncLog struct {
	mu sync.Mutex
	b  strings.Builder
}

// Write acrescenta ao log.
func (l *syncLog) Write(p []byte) (int, error) { l.mu.Lock(); defer l.mu.Unlock(); return l.b.Write(p) }

// String devolve o log até agora.
func (l *syncLog) String() string { l.mu.Lock(); defer l.mu.Unlock(); return l.b.String() }

// TestSharedImageCache: imagem que outro host já subiu é BAIXADA — sem VM de build, com o symlink apontando para ela —
// e a que ninguém subiu é construída aqui e subida com a mesma chave, para o próximo host só baixar. Upload que falha
// não derruba o build: a imagem local está pronta, só fica no log.
func TestSharedImageCache(t *testing.T) {
	img, _ := random.Image(256, 2)
	ctx := context.Background()

	// Falta no bucket: constrói e sobe.
	l := &fakeLauncher{handle: buildOK}
	b := newBuilder(t, l, img)
	sh := &fakeShared{has: map[string][]byte{}, put: make(chan string, 1)}
	b.Shared = sh
	b.Log = &syncLog{}
	res, err := b.Build(ctx, pyLang)
	if err != nil || res.Cached || l.calls() == 0 {
		t.Fatalf("falta no bucket devia construir: %+v %v", res, err)
	}
	key := <-sh.put
	if key != strings.TrimSuffix(filepath.Base(res.Path), ".ext4") || len(sh.gets) != 1 || sh.gets[0] != key {
		t.Fatalf("chave subida %q, pedida %v, arquivo %s", key, sh.gets, res.Path)
	}

	// Outro host, mesma imagem: baixa do bucket, sem VM de build.
	l2 := &fakeLauncher{handle: buildOK}
	b2 := newBuilder(t, l2, img)
	b2.GuestBinary = b.GuestBinary // o mesmo guest: a chave de conteúdo é a mesma
	b2.Shared = sh
	got, err := b2.Build(ctx, pyLang)
	if err != nil || !got.Cached || l2.calls() != 0 {
		t.Fatalf("acerto no bucket devia baixar sem VM: %+v %v (VMs %d)", got, err, l2.calls())
	}
	want, _ := os.ReadFile(res.Path)
	if data, _ := os.ReadFile(got.Path); string(data) != string(want) {
		t.Fatal("a imagem baixada não é a que o outro host construiu")
	}
	if p, ok := b2.ImagePath("python"); !ok || p != got.Path {
		t.Fatalf("symlink não aponta para a baixada: %s %v", p, ok)
	}

	// Upload recusado: o build vale assim mesmo, e a falha vai para o log.
	l3 := &fakeLauncher{handle: buildOK}
	b3 := newBuilder(t, l3, img)
	sh3 := &fakeShared{has: map[string][]byte{}, put: make(chan string, 1), putErr: errors.New("403")}
	b3.Shared = sh3
	log3 := &syncLog{}
	b3.Log = log3
	if _, err := b3.Build(ctx, pyLang); err != nil {
		t.Fatalf("upload recusado derrubou o build: %v", err)
	}
	<-sh3.put
	for i := 0; i < 100 && !strings.Contains(log3.String(), "upload failed"); i++ {
		time.Sleep(10 * time.Millisecond)
	}
	if !strings.Contains(log3.String(), "upload failed") {
		t.Fatalf("falha do upload sem log: %q", log3.String())
	}
}

// reinjectOK simula as duas VMs: a de build (buildOK) e a de reinjeção, que só confirma no console.
func reinjectOK(s ports.VMSpec, vm *fakeVM) int {
	if s.Exec == "/work/reinject.sh" {
		os.WriteFile(s.Console, []byte("RESULT reinject_ms=5\n"), 0o644)
		return 0
	}
	return buildOK(s, vm)
}

// TestSharedRebase: um rc novo (outro guest) acha no bucket a imagem da MESMA base e só troca a injeção — baixa a
// antiga, roda o reinject.sh na VM de build (nunca o build.sh) com o guest novo em /work/inject, e sobe com a chave nova.
// Reinjeção que falha, ou bucket que não lista, volta a construir inteira como antes.
func TestSharedRebase(t *testing.T) {
	img, _ := random.Image(256, 2)
	ctx := context.Background()
	digest, _ := img.Digest()
	oldKey := "python-" + baseKey(digest.String(), pyLang.Warmup) + "-aaaaaaaaaaaa"

	l := &fakeLauncher{handle: reinjectOK}
	b := newBuilder(t, l, img)
	sh := &fakeShared{has: map[string][]byte{oldKey: []byte("ext4-da-base")}, put: make(chan string, 1)}
	b.Shared = sh
	b.Log = &syncLog{}
	res, err := b.Build(ctx, pyLang)
	if err != nil || !res.Cached {
		t.Fatalf("reaproveitamento: %+v %v", res, err)
	}
	for _, spec := range l.specs {
		if spec.Exec == "/work/build.sh" {
			t.Fatal("rodou o build.sh: devia só reinjetar")
		}
	}
	last := l.specs[len(l.specs)-1]
	if last.Exec != "/work/reinject.sh" || last.Args[0] != "/work/python.ext4" {
		t.Fatalf("VM de reinjeção: %+v", last)
	}
	if data, _ := os.ReadFile(res.Path); string(data) != "ext4-da-base" {
		t.Fatalf("a imagem não é a da base baixada: %q", data)
	}
	if g, _ := os.ReadFile(filepath.Join(b.builderRoot(), "work", "inject", "guest")); string(g) != "guest-binary" {
		t.Fatal("guest novo não foi para /work/inject")
	}
	if env, _ := os.ReadFile(filepath.Join(b.builderRoot(), "work", "inject", "env")); len(env) == 0 {
		t.Fatal("env da imagem não foi para /work/inject")
	}
	if up := <-sh.put; up == oldKey || up != strings.TrimSuffix(filepath.Base(res.Path), ".ext4") {
		t.Fatalf("subiu %q (antiga %q)", up, oldKey)
	}
	if _, err := os.Stat(filepath.Join(b.builderRoot(), "work", "python.ext4")); !os.IsNotExist(err) {
		t.Fatal("o ext4 de trabalho ficou para trás")
	}

	// Reinjeção que falha: constrói inteira, e o motivo vai para o log.
	fail := &fakeLauncher{handle: func(s ports.VMSpec, vm *fakeVM) int {
		if s.Exec == "/work/reinject.sh" {
			os.WriteFile(s.Console, []byte("reinject: /opt/runner/guest não confere\n"), 0o644)
			return 1
		}
		return buildOK(s, vm)
	}}
	b2 := newBuilder(t, fail, img)
	sh2 := &fakeShared{has: map[string][]byte{oldKey: []byte("x")}, put: make(chan string, 1)}
	b2.Shared = sh2
	log2 := &syncLog{}
	b2.Log = log2
	if res, err := b2.Build(ctx, pyLang); err != nil || res.Cached {
		t.Fatalf("falha da reinjeção devia construir: %+v %v", res, err)
	}
	<-sh2.put
	if !strings.Contains(log2.String(), "reuse of "+oldKey+" failed") || fail.specs[len(fail.specs)-1].Exec != "/work/build.sh" {
		t.Fatalf("sem queda para o build: %q", log2.String())
	}

	// Download da base que falha (some entre o Find e o Get): constrói.
	b3 := newBuilder(t, &fakeLauncher{handle: reinjectOK}, img)
	sh3 := &vanishing{fakeShared{has: map[string][]byte{oldKey: []byte("x")}, put: make(chan string, 1)}}
	b3.Shared = sh3
	b3.Log = &syncLog{}
	if res, err := b3.Build(ctx, pyLang); err != nil || res.Cached {
		t.Fatalf("base sumida devia construir: %+v %v", res, err)
	}
	<-sh3.put

	// Bucket que não lista: constrói como antes.
	b4 := newBuilder(t, &fakeLauncher{handle: reinjectOK}, img)
	sh4 := &fakeShared{has: map[string][]byte{}, put: make(chan string, 1), findErr: errors.New("403")}
	b4.Shared = sh4
	b4.Log = &syncLog{}
	if res, err := b4.Build(ctx, pyLang); err != nil || res.Cached {
		t.Fatalf("sem listagem devia construir: %+v %v", res, err)
	}
	<-sh4.put
}

// vanishing acha a chave no Find mas falha o Get dela (o objeto some entre os dois).
type vanishing struct{ fakeShared }

// Get falha sempre.
func (v *vanishing) Get(context.Context, string, string) error { return errors.New("gone") }

// TestSharedRebaseFailures: VM de build que não sobe e diretório de trabalho impossível derrubam a reinjeção E o build
// (o erro chega a quem pediu); nada fica no lugar da imagem.
func TestSharedRebaseFailures(t *testing.T) {
	img, _ := random.Image(64, 1)
	ctx := context.Background()
	digest, _ := img.Digest()
	oldKey := "python-" + baseKey(digest.String(), pyLang.Warmup) + "-bbbbbbbbbbbb"

	// A VM de build não sobe: nem reinjeta nem constrói.
	l := &fakeLauncher{handle: reinjectOK, fail: errors.New("sem KVM")}
	b := newBuilder(t, l, img)
	b.Shared = &fakeShared{has: map[string][]byte{oldKey: []byte("x")}, put: make(chan string, 1)}
	b.Log = &syncLog{}
	if _, err := b.Build(ctx, pyLang); err == nil || !strings.Contains(err.Error(), "sem KVM") {
		t.Fatalf("VM que não sobe: %v", err)
	}

	// O diretório de trabalho é um arquivo: a reinjeção falha ao preparar, e o build também.
	b2 := newBuilder(t, &fakeLauncher{handle: reinjectOK}, img)
	b2.Shared = &fakeShared{has: map[string][]byte{oldKey: []byte("x")}, put: make(chan string, 1)}
	log2 := &syncLog{}
	b2.Log = log2
	if err := b2.EnsureBuilder(ctx); err != nil {
		t.Fatal(err)
	}
	os.RemoveAll(filepath.Join(b2.builderRoot(), "work"))
	os.WriteFile(filepath.Join(b2.builderRoot(), "work"), []byte("não é diretório"), 0o644)
	if _, err := b2.Build(ctx, pyLang); err == nil {
		t.Fatal("diretório de trabalho impossível passou")
	}
	if !strings.Contains(log2.String(), "reuse of "+oldKey+" failed") {
		t.Fatalf("falha da reinjeção sem log: %q", log2.String())
	}
	if entries, _ := os.ReadDir(b2.imagesDir()); len(entries) != 0 {
		t.Fatalf("sobrou imagem: %v", entries)
	}
}

// TestWriteInjectFailures: destino que não existe (o guest não copia) e arquivo de script que é diretório são erro.
func TestWriteInjectFailures(t *testing.T) {
	img, _ := random.Image(64, 1)
	b := newBuilder(t, &fakeLauncher{handle: reinjectOK}, img)
	if err := b.writeInject(img, filepath.Join(t.TempDir(), "nao", "existe")); err == nil {
		t.Fatal("destino inexistente passou")
	}
	dir := t.TempDir()
	os.Mkdir(filepath.Join(dir, "start.sh"), 0o755)
	if err := b.writeInject(img, dir); err == nil {
		t.Fatal("script que é diretório passou")
	}
}

// TestRebaseScriptNotWritten: o reinject.sh que não grava faz a reinjeção desistir e o build inteiro seguir — e dar
// certo (o build não depende do reinject.sh).
func TestRebaseScriptNotWritten(t *testing.T) {
	img, _ := random.Image(64, 1)
	ctx := context.Background()
	digest, _ := img.Digest()
	oldKey := "python-" + baseKey(digest.String(), pyLang.Warmup) + "-cccccccccccc"
	l := &fakeLauncher{handle: reinjectOK}
	b := newBuilder(t, l, img)
	sh := &fakeShared{has: map[string][]byte{oldKey: []byte("x")}, put: make(chan string, 1)}
	b.Shared = sh
	b.Log = &syncLog{}
	if err := b.EnsureBuilder(ctx); err != nil {
		t.Fatal(err)
	}
	os.MkdirAll(filepath.Join(b.builderRoot(), "work", "reinject.sh"), 0o755)
	res, err := b.Build(ctx, pyLang)
	if err != nil || res.Cached || l.specs[len(l.specs)-1].Exec != "/work/build.sh" {
		t.Fatalf("devia construir inteira: %+v %v", res, err)
	}
	<-sh.put
}

// TestScriptsOnlyChangeInjection: os scripts de arranque (start, shell, machine) vão em /opt/runner pela injeção, então
// mudar um deles troca SÓ a parte de injeção da chave — a base (camadas + warmup + build.sh) continua a mesma, e o rc
// reaproveita a imagem em vez de reconstruí-la. O build.sh, ao contrário, muda a base.
func TestScriptsOnlyChangeInjection(t *testing.T) {
	base, inj := baseKey("sha256:a", "w"), injectKey([]byte("g"))
	for name, script := range map[string]*[]byte{"start": &startScript, "shell": &shellScript, "machine": &machineScript} {
		orig := *script
		*script = append(append([]byte(nil), orig...), []byte("\n# mudou")...)
		if baseKey("sha256:a", "w") != base || injectKey([]byte("g")) == inj {
			t.Errorf("%s.sh: base devia ficar, injeção devia mudar", name)
		}
		*script = orig
	}
	orig := buildScript
	buildScript = append(append([]byte(nil), orig...), []byte("\n# mudou")...)
	if baseKey("sha256:a", "w") == base {
		t.Error("build.sh novo devia mudar a base")
	}
	buildScript = orig
}
