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

	"runner.code/internal/domain"
	"runner.code/internal/ports"
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
		t.Fatalf("bootstrap com spec errada: %+v", l.specs)
	}
	if b, _ := os.ReadFile(filepath.Join(b.builderRoot(), "etc", "resolv.conf")); !strings.Contains(string(b), "nameserver") {
		t.Fatal("resolv.conf da VM de build ausente")
	}
	if err := b.EnsureBuilder(context.Background()); err != nil || l.calls() != 1 {
		t.Fatalf("segunda chamada não deveria subir VM: err=%v calls=%d", err, l.calls())
	}
}

// TestEnsureBuilderRefuses cobre fonte ausente, sha errado, HTTP 404 e bootstrap que falha.
func TestEnsureBuilderRefuses(t *testing.T) {
	ctx := context.Background()
	b := newBuilder(t, &fakeLauncher{handle: buildOK}, nil)
	b.Source = Source{}
	if err := b.EnsureBuilder(ctx); err == nil {
		t.Fatal("sem fonte deveria falhar")
	}
	b = newBuilder(t, &fakeLauncher{handle: buildOK}, nil)
	b.Source.SHA256 = strings.Repeat("0", 64)
	if err := b.EnsureBuilder(ctx); err == nil || !strings.Contains(err.Error(), "não confere") {
		t.Fatalf("sha errado deveria falhar antes de extrair: %v", err)
	}
	if _, err := os.Stat(filepath.Join(b.builderRoot(), "bin", "sh")); !os.IsNotExist(err) {
		t.Fatal("nada pode ser extraído com sha errado")
	}
	b = newBuilder(t, &fakeLauncher{handle: buildOK}, nil)
	b.Source.URL += "-404"
	if err := b.EnsureBuilder(ctx); err == nil {
		t.Fatal("HTTP 404 deveria falhar")
	}
	b = newBuilder(t, &fakeLauncher{handle: buildOK}, nil)
	b.Source.URL = "http://127.0.0.1:1/x"
	if err := b.EnsureBuilder(ctx); err == nil {
		t.Fatal("conexão recusada deveria falhar")
	}
	b = newBuilder(t, &fakeLauncher{handle: buildOK}, nil)
	b.Source.URL = "::url-inválida"
	if err := b.EnsureBuilder(ctx); err == nil {
		t.Fatal("URL inválida deveria falhar")
	}
	fail := &fakeLauncher{handle: func(s ports.VMSpec, _ *fakeVM) int {
		os.WriteFile(s.Console, []byte("ERROR: unable to select packages"), 0o644)
		return 1
	}}
	b = newBuilder(t, fail, nil)
	err := b.EnsureBuilder(ctx)
	if err == nil || !strings.Contains(err.Error(), "unable to select packages") {
		t.Fatalf("falha do apk deveria trazer o console: %v", err)
	}
	if _, err := os.Stat(filepath.Join(b.builderRoot(), ".runner-ready")); !os.IsNotExist(err) {
		t.Fatal("marcador não pode existir depois de bootstrap falho")
	}
	b = newBuilder(t, &fakeLauncher{fail: errors.New("sem hypervisor")}, nil)
	if err := b.EnsureBuilder(ctx); err == nil {
		t.Fatal("launcher que falha deveria falhar")
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
		t.Fatalf("resultado inesperado: %+v", res)
	}
	spec := l.specs[len(l.specs)-1]
	if spec.Exec != "/work/build.sh" || spec.Args[0] != "/work/python.tar" || spec.Args[2] != "/work/python.warmup.sh" {
		t.Fatalf("spec de build errada: %+v", spec)
	}
	// Trava o defeito medido: sem PATH com /usr/local/bin o warmup do Python não roda.
	if !strings.Contains(strings.Join(spec.Env, " "), "/usr/local/bin") {
		t.Fatalf("VM de build sem PATH com /usr/local/bin: %v", spec.Env)
	}
	if p, ok := b.ImagePath("python"); !ok || p != res.Path {
		t.Fatalf("symlink não aponta para o build: %s %v", p, ok)
	}
	if _, err := os.Stat(filepath.Join(b.builderRoot(), "work", "python.tar")); !os.IsNotExist(err) {
		t.Fatal("tar intermediário deveria ser apagado")
	}
	if g, _ := os.ReadFile(filepath.Join(b.builderRoot(), "work", "inject", "guest")); string(g) != "guest-binary" {
		t.Fatal("guest não foi injetado")
	}
	before := l.calls()
	again, err := b.Build(context.Background(), pyLang)
	if err != nil || !again.Cached || again.Path != res.Path || l.calls() != before {
		t.Fatalf("segundo build deveria ser cache sem VM: %+v err=%v", again, err)
	}
	if _, ok := b.ImagePath("ruby"); ok {
		t.Fatal("ruby nunca foi construído")
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
	changed.Warmup = "true # mudou"
	second, err := b.Build(context.Background(), changed)
	if err != nil {
		t.Fatal(err)
	}
	if second.Path == first.Path {
		t.Fatal("warmup novo deveria gerar outra imagem")
	}
	if _, err := os.Stat(first.Path); !os.IsNotExist(err) {
		t.Fatal("imagem anterior deveria ter sido apagada")
	}
	if _, err := os.Stat(second.Path); err != nil {
		t.Fatal("imagem atual sumiu")
	}
	if _, err := os.Stat(other); err != nil {
		t.Fatal("imagem de outra linguagem não pode ser apagada")
	}
}

// TestCacheKeyInvalidation: warmup, guest ou imagem diferentes geram chave diferente.
func TestCacheKeyInvalidation(t *testing.T) {
	base := cacheKey("sha256:a", "w", []byte("g"))
	for name, k := range map[string]string{
		"warmup": cacheKey("sha256:a", "w2", []byte("g")),
		"guest":  cacheKey("sha256:a", "w", []byte("g2")),
		"imagem": cacheKey("sha256:b", "w", []byte("g")),
	} {
		if k == base {
			t.Errorf("mudar %s não mudou a chave", name)
		}
	}
	if len(base) != 12 {
		t.Fatalf("chave com %d caracteres", len(base))
	}
}

// TestBuildFailures cobre guest ausente, fetch falho, VM de build falha e ctx cancelado.
func TestBuildFailures(t *testing.T) {
	img, _ := random.Image(64, 1)
	ctx := context.Background()

	b := newBuilder(t, &fakeLauncher{handle: buildOK}, img)
	b.GuestBinary = "/nao/existe"
	if _, err := b.Build(ctx, pyLang); err == nil {
		t.Fatal("guest ausente deveria falhar")
	}

	b = newBuilder(t, &fakeLauncher{handle: buildOK}, img)
	b.Fetch = func(context.Context, string, string) (v1.Image, error) { return nil, errors.New("rate limit") }
	if _, err := b.Build(ctx, pyLang); err == nil || !strings.Contains(err.Error(), "rate limit") {
		t.Fatalf("erro de fetch deveria subir: %v", err)
	}

	b = newBuilder(t, &fakeLauncher{handle: func(s ports.VMSpec, _ *fakeVM) int {
		if s.Exec == "/work/build.sh" {
			os.WriteFile(s.Console, []byte("mkfs.ext4: No space left on device"), 0o644)
			return 1
		}
		return 0
	}}, img)
	if _, err := b.Build(ctx, pyLang); err == nil || !strings.Contains(err.Error(), "No space left") {
		t.Fatalf("falha do mkfs deveria trazer o console: %v", err)
	}

	b = newBuilder(t, &fakeLauncher{handle: func(s ports.VMSpec, _ *fakeVM) int { return 0 }}, img)
	if _, err := b.Build(ctx, pyLang); err == nil {
		t.Fatal("VM que sai 0 sem gerar o ext4 deveria falhar no rename")
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
		t.Fatalf("cancelamento deveria matar a VM de build e devolver o erro do ctx: %v", err)
	}

	b = newBuilder(t, &fakeLauncher{handle: buildOK}, img)
	b.Source = Source{}
	if _, err := b.Build(ctx, pyLang); err == nil {
		t.Fatal("sem VM de build não há build")
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
		t.Fatalf("digest %s, quer %s", d, want)
	}
	if _, err := RemoteFetcher(context.Background(), "::ref::inválida", "arm64"); err == nil {
		t.Fatal("referência inválida deveria falhar")
	}
	tarPath := filepath.Join(t.TempDir(), "flat.tar")
	if err := writeFlat(got, tarPath); err != nil {
		t.Fatal(err)
	}
	if err := writeFlat(got, filepath.Join(t.TempDir(), "sem", "dir.tar")); err == nil {
		t.Fatal("destino inexistente deveria falhar")
	}
}

// TestImageEnvAndWarmupScript trava o conserto do `go`/`javac` não encontrados: o Env da
// config OCI chega à imagem, e um valor hostil não vira comando no warmup.
func TestImageEnvAndWarmupScript(t *testing.T) {
	img, _ := random.Image(64, 1)
	cfg, _ := img.ConfigFile()
	cfg.Config.Env = []string{"PATH=/usr/local/go/bin:/usr/bin", "JAVA_HOME=/opt/java", "MAL='; rm -rf /", "QUEBRA=a\nb", "SEMIGUAL"}
	img, _ = mutate.ConfigFile(img, cfg)
	env, err := imageEnv(img)
	if err != nil {
		t.Fatal(err)
	}
	if strings.Join(env, "|") != "PATH=/usr/local/go/bin:/usr/bin|JAVA_HOME=/opt/java|MAL='; rm -rf /" {
		t.Fatalf("env filtrado errado: %q", env)
	}
	script := warmupScript(env, "go version")
	want := "export 'PATH=/usr/local/go/bin:/usr/bin'\nexport 'JAVA_HOME=/opt/java'\nexport 'MAL='\\''; rm -rf /'\ngo version\n"
	if script != want {
		t.Fatalf("script:\n%s\nquer:\n%s", script, want)
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
		t.Fatalf("env injetado: %q", got)
	}
}

// TestScanConsoleLongLine trava o conserto do build do C: linha de 1 MiB não esconde o RESULT.
func TestScanConsoleLongLine(t *testing.T) {
	p := filepath.Join(t.TempDir(), "c.log")
	os.WriteFile(p, []byte(strings.Repeat("x", 1<<20)+"\nRESULT used_mib=9\n"), 0o644)
	if r, _ := scanConsole(p); r != "used_mib=9" {
		t.Fatalf("RESULT perdido depois de linha longa: %q", r)
	}
	// Caso medido no build do C: RESULT colado no fim da barra de progresso do apk, com \r.
	os.WriteFile(p, []byte("\x1b7 99% ███\x1b8\x1b[0KRESULT used_mib=240 disk_mib=332\r\n"), 0o644)
	if r, _ := scanConsole(p); r != "used_mib=240 disk_mib=332" {
		t.Fatalf("RESULT colado na barra de progresso não foi achado: %q", r)
	}
}

// TestHelpers cobre console ausente, cópia com origem/destino inválidos e link sem diretório.
func TestHelpers(t *testing.T) {
	if r, tail := scanConsole("/nao/existe"); r != "" || tail != "(sem console)" {
		t.Fatalf("console ausente: %q %q", r, tail)
	}
	long := filepath.Join(t.TempDir(), "c.log")
	var sb strings.Builder
	for i := 0; i < 30; i++ {
		sb.WriteString("linha\n")
	}
	os.WriteFile(long, []byte(sb.String()), 0o644)
	if _, tail := scanConsole(long); strings.Count(tail, "\n") != 19 {
		t.Fatalf("tail deveria ter 20 linhas, tem %d", strings.Count(tail, "\n")+1)
	}
	if err := copyFile("/nao/existe", filepath.Join(t.TempDir(), "x"), 0o644); err == nil {
		t.Fatal("origem ausente deveria falhar")
	}
	src := filepath.Join(t.TempDir(), "s")
	os.WriteFile(src, []byte("x"), 0o644)
	if err := copyFile(src, "/nao/existe/x", 0o644); err == nil {
		t.Fatal("destino inválido deveria falhar")
	}
	if _, err := fileSum("/nao/existe"); err == nil {
		t.Fatal("fileSum de arquivo ausente deveria falhar")
	}
	b := &Builder{CacheDir: "/nao/existe"}
	if err := b.link("python", "/x"); err == nil {
		t.Fatal("link sem diretório deveria falhar")
	}
	b.logf("sem destino de log não pode entrar em pânico")
}
