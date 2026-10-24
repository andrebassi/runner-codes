// Package oci transforma a imagem OCI oficial de uma linguagem num ext4 read-only
// pronto para boot. O pull e o achatamento acontecem no host (Go puro, sem daemon);
// a extração, o warmup e o mkfs acontecem numa VM de build Linux, porque o APFS do
// macOS não diferencia maiúsculas de minúsculas e o e2fsprogs do Homebrew não lê tar.
package oci

import (
	"bufio"
	"context"
	"crypto/sha256"
	_ "embed"
	"encoding/hex"
	"fmt"
	"io"
	"net/http"
	"os"
	"path/filepath"
	"strings"
	"time"

	"github.com/google/go-containerregistry/pkg/name"
	v1 "github.com/google/go-containerregistry/pkg/v1"
	"github.com/google/go-containerregistry/pkg/v1/mutate"
	"github.com/google/go-containerregistry/pkg/v1/remote"

	"runner.codes/internal/domain"
	"runner.codes/internal/ports"
)

//go:embed scripts/build.sh
var buildScript []byte

//go:embed scripts/start.sh
var startScript []byte

// machineScript é o primeiro processo de uma máquina persistente: monta o disco dela e o
// overlay sobre a imagem read-only (internal/adapters/oci/scripts/machine.sh).
//
//go:embed scripts/machine.sh
var machineScript []byte

// diskScript formata o modelo vazio do disco de máquina, dentro da VM de build (o host
// não tem mkfs.ext4 sem Homebrew, e a VM de build já tem o e2fsprogs).
//
//go:embed scripts/mkdisk.sh
var diskScript []byte

//go:embed scripts/shell.sh
var shellScript []byte

//go:embed scripts/bootstrap.sh
var bootstrapScript []byte

// Source é o tarball do Alpine mínimo da VM de build e o sha256 publicado dele.
type Source struct {
	URL    string
	SHA256 string
}

// AlpineSources fixa o minirootfs 3.22.6 por arquitetura do host; os dois sha256 foram
// conferidos contra latest-releases.yaml e contra o arquivo baixado em 19/10/2026.
var AlpineSources = map[string]Source{
	"arm64": {
		URL:    "https://dl-cdn.alpinelinux.org/alpine/v3.22/releases/aarch64/alpine-minirootfs-3.22.6-aarch64.tar.gz",
		SHA256: "821565fa8f3953eefd12497b166b4b50add2f7c57fb312e75862f5867e06fefe",
	},
	"amd64": {
		URL:    "https://dl-cdn.alpinelinux.org/alpine/v3.22/releases/x86_64/alpine-minirootfs-3.22.6-x86_64.tar.gz",
		SHA256: "27694aaa55fd7a9e3ef596e0ad4eb66802308bb20172b17030cd5f4d8ae9bac2",
	},
}

// buildVMMemoryMiB é alto de propósito: o tmpfs de extração vive na RAM do guest e uma
// imagem de Rust passa de 1 GiB. A memória da libkrun é elástica, o host só paga o que
// o build tocar.
const buildVMMemoryMiB = 8192

// buildEnv é o ambiente da VM de build e, por herança, do chroot do warmup. Sem PATH a
// VM sobe com ambiente vazio e o warmup não acha nada em /usr/local/bin (onde as imagens
// oficiais instalam python, node e go): o warmup do Python "rodou" em 20 ms sem compilar.
var buildEnv = []string{
	"PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/local/go/bin",
	"HOME=/root", "LANG=C.UTF-8",
}

// Fetcher busca a imagem para a arquitetura; trocado nos testes por um registry local.
type Fetcher func(ctx context.Context, ref, arch string) (v1.Image, error)

// RemoteFetcher busca no registry público (Docker Hub, ghcr.io) sem daemon.
func RemoteFetcher(ctx context.Context, ref, arch string) (v1.Image, error) {
	r, err := name.ParseReference(ref)
	if err != nil {
		return nil, err
	}
	return remote.Image(r, remote.WithContext(ctx), remote.WithPlatform(v1.Platform{OS: "linux", Architecture: arch}))
}

// Builder gera e guarda as imagens das linguagens.
type Builder struct {
	CacheDir    string
	Arch        string
	GuestBinary string
	Launcher    ports.VMLauncher
	Fetch       Fetcher
	Source      Source
	HTTP        *http.Client
	Log         io.Writer
}

// Image é o resultado de um build.
type Image struct {
	Language string
	Path     string
	Digest   string
	Cached   bool
	Metrics  string
	Elapsed  time.Duration
}

// builderRoot é o rootfs da VM de build, compartilhado por virtiofs.
func (b *Builder) builderRoot() string { return filepath.Join(b.CacheDir, "builder", "rootfs") }

// imagesDir guarda os ext4 prontos.
func (b *Builder) imagesDir() string { return filepath.Join(b.CacheDir, "images") }

// logf escreve progresso quando há destino para isso.
func (b *Builder) logf(format string, args ...interface{}) {
	if b.Log != nil {
		fmt.Fprintf(b.Log, format+"\n", args...)
	}
}

// ImagePath devolve o ext4 atual da linguagem (o alvo do symlink <lang>.ext4).
func (b *Builder) ImagePath(language string) (string, bool) {
	// Readlink e não EvalSymlinks: no macOS o EvalSymlinks também resolve /var → /private/var
	// e o caminho devolvido deixaria de bater com o que o build gravou.
	target, err := os.Readlink(filepath.Join(b.imagesDir(), language+".ext4"))
	if err != nil {
		return "", false
	}
	path := filepath.Join(b.imagesDir(), target)
	if _, err := os.Stat(path); err != nil {
		return "", false
	}
	return path, true
}

// EnsureBuilder prepara a VM de build uma vez: baixa o minirootfs, confere o sha256,
// extrai e roda o bootstrap (apk add e2fsprogs tar) com rede. O marcador .runner-ready
// só é gravado se tudo der certo.
func (b *Builder) EnsureBuilder(ctx context.Context) error {
	root := b.builderRoot()
	marker := filepath.Join(root, ".runner-ready")
	if _, err := os.Stat(marker); err == nil {
		return nil
	}
	if b.Source.URL == "" || b.Source.SHA256 == "" {
		return fmt.Errorf("no verified minirootfs for architecture %s", b.Arch)
	}
	os.RemoveAll(root)
	if err := os.MkdirAll(root, 0o755); err != nil {
		return err
	}
	b.logf("downloading %s", b.Source.URL)
	if err := b.downloadVerified(ctx, root); err != nil {
		return err
	}
	if err := os.WriteFile(filepath.Join(root, "etc", "resolv.conf"), []byte("nameserver 1.1.1.1\n"), 0o644); err != nil {
		return err
	}
	work := filepath.Join(root, "work")
	if err := os.MkdirAll(work, 0o755); err != nil {
		return err
	}
	if err := os.WriteFile(filepath.Join(work, "bootstrap.sh"), bootstrapScript, 0o755); err != nil {
		return err
	}
	b.logf("installing e2fsprogs and tar in the build VM")
	if _, err := b.runBuildVM(ctx, "/work/bootstrap.sh", nil, filepath.Join(work, "bootstrap.console")); err != nil {
		return fmt.Errorf("build VM bootstrap: %w", err)
	}
	return os.WriteFile(marker, []byte(time.Now().UTC().Format(time.RFC3339)), 0o644)
}

// downloadVerified baixa o tarball, confere o sha256 antes de extrair qualquer byte e extrai.
func (b *Builder) downloadVerified(ctx context.Context, root string) error {
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, b.Source.URL, nil)
	if err != nil {
		return err
	}
	client := b.HTTP
	if client == nil {
		client = http.DefaultClient
	}
	resp, err := client.Do(req)
	if err != nil {
		return fmt.Errorf("download minirootfs: %w", err)
	}
	defer resp.Body.Close()
	if resp.StatusCode != http.StatusOK {
		return fmt.Errorf("download minirootfs: HTTP %d", resp.StatusCode)
	}
	tmp, err := os.CreateTemp(filepath.Dir(root), "minirootfs-*.tar.gz")
	if err != nil {
		return err
	}
	defer os.Remove(tmp.Name())
	defer tmp.Close()
	h := sha256.New()
	if _, err := io.Copy(io.MultiWriter(tmp, h), resp.Body); err != nil {
		return err
	}
	if got := hex.EncodeToString(h.Sum(nil)); got != b.Source.SHA256 {
		return fmt.Errorf("minirootfs sha256 does not match: %s, expected %s", got, b.Source.SHA256)
	}
	if _, err := tmp.Seek(0, io.SeekStart); err != nil {
		return err
	}
	return extractTarGz(tmp, root)
}

// cacheKey identifica o ext4: muda se mudar a imagem, o warmup, os scripts ou o guest
// (o guest vai dentro da imagem, então guest novo exige imagem nova).
func cacheKey(digest, warmup string, guestSum []byte) string {
	h := sha256.New()
	for _, part := range [][]byte{[]byte(digest), []byte(warmup), buildScript, startScript, shellScript, machineScript, guestSum} {
		h.Write(part)
		h.Write([]byte{0})
	}
	return hex.EncodeToString(h.Sum(nil))[:12]
}

// fileSum devolve o sha256 de um arquivo.
func fileSum(path string) ([]byte, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	h := sha256.New()
	if _, err := io.Copy(h, f); err != nil {
		return nil, err
	}
	return h.Sum(nil), nil
}

// Build gera (ou reaproveita) o ext4 da linguagem.
func (b *Builder) Build(ctx context.Context, lang domain.Language) (Image, error) {
	start := time.Now()
	res := Image{Language: lang.Name}
	if err := b.EnsureBuilder(ctx); err != nil {
		return res, err
	}
	guestSum, err := fileSum(b.GuestBinary)
	if err != nil {
		return res, fmt.Errorf("guest to inject: %w", err)
	}
	b.logf("[%s] resolving %s (linux/%s)", lang.Name, lang.Image, b.Arch)
	img, err := b.Fetch(ctx, lang.Image, b.Arch)
	if err != nil {
		return res, fmt.Errorf("fetch %s: %w", lang.Image, err)
	}
	digest, err := img.Digest()
	if err != nil {
		return res, err
	}
	res.Digest = digest.String()
	out := filepath.Join(b.imagesDir(), fmt.Sprintf("%s-%s.ext4", lang.Name, cacheKey(res.Digest, lang.Warmup, guestSum)))
	if _, err := os.Stat(out); err == nil {
		res.Path, res.Cached, res.Elapsed = out, true, time.Since(start)
		return res, b.link(lang.Name, out)
	}

	work := filepath.Join(b.builderRoot(), "work")
	inject := filepath.Join(work, "inject")
	if err := os.MkdirAll(inject, 0o755); err != nil {
		return res, err
	}
	tarPath := filepath.Join(work, lang.Name+".tar")
	defer os.Remove(tarPath)
	b.logf("[%s] downloading and flattening layers", lang.Name)
	if err := writeFlat(img, tarPath); err != nil {
		return res, err
	}
	if err := copyFile(b.GuestBinary, filepath.Join(inject, "guest"), 0o755); err != nil {
		return res, err
	}
	env, err := imageEnv(img)
	if err != nil {
		return res, err
	}
	writes := map[string][]byte{
		filepath.Join(inject, "start.sh"):           startScript,
		filepath.Join(inject, "shell.sh"):           shellScript,
		filepath.Join(inject, "machine.sh"):         machineScript,
		filepath.Join(inject, "env"):                []byte(strings.Join(env, "\n") + "\n"),
		filepath.Join(work, "build.sh"):             buildScript,
		filepath.Join(work, lang.Name+".warmup.sh"): []byte(warmupScript(env, lang.Warmup)),
	}
	for p, data := range writes {
		if err := os.WriteFile(p, data, 0o755); err != nil {
			return res, err
		}
	}
	console := filepath.Join(work, lang.Name+".console")
	args := []string{"/work/" + lang.Name + ".tar", "/work/" + lang.Name + ".ext4", "/work/" + lang.Name + ".warmup.sh"}
	b.logf("[%s] extracting, warming up and building ext4 in the build VM", lang.Name)
	metrics, err := b.runBuildVM(ctx, "/work/build.sh", args, console)
	if err != nil {
		return res, err
	}
	if err := os.MkdirAll(b.imagesDir(), 0o755); err != nil {
		return res, err
	}
	if err := os.Rename(filepath.Join(work, lang.Name+".ext4"), out); err != nil {
		return res, fmt.Errorf("move ext4: %w", err)
	}
	res.Path, res.Metrics, res.Elapsed = out, metrics, time.Since(start)
	return res, b.link(lang.Name, out)
}

// runBuildVM sobe a VM de build com rede (código confiável: só o nosso script), espera
// o fim e devolve a linha RESULT do console. Código ≠ 0 vira erro com o fim do console.
func (b *Builder) runBuildVM(ctx context.Context, exec string, args []string, console string) (string, error) {
	os.Remove(console)
	vm, err := b.Launcher.StartVM(ports.VMSpec{
		CPUs: 4, MemoryMiB: buildVMMemoryMiB, RootDir: b.builderRoot(), Network: true,
		Console: console, Exec: exec, Args: args, Env: buildEnv,
	})
	if err != nil {
		return "", err
	}
	stop := context.AfterFunc(ctx, vm.Kill)
	defer stop()
	code := vm.Wait()
	if ctx.Err() != nil {
		return "", ctx.Err()
	}
	result, tail := scanConsole(console)
	if code != 0 {
		return "", fmt.Errorf("build VM exited with code %d; console tail:\n%s", code, tail)
	}
	return result, nil
}

// scanConsole devolve a linha RESULT e as últimas 20 linhas do console da VM.
func scanConsole(path string) (result, tail string) {
	f, err := os.Open(path)
	if err != nil {
		return "", "(no console)"
	}
	defer f.Close()
	var last []string
	sc := bufio.NewScanner(f)
	// Linha de progresso do apk passa do limite padrão de 64 KiB; sem buffer maior o
	// Scanner para em silêncio e a linha RESULT nunca é lida (medido no build do C).
	sc.Buffer(make([]byte, 64<<10), 4<<20)
	for sc.Scan() {
		line := sc.Text()
		// A linha RESULT pode vir colada no fim da barra de progresso do apk (ESC e \r,
		// sem quebra de linha): procura o marcador em qualquer ponto da linha.
		if i := strings.LastIndex(line, "RESULT "); i >= 0 {
			result = strings.TrimSpace(line[i+len("RESULT "):])
		}
		last = append(last, line)
		if len(last) > 20 {
			last = last[1:]
		}
	}
	return result, strings.Join(last, "\n")
}

// writeFlat grava o sistema de arquivos achatado da imagem: camadas aplicadas em ordem
// e whiteouts resolvidos por mutate.Extract.
func writeFlat(img v1.Image, path string) error {
	f, err := os.Create(path)
	if err != nil {
		return err
	}
	rc := mutate.Extract(img)
	_, err = io.Copy(f, rc)
	rc.Close()
	if cerr := f.Close(); err == nil {
		err = cerr
	}
	if err != nil {
		return fmt.Errorf("flatten layers: %w", err)
	}
	return nil
}

// imageEnv devolve o Env da config OCI (PATH, JAVA_HOME, GOPATH...). As imagens oficiais
// põem os binários fora do PATH padrão (/usr/local/go/bin, /opt/java/openjdk/bin); sem
// este ambiente `go` e `javac` não são achados dentro da VM (medido).
func imageEnv(img v1.Image) ([]string, error) {
	cfg, err := img.ConfigFile()
	if err != nil {
		return nil, fmt.Errorf("read image config: %w", err)
	}
	var env []string
	for _, kv := range cfg.Config.Env {
		// Uma variável por linha no arquivo: valor com quebra de linha não cabe e é descartado.
		if strings.Contains(kv, "=") && !strings.ContainsAny(kv, "\n\r") {
			env = append(env, kv)
		}
	}
	return env, nil
}

// warmupScript exporta o ambiente da imagem antes do warmup, com aspas simples
// escapadas: o valor vem da config da imagem e não pode virar comando no shell.
func warmupScript(env []string, warmup string) string {
	var b strings.Builder
	for _, kv := range env {
		b.WriteString("export '" + strings.ReplaceAll(kv, "'", `'\''`) + "'\n")
	}
	b.WriteString(warmup + "\n")
	return b.String()
}

// copyFile copia src para dst com o modo pedido.
func copyFile(src, dst string, mode os.FileMode) error {
	in, err := os.Open(src)
	if err != nil {
		return err
	}
	defer in.Close()
	out, err := os.OpenFile(dst, os.O_CREATE|os.O_TRUNC|os.O_WRONLY, mode)
	if err != nil {
		return err
	}
	_, err = io.Copy(out, in)
	if cerr := out.Close(); err == nil {
		err = cerr
	}
	return err
}

// link aponta <lang>.ext4 para o build atual, trocando o symlink de forma atômica
// (rename) para um pool que lê no mesmo instante nunca ver o link ausente.
func (b *Builder) link(language, target string) error {
	linkPath := filepath.Join(b.imagesDir(), language+".ext4")
	previous, _ := os.Readlink(linkPath)
	tmp := filepath.Join(b.imagesDir(), "."+language+".ext4.tmp")
	os.Remove(tmp)
	if err := os.Symlink(filepath.Base(target), tmp); err != nil {
		return err
	}
	if err := os.Rename(tmp, linkPath); err != nil {
		return err
	}
	// O ext4 anterior deixa de ser usado: sem apagar, cada rebuild acumulava a imagem
	// inteira (4,4 GB medidos depois de quatro rodadas, 1,1 GB úteis). VM ainda rodando
	// sobre ele não sofre: no Unix o arquivo aberto vive até o último descritor fechar.
	if previous != "" && previous != filepath.Base(target) && !strings.Contains(previous, "/") {
		os.Remove(filepath.Join(b.imagesDir(), previous))
	}
	return nil
}

// DiskTemplate devolve o modelo vazio (ext4 esparso) do disco de máquina com gib GiB,
// criando-o na VM de build na primeira vez. As máquinas recebem um clone dele.
func (b *Builder) DiskTemplate(ctx context.Context, gib int) (string, error) {
	if gib < 1 || gib > 1024 {
		return "", fmt.Errorf("disk size %d GiB out of range [1, 1024]", gib)
	}
	out := filepath.Join(b.CacheDir, "disks", fmt.Sprintf("data-%dg.ext4", gib))
	if _, err := os.Stat(out); err == nil {
		return out, nil
	}
	if err := b.EnsureBuilder(ctx); err != nil {
		return "", err
	}
	work := filepath.Join(b.builderRoot(), "work")
	if err := os.MkdirAll(work, 0o755); err != nil {
		return "", err
	}
	if err := os.WriteFile(filepath.Join(work, "mkdisk.sh"), diskScript, 0o755); err != nil {
		return "", err
	}
	name := fmt.Sprintf("data-%dg.ext4", gib)
	b.logf("[disk] formatting an empty %d GiB ext4 in the build VM", gib)
	if _, err := b.runBuildVM(ctx, "/work/mkdisk.sh", []string{"/work/" + name, fmt.Sprint(gib)}, filepath.Join(work, "mkdisk.console")); err != nil {
		return "", err
	}
	if err := os.MkdirAll(filepath.Dir(out), 0o755); err != nil {
		return "", err
	}
	if err := os.Rename(filepath.Join(work, name), out); err != nil {
		return "", fmt.Errorf("move disk template: %w", err)
	}
	return out, nil
}
