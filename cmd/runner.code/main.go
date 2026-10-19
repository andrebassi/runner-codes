// Command runner.code é o binário do host: serviço HTTP com pool de VMs, CLI de
// execução e build de imagem, e o processo `vmm` (uma microVM libkrun por processo).
// O que roda dentro da VM é outro binário, cmd/runner-guest. Este arquivo só liga as peças.
package main

import (
	"context"
	"errors"
	"fmt"
	"net/http"
	"os"
	"os/signal"
	"path/filepath"
	"runtime"
	"strings"
	"syscall"
	"time"

	"github.com/spf13/cobra"

	"runner.code/internal/adapters/catalog"
	"runner.code/internal/adapters/guestproto"
	"runner.code/internal/adapters/httpapi"
	"runner.code/internal/adapters/krun"
	"runner.code/internal/adapters/oci"
	"runner.code/internal/app"
	"runner.code/internal/domain"
)

// Version é preenchida no release por -ldflags "-X main.Version=<tag>".
var Version = "dev"

// env lê uma variável com valor padrão.
func env(key, def string) string {
	if v := os.Getenv(key); v != "" {
		return v
	}
	return def
}

// cacheDir é onde ficam imagens, VM de build e sockets.
func cacheDir() string {
	if d := os.Getenv("RUNNER_CACHE_DIR"); d != "" {
		return d
	}
	base, err := os.UserCacheDir()
	if err != nil {
		base = os.TempDir()
	}
	return filepath.Join(base, "runner-codes")
}

// libPath é a libkrun: no macOS o caminho do brew `slp/krun`; no Linux o soname, que o
// loader resolve pelos caminhos do sistema (nixpkgs ou pacote da distro).
func libPath() string {
	if runtime.GOOS == "darwin" {
		return env("RUNNER_KRUN_LIB", "/opt/homebrew/lib/libkrun.dylib")
	}
	return env("RUNNER_KRUN_LIB", "libkrun.so.1")
}

// guestBinary é o guest Linux injetado nas imagens; o build o deixa ao lado do host.
func guestBinary() string {
	if p := os.Getenv("RUNNER_GUEST_BIN"); p != "" {
		return p
	}
	exe, _ := os.Executable()
	return filepath.Join(filepath.Dir(exe), "runner.code-guest-linux-"+runtime.GOARCH)
}

// deps reúne as peças montadas para os subcomandos.
type deps struct {
	catalog  *catalog.Catalog
	launcher *krun.Launcher
	builder  *oci.Builder
}

// wire monta catálogo, launcher e builder a partir do ambiente.
func wire() (*deps, error) {
	cat, err := catalog.Default()
	if err != nil {
		return nil, err
	}
	exe, err := os.Executable()
	if err != nil {
		return nil, err
	}
	lib := libPath()
	libDir := ""
	if strings.Contains(lib, "/") {
		libDir = filepath.Dir(lib)
	}
	cache := cacheDir()
	l := &krun.Launcher{Exe: exe, LibPath: lib, LibDir: libDir, WorkDir: filepath.Join(cache, "run"), Stderr: os.Stderr}
	b := &oci.Builder{
		CacheDir: cache, Arch: runtime.GOARCH, GuestBinary: guestBinary(), Launcher: l,
		Fetch: oci.RemoteFetcher, Source: oci.AlpineSources[runtime.GOARCH], Log: os.Stderr,
	}
	return &deps{catalog: cat, launcher: l, builder: b}, nil
}

// newExecutor monta pool e executor com o tamanho de pool pedido.
func (d *deps) newExecutor(poolSize int) (*app.Executor, *app.Pool) {
	g := guestproto.Client{}
	p := &app.Pool{
		Launcher: d.launcher, Guest: g, Images: d.builder, Catalog: d.catalog,
		WorkDir: d.launcher.WorkDir, Size: poolSize, Log: os.Stderr,
	}
	return &app.Executor{VMs: p, Guest: g, Images: d.builder, Catalog: d.catalog}, p
}

// main registra os subcomandos.
func main() {
	root := &cobra.Command{Use: "runner.code", Short: "Executa código não confiável em microVMs libkrun", Version: Version, SilenceUsage: true}
	root.AddCommand(vmmCmd(), imageCmd(), runCmd(), serveCmd(), languagesCmd(), doctorCmd())
	if err := root.Execute(); err != nil {
		os.Exit(1)
	}
}

// vmmCmd é o processo de uma VM; o launcher chama, não o usuário.
func vmmCmd() *cobra.Command {
	var spec, lib string
	var watchFD int
	c := &cobra.Command{
		Use: "vmm", Hidden: true, Short: "Processo de uma microVM (uso interno)",
		RunE: func(*cobra.Command, []string) error {
			s, err := krun.ReadSpec(spec)
			if err != nil {
				return err
			}
			api, err := krun.Load(lib)
			if err != nil {
				return err
			}
			var watch *os.File
			if watchFD > 0 {
				watch = os.NewFile(uintptr(watchFD), "watch")
			}
			return krun.RunVMM(api, s, watch)
		},
	}
	c.Flags().StringVar(&spec, "spec", "", "arquivo JSON da spec")
	c.Flags().StringVar(&lib, "lib", libPath(), "caminho da libkrun")
	c.Flags().IntVar(&watchFD, "watch-fd", 0, "fd do pipe de vigia do pai")
	return c
}

// imageCmd agrupa o build de imagens.
func imageCmd() *cobra.Command {
	var langs []string
	var all bool
	build := &cobra.Command{
		Use: "build", Short: "Gera o ext4 da linguagem a partir da imagem OCI oficial",
		RunE: func(cmd *cobra.Command, _ []string) error {
			d, err := wire()
			if err != nil {
				return err
			}
			if all {
				langs = d.catalog.Names()
			}
			if len(langs) == 0 {
				return errors.New("informe --lang ou --all")
			}
			for _, name := range langs {
				l, ok := d.catalog.Get(name)
				if !ok {
					return domain.ErrUnsupportedLanguage{Name: name}
				}
				res, err := d.builder.Build(cmd.Context(), l)
				if err != nil {
					return fmt.Errorf("%s: %w", name, err)
				}
				fmt.Printf("%s %s cached=%v elapsed=%v %s\n", name, res.Path, res.Cached, res.Elapsed.Round(time.Millisecond), res.Metrics)
			}
			return nil
		},
	}
	build.Flags().StringSliceVar(&langs, "lang", nil, "linguagens (repetível)")
	build.Flags().BoolVar(&all, "all", false, "todas do catálogo")
	img := &cobra.Command{Use: "image", Short: "Imagens das linguagens"}
	img.AddCommand(build)
	return img
}

// runCmd executa um job pela CLI, com boot frio (sem pool).
func runCmd() *cobra.Command {
	var lang, code, file, stdin string
	var timeout time.Duration
	c := &cobra.Command{
		Use: "run", Short: "Executa um código numa microVM nova",
		RunE: func(cmd *cobra.Command, _ []string) error {
			d, err := wire()
			if err != nil {
				return err
			}
			if file != "" {
				b, err := os.ReadFile(file)
				if err != nil {
					return err
				}
				code = string(b)
			}
			exec, pool := d.newExecutor(0)
			pool.Start(nil)
			defer pool.Close()
			start := time.Now()
			res, err := exec.Run(cmd.Context(), domain.Job{TraceID: "cli", Language: lang, Code: code, Stdin: stdin, Timeout: timeout})
			if err != nil {
				return err
			}
			fmt.Print(res.Stdout)
			fmt.Fprint(os.Stderr, res.Stderr)
			fmt.Fprintf(os.Stderr, "[status=%s exit=%d exec=%v total=%v timeout=%v]\n", res.Status, res.ExitCode,
				res.Duration.Round(time.Millisecond), time.Since(start).Round(time.Millisecond), res.TimedOut)
			// status "success" só diz que a API executou; o programa falhou se saiu ≠ 0, estourou
			// o tempo ou não compilou (exception "Compilation failed" com exit 0, medido no Java).
			if res.TimedOut || res.ExitCode != 0 || res.Exception != "" || res.Status != "success" {
				os.Exit(1)
			}
			return nil
		},
	}
	c.Flags().StringVar(&lang, "lang", "", "linguagem")
	c.Flags().StringVar(&code, "code", "", "código")
	c.Flags().StringVar(&file, "file", "", "arquivo com o código")
	c.Flags().StringVar(&stdin, "stdin", "", "stdin")
	c.Flags().DurationVar(&timeout, "timeout", domain.DefaultTimeout, "timeout do job")
	return c
}

// serveCmd sobe a API com o pool; SIGINT/SIGTERM fecham o servidor e matam as VMs.
func serveCmd() *cobra.Command {
	var listen string
	var poolSize int
	c := &cobra.Command{
		Use: "serve", Short: "API HTTP com pool de VMs pré-bootadas",
		RunE: func(*cobra.Command, []string) error {
			d, err := wire()
			if err != nil {
				return err
			}
			exec, pool := d.newExecutor(poolSize)
			var built []string
			for _, n := range d.catalog.Names() {
				if _, ok := d.builder.ImagePath(n); ok {
					built = append(built, n)
				}
			}
			pool.Start(built)
			defer pool.Close()
			srv := &http.Server{Addr: listen, Handler: (&httpapi.Server{Runner: exec, Version: Version, Languages: func() []httpapi.LanguageInfo {
				return languageInfo(d)
			}}).Handler(), ReadHeaderTimeout: 10 * time.Second}
			ctx, stop := signal.NotifyContext(context.Background(), syscall.SIGINT, syscall.SIGTERM)
			defer stop()
			go func() {
				<-ctx.Done()
				shut, cancel := context.WithTimeout(context.Background(), 10*time.Second)
				defer cancel()
				srv.Shutdown(shut)
			}()
			fmt.Fprintf(os.Stderr, "ouvindo em %s; pool=%d por linguagem; imagens prontas: %v\n", listen, poolSize, built)
			if err := srv.ListenAndServe(); !errors.Is(err, http.ErrServerClosed) {
				return err
			}
			return nil
		},
	}
	c.Flags().StringVar(&listen, "listen", env("RUNNER_LISTEN", "127.0.0.1:8080"), "endereço (padrão só loopback: a API não tem autenticação)")
	c.Flags().IntVar(&poolSize, "pool-size", 2, "VMs prontas por linguagem")
	return c
}

// languageInfo lista o catálogo com o estado da imagem.
func languageInfo(d *deps) []httpapi.LanguageInfo {
	var out []httpapi.LanguageInfo
	for _, n := range d.catalog.Names() {
		l, _ := d.catalog.Get(n)
		_, ready := d.builder.ImagePath(n)
		out = append(out, httpapi.LanguageInfo{Name: n, Image: l.Image, Ready: ready})
	}
	return out
}

// languagesCmd imprime o catálogo e se a imagem existe.
func languagesCmd() *cobra.Command {
	return &cobra.Command{
		Use: "languages", Short: "Lista as linguagens e se a imagem já foi construída",
		RunE: func(*cobra.Command, []string) error {
			d, err := wire()
			if err != nil {
				return err
			}
			for _, li := range languageInfo(d) {
				fmt.Printf("%-8s ready=%-5v %s\n", li.Name, li.Ready, li.Image)
			}
			return nil
		},
	}
}

// doctorCmd confere o que o host precisa para rodar VMs, um motivo por item.
func doctorCmd() *cobra.Command {
	return &cobra.Command{
		Use: "doctor", Short: "Confere se este host consegue rodar microVMs",
		RunE: func(*cobra.Command, []string) error {
			d, err := wire()
			if err != nil {
				return err
			}
			fail := 0
			check := func(name string, err error) {
				if err != nil {
					fail++
					fmt.Printf("FALHA %-12s %v\n", name, err)
					return
				}
				fmt.Printf("ok    %s\n", name)
			}
			check("plataforma", platformOK())
			_, err = krun.Load(d.launcher.LibPath)
			check("libkrun", err)
			_, err = os.Stat(d.builder.GuestBinary)
			check("guest", err)
			for _, li := range languageInfo(d) {
				if li.Ready {
					fmt.Printf("ok    imagem %s\n", li.Name)
				}
			}
			if fail > 0 {
				return fmt.Errorf("%d verificação(ões) falharam", fail)
			}
			return nil
		},
	}
}

// platformOK diz se o SO tem hypervisor que a libkrun upstream suporta.
func platformOK() error {
	switch runtime.GOOS {
	case "darwin":
		if runtime.GOARCH != "arm64" {
			return errors.New("libkrun no macOS só roda em Apple Silicon")
		}
		return nil
	case "linux":
		f, err := os.OpenFile("/dev/kvm", os.O_RDWR, 0)
		if err != nil {
			return fmt.Errorf("sem acesso a /dev/kvm: %w", err)
		}
		f.Close()
		return nil
	}
	return fmt.Errorf("%s não é suportado", runtime.GOOS)
}
