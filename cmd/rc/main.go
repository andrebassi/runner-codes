// Command rc é o binário do host: serviço HTTP com pool de VMs, CLI de
// execução e build de imagem, e o processo `vmm` (uma microVM libkrun por processo).
// O que roda dentro da VM é outro binário, cmd/runner-guest. Este arquivo só liga as peças.
package main

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"os"
	"os/exec"
	"os/signal"
	"path/filepath"
	"runtime"
	"strings"
	"syscall"
	"time"

	"github.com/spf13/cobra"
	"golang.org/x/term"

	"runner.codes/internal/adapters/catalog"
	"runner.codes/internal/adapters/guestproto"
	"runner.codes/internal/adapters/httpapi"
	"runner.codes/internal/adapters/krun"
	"runner.codes/internal/adapters/oci"
	"runner.codes/internal/app"
	"runner.codes/internal/domain"
	"runner.codes/internal/ports"
	"runner.codes/internal/protocol"
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

// libPath é a libkrun. O padrão é o perfil do Nix (`task krun:install` instala `.#krun` ali,
// nos dois sistemas). No Linux, sem o perfil, cai no soname, que o loader resolve pelos
// caminhos do sistema (pacote da distro).
func libPath() string {
	if p := os.Getenv("RUNNER_KRUN_LIB"); p != "" {
		return p
	}
	home, _ := os.UserHomeDir()
	if runtime.GOOS == "darwin" {
		return filepath.Join(home, ".nix-profile/lib/libkrun.dylib")
	}
	if p := filepath.Join(home, ".nix-profile/lib/libkrun.so.1"); fileExists(p) {
		return p
	}
	return "libkrun.so.1"
}

// fileExists diz se o caminho existe; erro de permissão conta como inexistente, e o Load
// seguinte reporta o motivo real.
func fileExists(p string) bool {
	_, err := os.Stat(p)
	return err == nil
}

// guestBinary é o guest Linux injetado nas imagens; o build o deixa ao lado do host.
func guestBinary() string {
	if p := os.Getenv("RUNNER_GUEST_BIN"); p != "" {
		return p
	}
	exe, _ := os.Executable()
	return filepath.Join(filepath.Dir(exe), "rc-guest-linux-"+runtime.GOARCH)
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
		Fetch: oci.Fetch, Source: oci.AlpineSources[runtime.GOARCH], Log: os.Stderr,
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
	root := &cobra.Command{Use: "rc", Short: "Run untrusted code in libkrun microVMs", Version: Version, SilenceUsage: true}
	root.AddCommand(vmmCmd(), imageCmd(), runCmd(), shellCmd(), machineCmd(), serveCmd(), languagesCmd(), doctorCmd(), packCmd(), playgroundCmd(), mcpCmd())
	// Executável com payload (um artefato de `rc pack create`): os subcomandos dele viram os
	// da máquina empacotada. O vmm é o próprio stub chamado pelo launcher, e passa direto.
	if exe, err := os.Executable(); err == nil && (len(os.Args) < 2 || os.Args[1] != "vmm") {
		args, isPack, err := packMode(exe, os.Args[1:])
		if err != nil {
			fmt.Fprintln(os.Stderr, err)
			os.Exit(2)
		}
		if isPack {
			root.SetArgs(args)
		}
	}
	if err := root.Execute(); err != nil {
		os.Exit(1)
	}
}

// vmmCmd é o processo de uma VM; o launcher chama, não o usuário.
func vmmCmd() *cobra.Command {
	var spec, lib string
	var watchFD int
	c := &cobra.Command{
		Use: "vmm", Hidden: true, Short: "Process of one microVM (internal use)",
		RunE: func(*cobra.Command, []string) error {
			s, err := krun.ReadSpec(spec)
			if err != nil {
				return err
			}
			api, err := krun.Load(lib)
			if err != nil {
				return err
			}
			// io.Reader, não *os.File: um *os.File nil dentro da interface a deixa não nula,
			// o vigia lê, falha na hora e mata a VM com 137 (medido rodando vmm à mão).
			var watch io.Reader
			if watchFD > 0 {
				watch = os.NewFile(uintptr(watchFD), "watch")
			}
			return krun.RunVMM(api, s, watch)
		},
	}
	c.Flags().StringVar(&spec, "spec", "", "spec JSON file")
	c.Flags().StringVar(&lib, "lib", libPath(), "path to libkrun")
	c.Flags().IntVar(&watchFD, "watch-fd", 0, "fd of the parent watchdog pipe")
	return c
}

// imageCmd agrupa o build de imagens.
func imageCmd() *cobra.Command {
	var langs []string
	var all bool
	build := &cobra.Command{
		Use: "build", Short: "Build the ext4 image of a stack from its official OCI image",
		RunE: func(cmd *cobra.Command, _ []string) error {
			d, err := wire()
			if err != nil {
				return err
			}
			if all {
				langs = d.catalog.Names()
			}
			if len(langs) == 0 {
				return errors.New("pass --stack or --all")
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
	build.Flags().StringSliceVar(&langs, "stack", nil, "stacks to build (repeatable)")
	build.Flags().BoolVar(&all, "all", false, "every stack in the catalog")
	img := &cobra.Command{Use: "image", Short: "Stack images"}
	img.AddCommand(build, imagePruneCmd("prune"))
	return img
}

// runCmd executa um job pela CLI, com boot frio (sem pool).
func runCmd() *cobra.Command {
	var lang, code, file, stdin string
	var timeout time.Duration
	var gpu bool
	c := &cobra.Command{
		Use: "run", Short: "Run code in a fresh microVM",
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
			// A saída aparece enquanto o programa roda: cada pedaço vai direto para o
			// stdout/stderr do host, e o Result final só traz o status.
			res, err := exec.RunStream(cmd.Context(), domain.Job{TraceID: "cli", Language: lang, Code: code, Stdin: stdin, Timeout: timeout, GPU: gpu},
				func(stream, data string) {
					if stream == protocol.EventStderr {
						fmt.Fprint(os.Stderr, data)
						return
					}
					fmt.Print(data)
				})
			if err != nil {
				return err
			}
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
	c.Flags().StringVar(&lang, "stack", "", "stack (python, go, rust, ...)")
	c.Flags().StringVar(&code, "code", "", "source code")
	c.Flags().StringVar(&file, "file", "", "file with the source code")
	c.Flags().StringVar(&stdin, "stdin", "", "stdin for the program")
	c.Flags().DurationVar(&timeout, "timeout", domain.DefaultTimeout, "job timeout")
	c.Flags().BoolVar(&gpu, "gpu", false, "give the VM the host GPU through Vulkan (virtio-gpu/Venus); the stack must declare gpu")
	return c
}

// shellCmd abre um shell interativo numa VM nova da stack: mesmo root read-only, sem rede,
// /tmp gravável. Relança o próprio binário como `vmm` em primeiro plano, com o terminal
// ligado ao console da VM; relançar é obrigatório porque o loader só lê a variável que
// acha a libkrunfw (DYLD_LIBRARY_PATH/LD_LIBRARY_PATH) quando o processo nasce.
func shellCmd() *cobra.Command {
	var stack string
	var gpu bool
	c := &cobra.Command{
		Use: "shell", Short: "Open an interactive shell inside a fresh microVM of a stack",
		RunE: func(*cobra.Command, []string) error {
			d, err := wire()
			if err != nil {
				return err
			}
			l, ok := d.catalog.Get(stack)
			if !ok {
				return domain.ErrUnsupportedLanguage{Name: stack}
			}
			image, ok := d.builder.ImagePath(stack)
			if !ok {
				return fmt.Errorf("image for %s not built (run `rc image build --stack %s`)", stack, stack)
			}
			if err := (domain.Job{GPU: gpu}).CheckGPU(l); err != nil {
				return err
			}
			// Com terminal de verdade no stdin a VM ganha console TTY (/dev/hvc0): Ctrl+C,
			// job control e tamanho da janela funcionam. Com pipe (script, teste) fica o
			// console implícito, que entrega stdin/stdout como fluxo.
			interactive := term.IsTerminal(int(os.Stdin.Fd())) && term.IsTerminal(int(os.Stdout.Fd()))
			spec := ports.VMSpec{CPUs: l.CPUs, MemoryMiB: l.MemoryMiB, RootDisk: image, Exec: "/opt/runner/shell.sh",
				Env: []string{"TERM=" + env("TERM", "xterm")}, TTY: interactive}
			if gpu {
				spec.GPU, spec.GPUVRAMMiB = true, l.GPUVRAMMiB
			}
			if err := os.MkdirAll(d.launcher.WorkDir, 0o700); err != nil {
				return err
			}
			// Nome único: dois `rc shell` da mesma stack não podem apagar a spec um do outro.
			f, err := os.CreateTemp(d.launcher.WorkDir, "shell-"+stack+"-*.json")
			if err != nil {
				return err
			}
			specPath := f.Name()
			raw, _ := json.Marshal(spec)
			_, err = f.Write(raw)
			f.Close()
			if err != nil {
				return err
			}
			defer os.Remove(specPath)
			cmd := exec.Command(d.launcher.Exe, "vmm", "--spec", specPath, "--lib", d.launcher.LibPath)
			cmd.Stdin, cmd.Stdout, cmd.Stderr = os.Stdin, os.Stdout, os.Stderr
			cmd.Env = os.Environ()
			if d.launcher.LibDir != "" {
				cmd.Env = append(cmd.Env, libPathEnv()+"="+d.launcher.LibDir)
			}
			code, err := runInTerminal(cmd, interactive)
			if err != nil {
				return err
			}
			if code != 0 {
				os.Remove(specPath) // os.Exit não roda os defer
				os.Exit(code)
			}
			return nil
		},
	}
	c.Flags().StringVar(&stack, "stack", "", "stack whose image the VM boots (python, go, rust, ...)")
	c.Flags().BoolVar(&gpu, "gpu", false, "give the VM the host GPU through Vulkan (virtio-gpu/Venus)")
	c.MarkFlagRequired("stack")
	return c
}

// runInTerminal roda o processo da VM. Interativo, põe o terminal do host em modo raw:
// sem isso o driver de terminal do host converte Ctrl+C em SIGINT e mata o rc e a VM; em
// raw, o byte 0x03 segue para o terminal do guest, que interrompe só o comando. O
// terminal é restaurado antes de devolver o código, porque o chamador pode sair com
// os.Exit, que pularia um defer.
func runInTerminal(cmd *exec.Cmd, interactive bool) (int, error) {
	if interactive {
		state, err := term.MakeRaw(int(os.Stdin.Fd()))
		if err != nil {
			return 0, fmt.Errorf("raw terminal: %w", err)
		}
		defer term.Restore(int(os.Stdin.Fd()), state)
	}
	err := cmd.Run()
	var ee *exec.ExitError
	if errors.As(err, &ee) {
		return ee.ExitCode(), nil
	}
	return 0, err
}

// libPathEnv é a variável do loader que acha a libkrunfw no SO atual.
func libPathEnv() string {
	if runtime.GOOS == "darwin" {
		return "DYLD_LIBRARY_PATH"
	}
	return "LD_LIBRARY_PATH"
}

// serveCmd sobe a API com o pool; SIGINT/SIGTERM fecham o servidor e matam as VMs.
func serveCmd() *cobra.Command {
	var listen string
	var poolSize int
	var allowNested bool
	c := &cobra.Command{
		Use: "serve", Short: "HTTP API with a pool of pre-booted VMs",
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
			srv := &http.Server{Addr: listen, Handler: (&httpapi.Server{Runner: exec, Version: Version, Pool: pool.Status, Machines: d.newMachines(), AllowNested: allowNested, Languages: func() []httpapi.LanguageInfo {
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
			fmt.Fprintf(os.Stderr, "listening on %s; pool=%d per stack; images ready: %v\n", listen, poolSize, built)
			if err := srv.ListenAndServe(); !errors.Is(err, http.ErrServerClosed) {
				return err
			}
			return nil
		},
	}
	c.Flags().StringVar(&listen, "listen", env("RUNNER_LISTEN", "127.0.0.1:8080"), "listen address (loopback by default: the API has no authentication)")
	c.Flags().IntVar(&poolSize, "pool-size", 2, "ready VMs per stack")
	c.Flags().BoolVar(&allowNested, "allow-nested-virt", false, "accept nestedVirt in machine create bodies (off: a client could otherwise hand guests the host's virtualization)")
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
		Use: "languages", Short: "List the stacks and whether their image is built",
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
		Use: "doctor", Short: "Check whether this host can run microVMs",
		RunE: func(*cobra.Command, []string) error {
			d, err := wire()
			if err != nil {
				return err
			}
			fail := 0
			check := func(name string, err error) {
				if err != nil {
					fail++
					fmt.Printf("FAIL  %-12s %v\n", name, err)
					return
				}
				fmt.Printf("ok    %s\n", name)
			}
			check("platform", platformOK())
			api, err := krun.Load(d.launcher.LibPath)
			check("libkrun", err)
			// GPU é opcional: a libkrun sem GPU=1 (a do nixpkgs no Linux) só não aceita --gpu.
			if g, ok := api.(interface{ HasGPU() bool }); ok && g.HasGPU() {
				fmt.Println("ok    gpu: libkrun built with virtio-gpu (Venus); use --gpu on a stack with gpu")
			} else if err == nil {
				fmt.Println("info  gpu: this libkrun has no virtio-gpu; --gpu is unavailable")
			}
			// CUDA é a pergunta que um preflight responde sem ligar VM: aqui a resposta
			// é sempre não, e dita, para ninguém ler um teste de Vulkan como sim.
			fmt.Println("info  cuda: not supported (no CUDA remoting; --gpu is Vulkan)")
			// Nested depende do hardware (M3+ no Mac, KVM aninhado no Linux): o doctor responde
			// antes de alguém descobrir no start de uma máquina --nested.
			if n, ok := api.(krun.NestedAPI); ok {
				switch rc := n.CheckNestedVirt(); {
				case rc == 1:
					fmt.Println("ok    nested: supported (--nested)")
				case rc == 0:
					fmt.Println("info  nested: not supported on this host; --nested is refused")
				default:
					fmt.Printf("info  nested: check failed (%d); --nested is refused\n", rc)
				}
			}
			_, err = os.Stat(d.builder.GuestBinary)
			check("guest", err)
			// O confinamento do processo vmm depende do kernel: diz o que ele cobre aqui.
			switch abi := krun.LandlockABI(); {
			case !krun.SandboxSupported:
				fmt.Println("info  sandbox: macOS has no Landlock/seccomp; the VM process runs unconfined")
			case abi == 0:
				check("sandbox", errors.New("landlock unavailable: VM processes run without filesystem confinement"))
			default:
				fmt.Printf("ok    sandbox landlock ABI %d + seccomp denylist\n", abi)
				if abi < 9 {
					fmt.Printf("warn  landlock ABI %d < 9: a VM process can still connect to another VM's unix socket\n", abi)
				}
			}
			for _, li := range languageInfo(d) {
				if li.Ready {
					fmt.Printf("ok    image %s\n", li.Name)
				}
			}
			if fail > 0 {
				return fmt.Errorf("%d check(s) failed", fail)
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
			return errors.New("libkrun on macOS only runs on Apple Silicon")
		}
		return nil
	case "linux":
		f, err := os.OpenFile("/dev/kvm", os.O_RDWR, 0)
		if err != nil {
			return fmt.Errorf("no access to /dev/kvm: %w", err)
		}
		f.Close()
		return nil
	}
	return fmt.Errorf("%s is not supported", runtime.GOOS)
}
