// rc-visitor roda dentro da Fly Machine de cada visitante do playground (claude-in-chrome.runner.codes) e só liga as
// peças de internal/adapters/visitor: fecha a rede privada do Fly com o firewall, prepara o HOME e o /workspace no
// volume, e serve na rede privada o terminal web (Claude Code na aba principal) e a ponte de portas do front.
// Roda como root para o firewall; tudo o que o visitante executa roda como o uid sem privilégio (RC_UID).
//
// Ambiente (posto pelo front ao criar a máquina): RC_TOKEN, RC_PORTS_TOKEN, RC_PUBLIC_HOST, RC_PORT_URL; e da imagem:
// RC_DATA (o volume), RC_UID.
package main

import (
	"context"
	"errors"
	"flag"
	"fmt"
	"io"
	"log"
	"net"
	"net/http"
	"os"
	"os/exec"
	"os/signal"
	"path/filepath"
	"strconv"
	"syscall"
	"time"

	"runner.codes/internal/adapters/localpty"
	"runner.codes/internal/adapters/playground"
	"runner.codes/internal/adapters/visitor"
	"runner.codes/internal/adapters/webterm"
	"runner.codes/internal/ports"
)

// terminalSize é o tamanho do PTY até a página mandar o da janela (o mesmo do rc machine web-terminal).
var terminalSize = ports.Winsize{Rows: 32, Cols: 120}

// mainCommand é a aba principal: o Claude Code sem confirmação de permissão (a máquina inteira é descartável e não
// alcança nada privado) e, quando ele sai, um shell, para a aba não morrer com o /exit.
const mainCommand = "claude --dangerously-skip-permissions; echo; echo 'Claude Code saiu. Rode claude para abrir de novo.'; exec bash -l"

// main falha cedo e alto: sem firewall ou sem token a máquina não deve atender ninguém.
func main() {
	noFirewall := flag.Bool("no-firewall", false, "skip the firewall (local test outside Fly only)")
	flag.Parse()
	if err := run(*noFirewall); err != nil {
		log.Fatal("rc-visitor: ", err)
	}
}

// run aplica o firewall, prepara o volume e serve até SIGTERM (o stop do Fly).
func run(noFirewall bool) error {
	token, portsToken := os.Getenv("RC_TOKEN"), os.Getenv("RC_PORTS_TOKEN")
	if len(token) < 16 || len(portsToken) < 16 {
		return errors.New("RC_TOKEN and RC_PORTS_TOKEN are required")
	}
	data := envOr("RC_DATA", "/data")
	uid, err := strconv.Atoi(envOr("RC_UID", "1000"))
	if err != nil {
		return err
	}
	if err := visitor.LockFlyAPI("/"); err != nil {
		return fmt.Errorf("lock /.fly: %w", err)
	}
	if !noFirewall {
		if err := visitor.ApplyFirewall(func(argv []string) error {
			out, err := exec.Command(argv[0], argv[1:]...).CombinedOutput()
			if err != nil {
				return fmt.Errorf("%w: %s", err, out)
			}
			return nil
		}); err != nil {
			return err
		}
	}
	// O disco raiz da Fly Machine nasce da imagem do Nix, sem /tmp: sem ele, npm, pip e o próprio Claude Code falham.
	if err := os.MkdirAll("/tmp", 0o777); err != nil {
		return err
	}
	if err := os.Chmod("/tmp", 0o777|os.ModeSticky); err != nil {
		return err
	}
	home, work := filepath.Join(data, "home"), filepath.Join(data, "workspace")
	for _, d := range []string{home, work} {
		if err := os.MkdirAll(d, 0o755); err != nil {
			return err
		}
		if err := os.Chown(d, uid, uid); err != nil {
			return err
		}
	}
	ctx, stop := signal.NotifyContext(context.Background(), syscall.SIGTERM, syscall.SIGINT)
	defer stop()

	cred := &syscall.Credential{Uid: uint32(uid), Gid: uint32(uid)}
	env := localpty.Environ(home, "visitor", "DISABLE_AUTOUPDATER=1")
	env = visitor.FilterEnv(env)
	shell := func(argv ...string) webterm.ShellFunc {
		return func(ctx context.Context, size ports.Winsize, in io.Reader, out io.Writer, resize <-chan ports.Winsize) (int, error) {
			return localpty.Run(ctx, localpty.Command{Argv: argv, Env: env, Dir: work, Cred: cred}, size, in, out, resize)
		}
	}
	lister := visitor.Ports{ProcRoot: "/proc", Exclude: []uint16{playground.VisitorTerminalPort, playground.VisitorPortsPort}, HideOwners: []string{"claude"}}
	portURL := os.Getenv("RC_PORT_URL")
	holder := &visitor.Holder{New: func() *webterm.Hub {
		fw := webterm.NewForwarder(lister.List, visitor.Dial, webterm.PollInterval)
		fw.WatchOnly = true
		go fw.Run(ctx)
		main := webterm.Start(ctx, shell("bash", "-lc", mainCommand), terminalSize)
		h := webterm.NewHub(ctx, token, "playground", main, "claude", shell("bash", "-l"), fw)
		h.PublicHost = os.Getenv("RC_PUBLIC_HOST")
		h.PortURL = func(guest uint16, _ int) string { return fmt.Sprintf(portURL, guest) }
		return h
	}}
	bridge := visitor.PortBridge(portsToken, playground.HeaderPort, playground.HeaderToken, visitor.Dial)

	errc := make(chan error, 2)
	serve := func(port int, h http.Handler) *http.Server {
		hs := &http.Server{Handler: h, ReadHeaderTimeout: 10 * time.Second}
		// [::] atende a rede privada do Fly (IPv6); a máquina não tem serviço público, então só o front chega aqui.
		ln, err := net.Listen("tcp", net.JoinHostPort("::", strconv.Itoa(port)))
		if err != nil {
			errc <- err
			return hs
		}
		go func() { errc <- hs.Serve(ln) }()
		return hs
	}
	term := serve(playground.VisitorTerminalPort, holder)
	portSrv := serve(playground.VisitorPortsPort, bridge)
	log.Printf("rc-visitor: terminal on :%d, ports on :%d", playground.VisitorTerminalPort, playground.VisitorPortsPort)
	select {
	case <-ctx.Done():
	case err := <-errc:
		if err != nil && !errors.Is(err, http.ErrServerClosed) {
			return err
		}
	}
	term.Close()
	portSrv.Close()
	return nil
}

// envOr é a variável ou o padrão.
func envOr(k, def string) string {
	if v := os.Getenv(k); v != "" {
		return v
	}
	return def
}
