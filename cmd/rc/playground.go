package main

import (
	"context"
	"crypto/rand"
	"errors"
	"fmt"
	"io"
	"log"
	"net"
	"net/http"
	"strings"
	"time"

	"github.com/spf13/cobra"

	"runner.code/internal/adapters/playground"
	"runner.code/internal/adapters/runfile"
	"runner.code/internal/adapters/webterm"
	"runner.code/internal/app"
	"runner.code/internal/domain"
	"runner.code/internal/ports"
)

// reapEvery é de quanto em quanto tempo o playground desliga e apaga as máquinas ociosas: 1 min basta para os prazos
// de 30 min e 24 h.
const reapEvery = time.Minute

// playgroundCmd serve o playground público: cada visitante (pelo visitorId do navegador) ganha uma VM própria, branch
// de uma máquina-base, com o Claude Code no terminal web e as portas por subdomínio. Escuta em loopback; quem publica
// é um túnel (Cloudflare Tunnel) até o Host configurado.
func playgroundCmd() *cobra.Command {
	var listen, host, portDomain string
	var maxRunning int
	var idle, maxAge time.Duration
	c := &cobra.Command{
		Use:   "playground",
		Short: "Serve a public playground: one machine per visitor with Claude Code in the browser",
		RunE: withMachines(func(m *app.Machines, cmd *cobra.Command, _ []string) error {
			base, err := runfile.Parse(playground.BaseCode)
			if err != nil {
				return fmt.Errorf("playground base: %w", err)
			}
			base.Name = "playground-base"
			pg := &app.Playground{M: m, Base: base, MaxRunning: maxRunning, IdleStop: idle, MaxAge: maxAge, Now: time.Now}
			secret := make([]byte, 32)
			if _, err := rand.Read(secret); err != nil {
				return err
			}
			ctx := cmd.Context()
			srv := playground.New(playground.Config{
				Host: host, PortDomain: portDomain, Secret: secret,
				Backend: &playground.LocalBackend{
					Ctx: ctx, Secret: secret, EnsureVM: pg.Ensure, TouchVM: pg.Touch,
					NewHub: func(ctx context.Context, machine, token string) *webterm.Hub {
						return playgroundHub(ctx, m, machine, token, host, portDomain)
					},
					Dial: m.DialPort,
				},
				IsFull: func(err error) bool { return errors.Is(err, app.ErrPlaygroundFull) },
			})
			go reapLoop(ctx, pg, cmd.ErrOrStderr())
			ln, err := net.Listen("tcp", listen)
			if err != nil {
				return err
			}
			fmt.Fprintf(cmd.ErrOrStderr(), "playground on http://%s (public: https://%s, ports: https://<port>-<id>.%s)\n", ln.Addr(), host, portDomain)
			hs := &http.Server{Handler: srv, ReadHeaderTimeout: 10 * time.Second}
			go func() { <-ctx.Done(); hs.Close() }()
			if err := hs.Serve(ln); err != nil && !errors.Is(err, http.ErrServerClosed) {
				return err
			}
			return nil
		}),
	}
	c.Flags().StringVar(&listen, "listen", "127.0.0.1:8095", "address to listen on (keep it on loopback; a tunnel publishes it)")
	c.Flags().StringVar(&host, "host", "claude-in-chrome.runner.codes", "public host name of the page")
	c.Flags().StringVar(&portDomain, "port-domain", "runner.codes", "domain of the port subdomains (<port>-<id>.<domain>)")
	c.Flags().IntVar(&maxRunning, "max-running", 3, "visitor machines running at the same time")
	c.Flags().DurationVar(&idle, "idle", 30*time.Minute, "stop a visitor machine after this long without use")
	c.Flags().DurationVar(&maxAge, "max-age", 24*time.Hour, "delete a visitor machine after this long without use")
	return c
}

// playgroundHub abre o terminal web de uma máquina de visitante: o Claude Code sem confirmação de permissão na aba
// principal, a aba shell e as portas acompanhadas sem abrir nada no host (o tráfego vai pelo subdomínio).
func playgroundHub(ctx context.Context, m *app.Machines, machine, token, host, portDomain string) *webterm.Hub {
	env := []string{"TERM=xterm-256color", "COLORTERM=truecolor"}
	shellOf := func(argv []string) webterm.ShellFunc {
		return func(ctx context.Context, size ports.Winsize, in io.Reader, out io.Writer, resize <-chan ports.Winsize) (int, error) {
			return m.Shell(ctx, machine, app.ShellRequest{Argv: argv, Env: env, Size: size}, in, out, resize)
		}
	}
	fw := webterm.NewForwarder(
		func(ctx context.Context) ([]uint16, error) { return m.ListeningPorts(ctx, machine) },
		func(ctx context.Context, p uint16) (net.Conn, error) { return m.DialPort(ctx, machine, p) },
		webterm.PollInterval)
	fw.WatchOnly = true
	go fw.Run(ctx)
	main := webterm.Start(ctx, shellOf([]string{"claude", "--dangerously-skip-permissions"}), webTerminalSize)
	h := webterm.NewHub(ctx, token, "playground", main, "claude", shellOf(nil), fw)
	h.PublicHost = host
	id := strings.TrimPrefix(machine, domain.VisitorPrefix)
	h.PortURL = func(guest uint16, _ int) string { return fmt.Sprintf("https://%d-%s.%s/", guest, id, portDomain) }
	return h
}

// reapLoop desliga e apaga as máquinas ociosas de minuto em minuto, registrando o que fez.
func reapLoop(ctx context.Context, pg *app.Playground, w io.Writer) {
	logger := log.New(w, "playground: ", log.LstdFlags)
	t := time.NewTicker(reapEvery)
	defer t.Stop()
	for {
		select {
		case <-ctx.Done():
			return
		case <-t.C:
			for _, line := range pg.Reap(ctx) {
				logger.Println(line)
			}
		}
	}
}
