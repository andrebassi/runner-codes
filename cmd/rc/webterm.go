package main

import (
	"context"
	"crypto/rand"
	"encoding/hex"
	"errors"
	"fmt"
	"io"
	"net"
	"net/http"
	"os"
	"os/exec"
	"path/filepath"
	"runtime"
	"strconv"
	"time"

	"github.com/spf13/cobra"

	"runner.code/internal/adapters/webterm"
	"runner.code/internal/app"
	"runner.code/internal/ports"
)

// webTerminalSize é o tamanho do PTY até a página mandar o da janela: 120×32 cabe num notebook sem quebrar a tela
// do Claude Code, e o primeiro resize do navegador corrige em milissegundos.
var webTerminalSize = ports.Winsize{Rows: 32, Cols: 120}

// exitGrace é quanto o servidor fica de pé depois de o programa sair, para a página receber o evento de fim.
const exitGrace = 1500 * time.Millisecond

// machineWebTerminalCmd abre o terminal de uma máquina ligada no navegador (xterm.js + PTY no guest), servido só em
// 127.0.0.1 com um token aleatório na URL. O rc do `rc` é o do programa.
func machineWebTerminalCmd() *cobra.Command {
	var name, workdir, user string
	var envs []string
	var port int
	var noOpen bool
	c := &cobra.Command{
		Use:   "web-terminal [flags] [-- COMMAND [ARGS...]]",
		Short: "Open an interactive terminal of a running machine in the browser (127.0.0.1, token in the URL)",
		RunE: withMachines(func(m *app.Machines, cmd *cobra.Command, args []string) error {
			st, err := m.Status(name)
			if err != nil {
				return err
			}
			if st.State != app.StateRunning {
				return fmt.Errorf("machine %s is %s; start it first (rc machine start --name %s)", name, st.State, name)
			}
			env := append([]string{"TERM=xterm-256color", "COLORTERM=truecolor"}, envs...)
			// shellOf liga um PTY na máquina com o argv dado (vazio é o shell padrão da imagem).
			shellOf := func(argv []string) webterm.ShellFunc {
				return func(ctx context.Context, size ports.Winsize, in io.Reader, out io.Writer, resize <-chan ports.Winsize) (int, error) {
					return m.Shell(ctx, name, app.ShellRequest{Argv: argv, Env: env, Cwd: workdir, User: user, Size: size}, in, out, resize)
				}
			}
			spec := webTermSpec{
				Machine:  name,
				Label:    tabLabel(args),
				Shell:    shellOf(args),
				NewShell: shellOf(nil),
				Forwarder: webterm.NewForwarder(
					func(ctx context.Context) ([]uint16, error) { return m.ListeningPorts(ctx, name) },
					func(ctx context.Context, p uint16) (net.Conn, error) { return m.DialPort(ctx, name, p) },
					webterm.PollInterval),
			}
			code, err := serveWebTerminal(cmd.Context(), spec, port, !noOpen, cmd.ErrOrStderr())
			if err != nil {
				return err
			}
			if code != 0 {
				os.Exit(code)
			}
			return nil
		}),
	}
	c.Flags().StringVar(&name, "name", defaultMachineName(), "machine name")
	c.Flags().StringArrayVarP(&envs, "env", "e", nil, "environment variable K=V (repeatable)")
	c.Flags().StringVarP(&workdir, "workdir", "w", "", "working directory inside the machine (default: the machine's)")
	c.Flags().StringVarP(&user, "user", "u", "", "run as this user (passwd name or uid[:gid]); overrides the machine's user")
	c.Flags().IntVar(&port, "port", 0, "port on 127.0.0.1 (0 picks a free one)")
	c.Flags().BoolVar(&noOpen, "no-open", false, "print the URL without opening the browser")
	return c
}

// webTermSpec descreve a página: a máquina, o rótulo e o programa da aba principal, a fábrica da aba "shell" e o
// encaminhamento de portas (os dois últimos podem ser nil).
type webTermSpec struct {
	Machine, Label  string
	Shell, NewShell webterm.ShellFunc
	Forwarder       *webterm.Forwarder
}

// tabLabel é o nome da aba principal: o programa sem o caminho (claude para /usr/bin/claude), ou "shell" sem comando.
func tabLabel(argv []string) string {
	if len(argv) == 0 {
		return "shell"
	}
	return filepath.Base(argv[0])
}

// serveWebTerminal liga a sessão principal, serve em 127.0.0.1:port e espera o programa dela sair (ou o Ctrl+C de
// quem rodou). Imprime a URL com o token em w e, se open, abre o navegador.
func serveWebTerminal(ctx context.Context, spec webTermSpec, port int, open bool, w io.Writer) (int, error) {
	// 127.0.0.1 fixo: o terminal é um shell na máquina, e nunca deve ficar alcançável pela rede do host.
	ln, err := net.Listen("tcp", net.JoinHostPort("127.0.0.1", strconv.Itoa(port)))
	if err != nil {
		return 0, fmt.Errorf("web terminal: %w", err)
	}
	token, err := newToken()
	if err != nil {
		ln.Close()
		return 0, err
	}
	ctx, cancel := context.WithCancel(ctx)
	defer cancel()
	sess := webterm.Start(ctx, spec.Shell, webTerminalSize)
	if spec.Forwarder != nil {
		go spec.Forwarder.Run(ctx)
	}
	hub := webterm.NewHub(ctx, token, spec.Machine, sess, spec.Label, spec.NewShell, spec.Forwarder)
	srv := &http.Server{Handler: webterm.Handler(hub), ReadHeaderTimeout: 10 * time.Second}
	go srv.Serve(ln)
	defer srv.Close()

	url := fmt.Sprintf("http://%s/?t=%s", ln.Addr().String(), token)
	fmt.Fprintf(w, "web terminal: %s\n(Ctrl+C here ends the session)\n", url)
	if open {
		if err := openBrowser(url); err != nil {
			fmt.Fprintf(w, "could not open the browser (%v); open the URL above\n", err)
		}
	}
	select {
	case <-sess.Done():
		time.Sleep(exitGrace)
	case <-ctx.Done():
	}
	code, err := sess.Wait()
	if err != nil && !errors.Is(err, context.Canceled) {
		return code, err
	}
	return code, nil
}

// newToken gera 128 bits aleatórios em hex: sem ele, qualquer página aberta no navegador poderia mandar teclas para
// o shell em 127.0.0.1.
func newToken() (string, error) {
	b := make([]byte, 16)
	if _, err := rand.Read(b); err != nil {
		return "", fmt.Errorf("web terminal token: %w", err)
	}
	return hex.EncodeToString(b), nil
}

// browserCommand é o abridor de URL do sistema: open no macOS, xdg-open no Linux. Variável para o teste trocar.
var browserCommand = func(url string) *exec.Cmd {
	if runtime.GOOS == "darwin" {
		return exec.Command("open", url)
	}
	return exec.Command("xdg-open", url)
}

// openBrowser abre a URL no navegador padrão sem esperar ele fechar.
func openBrowser(url string) error {
	c := browserCommand(url)
	c.Stdout, c.Stderr = nil, nil
	if err := c.Start(); err != nil {
		return err
	}
	go c.Wait()
	return nil
}
