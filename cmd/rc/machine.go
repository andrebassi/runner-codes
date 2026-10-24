package main

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"os/signal"
	"path/filepath"
	"strings"
	"syscall"
	"text/tabwriter"
	"time"

	"github.com/spf13/cobra"
	"golang.org/x/term"

	"runner.codes/internal/adapters/guestproto"
	"runner.codes/internal/adapters/hostfs"
	"runner.codes/internal/adapters/machinestore"
	"runner.codes/internal/adapters/netgw"
	"runner.codes/internal/app"
	"runner.codes/internal/credca"
	"runner.codes/internal/domain"
	"runner.codes/internal/ports"
	"runner.codes/internal/protocol"
)

// newMachines monta o serviço de máquinas persistentes sobre o cache do host.
func (d *deps) newMachines() *app.Machines {
	return &app.Machines{
		Store: machinestore.New(filepath.Join(d.builder.CacheDir, "machines")), Disks: d.builder,
		Images: d.builder, Catalog: d.catalog, Launcher: d.launcher, Procs: machinestore.Processes{},
		Guest: guestproto.Client{},
		// `image` do runner.code: o ext4 sai do mesmo build das stacks.
		BuildImage: func(ctx context.Context, l domain.Language) error {
			return buildImage(ctx, d, l)
		},
		UpdateEgress: netgw.UpdatePolicy,
		// Caminho curto de propósito: o socket de cada máquina mora aqui e o macOS limita
		// caminho de socket Unix a 104 bytes.
		NetworksDir: filepath.Join(d.builder.CacheDir, "n"),
		NewCredentialCA: func(hosts []string) ([]byte, []byte, error) {
			return credca.New(hosts, time.Now())
		},
		ApplyGuestModes: hostfs.ApplyGuestModes,
	}
}

// defaultMachineName é o nome quando --name falta: $RC_MACHINE_NAME (para fixar uma máquina
// por projeto com direnv), senão "default".
func defaultMachineName() string { return env("RC_MACHINE_NAME", "default") }

// machineCmd agrupa o ciclo de vida das máquinas persistentes.
func machineCmd() *cobra.Command {
	c := &cobra.Command{Use: "machine", Short: "Persistent machines: create, start, exec, cp, logs, stop, delete"}
	c.AddCommand(machineCreateCmd(), machineStartCmd(), machineStopCmd(), machineStatusCmd(), machineListCmd(),
		machineDeleteCmd(), machineExecCmd(), machineShellCmd(), machineWebTerminalCmd(), machineCopyCmd(), machineLogsCmd(), machineUpdateCmd(),
		machineRunCmd(), machineBranchCmd(), machinePauseCmd(), machineResumeCmd(), machineSyncCmd(), machineDataDirCmd(),
		machineScreenCmd(),
		imagePruneCmd("prune"))
	return c
}

// withMachines liga as dependências e entrega o serviço ao corpo do comando.
func withMachines(run func(m *app.Machines, cmd *cobra.Command, args []string) error) func(*cobra.Command, []string) error {
	return func(cmd *cobra.Command, args []string) error {
		d, err := wire()
		if err != nil {
			return err
		}
		return run(d.newMachines(), cmd, args)
	}
}

// machineCreateCmd registra uma máquina e o disco dela (não liga). A declaração vem do
// runner.code (-f, ou ./runner.code se existir) e as flags passadas vencem o arquivo; o que
// vem depois de -- é o workload.
func machineCreateCmd() *cobra.Command {
	var mf *machineFlags
	c := &cobra.Command{
		Use:   "create [flags] [-- workload...]",
		Short: "Create a persistent machine from flags and/or a runner.code file",
		RunE: withMachines(func(m *app.Machines, cmd *cobra.Command, args []string) error {
			mc, file, err := mf.build(cmd.Flags(), args)
			if err != nil {
				return err
			}
			out, err := m.CreateFrom(cmd.Context(), mc, mf.diskFrom)
			if err != nil {
				return err
			}
			from := "stack=" + out.Stack
			if out.Image != "" {
				from = "image=" + out.Image
			}
			fmt.Printf("created %s (%s cpus=%d memory=%dMiB disk=%dGiB gpu=%v)\n", out.Name, from, out.CPUs, out.MemoryMiB, out.DiskGiB, out.GPU)
			if file != "" {
				fmt.Printf("from %s\n", file)
			}
			return nil
		}),
	}
	mf = addMachineFlags(c.Flags(), defaultMachineName())
	return c
}

// machineStartCmd liga a VM destacada e espera o guest responder.
func machineStartCmd() *cobra.Command {
	var name, from string
	c := &cobra.Command{
		Use: "start", Short: "Start a machine (it keeps running after this command exits)",
		RunE: withMachines(func(m *app.Machines, cmd *cobra.Command, _ []string) error {
			if from != "" {
				// O start de um artefato cria a máquina dele na primeira vez.
				if err := ensurePackMachine(cmd.Context(), m, from, name); err != nil {
					return err
				}
			}
			start := time.Now()
			st, err := m.Start(cmd.Context(), name)
			if err != nil {
				return err
			}
			fmt.Printf("started %s (pid %d) in %v\n", st.Name, st.PID, time.Since(start).Round(time.Millisecond))
			return nil
		}),
	}
	c.Flags().StringVar(&name, "name", defaultMachineName(), "machine name")
	c.Flags().StringVar(&from, "from", "", "create the machine from this pack first, if it does not exist")
	return c
}

// machineStopCmd desliga a máquina gravando o disco antes.
func machineStopCmd() *cobra.Command {
	var name string
	var timeout time.Duration
	c := &cobra.Command{
		Use: "stop", Short: "Stop a machine (sync the disk, then exit; kill after --timeout)",
		RunE: withMachines(func(m *app.Machines, cmd *cobra.Command, _ []string) error {
			if err := m.Stop(cmd.Context(), name, timeout); err != nil {
				return err
			}
			fmt.Printf("stopped %s\n", name)
			return nil
		}),
	}
	c.Flags().StringVar(&name, "name", defaultMachineName(), "machine name")
	c.Flags().DurationVar(&timeout, "timeout", -1, "wait for a clean shutdown before killing (0 kills at once; default: the machine's stop_timeout, or 10s)")
	return c
}

// machineStatusCmd mostra uma máquina e o estado dela.
func machineStatusCmd() *cobra.Command {
	var name string
	var asJSON bool
	c := &cobra.Command{
		Use: "status", Short: "Show a machine and whether it is running",
		RunE: withMachines(func(m *app.Machines, _ *cobra.Command, _ []string) error {
			st, err := m.Status(name)
			if err != nil {
				return err
			}
			return printMachines([]app.MachineStatus{st}, asJSON, os.Stdout)
		}),
	}
	c.Flags().StringVar(&name, "name", defaultMachineName(), "machine name")
	c.Flags().BoolVar(&asJSON, "json", false, "JSON output")
	return c
}

// machineListCmd lista todas as máquinas.
func machineListCmd() *cobra.Command {
	var asJSON bool
	c := &cobra.Command{
		Use: "ls", Aliases: []string{"list"}, Short: "List machines",
		RunE: withMachines(func(m *app.Machines, _ *cobra.Command, _ []string) error {
			ms, err := m.List()
			if err != nil {
				return err
			}
			return printMachines(ms, asJSON, os.Stdout)
		}),
	}
	c.Flags().BoolVar(&asJSON, "json", false, "JSON output")
	return c
}

// printMachines imprime em tabela ou JSON.
func printMachines(ms []app.MachineStatus, asJSON bool, w io.Writer) error {
	if asJSON {
		if ms == nil {
			ms = []app.MachineStatus{}
		}
		enc := json.NewEncoder(w)
		enc.SetIndent("", "  ")
		return enc.Encode(ms)
	}
	if len(ms) == 0 {
		// Frase fixa: scripts de limpeza procuram por ela.
		fmt.Fprintln(w, "No machines found")
		return nil
	}
	tw := tabwriter.NewWriter(w, 0, 0, 2, ' ', 0)
	fmt.Fprintln(tw, "NAME\tSTACK\tSTATE\tCPUS\tMEMORY\tDISK\tGPU\tCREATED")
	for _, m := range ms {
		fmt.Fprintf(tw, "%s\t%s\t%s\t%d\t%dMiB\t%dGiB\t%v\t%s\n", m.Name, m.Stack, m.State, m.CPUs, m.MemoryMiB, m.DiskGiB, m.GPU, m.CreatedAt.Format(time.RFC3339))
	}
	return tw.Flush()
}

// machineDeleteCmd apaga a máquina e o disco.
func machineDeleteCmd() *cobra.Command {
	var name string
	var force bool
	c := &cobra.Command{
		Use: "delete", Short: "Delete a machine and its disk",
		RunE: withMachines(func(m *app.Machines, cmd *cobra.Command, _ []string) error {
			if err := m.Delete(cmd.Context(), name, force); err != nil {
				return err
			}
			fmt.Printf("deleted %s\n", name)
			return nil
		}),
	}
	c.Flags().StringVar(&name, "name", defaultMachineName(), "machine name")
	c.Flags().BoolVarP(&force, "force", "f", false, "kill a running machine instead of refusing")
	// --cascade apagaria junto os filhos de um branch vivo; no rc o branch é uma cópia
	// independente do disco, então não há o que levar junto. Aceito para scripts que já o
	// passam rodarem iguais.
	c.Flags().Bool("cascade", false, "accepted for compatibility (rc branches are independent copies)")
	return c
}

// machineDataDirCmd imprime o diretório da máquina no host (disco, console, logs de
// recusa), o `machine data-dir`.
func machineDataDirCmd() *cobra.Command {
	var name string
	c := &cobra.Command{
		Use: "data-dir", Short: "Print the machine's directory on the host",
		RunE: withMachines(func(m *app.Machines, _ *cobra.Command, _ []string) error {
			if _, err := m.Status(name); err != nil {
				return err
			}
			fmt.Println(m.Store.Dir(name))
			return nil
		}),
	}
	c.Flags().StringVar(&name, "name", defaultMachineName(), "machine name")
	return c
}

// machineExecCmd roda um comando na máquina com a saída em streaming; o rc do `rc` é o
// do comando (124 no timeout).
func machineExecCmd() *cobra.Command {
	var name, workdir, user string
	var envs []string
	var stdin bool
	var timeout time.Duration
	c := &cobra.Command{
		Use: "exec [flags] -- COMMAND [ARGS...]", Short: "Run a command in a running machine (output streams as it runs)",
		Args: cobra.MinimumNArgs(1),
		RunE: withMachines(func(m *app.Machines, cmd *cobra.Command, args []string) error {
			req := app.ExecRequest{Argv: args, Env: envs, Cwd: workdir, Timeout: timeout, User: user}
			if stdin {
				b, err := io.ReadAll(os.Stdin)
				if err != nil {
					return err
				}
				req.Stdin = string(b)
			}
			res, err := m.Exec(cmd.Context(), name, req, func(stream, data string) {
				if stream == protocol.EventStderr {
					fmt.Fprint(os.Stderr, data)
					return
				}
				fmt.Print(data)
			})
			if err != nil {
				return err
			}
			if res.TimedOut {
				fmt.Fprintln(os.Stderr, "[timeout]")
				os.Exit(124)
			}
			if res.ExitCode != 0 {
				os.Exit(res.ExitCode)
			}
			return nil
		}),
	}
	c.Flags().StringVar(&name, "name", defaultMachineName(), "machine name")
	c.Flags().StringArrayVarP(&envs, "env", "e", nil, "environment variable K=V (repeatable)")
	c.Flags().StringVarP(&workdir, "workdir", "w", "", "working directory inside the machine (default /)")
	c.Flags().BoolVarP(&stdin, "interactive", "i", false, "send this command's stdin to the program")
	c.Flags().StringVarP(&user, "user", "u", "", "run as this user (passwd name or uid[:gid]); overrides the machine's user")
	c.Flags().DurationVar(&timeout, "timeout", 0, "kill the command after this long (0 = no limit)")
	c.Flags().Bool("stream", true, "accepted for compatibility; output always streams")
	return c
}

// machineShellCmd abre um terminal de verdade (PTY no guest) numa máquina ligada: Ctrl+C,
// job control, editores de tela cheia e o tamanho da janela funcionam. O rc do `rc` é o do
// shell (ou do comando passado depois de --).
func machineShellCmd() *cobra.Command {
	var name, workdir, user string
	var envs []string
	c := &cobra.Command{
		Use: "shell [flags] [-- COMMAND [ARGS...]]", Short: "Open an interactive terminal in a running machine (bash if the image has it)",
		RunE: withMachines(func(m *app.Machines, cmd *cobra.Command, args []string) error {
			req := app.ShellRequest{Argv: args, Env: append([]string{"TERM=" + env("TERM", "xterm")}, envs...), Cwd: workdir, User: user}
			code, err := runMachineShell(cmd.Context(), m, name, req)
			if err != nil {
				return err
			}
			if code != 0 {
				os.Exit(code) // depois do runMachineShell: os defer de lá já restauraram o terminal
			}
			return nil
		}),
	}
	c.Flags().StringVar(&name, "name", defaultMachineName(), "machine name")
	c.Flags().StringArrayVarP(&envs, "env", "e", nil, "environment variable K=V (repeatable)")
	c.Flags().StringVarP(&workdir, "workdir", "w", "", "working directory inside the machine (default $HOME)")
	c.Flags().StringVarP(&user, "user", "u", "", "run as this user (passwd name or uid[:gid]); overrides the machine's user")
	return c
}

// runMachineShell abre o terminal de uma máquina ligada.
func runMachineShell(ctx context.Context, m *app.Machines, name string, req app.ShellRequest) (int, error) {
	return withTerminal(func(size ports.Winsize, resize <-chan ports.Winsize) (int, error) {
		if size.Rows > 0 {
			req.Size = size
		}
		return m.Shell(ctx, name, req, os.Stdin, os.Stdout, resize)
	})
}

// withTerminal põe o terminal do host em modo raw (Ctrl+C, setas e o resto vão para o
// guest) e entrega o tamanho atual e um canal com cada resize (SIGWINCH). Sem terminal
// (pipe) roda direto. O modo raw é desfeito antes de voltar, mesmo em erro.
func withTerminal(run func(size ports.Winsize, resize <-chan ports.Winsize) (int, error)) (int, error) {
	fd := int(os.Stdin.Fd())
	resize := make(chan ports.Winsize, 1)
	var size ports.Winsize
	if term.IsTerminal(fd) && term.IsTerminal(int(os.Stdout.Fd())) {
		if w, h, err := term.GetSize(int(os.Stdout.Fd())); err == nil {
			size = ports.Winsize{Rows: uint16(h), Cols: uint16(w)}
		}
		state, err := term.MakeRaw(fd)
		if err != nil {
			return 0, err
		}
		defer term.Restore(fd, state)
		winch := make(chan os.Signal, 1)
		signal.Notify(winch, syscall.SIGWINCH)
		defer signal.Stop(winch)
		go func() {
			for range winch {
				if w, h, err := term.GetSize(int(os.Stdout.Fd())); err == nil {
					// Não bloqueia: se um resize ainda não saiu, o próximo já traz o tamanho atual.
					select {
					case resize <- ports.Winsize{Rows: uint16(h), Cols: uint16(w)}:
					default:
					}
				}
			}
		}()
	}
	return run(size, resize)
}

// splitMachinePath separa "nome:/caminho"; sem nome antes dos dois-pontos é caminho local.
func splitMachinePath(s string) (name, path string, remote bool) {
	i := strings.Index(s, ":")
	if i <= 0 || strings.HasPrefix(s, "/") || strings.HasPrefix(s, ".") {
		return "", s, false
	}
	return s[:i], s[i+1:], true
}

// machineCopyCmd copia entre o host e a máquina: um dos lados é nome:/caminho.
func machineCopyCmd() *cobra.Command {
	c := &cobra.Command{
		Use: "cp SRC DST", Short: "Copy a file between the host and a machine (name:/path on one side)",
		Args: cobra.ExactArgs(2),
		RunE: withMachines(func(m *app.Machines, cmd *cobra.Command, args []string) error {
			srcName, srcPath, srcRemote := splitMachinePath(args[0])
			dstName, dstPath, dstRemote := splitMachinePath(args[1])
			start := time.Now()
			switch {
			case srcRemote == dstRemote:
				return errors.New("exactly one side must be name:/path")
			case dstRemote:
				f, err := os.Open(srcPath)
				if err != nil {
					return err
				}
				defer f.Close()
				st, err := f.Stat()
				if err != nil {
					return err
				}
				n, err := m.CopyIn(cmd.Context(), dstName, dstPath, f, st.Mode())
				if err != nil {
					return err
				}
				fmt.Fprintf(os.Stderr, "uploaded %d bytes to %s:%s in %v\n", n, dstName, dstPath, time.Since(start).Round(time.Millisecond))
			default:
				// Grava num temporário ao lado e renomeia no fim: atômico também no host.
				tmp, err := os.CreateTemp(filepath.Dir(dstPath), ".rc-cp-*")
				if err != nil {
					return err
				}
				n, err := m.CopyOut(cmd.Context(), srcName, srcPath, tmp)
				cerr := tmp.Close()
				if err == nil {
					err = cerr
				}
				if err == nil {
					err = os.Rename(tmp.Name(), dstPath)
				}
				if err != nil {
					os.Remove(tmp.Name())
					return err
				}
				fmt.Fprintf(os.Stderr, "downloaded %d bytes from %s:%s in %v\n", n, srcName, srcPath, time.Since(start).Round(time.Millisecond))
			}
			return nil
		}),
	}
	return c
}

// machineLogsCmd mostra o console da máquina (funciona com ela parada).
func machineLogsCmd() *cobra.Command {
	var name string
	var tail int
	var source string
	c := &cobra.Command{
		Use: "logs", Short: "Show a machine's console (readable while stopped)",
		RunE: withMachines(func(m *app.Machines, _ *cobra.Command, _ []string) error {
			out, err := m.Logs(name, tail, source)
			if err != nil {
				return err
			}
			fmt.Print(out)
			return nil
		}),
	}
	c.Flags().StringVar(&name, "name", defaultMachineName(), "machine name")
	c.Flags().IntVar(&tail, "tail", 0, "only the last N lines")
	c.Flags().StringVar(&source, "source", app.LogSourceAll, "all, or workload (only the runner.code workload output)")
	return c
}
