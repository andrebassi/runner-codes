package main

import (
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"text/tabwriter"
	"time"

	"github.com/spf13/cobra"

	"runner.code/internal/adapters/guestproto"
	"runner.code/internal/adapters/machinestore"
	"runner.code/internal/app"
	"runner.code/internal/domain"
	"runner.code/internal/protocol"
)

// newMachines monta o serviço de máquinas persistentes sobre o cache do host.
func (d *deps) newMachines() *app.Machines {
	return &app.Machines{
		Store: machinestore.New(filepath.Join(d.builder.CacheDir, "machines")), Disks: d.builder,
		Images: d.builder, Catalog: d.catalog, Launcher: d.launcher, Procs: machinestore.Processes{},
		Guest: guestproto.Client{},
	}
}

// defaultMachineName é o nome quando --name falta: $RC_MACHINE_NAME (para fixar uma máquina
// por projeto com direnv), senão "default"
func defaultMachineName() string { return env("RC_MACHINE_NAME", "default") }

// machineCmd agrupa o ciclo de vida das máquinas persistentes.
func machineCmd() *cobra.Command {
	c := &cobra.Command{Use: "machine", Short: "Persistent machines: create, start, exec, cp, logs, stop, delete"}
	c.AddCommand(machineCreateCmd(), machineStartCmd(), machineStopCmd(), machineStatusCmd(), machineListCmd(),
		machineDeleteCmd(), machineExecCmd(), machineCopyCmd(), machineLogsCmd())
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

// machineCreateCmd registra uma máquina e o disco dela (não liga).
func machineCreateCmd() *cobra.Command {
	var mc domain.Machine
	var cpus uint
	c := &cobra.Command{
		Use: "create", Short: "Create a persistent machine (its own writable disk over a stack image)",
		RunE: withMachines(func(m *app.Machines, cmd *cobra.Command, _ []string) error {
			mc.CPUs = uint8(cpus)
			out, err := m.Create(cmd.Context(), mc)
			if err != nil {
				return err
			}
			fmt.Printf("created %s (stack=%s cpus=%d memory=%dMiB disk=%dGiB gpu=%v)\n", out.Name, out.Stack, out.CPUs, out.MemoryMiB, out.DiskGiB, out.GPU)
			return nil
		}),
	}
	c.Flags().StringVar(&mc.Name, "name", defaultMachineName(), "machine name")
	c.Flags().StringVar(&mc.Stack, "stack", "", "stack whose image is the machine base (python, go, ...)")
	c.Flags().UintVar(&cpus, "cpus", 0, "vCPUs (default 2)")
	c.Flags().Uint32Var(&mc.MemoryMiB, "memory", 0, "memory ceiling in MiB (default 2048)")
	c.Flags().IntVar(&mc.DiskGiB, "disk-gib", 0, "writable disk in GiB (default 10, sparse)")
	c.Flags().BoolVar(&mc.GPU, "gpu", false, "give the machine the host GPU through Vulkan (the stack must declare gpu)")
	c.MarkFlagRequired("stack")
	return c
}

// machineStartCmd liga a VM destacada e espera o guest responder.
func machineStartCmd() *cobra.Command {
	var name string
	c := &cobra.Command{
		Use: "start", Short: "Start a machine (it keeps running after this command exits)",
		RunE: withMachines(func(m *app.Machines, cmd *cobra.Command, _ []string) error {
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
	c.Flags().DurationVar(&timeout, "timeout", app.DefaultStopTimeout, "wait for a clean shutdown before killing (0 kills at once)")
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
		Use: "ls", Short: "List machines",
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
	return c
}

// machineExecCmd roda um comando na máquina com a saída em streaming; o rc do `rc` é o
// do comando (124 no timeout).
func machineExecCmd() *cobra.Command {
	var name, workdir string
	var envs []string
	var stdin bool
	var timeout time.Duration
	c := &cobra.Command{
		Use: "exec [flags] -- COMMAND [ARGS...]", Short: "Run a command in a running machine (output streams as it runs)",
		Args: cobra.MinimumNArgs(1),
		RunE: withMachines(func(m *app.Machines, cmd *cobra.Command, args []string) error {
			req := app.ExecRequest{Argv: args, Env: envs, Cwd: workdir, Timeout: timeout}
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
	c.Flags().DurationVar(&timeout, "timeout", 0, "kill the command after this long (0 = no limit)")
	c.Flags().Bool("stream", true, "accepted for compatibility; output always streams")
	return c
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
	c := &cobra.Command{
		Use: "logs", Short: "Show a machine's console (readable while stopped)",
		RunE: withMachines(func(m *app.Machines, _ *cobra.Command, _ []string) error {
			out, err := m.Logs(name, tail)
			if err != nil {
				return err
			}
			fmt.Print(out)
			return nil
		}),
	}
	c.Flags().StringVar(&name, "name", defaultMachineName(), "machine name")
	c.Flags().IntVar(&tail, "tail", 0, "only the last N lines")
	return c
}
