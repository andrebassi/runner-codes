package main

import (
	"fmt"
	"io"
	"os"
	"os/signal"
	"syscall"

	"github.com/spf13/cobra"

	"runner.code/internal/app"
	"runner.code/internal/ports"
)

// machineRunCmd cria, liga e usa uma máquina de uma vez (o `machine run`). Sem
// -d ela é efêmera: o comando roda em primeiro plano e a máquina some no fim (inclusive no
// Ctrl+C). Com -d fica ligada com o workload.
func machineRunCmd() *cobra.Command {
	var mf *machineFlags
	var detach, interactive, tty bool
	c := &cobra.Command{
		Use:   "run [flags] [-- COMMAND [ARGS...]]",
		Short: "Create, start and use a machine in one step (removed on exit unless -d)",
		Example: `  rc machine run --net --image alpine -- uname -a
  rc machine run --net -it --image alpine -- /bin/sh
  rc machine run -d -s runner.code`,
		RunE: withMachines(func(m *app.Machines, cmd *cobra.Command, args []string) error {
			if !cmd.Flags().Changed("name") {
				mf.mc.Name = "" // efêmera ganha nome run-<hex>; -d usa "default"
			}
			mc, _, err := mf.build(cmd.Flags(), nil)
			if err != nil {
				return err
			}
			// Como no Docker: -t abre terminal (PTY); -i sem -t só repassa o stdin;
			// sem nenhum dos dois o stdin não é lido (ler um pipe que nunca fecha travaria o run).
			req := app.RunRequest{Machine: mc, Command: args, Interactive: tty, Detach: detach, DiskFrom: mf.diskFrom}
			if detach {
				name, _, err := m.Run(cmd.Context(), req, app.RunIO{})
				if err != nil {
					return err
				}
				fmt.Printf("started %s (detached; `rc machine stop --name %s` to stop)\n", name, name)
				return nil
			}
			// Ctrl+C (fora do modo raw) e SIGTERM cancelam o comando; a máquina é apagada
			// na volta do Run, e o rc é 130.
			ctx, stop := signal.NotifyContext(cmd.Context(), os.Interrupt, syscall.SIGTERM)
			defer stop()
			var code int
			if req.Interactive {
				code, err = withTerminal(func(size ports.Winsize, resize <-chan ports.Winsize) (int, error) {
					_, code, err := m.Run(ctx, req, app.RunIO{Stdin: os.Stdin, Stdout: os.Stdout, Stderr: os.Stderr,
						Size: size, Resize: resize, Term: env("TERM", "xterm")})
					return code, err
				})
			} else {
				var in io.Reader
				if interactive {
					in = os.Stdin
				}
				_, code, err = m.Run(ctx, req, app.RunIO{Stdin: in, Stdout: os.Stdout, Stderr: os.Stderr})
			}
			if err != nil {
				return err
			}
			if code != 0 {
				os.Exit(code) // depois do withTerminal: o terminal já foi restaurado
			}
			return nil
		}),
	}
	mf = addMachineFlags(c.Flags(), "")
	f := c.Flags()
	f.BoolVarP(&detach, "detach", "d", false, "keep the machine running with its workload and return")
	f.BoolVarP(&interactive, "interactive", "i", false, "send stdin to the command (with -t: an interactive terminal)")
	f.BoolVarP(&tty, "tty", "t", false, "attach a terminal (PTY in the guest)")
	return c
}
