package main

import (
	"context"
	"os"
	"time"

	"github.com/spf13/cobra"

	"runner.codes/internal/adapters/mcpserver"
	"runner.codes/internal/adapters/screenctl"
	"runner.codes/internal/app"
)

// mcpCmd sobe o servidor MCP de uma máquina por stdio: o agente que o registra (o Claude Code, por exemplo) ganha
// as ferramentas screenshot, click, type, key, scroll, actions e exec naquela VM.
func mcpCmd() *cobra.Command {
	var name string
	c := &cobra.Command{
		Use:   "mcp",
		Short: "MCP server (stdio) to operate a machine's screen and shell from an agent",
		Long: "MCP server over stdio for one machine: screenshot, click, move, drag, type, key, scroll, actions and exec.\n" +
			"Register it in Claude Code with:\n  claude mcp add rc -- rc mcp --name MACHINE",
		RunE: withMachines(func(m *app.Machines, cmd *cobra.Command, _ []string) error {
			if _, err := m.Status(name); err != nil {
				return err
			}
			srv := &mcpserver.Server{Backend: machineBackend{m: m, name: name}, Machine: name, Version: Version}
			return srv.Serve(cmd.Context(), os.Stdin, os.Stdout)
		}),
	}
	c.Flags().StringVar(&name, "name", defaultMachineName(), "machine name")
	return c
}

// machineBackend liga o servidor MCP a uma máquina: tela pelo visualizador, comando pelo exec do guest. A porta da
// tela é buscada a cada chamada: a máquina pode ter sido religada entre uma ferramenta e outra.
type machineBackend struct {
	m    *app.Machines
	name string
}

// Capture captura a tela da máquina.
func (b machineBackend) Capture(ctx context.Context) (screenctl.Shot, error) {
	s, err := screenOf(b.m, b.name)
	if err != nil {
		return screenctl.Shot{}, err
	}
	return s.Capture(ctx)
}

// Do executa ações na tela da máquina.
func (b machineBackend) Do(ctx context.Context, actions []screenctl.Action) error {
	s, err := screenOf(b.m, b.name)
	if err != nil {
		return err
	}
	return s.Do(ctx, actions)
}

// Exec roda o comando com sh -c na máquina.
func (b machineBackend) Exec(ctx context.Context, command string, timeoutSec int) (mcpserver.ExecResult, error) {
	res, err := b.m.Exec(ctx, b.name, app.ExecRequest{Argv: []string{"sh", "-c", command},
		Timeout: time.Duration(timeoutSec) * time.Second}, nil)
	if err != nil {
		return mcpserver.ExecResult{}, err
	}
	return mcpserver.ExecResult{Stdout: res.Stdout, Stderr: res.Stderr, ExitCode: res.ExitCode}, nil
}
