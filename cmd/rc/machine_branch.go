package main

import (
	"fmt"

	"github.com/spf13/cobra"

	"runner.codes/internal/app"
)

// machineBranchCmd cria uma máquina nova como cópia copy-on-write do disco de outra.
func machineBranchCmd() *cobra.Command {
	var from, name string
	c := &cobra.Command{
		Use:   "branch --from SRC --name NEW",
		Short: "Create a machine as a copy-on-write copy of another machine's disk",
		RunE: withMachines(func(m *app.Machines, cmd *cobra.Command, _ []string) error {
			mc, err := m.Branch(cmd.Context(), from, name)
			if err != nil {
				return err
			}
			fmt.Printf("branched %s from %s (start it with `rc machine start --name %s`)\n", mc.Name, from, mc.Name)
			return nil
		}),
	}
	c.Flags().StringVar(&from, "from", defaultMachineName(), "machine to copy (running or stopped)")
	c.Flags().StringVar(&name, "name", "", "name of the new machine")
	c.MarkFlagRequired("name")
	return c
}

// machinePauseCmd congela uma máquina ligada.
func machinePauseCmd() *cobra.Command {
	var name string
	c := &cobra.Command{
		Use:   "pause",
		Short: "Freeze a running machine (processes, memory and connections stay as they are)",
		RunE: withMachines(func(m *app.Machines, _ *cobra.Command, _ []string) error {
			if err := m.Pause(name); err != nil {
				return err
			}
			fmt.Printf("paused %s\n", name)
			return nil
		}),
	}
	c.Flags().StringVar(&name, "name", defaultMachineName(), "machine name")
	return c
}

// machineSyncCmd devolve ao host as cópias dos volumes staged (-v HOST:GUEST:staged).
func machineSyncCmd() *cobra.Command {
	var name string
	c := &cobra.Command{
		Use:   "sync",
		Short: "Write a running machine's staged volumes back to the host (host files missing from the copy are deleted)",
		RunE: withMachines(func(m *app.Machines, cmd *cobra.Command, _ []string) error {
			if err := m.Sync(cmd.Context(), name); err != nil {
				return err
			}
			fmt.Printf("synced %s\n", name)
			return nil
		}),
	}
	c.Flags().StringVar(&name, "name", defaultMachineName(), "machine name")
	return c
}

// machineResumeCmd continua uma máquina pausada.
func machineResumeCmd() *cobra.Command {
	var name string
	c := &cobra.Command{
		Use:   "resume",
		Short: "Resume a paused machine",
		RunE: withMachines(func(m *app.Machines, _ *cobra.Command, _ []string) error {
			if err := m.Resume(name); err != nil {
				return err
			}
			fmt.Printf("resumed %s\n", name)
			return nil
		}),
	}
	c.Flags().StringVar(&name, "name", defaultMachineName(), "machine name")
	return c
}
