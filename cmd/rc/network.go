package main

import (
	"fmt"
	"slices"

	"github.com/spf13/cobra"
	"github.com/spf13/pflag"

	"runner.code/internal/app"
	"runner.code/internal/domain"
)

// egressFlags registra as flags de política de saída sobre e.
func egressFlags(f *pflag.FlagSet, e *domain.Egress) {
	f.StringArrayVar(&e.AllowHosts, "allow-host", nil, "allow a host and its subdomains (=host: exact; *.host: subdomains only); implies --net")
	f.StringArrayVar(&e.AllowHostPatterns, "allow-host-pattern", nil, "allow an exact host or *.domain (subdomains only); implies --net")
	f.StringArrayVar(&e.AllowCIDRs, "allow-cidr", nil, "allow a CIDR or IP (127.0.0.1 opens the host loopback); implies --net")
	f.StringArrayVar(&e.DenyCIDRs, "deny-cidr", nil, "deny a CIDR or IP before any allow rule")
	f.BoolVar(&e.BlockPrivate, "block-private", false, "block private ranges (RFC 1918, CGNAT, ULA) while keeping the internet open")
	f.StringArrayVar(&e.AllowPrivate, "allow-private", nil, "reopen a private range (implies --block-private)")
}

// machineUpdateCmd troca a política de saída, também com a máquina ligada: a mudança vale
// na próxima conexão ou consulta, e os IPs de um nome removido são esquecidos.
func machineUpdateCmd() *cobra.Command {
	var name string
	var add domain.Egress
	var rmHosts, rmCIDRs []string
	c := &cobra.Command{
		Use:   "update",
		Short: "Change a machine's egress policy (live on a running machine)",
		RunE: withMachines(func(m *app.Machines, _ *cobra.Command, _ []string) error {
			mc, err := m.Update(name, func(e *domain.Egress) { applyEgressChange(e, add, rmHosts, rmCIDRs) })
			if err != nil {
				return err
			}
			e := mc.Network.Egress
			fmt.Printf("updated %s (allow_hosts=%v allow_host_patterns=%v allow_cidrs=%v)\n", mc.Name, e.AllowHosts, e.AllowHostPatterns, e.AllowCIDRs)
			return nil
		}),
	}
	f := c.Flags()
	f.StringVar(&name, "name", defaultMachineName(), "machine name")
	f.StringArrayVar(&add.AllowHosts, "allow-host", nil, "add an allowed host (repeatable)")
	f.StringArrayVar(&add.AllowHostPatterns, "allow-host-pattern", nil, "add an allowed host pattern (repeatable)")
	f.StringArrayVar(&add.AllowCIDRs, "allow-cidr", nil, "add an allowed CIDR (repeatable)")
	f.StringArrayVar(&rmHosts, "remove-allow-host", nil, "remove a host or pattern, as it was added (repeatable)")
	f.StringArrayVar(&rmCIDRs, "remove-allow-cidr", nil, "remove an allowed CIDR, as it was added (repeatable)")
	return c
}

// applyEgressChange acrescenta as entradas novas (sem repetir) e tira as removidas.
func applyEgressChange(e *domain.Egress, add domain.Egress, rmHosts, rmCIDRs []string) {
	merge := func(list, extra []string) []string {
		for _, x := range extra {
			if !slices.Contains(list, x) {
				list = append(list, x)
			}
		}
		return list
	}
	drop := func(list, gone []string) []string {
		return slices.DeleteFunc(list, func(x string) bool { return slices.Contains(gone, x) })
	}
	e.AllowHosts = drop(merge(e.AllowHosts, add.AllowHosts), rmHosts)
	e.AllowHostPatterns = drop(merge(e.AllowHostPatterns, add.AllowHostPatterns), rmHosts)
	e.AllowCIDRs = drop(merge(e.AllowCIDRs, add.AllowCIDRs), rmCIDRs)
}
