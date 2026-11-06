package hoststats

import "golang.org/x/sys/unix"

// hypervisor diz se o macOS oferece o Hypervisor.framework, que é o que a libkrun usa no lugar do
// /dev/kvm. Sem isto o rc no Mac respondia kvm=false, e o agendador do superbot recusava o único
// servidor do ambiente local ("0/1 servidores disponíveis: 1 sem /dev/kvm", medido em 06/11/2026).
func hypervisor() bool {
	v, err := unix.SysctlUint32("kern.hv_support")
	return err == nil && v == 1
}
