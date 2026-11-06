//go:build !darwin

package hoststats

import "os"

// hypervisor diz se o /dev/kvm existe: no Linux é o acelerador da libkrun.
func hypervisor() bool {
	_, err := os.Stat("/dev/kvm")
	return err == nil
}
