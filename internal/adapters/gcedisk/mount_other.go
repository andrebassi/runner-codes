//go:build !linux

package gcedisk

import "errors"

// errLinuxOnly é a recusa fora do Linux: Persistent Disk só existe numa VM do GCE, e o rc dela é Linux.
var errLinuxOnly = errors.New("persistent disks need Linux (a GCE VM)")

// mkfsExt4 recusa fora do Linux.
func mkfsExt4(string) error { return errLinuxOnly }

// mountExt4 recusa fora do Linux.
func mountExt4(string, string) error { return errLinuxOnly }

// unmountDir recusa fora do Linux.
func unmountDir(string) error { return errLinuxOnly }

// isMountpoint é falso fora do Linux: nada lá é montado por este pacote.
func isMountpoint(string) bool { return false }
