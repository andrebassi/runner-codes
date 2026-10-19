//go:build linux

package guest

import "syscall"

// mountProc monta o procfs em /proc; só existe no Linux, que é onde o guest roda.
func mountProc() error {
	return syscall.Mount("proc", "/proc", "proc", 0, "")
}
