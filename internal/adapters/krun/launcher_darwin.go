//go:build darwin

package krun

import "syscall"

// libPathEnv no macOS: o dyld procura a libkrunfw aqui.
const libPathEnv = "DYLD_LIBRARY_PATH"

// sysProcAttr põe o vmm num grupo próprio, para o SIGKILL do Kill pegar o grupo inteiro.
// O macOS não tem Pdeathsig; a morte do pai chega pelo pipe de vigia.
func sysProcAttr() *syscall.SysProcAttr {
	return &syscall.SysProcAttr{Setpgid: true}
}
