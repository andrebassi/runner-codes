//go:build linux

package krun

import "syscall"

// libPathEnv no Linux: o loader procura a libkrunfw aqui.
const libPathEnv = "LD_LIBRARY_PATH"

// sysProcAttr põe o vmm num grupo próprio e pede SIGKILL ao kernel se o pai morrer
// (Pdeathsig), além do pipe de vigia: dois avisos independentes contra VM órfã.
func sysProcAttr() *syscall.SysProcAttr {
	return &syscall.SysProcAttr{Setpgid: true, Pdeathsig: syscall.SIGKILL}
}
