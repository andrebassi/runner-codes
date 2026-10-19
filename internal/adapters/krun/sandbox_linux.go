//go:build linux

package krun

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"strings"
	"syscall"

	"github.com/ebitengine/purego"
	seccomp "github.com/elastic/go-seccomp-bpf"
	"github.com/landlock-lsm/go-landlock/landlock"
)

// SandboxSupported diz se este SO tem confinamento real (Landlock + seccomp).
const SandboxSupported = true

// landlockCreateRulesetVersion é a flag LANDLOCK_CREATE_RULESET_VERSION: com ruleset
// nulo, a syscall devolve a ABI do kernel em vez de criar um ruleset.
const landlockCreateRulesetVersion = 1

// sysLandlockCreateRuleset é o número da syscall, igual em arm64 e amd64 (unistd genérico).
const sysLandlockCreateRuleset = 444

// LandlockABI devolve a ABI do Landlock do kernel (0 = indisponível). Abaixo de 9 o
// kernel não controla connect() em socket Unix por caminho: o vmm de uma VM ainda
// alcança o socket de outra (medido no Ubuntu 26.04, kernel 7.0, ABI 8).
func LandlockABI() int {
	v, _, errno := syscall.Syscall(sysLandlockCreateRuleset, 0, 0, landlockCreateRulesetVersion)
	if errno != 0 {
		return 0
	}
	return int(v)
}

// preloadKrunfw carrega a libkrunfw antes do confinamento. A libkrun só a abre dentro de
// krun_start_enter; depois do Landlock esse open falharia. Com ela já carregada, o
// dlopen da libkrun acha o objeto pelo soname e não toca no disco.
func preloadKrunfw() error {
	var last error
	for _, name := range []string{"libkrunfw.so.5", "libkrunfw.so"} {
		if _, err := purego.Dlopen(name, purego.RTLD_NOW|purego.RTLD_GLOBAL); err == nil {
			return nil
		} else {
			last = err
		}
	}
	return fmt.Errorf("preload libkrunfw: %w", last)
}

// loadedLibDirs lê /proc/self/maps e devolve os diretórios das bibliotecas mapeadas,
// que continuam legíveis depois do confinamento (o loader pode reabrir na resolução lazy).
func loadedLibDirs() []string {
	f, err := os.Open("/proc/self/maps")
	if err != nil {
		return nil
	}
	defer f.Close()
	var dirs []string
	sc := bufio.NewScanner(f)
	for sc.Scan() {
		fields := strings.Fields(sc.Text())
		if len(fields) < 6 {
			continue
		}
		if p := fields[5]; strings.HasPrefix(p, "/") && strings.Contains(filepath.Base(p), ".so") {
			dirs = append(dirs, filepath.Dir(p))
		}
	}
	return dedupe(dirs)
}

// ApplySandbox confina o processo atual: Landlock (arquivos, rede, sinais e sockets
// Unix por caminho) e depois seccomp (denylist). O Landlock liga no_new_privs ele mesmo;
// a denylist não contém as syscalls do Landlock, então a ordem não as bloqueia.
func ApplySandbox(p SandboxPolicy) error {
	rules := []landlock.Rule{
		landlock.ROFiles(p.ReadOnlyFiles...).IgnoreIfMissing(),
		landlock.RODirs(p.ReadOnlyDirs...).IgnoreIfMissing(),
		landlock.RWFiles(p.ReadWriteFiles...).IgnoreIfMissing(),
		// /dev/kvm é controlado por ioctl: sem WithIoctlDev a libkrun não cria a VM.
		landlock.RWFiles(p.DeviceFiles...).WithIoctlDev().IgnoreIfMissing(),
	}
	if p.SocketDir != "" {
		// Só o diretório desta VM: criar e apagar o socket. Sem WithResolveUnix: o vmm não
		// conecta em socket Unix nenhum, e assim não alcança o socket de outra VM.
		rules = append(rules, landlock.RWDirs(p.SocketDir))
	}
	// V10 = arquivos + TCP/UDP + escopo de sinais e sockets abstratos + socket Unix por
	// caminho. BestEffort cai para a maior ABI que o kernel tiver, sem falhar.
	// Nenhuma regra de rede: toda conexão e bind TCP/UDP fica negada.
	if err := landlock.V10.BestEffort().Restrict(rules...); err != nil {
		return fmt.Errorf("landlock: %w", err)
	}
	filter := seccomp.Filter{
		NoNewPrivs: true,
		// TSYNC: o runtime do Go já tem várias threads; o filtro vale para todas.
		Flag: seccomp.FilterFlagTSync,
		Policy: seccomp.Policy{
			DefaultAction: seccomp.ActionAllow,
			Syscalls:      []seccomp.SyscallGroup{{Action: seccomp.ActionErrno, Names: deniedSyscalls(currentArch)}},
		},
	}
	if err := seccomp.LoadFilter(filter); err != nil {
		return fmt.Errorf("seccomp: %w", err)
	}
	return nil
}
