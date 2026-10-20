//go:build linux

package krun

import (
	"bufio"
	"fmt"
	"os"
	"path/filepath"
	"runtime"
	"strings"
	"syscall"

	"github.com/ebitengine/purego"
	seccomp "github.com/elastic/go-seccomp-bpf"
	"github.com/landlock-lsm/go-landlock/landlock"
	ll "github.com/landlock-lsm/go-landlock/landlock/syscall"
	"golang.org/x/sys/unix"
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

// restrictThread aplica o ruleset só na thread atual: prctl(PR_SET_NO_NEW_PRIVS) e
// landlock_restrict_self sem TSYNC, que valem para quem chama e para as threads criadas depois por ela.
// Caminho que não existe é pulado, como o IgnoreIfMissing do caminho TSYNC.
func restrictThread(attr ll.RulesetAttr, rules []threadRule) error {
	fd, err := ll.LandlockCreateRuleset(&attr, 0)
	if err != nil {
		return fmt.Errorf("landlock_create_ruleset: %w", err)
	}
	defer syscall.Close(fd)
	for _, r := range rules {
		pfd, err := syscall.Open(r.Path, unix.O_PATH|syscall.O_CLOEXEC, 0)
		if err != nil {
			continue
		}
		var st syscall.Stat_t
		isDir := syscall.Fstat(pfd, &st) == nil && st.Mode&syscall.S_IFMT == syscall.S_IFDIR
		if access := ruleAccess(r, isDir); access != 0 {
			err = ll.LandlockAddPathBeneathRule(fd, &ll.PathBeneathAttr{AllowedAccess: access, ParentFd: pfd}, 0)
		}
		syscall.Close(pfd)
		if err != nil {
			return fmt.Errorf("landlock_add_rule %s: %w", r.Path, err)
		}
	}
	if err := unix.Prctl(unix.PR_SET_NO_NEW_PRIVS, 1, 0, 0, 0); err != nil {
		return fmt.Errorf("prctl(PR_SET_NO_NEW_PRIVS): %w", err)
	}
	if err := ll.LandlockRestrictSelf(fd, 0); err != nil {
		return fmt.Errorf("landlock_restrict_self: %w", err)
	}
	return nil
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
	if len(p.UnixConnect) > 0 {
		// Connect só nos sockets pedidos (agente SSH, --mount-socket): o socket de outra VM
		// continua fora de alcance.
		rules = append(rules, landlock.ROFiles(p.UnixConnect...).WithResolveUnix().IgnoreIfMissing())
	}
	if len(p.ListenDirs) > 0 {
		// Diretório escolhido por quem deu --expose-socket HOST: criar e apagar o socket lá.
		rules = append(rules, landlock.RWDirs(p.ListenDirs...).IgnoreIfMissing())
	}
	if abi := LandlockABI(); abi > 0 && abi < TSyncLandlockABI {
		// Kernel sem TSYNC: o caminho do go-landlock para todas as threads não funciona com o purego
		// (ver TSyncLandlockABI). O Landlock vai na thread desta goroutine, que fica presa a ela até o
		// krun_start_enter; as threads que a libkrun cria dali (vCPUs, virtio) herdam o domínio. As
		// outras threads do runtime Go ficam só com o seccomp (TSYNC, abaixo).
		runtime.LockOSThread()
		attr, trules := legacyRuleset(p, abi)
		if err := restrictThread(attr, trules); err != nil {
			return fmt.Errorf("landlock (thread, ABI %d): %w", abi, err)
		}
	} else if err := landlock.V10.BestEffort().Restrict(rules...); err != nil {
		// V10 = arquivos + TCP/UDP + escopo de sinais e sockets abstratos + socket Unix por
		// caminho. BestEffort cai para a maior ABI que o kernel tiver, sem falhar.
		// Nenhuma regra de rede: toda conexão e bind TCP/UDP fica negada.
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
