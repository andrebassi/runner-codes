package krun

import (
	"os"
	"path/filepath"
	"runtime"
	"sort"
)

// SandboxPolicy descreve o que o processo vmm ainda pode tocar depois de confinado.
// A libkrun avisa que guest e VMM ficam no mesmo contexto de segurança: se o guest
// escapar para o VMM, é este confinamento que decide o que ele alcança no host.
type SandboxPolicy struct {
	// ReadOnlyFiles: o disco root da linguagem (compartilhado entre VMs, só leitura).
	ReadOnlyFiles []string
	// ReadOnlyDirs: bibliotecas já carregadas e o que a libkrun lê do kernel (/proc, /sys).
	ReadOnlyDirs []string
	// ReadWriteFiles: o console desta VM.
	ReadWriteFiles []string
	// DeviceFiles: /dev/kvm (precisa de ioctl) e os nós inofensivos que a libkrun abre.
	DeviceFiles []string
	// SocketDir: diretório exclusivo desta VM, onde a libkrun cria o socket do vsock.
	SocketDir string
}

// sandboxDisabledEnv desliga o confinamento para diagnóstico; o vmm avisa no stderr.
const sandboxDisabledEnv = "RUNNER_SANDBOX"

// SandboxEnabled diz se a spec recebe confinamento. Só VM de job: root em disco, sem rede.
// A VM de build usa diretório do host por virtiofs e TSI, roda só código nosso, e não
// caberia nesta política.
// VM com GPU também fica de fora: no Linux o virglrenderer abre /dev/dri e sobe o render
// server com execve, que o seccomp nega; liberar isso exige medir num host Linux com GPU,
// que não temos. Fica declarado como dívida, e o vmm avisa no stderr.
func SandboxEnabled(s Spec) bool {
	return s.RootDisk != "" && !s.Network && !s.GPU && os.Getenv(sandboxDisabledEnv) != "off"
}

// PolicyFor monta a política de uma VM de job. libDirs são os diretórios das
// bibliotecas já carregadas no processo (libkrun, libkrunfw, libc).
func PolicyFor(s Spec, libDirs []string) SandboxPolicy {
	p := SandboxPolicy{
		ReadOnlyFiles: []string{s.RootDisk},
		ReadOnlyDirs:  dedupe(append(append([]string{}, libDirs...), "/proc", "/sys")),
		DeviceFiles:   []string{"/dev/kvm", "/dev/null", "/dev/zero", "/dev/urandom", "/dev/random"},
	}
	if s.Console != "" {
		p.ReadWriteFiles = []string{s.Console}
	}
	if s.VsockSocket != "" {
		p.SocketDir = filepath.Dir(s.VsockSocket)
	}
	return p
}

// dedupe remove repetidos e vazios, em ordem estável (a política vai para log e teste).
func dedupe(in []string) []string {
	seen := map[string]bool{}
	var out []string
	for _, v := range in {
		if v != "" && !seen[v] {
			seen[v] = true
			out = append(out, v)
		}
	}
	sort.Strings(out)
	return out
}

// deniedSyscalls é a denylist do seccomp: chamadas que o VMM nunca faz e que seriam o
// próximo passo de quem escapou do guest. Denylist e não allowlist de propósito: a
// allowlist exigiria medir cada syscall da libkrun num Linux com KVM, e uma faltando
// derrubaria toda VM em produção.
func deniedSyscalls(goarch string) []string {
	names := []string{
		// executar outro programa no host
		"execve", "execveat",
		// inspecionar ou alterar outros processos
		"ptrace", "process_vm_readv", "process_vm_writev", "pidfd_getfd",
		// mexer em montagens e namespaces
		"mount", "umount2", "pivot_root", "chroot", "unshare", "setns",
		"fsopen", "fsmount", "fsconfig", "move_mount", "open_tree",
		// kernel: módulos, kexec, eBPF, perf, chaveiro
		"init_module", "finit_module", "delete_module", "kexec_load", "kexec_file_load",
		"bpf", "perf_event_open", "keyctl", "add_key", "request_key",
		// sair do controle de acesso por handle de arquivo
		"open_by_handle_at", "name_to_handle_at",
		// relógio, swap, reboot, contabilidade
		"settimeofday", "clock_settime", "adjtimex", "swapon", "swapoff", "reboot", "acct", "quotactl",
		"syslog", "vhangup", "personality", "userfaultfd",
	}
	if goarch == "amd64" {
		// acesso direto a portas de E/S só existe em x86
		names = append(names, "iopl", "ioperm")
	}
	return names
}

// currentArch é runtime.GOARCH; existe para o teste trocar a arquitetura.
var currentArch = runtime.GOARCH
