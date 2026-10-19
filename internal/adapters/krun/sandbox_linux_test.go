//go:build linux

package krun

import (
	"encoding/json"
	"fmt"
	"net"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"syscall"
	"testing"

	seccomp "github.com/elastic/go-seccomp-bpf"
)

// probeEnv liga o modo sonda: o binário de teste aplica a política recebida (ou não) e
// tenta cada operação, imprimindo uma linha por resultado. O processo de teste em si
// nunca é confinado, senão os testes seguintes rodariam presos.
const probeEnv = "KRUN_SANDBOX_PROBE"

// probeInput é o que o teste manda para a sonda.
type probeInput struct {
	Apply      bool
	Policy     SandboxPolicy
	Allowed    string // arquivo liberado para leitura
	Forbidden  string // arquivo fora da política
	OtherSock  string // socket de "outra VM", fora do diretório liberado
	OwnSockDir string // diretório liberado para o socket desta VM
}

// init desvia para a sonda antes do TestMain quando o binário é relançado como sonda.
func init() {
	if os.Getenv(probeEnv) != "" {
		runProbe()
	}
}

// runProbe aplica (ou não) a política e testa cada operação.
func runProbe() {
	var in probeInput
	json.Unmarshal([]byte(os.Getenv(probeEnv)), &in)
	if in.Apply {
		if err := ApplySandbox(in.Policy); err != nil {
			fmt.Println("apply=error", err)
			os.Exit(2)
		}
	}
	res := func(name string, err error) {
		if err != nil {
			fmt.Printf("%s=denied\n", name)
		} else {
			fmt.Printf("%s=allowed\n", name)
		}
	}
	_, err := os.ReadFile(in.Allowed)
	res("read_allowed", err)
	_, err = os.ReadFile(in.Forbidden)
	res("read_forbidden", err)
	err = os.WriteFile(filepath.Join(filepath.Dir(in.Allowed), "new-file"), []byte("x"), 0o600)
	res("write_next_to_root_disk", err)
	l, err := net.Listen("unix", filepath.Join(in.OwnSockDir, "v.sock"))
	res("bind_own_socket", err)
	if l != nil {
		l.Close()
	}
	c, err := net.Dial("unix", in.OtherSock)
	res("connect_other_vm_socket", err)
	if c != nil {
		c.Close()
	}
	c, err = net.Dial("tcp", "127.0.0.1:1")
	if err != nil && strings.Contains(err.Error(), "connection refused") {
		err = nil // a rede chegou ao kernel: não foi bloqueada
	}
	res("tcp_connect", err)
	// personality(0xffffffff) só consulta a persona: passa sem seccomp e dá EPERM com ele.
	// ptrace não serve de prova: no Ubuntu o Yama já nega ptrace ao pai sem confinamento.
	_, _, errno := syscall.Syscall(syscall.SYS_PERSONALITY, 0xffffffff, 0, 0)
	if errno != 0 {
		res("personality", errno)
	} else {
		res("personality", nil)
	}
	if in.Apply {
		// Sem confinamento o exec substituiria a sonda e cortaria a saída; por isso só aqui.
		err = syscall.Exec("/bin/true", []string{"true"}, nil)
		res("execve", err)
	}
	os.Exit(0)
}

// probe roda a sonda num subprocesso e devolve o mapa operação → allowed/denied.
func probe(t *testing.T, apply bool) map[string]string {
	t.Helper()
	dir := t.TempDir()
	disk := filepath.Join(dir, "disks", "root.ext4")
	os.MkdirAll(filepath.Dir(disk), 0o755)
	os.WriteFile(disk, []byte("ext4"), 0o644)
	secret := filepath.Join(dir, "secret")
	os.WriteFile(secret, []byte("host secret"), 0o600)
	own := filepath.Join(dir, "vm-a")
	other := filepath.Join(dir, "vm-b")
	os.MkdirAll(own, 0o700)
	os.MkdirAll(other, 0o700)
	otherSock := filepath.Join(other, "v.sock")
	ln, err := net.Listen("unix", otherSock)
	if err != nil {
		t.Fatal(err)
	}
	defer ln.Close()
	go func() {
		for {
			c, err := ln.Accept()
			if err != nil {
				return
			}
			c.Close()
		}
	}()
	spec := jobSpec()
	spec.RootDisk = disk
	spec.Console = filepath.Join(own, "console")
	spec.VsockSocket = filepath.Join(own, "v.sock")
	os.WriteFile(spec.Console, nil, 0o600)
	in := probeInput{Apply: apply, Policy: PolicyFor(spec, loadedLibDirs()), Allowed: disk, Forbidden: secret, OtherSock: otherSock, OwnSockDir: own}
	raw, _ := json.Marshal(in)
	cmd := exec.Command(os.Args[0], "-test.run=^$")
	cmd.Env = append(os.Environ(), probeEnv+"="+string(raw))
	out, err := cmd.CombinedOutput()
	if err != nil {
		t.Fatalf("probe failed: %v\n%s", err, out)
	}
	got := map[string]string{}
	for _, line := range strings.Split(strings.TrimSpace(string(out)), "\n") {
		if k, v, ok := strings.Cut(line, "="); ok {
			got[k] = v
		}
	}
	return got
}

// TestSandboxBlocks confina a sonda de verdade (Landlock + seccomp) e confere cada
// operação. É a prova de que a política morde, não só que ela é montada.
func TestSandboxBlocks(t *testing.T) {
	want := map[string]string{
		"read_allowed":            "allowed",
		"bind_own_socket":         "allowed",
		"read_forbidden":          "denied",
		"write_next_to_root_disk": "denied",
		"connect_other_vm_socket": "denied",
		"tcp_connect":             "denied",
		"execve":                  "denied",
		"personality":             "denied",
	}
	// Socket Unix por caminho só é controlado a partir da ABI 9. Abaixo dela o kernel não
	// oferece a proteção: o teste registra a lacuna em vez de fingir que ela não existe.
	abi := LandlockABI()
	if abi < 9 {
		want["connect_other_vm_socket"] = "allowed"
		t.Logf("landlock ABI %d: unix socket isolation needs ABI 9; connect to another VM's socket is NOT blocked on this kernel", abi)
	}
	got := probe(t, true)
	for k, v := range want {
		if got[k] != v {
			t.Errorf("%s = %q, want %q (all: %v)", k, got[k], v, got)
		}
	}
}

// TestProbeWithoutSandbox é a prova de falha da sonda: sem confinamento, tudo o que o
// teste acima espera negado passa. Se isto falhar, a sonda não distingue nada.
func TestProbeWithoutSandbox(t *testing.T) {
	got := probe(t, false)
	for _, k := range []string{"read_forbidden", "write_next_to_root_disk", "connect_other_vm_socket", "tcp_connect", "personality"} {
		if got[k] != "allowed" {
			t.Errorf("without sandbox %s should be allowed, got %q (all: %v)", k, got[k], got)
		}
	}
}

// TestDenylistAssemblesOnThisArch monta o filtro BPF: um nome de syscall que não existe
// nesta arquitetura (iopl em arm64) faria o vmm falhar em todo boot.
func TestDenylistAssemblesOnThisArch(t *testing.T) {
	pol := seccomp.Policy{DefaultAction: seccomp.ActionAllow,
		Syscalls: []seccomp.SyscallGroup{{Action: seccomp.ActionErrno, Names: deniedSyscalls(currentArch)}}}
	if _, err := pol.Assemble(); err != nil {
		t.Fatalf("denylist does not assemble on %s: %v", currentArch, err)
	}
	if !SandboxSupported {
		t.Fatal("Linux must report sandbox support")
	}
	if LandlockABI() < 1 {
		t.Fatal("this test host must have Landlock")
	}
}
