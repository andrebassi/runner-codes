package krun

import (
	"testing"

	ll "github.com/landlock-lsm/go-landlock/landlock/syscall"
)

// TestHandledFSByABI trava os direitos por ABI: um bit a mais é EINVAL no kernel antigo, um a menos
// deixa a ação liberada.
func TestHandledFSByABI(t *testing.T) {
	if handledFS(0) != 0 {
		t.Fatal("ABI 0 não tem Landlock")
	}
	v1 := handledFS(1)
	if v1&ll.AccessFSRefer != 0 || v1&ll.AccessFSTruncate != 0 || v1&ll.AccessFSIoctlDev != 0 {
		t.Fatalf("ABI 1 com direito que só existe depois: %b", v1)
	}
	if handledFS(2)&ll.AccessFSRefer == 0 || handledFS(2)&ll.AccessFSTruncate != 0 {
		t.Fatal("ABI 2 = REFER, sem TRUNCATE")
	}
	if handledFS(3)&ll.AccessFSTruncate == 0 || handledFS(4)&ll.AccessFSIoctlDev != 0 {
		t.Fatal("TRUNCATE na 3, sem IOCTL_DEV na 4")
	}
	if handledFS(5)&ll.AccessFSIoctlDev == 0 {
		t.Fatal("IOCTL_DEV na 5")
	}
	if handledFS(7)&ll.AccessFSResolveUnix != 0 {
		t.Fatal("RESOLVE_UNIX é da ABI 9: o caminho legado nunca pede")
	}
}

// TestLegacyRulesetNetAndScope: rede negada a partir da 4, escopo a partir da 6.
func TestLegacyRulesetNetAndScope(t *testing.T) {
	a3, _ := legacyRuleset(SandboxPolicy{}, 3)
	if a3.HandledAccessNet != 0 || a3.Scoped != 0 {
		t.Fatalf("ABI 3 não conhece rede nem escopo: %+v", a3)
	}
	a4, _ := legacyRuleset(SandboxPolicy{}, 4)
	if a4.HandledAccessNet != ll.AccessNetBindTCP|ll.AccessNetConnectTCP || a4.Scoped != 0 {
		t.Fatalf("ABI 4 nega TCP, sem escopo: %+v", a4)
	}
	a6, _ := legacyRuleset(SandboxPolicy{}, 6)
	if a6.Scoped != ll.ScopeAbstractUnixSocket|ll.ScopeSignal {
		t.Fatalf("ABI 6 com escopo: %+v", a6)
	}
}

// TestLegacyRulesetRules confere cada lista da política: o caminho certo, com os direitos certos e
// nunca um direito que a ABI não conhece.
func TestLegacyRulesetRules(t *testing.T) {
	p := SandboxPolicy{
		ReadOnlyFiles: []string{"/ro-file"}, ReadOnlyDirs: []string{"/ro-dir"}, ReadWriteFiles: []string{"/rw-file"},
		DeviceFiles: []string{"/dev/kvm"}, SocketDir: "/sock", UnixConnect: []string{"/agent.sock"}, ListenDirs: []string{"/listen"},
	}
	for _, abi := range []int{1, 4, 5, 7} {
		_, rules := legacyRuleset(p, abi)
		got := map[string]uint64{}
		for _, r := range rules {
			if r.Access&^handledFS(abi) != 0 {
				t.Fatalf("ABI %d: %s pede direito fora do ruleset: %b", abi, r.Path, r.Access)
			}
			got[r.Path] = r.Access
		}
		if len(got) != 7 {
			t.Fatalf("ABI %d: esperava 7 regras, veio %v", abi, got)
		}
		if got["/ro-file"]&ll.AccessFSWriteFile != 0 || got["/ro-dir"]&ll.AccessFSReadDir == 0 {
			t.Fatalf("ABI %d: leitura errada: %v", abi, got)
		}
		if got["/rw-file"]&ll.AccessFSWriteFile == 0 || got["/sock"] != handledFS(abi) || got["/listen"] != handledFS(abi) {
			t.Fatalf("ABI %d: escrita errada: %v", abi, got)
		}
		if (got["/dev/kvm"]&ll.AccessFSIoctlDev != 0) != (abi >= 5) {
			t.Fatalf("ABI %d: ioctl do /dev/kvm: %b", abi, got["/dev/kvm"])
		}
		if got["/agent.sock"] != ll.AccessFSReadFile {
			t.Fatalf("ABI %d: socket do agente só leitura: %b", abi, got["/agent.sock"])
		}
	}
}

// TestRuleAccessFile: em arquivo sobram só os direitos de arquivo; em diretório, todos.
func TestRuleAccessFile(t *testing.T) {
	r := threadRule{Path: "/x", Access: handledFS(5)}
	if got := ruleAccess(r, false); got&^fileAccess != 0 || got&ll.AccessFSReadFile == 0 {
		t.Fatalf("arquivo com direito de diretório: %b", got)
	}
	if ruleAccess(r, true) != handledFS(5) {
		t.Fatal("diretório perdeu direito")
	}
}
