package krun

import (
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// TestSandboxEnabled: só VM de job (disco, sem rede) é confinada; a env desliga.
func TestSandboxEnabled(t *testing.T) {
	if !SandboxEnabled(jobSpec()) {
		t.Fatal("job VM must be sandboxed")
	}
	gpu := jobSpec()
	gpu.GPU = true
	if SandboxEnabled(gpu) {
		// O virglrenderer precisa de /dev/dri e execve no Linux, que a política nega.
		t.Fatal("GPU VM does not fit the policy and must not be sandboxed")
	}
	build := Spec{CPUs: 1, MemoryMiB: 1, RootDir: "/r", Exec: "/x", Network: true}
	if SandboxEnabled(build) {
		t.Fatal("build VM (virtiofs + network) must not be sandboxed")
	}
	withNet := jobSpec()
	withNet.Network = true
	if SandboxEnabled(withNet) {
		t.Fatal("a VM with network does not fit the policy")
	}
	t.Setenv(sandboxDisabledEnv, "off")
	if SandboxEnabled(jobSpec()) {
		t.Fatal("RUNNER_SANDBOX=off must disable it")
	}
}

// TestPolicyFor trava o que a VM de job ainda alcança: o disco só para leitura, o
// console e o diretório do socket desta VM, /dev/kvm; nada de diretório de outra VM.
func TestPolicyFor(t *testing.T) {
	p := PolicyFor(jobSpec(), []string{"/usr/lib", "/opt/homebrew/lib", "/usr/lib", ""})
	if strings.Join(p.ReadOnlyFiles, ",") != "/img/python.ext4" {
		t.Fatalf("read-only files: %v", p.ReadOnlyFiles)
	}
	if strings.Join(p.ReadOnlyDirs, ",") != "/opt/homebrew/lib,/proc,/sys,/usr/lib" {
		t.Fatalf("read-only dirs (deduped, sorted): %v", p.ReadOnlyDirs)
	}
	if strings.Join(p.ReadWriteFiles, ",") != "/run/c.log" || p.SocketDir != "/run" {
		t.Fatalf("writable: %v socket dir %q", p.ReadWriteFiles, p.SocketDir)
	}
	if p.DeviceFiles[0] != "/dev/kvm" {
		t.Fatalf("devices: %v", p.DeviceFiles)
	}
	m := jobSpec()
	m.DataDisk = "/m/disk.ext4"
	if got := PolicyFor(m, nil).ReadWriteFiles; strings.Join(got, ",") != "/run/c.log,/m/disk.ext4" {
		t.Fatalf("machine disk must be writable, nothing else: %v", got)
	}
	bare := jobSpec()
	bare.Console, bare.VsockSocket, bare.VsockPort = "", "", 0
	if p := PolicyFor(bare, nil); p.ReadWriteFiles != nil || p.SocketDir != "" {
		t.Fatalf("no console/socket must grant nothing writable: %+v", p)
	}
}

// TestDeniedSyscalls trava o núcleo da denylist e as chamadas só de x86.
func TestDeniedSyscalls(t *testing.T) {
	arm := strings.Join(deniedSyscalls("arm64"), " ")
	for _, must := range []string{"execve", "ptrace", "mount", "bpf", "kexec_load", "init_module", "unshare", "setns"} {
		if !strings.Contains(" "+arm+" ", " "+must+" ") {
			t.Errorf("denylist without %s", must)
		}
	}
	if strings.Contains(arm, "iopl") {
		t.Fatal("iopl does not exist on arm64; the filter would fail to load")
	}
	if !strings.Contains(strings.Join(deniedSyscalls("amd64"), " "), "iopl") {
		t.Fatal("amd64 must also deny iopl/ioperm")
	}
}

// TestRunVMMAppliesSandbox: VM de job passa pela política antes do start_enter; VM de
// build não; falha do confinamento impede o boot.
func TestRunVMMAppliesSandbox(t *testing.T) {
	var got *SandboxPolicy
	applySandbox = func(p SandboxPolicy) error { got = &p; return nil }
	preload = func() error { return nil }
	defer func() { applySandbox, preload = ApplySandbox, preloadKrunfw }()

	s := jobSpec()
	s.Console = filepath.Join(t.TempDir(), "console")
	f := &fakeAPI{failOn: "krun_start_enter"}
	RunVMM(f, s, nil)
	if got == nil || got.ReadOnlyFiles[0] != s.RootDisk {
		t.Fatalf("job VM was not sandboxed: %+v", got)
	}
	if _, err := os.Stat(s.Console); err != nil {
		t.Fatal("console must exist before the sandbox (open after Landlock would fail)")
	}
	if f.calls[len(f.calls)-1] != "krun_start_enter" {
		t.Fatalf("start_enter must come after the sandbox: %v", f.calls)
	}

	got = nil
	RunVMM(&fakeAPI{failOn: "krun_start_enter"}, Spec{CPUs: 1, MemoryMiB: 1, RootDir: "/r", Exec: "/x", Network: true}, nil)
	if got != nil {
		t.Fatal("build VM must not be sandboxed")
	}

	applySandbox = func(SandboxPolicy) error { return errors.New("landlock: not supported") }
	f = &fakeAPI{}
	if err := RunVMM(f, s, nil); err == nil || !strings.Contains(err.Error(), "landlock") {
		t.Fatalf("a sandbox failure must stop the boot: %v", err)
	}
	for _, c := range f.calls {
		if c == "krun_start_enter" {
			t.Fatal("start_enter called after a sandbox failure")
		}
	}

	preload = func() error { return errors.New("preload libkrunfw: not found") }
	if err := RunVMM(&fakeAPI{}, s, nil); err == nil || !strings.Contains(err.Error(), "libkrunfw") {
		t.Fatalf("a preload failure must stop the boot: %v", err)
	}
	preload = func() error { return nil }

	bad := jobSpec()
	bad.Console = filepath.Join(t.TempDir(), "missing-dir", "console")
	applySandbox = func(SandboxPolicy) error { return nil }
	if err := RunVMM(&fakeAPI{}, bad, nil); err == nil {
		t.Fatal("console that cannot be created must stop the boot")
	}
}

// TestSandboxDisabledWarns: com RUNNER_SANDBOX=off a VM sobe sem confinamento.
func TestSandboxDisabledWarns(t *testing.T) {
	t.Setenv(sandboxDisabledEnv, "off")
	called := false
	applySandbox = func(SandboxPolicy) error { called = true; return nil }
	defer func() { applySandbox = ApplySandbox }()
	RunVMM(&fakeAPI{failOn: "krun_start_enter"}, jobSpec(), nil)
	if called {
		t.Fatal("sandbox applied while disabled")
	}
}
