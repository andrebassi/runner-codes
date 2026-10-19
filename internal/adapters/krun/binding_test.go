package krun

import (
	"os"
	"runtime"
	"testing"
)

// realLib é onde o brew `slp/krun` instala a libkrun no macOS arm64.
const realLib = "/opt/homebrew/lib/libkrun.dylib"

// TestLoadMissingLibrary: caminho inexistente vira erro, não pânico.
func TestLoadMissingLibrary(t *testing.T) {
	if _, err := Load("/nao/existe/libkrun.so"); err == nil {
		t.Fatal("esperava erro")
	}
}

// TestLoadWrongLibrary: biblioteca sem os símbolos da libkrun vira erro (pânico recuperado).
func TestLoadWrongLibrary(t *testing.T) {
	lib := "/usr/lib/libSystem.B.dylib"
	if runtime.GOOS == "linux" {
		lib = "libc.so.6"
	}
	if _, err := Load(lib); err == nil {
		t.Fatal("esperava erro de símbolo ausente")
	}
}

// TestLoadRealLibrary exercita cada método do binding contra a libkrun instalada.
// Nenhuma chamada sobe VM: start_enter recebe contexto inválido e só devolve erro.
func TestLoadRealLibrary(t *testing.T) {
	if _, err := os.Stat(realLib); err != nil {
		t.Skip("libkrun não instalada neste host")
	}
	api, err := Load(realLib)
	if err != nil {
		t.Fatal(err)
	}
	api.SetLogLevel(0)
	id := api.CreateCtx()
	if id < 0 {
		t.Fatalf("krun_create_ctx: %d", id)
	}
	ctx := uint32(id)
	if rc := api.SetVMConfig(ctx, 1, 256); rc < 0 {
		t.Fatalf("set_vm_config: %d", rc)
	}
	api.SetRoot(ctx, t.TempDir())
	api.AddDisk2(ctx, "x", "/nao/existe.ext4", 0, true)
	api.SetRootDiskRemount(ctx, "/dev/vda", "ext4", "ro")
	api.DisableImplicitVsock(ctx)
	api.AddVsock(ctx, 0)
	api.AddVsockPort2(ctx, 5000, t.TempDir()+"/v.sock", true)
	api.SetConsoleOutput(ctx, t.TempDir()+"/c.log")
	api.SetGID(ctx, uint32(os.Getgid()))
	api.SetUID(ctx, uint32(os.Getuid()))
	if rc := api.SetExec(ctx, "/bin/true", []string{"a"}, []string{"K=V"}); rc < 0 {
		t.Fatalf("set_exec: %d", rc)
	}
	// Contexto inexistente: a libkrun recusa antes de tocar no hypervisor.
	if rc := api.StartEnter(ctx + 1000); rc >= 0 {
		t.Fatalf("start_enter com contexto inválido deveria falhar, veio %d", rc)
	}
}
