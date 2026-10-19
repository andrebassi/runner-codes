package krun

import (
	"fmt"
	"strings"
	"testing"
)

// fakeAPI registra cada chamada e devolve erro na chamada nomeada em failOn.
type fakeAPI struct {
	calls  []string
	failOn string
	ctxID  int32
	tsi    uint32
	disk   struct {
		readOnly bool
		format   uint32
	}
	remountOpts string
	argv        []string
	uid, gid    uint32
	consoleFDs  [3]int32
	gpuFlags    uint32
	gpuShm      uint64
	// dataDisk e dataRO guardam o segundo disco (o da máquina persistente).
	dataDisk string
	dataRO   bool
}

// rec anota a chamada e devolve -22 (EINVAL) se for a escolhida para falhar.
func (f *fakeAPI) rec(name string) int32 {
	f.calls = append(f.calls, name)
	if f.failOn == name {
		return -22
	}
	return 0
}

// SetLogLevel anota a chamada.
func (f *fakeAPI) SetLogLevel(uint32) int32 { return f.rec("krun_set_log_level") }

// CreateCtx anota a chamada e devolve o id configurado.
func (f *fakeAPI) CreateCtx() int32 {
	if rc := f.rec("krun_create_ctx"); rc < 0 {
		return rc
	}
	return f.ctxID
}

// SetVMConfig anota a chamada.
func (f *fakeAPI) SetVMConfig(uint32, uint8, uint32) int32 { return f.rec("krun_set_vm_config") }

// SetRoot anota a chamada.
func (f *fakeAPI) SetRoot(uint32, string) int32 { return f.rec("krun_set_root") }

// AddDisk2 anota a chamada e guarda formato e modo do disco.
func (f *fakeAPI) AddDisk2(_ uint32, id, path string, format uint32, ro bool) int32 {
	if id == "data" {
		f.dataDisk, f.dataRO = path, ro
		return f.rec("krun_add_disk2_data")
	}
	f.disk.format, f.disk.readOnly = format, ro
	return f.rec("krun_add_disk2")
}

// SetRootDiskRemount anota a chamada e guarda as opções de montagem.
func (f *fakeAPI) SetRootDiskRemount(_ uint32, _, _, opts string) int32 {
	f.remountOpts = opts
	return f.rec("krun_set_root_disk_remount")
}

// DisableImplicitVsock anota a chamada.
func (f *fakeAPI) DisableImplicitVsock(uint32) int32 { return f.rec("krun_disable_implicit_vsock") }

// AddVsock anota a chamada e guarda os bits de TSI.
func (f *fakeAPI) AddVsock(_ uint32, tsi uint32) int32 {
	f.tsi = tsi
	return f.rec("krun_add_vsock")
}

// AddVsockPort2 anota a chamada.
func (f *fakeAPI) AddVsockPort2(uint32, uint32, string, bool) int32 {
	return f.rec("krun_add_vsock_port2")
}

// SetConsoleOutput anota a chamada.
func (f *fakeAPI) SetConsoleOutput(uint32, string) int32 { return f.rec("krun_set_console_output") }

// SetUID anota a chamada e guarda o uid.
func (f *fakeAPI) SetUID(_ uint32, uid uint32) int32 { f.uid = uid; return f.rec("krun_setuid") }

// SetGID anota a chamada e guarda o gid.
func (f *fakeAPI) SetGID(_ uint32, gid uint32) int32 { f.gid = gid; return f.rec("krun_setgid") }

// SetExec anota a chamada e guarda o argv.
func (f *fakeAPI) SetExec(_ uint32, _ string, argv, _ []string) int32 {
	f.argv = argv
	return f.rec("krun_set_exec")
}

// DisableImplicitConsole anota a chamada.
func (f *fakeAPI) DisableImplicitConsole(uint32) int32 { return f.rec("krun_disable_implicit_console") }

// AddVirtioConsoleDefault anota a chamada e guarda os fds.
func (f *fakeAPI) AddVirtioConsoleDefault(_ uint32, in, out, errFD int32) int32 {
	f.consoleFDs = [3]int32{in, out, errFD}
	return f.rec("krun_add_virtio_console_default")
}

// SetGPUOptions2 guarda os flags e o tamanho da janela de VRAM.
func (f *fakeAPI) SetGPUOptions2(_ uint32, flags uint32, shm uint64) int32 {
	f.gpuFlags, f.gpuShm = flags, shm
	return f.rec("krun_set_gpu_options2")
}

// StartEnter anota a chamada; o fake "volta" como se a VM tivesse falhado ao iniciar.
func (f *fakeAPI) StartEnter(uint32) int32 { return f.rec("krun_start_enter") }

// jobSpec é a spec típica de uma VM de job: root em disco, vsock, sem rede.
func jobSpec() Spec {
	return Spec{CPUs: 1, MemoryMiB: 512, RootDisk: "/img/python.ext4", VsockSocket: "/run/v.sock",
		VsockPort: 5000, Console: "/run/c.log", Exec: "/opt/runner/start.sh"}
}

// TestConfigureJobVMOrder trava a sequência validada no spike para VM de job.
func TestConfigureJobVMOrder(t *testing.T) {
	f := &fakeAPI{ctxID: 7}
	ctx, err := Configure(f, jobSpec())
	if err != nil {
		t.Fatal(err)
	}
	if ctx != 7 {
		t.Fatalf("ctx = %d, want 7", ctx)
	}
	want := "krun_set_log_level,krun_create_ctx,krun_set_vm_config,krun_add_disk2,krun_set_root_disk_remount," +
		"krun_disable_implicit_vsock,krun_add_vsock,krun_add_vsock_port2,krun_set_console_output,krun_set_exec"
	if got := strings.Join(f.calls, ","); got != want {
		t.Fatalf("order:\n got %s\nwant %s", got, want)
	}
	if !f.disk.readOnly || f.disk.format != diskFormatRaw || f.remountOpts != "ro" {
		t.Fatalf("root must be a read-only RAW disk remounted ro: %+v opts=%q", f.disk, f.remountOpts)
	}
}

// TestNoNetworkUnlessAsked trava a decisão de segurança: VM de job nunca liga TSI.
func TestNoNetworkUnlessAsked(t *testing.T) {
	f := &fakeAPI{}
	if _, err := Configure(f, jobSpec()); err != nil {
		t.Fatal(err)
	}
	if f.tsi != 0 {
		t.Fatalf("TSI enabled without requesting network: %d", f.tsi)
	}
	if f.calls[5] != "krun_disable_implicit_vsock" {
		t.Fatalf("the implicit vsock (with TSI) must be disabled before add_vsock: %v", f.calls)
	}
	s := jobSpec()
	s.Network = true
	f = &fakeAPI{}
	if _, err := Configure(f, s); err != nil {
		t.Fatal(err)
	}
	if f.tsi != tsiHijackInet {
		t.Fatalf("Network=true should enable TSI INET, got %d", f.tsi)
	}
}

// TestConfigureBuildVM cobre o root por diretório, sem vsock nem console, com uid/gid.
func TestConfigureBuildVM(t *testing.T) {
	uid, gid := uint32(1000), uint32(1001)
	f := &fakeAPI{}
	s := Spec{CPUs: 2, MemoryMiB: 1024, RootDir: "/build/root", Exec: "/work/build.sh", Args: []string{"a", "b"}, UID: &uid, GID: &gid}
	if _, err := Configure(f, s); err != nil {
		t.Fatal(err)
	}
	got := strings.Join(f.calls, ",")
	if !strings.Contains(got, "krun_set_root,") || strings.Contains(got, "krun_add_disk2") || strings.Contains(got, "vsock_port2") {
		t.Fatalf("unexpected calls for a build VM: %s", got)
	}
	if strings.Index(got, "krun_setgid") > strings.Index(got, "krun_setuid") {
		t.Fatalf("setgid must come before setuid: %s", got)
	}
	if f.uid != 1000 || f.gid != 1001 || strings.Join(f.argv, " ") != "a b" {
		t.Fatalf("wrong uid/gid/argv: %d %d %v", f.uid, f.gid, f.argv)
	}
}

// TestConfigureTTY trava o console TTY do `rc shell`: console implícito desligado e
// stdin/stdout/stderr (0, 1, 2) do vmm virando a porta de console do guest.
func TestConfigureTTY(t *testing.T) {
	f := &fakeAPI{}
	s := jobSpec()
	s.Console, s.TTY = "", true
	if _, err := Configure(f, s); err != nil {
		t.Fatal(err)
	}
	got := strings.Join(f.calls, ",")
	if !strings.Contains(got, "krun_disable_implicit_console,krun_add_virtio_console_default") {
		t.Fatalf("TTY calls missing or out of order: %s", got)
	}
	if f.consoleFDs != [3]int32{0, 1, 2} {
		t.Fatalf("console fds %v, want 0 1 2", f.consoleFDs)
	}
	f = &fakeAPI{}
	Configure(f, jobSpec())
	if strings.Contains(strings.Join(f.calls, ","), "console_default") {
		t.Fatal("a VM without tty must keep the implicit console")
	}
}

// TestConfigureDataDisk: o disco da máquina entra como segundo disco (/dev/vdb), gravável,
// depois do root; VM de job não ganha disco nenhum além do root.
func TestConfigureDataDisk(t *testing.T) {
	f := &fakeAPI{}
	s := jobSpec()
	s.DataDisk = "/m/disk.ext4"
	if _, err := Configure(f, s); err != nil {
		t.Fatal(err)
	}
	got := strings.Join(f.calls, ",")
	if f.dataDisk != "/m/disk.ext4" || f.dataRO || !strings.Contains(got, "krun_set_root_disk_remount,krun_add_disk2_data") {
		t.Fatalf("data disk: %q ro=%v calls=%s", f.dataDisk, f.dataRO, got)
	}
	f = &fakeAPI{}
	Configure(f, jobSpec())
	if f.dataDisk != "" {
		t.Fatal("a job VM must not get a data disk")
	}
	f = &fakeAPI{failOn: "krun_add_disk2_data"}
	if _, err := Configure(f, s); err == nil || !strings.Contains(err.Error(), "krun_add_disk2_data") {
		t.Fatalf("data disk failure must name the call: %v", err)
	}
}

// TestConfigureGPU trava a chamada de GPU: Venus sem virgl (1<<6|1<<7, o que o MoltenVK
// aceita), VRAM em bytes, 4 GiB por padrão; e VM sem GPU não chama nada de GPU.
func TestConfigureGPU(t *testing.T) {
	f := &fakeAPI{}
	s := jobSpec()
	s.GPU, s.GPUVRAMMiB = true, 2048
	if _, err := Configure(f, s); err != nil {
		t.Fatal(err)
	}
	if f.gpuFlags != 0xC0 || f.gpuShm != 2048<<20 {
		t.Fatalf("gpu flags %#x shm %d", f.gpuFlags, f.gpuShm)
	}
	got := strings.Join(f.calls, ",")
	if strings.Index(got, "krun_set_gpu_options2") > strings.Index(got, "krun_set_exec") {
		t.Fatalf("gpu must be configured before exec: %s", got)
	}
	f = &fakeAPI{}
	s.GPUVRAMMiB = 0
	Configure(f, s)
	if f.gpuShm != DefaultGPUVRAMMiB<<20 {
		t.Fatalf("default vram %d", f.gpuShm)
	}
	f = &fakeAPI{}
	Configure(f, jobSpec())
	if strings.Contains(strings.Join(f.calls, ","), "gpu") {
		t.Fatal("a VM without gpu must not touch the gpu device")
	}
}

// TestConfigureFailsOnEachCall garante que toda chamada C que falha vira erro com o nome dela.
func TestConfigureFailsOnEachCall(t *testing.T) {
	uid, gid := uint32(1), uint32(1)
	full := jobSpec()
	full.UID, full.GID = &uid, &gid
	dir := Spec{CPUs: 1, MemoryMiB: 1, RootDir: "/r", Exec: "/x"}
	tty := jobSpec()
	tty.Console, tty.TTY = "", true
	gpu := jobSpec()
	gpu.GPU = true
	names := map[string]Spec{
		"krun_set_gpu_options2": gpu,
		"krun_create_ctx":       full, "krun_set_vm_config": full, "krun_add_disk2": full,
		"krun_set_root_disk_remount": full, "krun_disable_implicit_vsock": full, "krun_add_vsock": full,
		"krun_add_vsock_port2": full, "krun_set_console_output": full, "krun_setgid": full,
		"krun_setuid": full, "krun_set_exec": full, "krun_set_root": dir,
		"krun_disable_implicit_console": tty, "krun_add_virtio_console_default": tty,
	}
	for name, s := range names {
		t.Run(name, func(t *testing.T) {
			_, err := Configure(&fakeAPI{failOn: name}, s)
			if err == nil || !strings.Contains(err.Error(), name) || !strings.Contains(err.Error(), "errno 22") {
				t.Fatalf("expected an error citing %s and errno 22, got %v", name, err)
			}
		})
	}
}

// TestValidate cobre cada recusa da spec, incluindo o argv com espaço medido no spike.
func TestValidate(t *testing.T) {
	ok := jobSpec()
	mut := func(f func(*Spec)) Spec { s := ok; f(&s); return s }
	bad := map[string]Spec{
		"no cpu":              mut(func(s *Spec) { s.CPUs = 0 }),
		"no memory":           mut(func(s *Spec) { s.MemoryMiB = 0 }),
		"two roots":           mut(func(s *Spec) { s.RootDir = "/x" }),
		"no root":             mut(func(s *Spec) { s.RootDisk = "" }),
		"no exec":             mut(func(s *Spec) { s.Exec = "" }),
		"socket without port": mut(func(s *Spec) { s.VsockPort = 0 }),
		"port without socket": mut(func(s *Spec) { s.VsockSocket = "" }),
		"argv with space":     mut(func(s *Spec) { s.Args = []string{"-c", "echo oi"} }),
		"empty argv":          mut(func(s *Spec) { s.Args = []string{""} }),
		"exec with tab":       mut(func(s *Spec) { s.Exec = "/bin/a\tb" }),
	}
	for name, s := range bad {
		t.Run(name, func(t *testing.T) {
			if err := s.Validate(); err == nil {
				t.Fatal("expected rejection")
			}
			if _, err := Configure(&fakeAPI{}, s); err == nil {
				t.Fatal("Configure should pass the rejection through")
			}
		})
	}
	if err := ok.Validate(); err != nil {
		t.Fatalf("valid spec rejected: %v", err)
	}
}

// TestCallErr trava o formato da mensagem de erro de chamada C.
func TestCallErr(t *testing.T) {
	if got := fmt.Sprint(callErr("krun_x", -2)); got != "krun_x failed: errno 2" {
		t.Fatalf("unexpected message: %s", got)
	}
}
