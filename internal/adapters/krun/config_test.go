package krun

import (
	"errors"
	"fmt"
	"strings"
	"testing"
	"unsafe"

	"runner.code/internal/ports"
)

// fakeAPI registra cada chamada e devolve erro na chamada nomeada em failOn.
type fakeAPI struct {
	calls []string
	// vsockPorts são as portas vsock pedidas ("porta caminho listen").
	vsockPorts []string
	failOn     string
	ctxID      int32
	tsi        uint32
	disk       struct {
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
	// mounts guarda cada virtiofs pedido: "tag=path:ro|rw".
	mounts []string
	// netFD e netMAC guardam a virtio-net pedida.
	netFD  int32
	netMAC []byte
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

// AddVsockPort2 anota a chamada e os argumentos (porta, caminho, listen).
func (f *fakeAPI) AddVsockPort2(_ uint32, port uint32, path string, listen bool) int32 {
	f.vsockPorts = append(f.vsockPorts, fmt.Sprintf("%d %s %v", port, path, listen))
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

// AddInputDevice anota a chamada.
func (f *fakeAPI) AddInputDevice(uint32, unsafe.Pointer, uintptr, unsafe.Pointer, uintptr) int32 {
	return f.rec("krun_add_input_device")
}

// AddDisplay anota a chamada.
func (f *fakeAPI) AddDisplay(uint32, uint32, uint32) int32 { return f.rec("krun_add_display") }

// SetDisplayBackend anota a chamada.
func (f *fakeAPI) SetDisplayBackend(uint32, unsafe.Pointer, uintptr) int32 {
	return f.rec("krun_set_display_backend")
}

// AddNetUnixstream anota a chamada e guarda fd e MAC.
func (f *fakeAPI) AddNetUnixstream(_ uint32, fd int32, mac []byte) int32 {
	f.netFD, f.netMAC = fd, mac
	return f.rec("krun_add_net_unixstream")
}

// AddVirtiofs3 anota a chamada e guarda tag, caminho e modo.
func (f *fakeAPI) AddVirtiofs3(_ uint32, tag, path string, shm uint64, ro bool) int32 {
	mode := "rw"
	if ro {
		mode = "ro"
	}
	f.mounts = append(f.mounts, fmt.Sprintf("%s=%s:%s:%d", tag, path, mode, shm))
	return f.rec("krun_add_virtiofs3")
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

// TestConfigureMounts: cada montagem vira um virtiofs3 com a tag e o modo pedidos, sem janela
// DAX, antes do exec; VM sem montagem não expõe nada do host.
func TestConfigureMounts(t *testing.T) {
	f := &fakeAPI{}
	s := jobSpec()
	s.Mounts = []ports.VMMount{{Tag: "vol0", HostPath: "/h/www"}, {Tag: "vol1", HostPath: "/h/models", ReadOnly: true}}
	if _, err := Configure(f, s); err != nil {
		t.Fatal(err)
	}
	if strings.Join(f.mounts, ",") != "vol0=/h/www:rw:0,vol1=/h/models:ro:0" {
		t.Fatalf("mounts: %v", f.mounts)
	}
	got := strings.Join(f.calls, ",")
	if strings.LastIndex(got, "krun_add_virtiofs3") > strings.Index(got, "krun_set_exec") {
		t.Fatalf("mounts must come before exec: %s", got)
	}
	f = &fakeAPI{}
	Configure(f, jobSpec())
	if len(f.mounts) != 0 {
		t.Fatal("a VM without mounts must not share any host directory")
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
	mnt := jobSpec()
	mnt.Mounts = []ports.VMMount{{Tag: "vol0", HostPath: "/h"}}
	names := map[string]Spec{
		"krun_set_gpu_options2": gpu, "krun_add_virtiofs3": mnt,
		"krun_create_ctx": full, "krun_set_vm_config": full, "krun_add_disk2": full,
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
		"mount without tag":   mut(func(s *Spec) { s.Mounts = []ports.VMMount{{HostPath: "/h"}} }),
		"mount relative":      mut(func(s *Spec) { s.Mounts = []ports.VMMount{{Tag: "v", HostPath: "h"}} }),
		"mount tag twice":     mut(func(s *Spec) { s.Mounts = []ports.VMMount{{Tag: "v", HostPath: "/a"}, {Tag: "v", HostPath: "/b"}} }),
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

// TestConfigureSSHAgent: com o agente SSH, a libkrun liga a porta 5001 do guest ao
// SSH_AUTH_SOCK do host com listen=false (quem conecta é o guest); sem ele, só a porta do guest.
func TestConfigureSSHAgent(t *testing.T) {
	api := &fakeAPI{}
	s := jobSpec()
	s.SSHAgent = "/tmp/agent.sock"
	if _, err := configure(api, s, -1); err != nil {
		t.Fatal(err)
	}
	if len(api.vsockPorts) != 2 || api.vsockPorts[1] != "5001 /tmp/agent.sock false" || !strings.HasSuffix(api.vsockPorts[0], " true") {
		t.Fatalf("vsock ports: %v", api.vsockPorts)
	}
	api = &fakeAPI{}
	configure(api, jobSpec(), -1)
	if len(api.vsockPorts) != 1 {
		t.Fatalf("without the agent, only the guest port: %v", api.vsockPorts)
	}
}

// nestedFake é um fakeAPI com a parte opcional de nested: check é o que o host responde.
type nestedFake struct {
	fakeAPI
	check int32
	on    bool
}

// CheckNestedVirt devolve a resposta configurada do host.
func (f *nestedFake) CheckNestedVirt() int32 { return f.check }

// SetNestedVirt anota o pedido.
func (f *nestedFake) SetNestedVirt(_ uint32, on bool) int32 {
	f.on = on
	return f.rec("krun_set_nested_virt")
}

// TestConfigureNested: host que aninha liga o nested; host que não aninha, erro na checagem e
// libkrun sem o símbolo recusam antes do boot; VM sem Nested nem pergunta.
func TestConfigureNested(t *testing.T) {
	s := jobSpec()
	s.Nested = true
	api := &nestedFake{check: 1}
	if _, err := configure(api, s, -1); err != nil || !api.on {
		t.Fatalf("supported host: %v on=%v", err, api.on)
	}
	if _, err := configure(&nestedFake{check: 0}, s, -1); !errors.Is(err, ErrNestedUnsupported) {
		t.Fatalf("host without nested: %v", err)
	}
	if _, err := configure(&nestedFake{check: -22}, s, -1); err == nil || !strings.Contains(err.Error(), "krun_check_nested_virt") {
		t.Fatalf("failed check: %v", err)
	}
	if _, err := configure(&fakeAPI{}, s, -1); err == nil || !strings.Contains(err.Error(), "krun_set_nested_virt") {
		t.Fatalf("libkrun without nested: %v", err)
	}
	api = &nestedFake{check: 0}
	if _, err := configure(api, jobSpec(), -1); err != nil || api.on {
		t.Fatalf("a VM without Nested must not ask: %v", err)
	}
	fail := &nestedFake{check: 1}
	fail.failOn = "krun_set_nested_virt"
	if _, err := configure(fail, s, -1); err == nil {
		t.Fatal("a failing krun_set_nested_virt must fail the configure")
	}
}

// TestConfigureHostSockets: cada socket do host montado no guest vira uma porta vsock em que
// a libkrun disca (listen=false), como o agente SSH.
func TestConfigureHostSockets(t *testing.T) {
	api := &fakeAPI{}
	s := jobSpec()
	s.HostSockets = []ports.VMSocket{{Host: "/h/a.sock", Guest: "/run/a.sock", Port: 5100}, {Host: "/h/b.sock", Guest: "/run/b.sock", Port: 5101}}
	if _, err := configure(api, s, -1); err != nil {
		t.Fatal(err)
	}
	if len(api.vsockPorts) != 3 || api.vsockPorts[1] != "5100 /h/a.sock false" || api.vsockPorts[2] != "5101 /h/b.sock false" {
		t.Fatalf("vsock ports: %v", api.vsockPorts)
	}
}
