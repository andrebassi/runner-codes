package krun

import (
	"fmt"
	"syscall"
	"unsafe"

	"github.com/ebitengine/purego"
)

// libAPI implementa API chamando a libkrun carregada por dlopen (purego, sem cgo,
// para manter CGO_ENABLED=0 e o cross-compile). Cada método só repassa ao símbolo C.
type libAPI struct {
	setLogLevel          func(uint32) int32
	createCtx            func() int32
	setVMConfig          func(uint32, uint8, uint32) int32
	setRoot              func(uint32, string) int32
	addDisk2             func(uint32, string, string, uint32, bool) int32
	setRootDiskRemount   func(uint32, string, string, string) int32
	disableImplicitVsock func(uint32) int32
	addVsock             func(uint32, uint32) int32
	addVsockPort2        func(uint32, uint32, string, bool) int32
	setConsoleOutput     func(uint32, string) int32
	setUID               func(uint32, uint32) int32
	setGID               func(uint32, uint32) int32
	setExec              func(uint32, string, unsafe.Pointer, unsafe.Pointer) int32
	startEnter           func(uint32) int32
	disableImplConsole   func(uint32) int32
	addVirtioConsoleDef  func(uint32, int32, int32, int32) int32
	// setGPUOptions2 fica nil quando a libkrun foi compilada sem GPU=1 (a do nixpkgs no
	// Linux): o símbolo nem existe, e só a VM que pedir GPU deve falhar.
	setGPUOptions2 func(uint32, uint32, uint64) int32
	addVirtiofs3   func(uint32, string, string, uint64, bool) int32
	// addNetUnixstream recebe o caminho como ponteiro para poder passar NULL (o fd é que
	// liga o device; a libkrun exige path NULL quando fd != -1).
	addNetUnixstream func(uint32, *byte, int32, *byte, uint32, uint32) int32
	addDisplay       func(uint32, uint32, uint32) int32
	setDisplayBack   func(uint32, unsafe.Pointer, uintptr) int32
	addInputDevice   func(uint32, unsafe.Pointer, uintptr, unsafe.Pointer, uintptr) int32
	// setNestedVirt e checkNestedVirt ficam nil numa libkrun anterior ao suporte a nested.
	setNestedVirt   func(uint32, bool) int32
	checkNestedVirt func() int32
	// snapshotRequest e restore ficam nil numa libkrun sem snapshot (a 1.x do nixpkgs; o fork do superbot tem).
	snapshotRequest func(uint32, string, uint32) int32
	restore         func(uint32, string, uint32) int32
}

// keepAlive segura os arrays argv/envp passados à libkrun: krun_start_enter não volta
// em caso de sucesso e a libkrun lê os ponteiros depois, então o GC não pode recolhê-los.
var keepAlive [][]*byte

// Load abre a libkrun em path e resolve os símbolos. RegisterLibFunc entra em pânico
// quando o símbolo falta; o recover transforma isso em erro (libkrun antiga demais).
func Load(path string) (api API, err error) {
	h, err := purego.Dlopen(path, purego.RTLD_NOW|purego.RTLD_GLOBAL)
	if err != nil {
		return nil, fmt.Errorf("open libkrun at %s: %w", path, err)
	}
	defer func() {
		if r := recover(); r != nil {
			api, err = nil, fmt.Errorf("libkrun at %s lacks an expected symbol: %v", path, r)
		}
	}()
	l := &libAPI{}
	reg := func(fn interface{}, name string) { purego.RegisterLibFunc(fn, h, name) }
	reg(&l.setLogLevel, "krun_set_log_level")
	reg(&l.createCtx, "krun_create_ctx")
	reg(&l.setVMConfig, "krun_set_vm_config")
	reg(&l.setRoot, "krun_set_root")
	reg(&l.addDisk2, "krun_add_disk2")
	reg(&l.setRootDiskRemount, "krun_set_root_disk_remount")
	reg(&l.disableImplicitVsock, "krun_disable_implicit_vsock")
	reg(&l.addVsock, "krun_add_vsock")
	reg(&l.addVsockPort2, "krun_add_vsock_port2")
	reg(&l.setConsoleOutput, "krun_set_console_output")
	reg(&l.setUID, "krun_setuid")
	reg(&l.setGID, "krun_setgid")
	reg(&l.setExec, "krun_set_exec")
	reg(&l.startEnter, "krun_start_enter")
	reg(&l.disableImplConsole, "krun_disable_implicit_console")
	reg(&l.addVirtioConsoleDef, "krun_add_virtio_console_default")
	reg(&l.addVirtiofs3, "krun_add_virtiofs3")
	reg(&l.addNetUnixstream, "krun_add_net_unixstream")
	reg(&l.addDisplay, "krun_add_display")
	reg(&l.setDisplayBack, "krun_set_display_backend")
	reg(&l.addInputDevice, "krun_add_input_device")
	// Opcional: confere com Dlsym antes, porque o RegisterLibFunc entraria em pânico e o
	// recover acima recusaria a biblioteca inteira.
	if _, err := purego.Dlsym(h, "krun_set_gpu_options2"); err == nil {
		reg(&l.setGPUOptions2, "krun_set_gpu_options2")
	}
	if _, err := purego.Dlsym(h, "krun_set_nested_virt"); err == nil {
		reg(&l.setNestedVirt, "krun_set_nested_virt")
		reg(&l.checkNestedVirt, "krun_check_nested_virt")
	}
	if _, err := purego.Dlsym(h, "krun_snapshot_request"); err == nil {
		reg(&l.snapshotRequest, "krun_snapshot_request")
		reg(&l.restore, "krun_restore")
	}
	return l, nil
}

// SetGPUOptions2 repassa a krun_set_gpu_options2; sem o símbolo devolve -ENOTSUP, que o
// Configure transforma em erro com o nome da chamada.
func (l *libAPI) SetGPUOptions2(ctx uint32, virglFlags uint32, shmSize uint64) int32 {
	if l.setGPUOptions2 == nil {
		return -int32(syscall.ENOTSUP)
	}
	return l.setGPUOptions2(ctx, virglFlags, shmSize)
}

// HasGPU diz se a libkrun carregada foi compilada com GPU (tem krun_set_gpu_options2).
func (l *libAPI) HasGPU() bool { return l.setGPUOptions2 != nil }

// CheckNestedVirt repassa a krun_check_nested_virt: 1 o host suporta, 0 não, < 0 erro.
// Sem o símbolo devolve -ENOTSUP.
func (l *libAPI) CheckNestedVirt() int32 {
	if l.checkNestedVirt == nil {
		return -int32(syscall.ENOTSUP)
	}
	return l.checkNestedVirt()
}

// SetNestedVirt repassa a krun_set_nested_virt; sem o símbolo devolve -ENOTSUP.
func (l *libAPI) SetNestedVirt(ctx uint32, on bool) int32 {
	if l.setNestedVirt == nil {
		return -int32(syscall.ENOTSUP)
	}
	return l.setNestedVirt(ctx, on)
}

// cStringArray monta um `const char *const[]` terminado em NULL e o mantém vivo.
func cStringArray(items []string) unsafe.Pointer {
	ptrs := make([]*byte, 0, len(items)+1)
	for _, s := range items {
		b := append([]byte(s), 0)
		ptrs = append(ptrs, &b[0])
	}
	ptrs = append(ptrs, nil)
	keepAlive = append(keepAlive, ptrs)
	return unsafe.Pointer(&ptrs[0])
}

// SetLogLevel repassa a krun_set_log_level.
func (l *libAPI) SetLogLevel(level uint32) int32 { return l.setLogLevel(level) }

// CreateCtx repassa a krun_create_ctx.
func (l *libAPI) CreateCtx() int32 { return l.createCtx() }

// SetVMConfig repassa a krun_set_vm_config.
func (l *libAPI) SetVMConfig(ctx uint32, vcpus uint8, ramMiB uint32) int32 {
	return l.setVMConfig(ctx, vcpus, ramMiB)
}

// SetRoot repassa a krun_set_root.
func (l *libAPI) SetRoot(ctx uint32, path string) int32 { return l.setRoot(ctx, path) }

// AddDisk2 repassa a krun_add_disk2.
func (l *libAPI) AddDisk2(ctx uint32, blockID, path string, format uint32, readOnly bool) int32 {
	return l.addDisk2(ctx, blockID, path, format, readOnly)
}

// AddNetUnixstream repassa a krun_add_net_unixstream com path NULL, o fd do socketpair e
// features 0: sem offload de checksum/TSO, porque o gateway não trata o cabeçalho
// virtio-net (cada frame é Ethernet cru).
func (l *libAPI) AddNetUnixstream(ctx uint32, fd int32, mac []byte) int32 {
	if len(mac) != 6 {
		return -int32(syscall.EINVAL)
	}
	return l.addNetUnixstream(ctx, nil, fd, &mac[0], 0, 0)
}

// AddDisplay repassa a krun_add_display; devolve o id da tela (0..) ou -errno.
func (l *libAPI) AddDisplay(ctx uint32, width, height uint32) int32 {
	return l.addDisplay(ctx, width, height)
}

// SetDisplayBackend repassa a krun_set_display_backend com a vtable de callbacks.
func (l *libAPI) SetDisplayBackend(ctx uint32, backend unsafe.Pointer, size uintptr) int32 {
	return l.setDisplayBack(ctx, backend, size)
}

// AddInputDevice repassa a krun_add_input_device (config e provedor de eventos separados).
func (l *libAPI) AddInputDevice(ctx uint32, config unsafe.Pointer, configSize uintptr, events unsafe.Pointer, eventsSize uintptr) int32 {
	return l.addInputDevice(ctx, config, configSize, events, eventsSize)
}

// AddVirtiofs3 repassa a krun_add_virtiofs3 (diretório do host com tag, opcionalmente ro).
func (l *libAPI) AddVirtiofs3(ctx uint32, tag, path string, shmSize uint64, readOnly bool) int32 {
	return l.addVirtiofs3(ctx, tag, path, shmSize, readOnly)
}

// SetRootDiskRemount repassa a krun_set_root_disk_remount.
func (l *libAPI) SetRootDiskRemount(ctx uint32, device, fstype, options string) int32 {
	return l.setRootDiskRemount(ctx, device, fstype, options)
}

// DisableImplicitVsock repassa a krun_disable_implicit_vsock.
func (l *libAPI) DisableImplicitVsock(ctx uint32) int32 { return l.disableImplicitVsock(ctx) }

// AddVsock repassa a krun_add_vsock.
func (l *libAPI) AddVsock(ctx uint32, tsiFeatures uint32) int32 { return l.addVsock(ctx, tsiFeatures) }

// AddVsockPort2 repassa a krun_add_vsock_port2.
func (l *libAPI) AddVsockPort2(ctx uint32, port uint32, path string, listen bool) int32 {
	return l.addVsockPort2(ctx, port, path, listen)
}

// SetConsoleOutput repassa a krun_set_console_output.
func (l *libAPI) SetConsoleOutput(ctx uint32, path string) int32 {
	return l.setConsoleOutput(ctx, path)
}

// SetUID repassa a krun_setuid.
func (l *libAPI) SetUID(ctx uint32, uid uint32) int32 { return l.setUID(ctx, uid) }

// SetGID repassa a krun_setgid.
func (l *libAPI) SetGID(ctx uint32, gid uint32) int32 { return l.setGID(ctx, gid) }

// SetExec repassa a krun_set_exec, convertendo argv e envp para arrays C.
func (l *libAPI) SetExec(ctx uint32, path string, argv, envp []string) int32 {
	return l.setExec(ctx, path, cStringArray(argv), cStringArray(envp))
}

// DisableImplicitConsole repassa a krun_disable_implicit_console.
func (l *libAPI) DisableImplicitConsole(ctx uint32) int32 { return l.disableImplConsole(ctx) }

// AddVirtioConsoleDefault repassa a krun_add_virtio_console_default.
func (l *libAPI) AddVirtioConsoleDefault(ctx uint32, inFD, outFD, errFD int32) int32 {
	return l.addVirtioConsoleDef(ctx, inFD, outFD, errFD)
}

// StartEnter repassa a krun_start_enter; só volta em erro.
func (l *libAPI) StartEnter(ctx uint32) int32 { return l.startEnter(ctx) }

// SnapshotRequest repassa a krun_snapshot_request (pedido feito de outra thread: a libkrun grava e encerra o
// processo); sem o símbolo devolve -ENOTSUP.
func (l *libAPI) SnapshotRequest(ctx uint32, path string, flags uint32) int32 {
	if l.snapshotRequest == nil {
		return -int32(syscall.ENOTSUP)
	}
	return l.snapshotRequest(ctx, path, flags)
}

// Restore repassa a krun_restore (no lugar do krun_start_enter); sem o símbolo devolve -ENOTSUP.
func (l *libAPI) Restore(ctx uint32, path string, flags uint32) int32 {
	if l.restore == nil {
		return -int32(syscall.ENOTSUP)
	}
	return l.restore(ctx, path, flags)
}

// HasSnapshot diz se a libkrun carregada sabe suspender e restaurar.
func (l *libAPI) HasSnapshot() bool { return l.snapshotRequest != nil }
