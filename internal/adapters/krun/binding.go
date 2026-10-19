package krun

import (
	"fmt"
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
}

// keepAlive segura os arrays argv/envp passados à libkrun: krun_start_enter não volta
// em caso de sucesso e a libkrun lê os ponteiros depois, então o GC não pode recolhê-los.
var keepAlive [][]*byte

// Load abre a libkrun em path e resolve os símbolos. RegisterLibFunc entra em pânico
// quando o símbolo falta; o recover transforma isso em erro (libkrun antiga demais).
func Load(path string) (api API, err error) {
	h, err := purego.Dlopen(path, purego.RTLD_NOW|purego.RTLD_GLOBAL)
	if err != nil {
		return nil, fmt.Errorf("abrir libkrun em %s: %w", path, err)
	}
	defer func() {
		if r := recover(); r != nil {
			api, err = nil, fmt.Errorf("libkrun em %s sem símbolo esperado: %v", path, r)
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
	return l, nil
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

// StartEnter repassa a krun_start_enter; só volta em erro.
func (l *libAPI) StartEnter(ctx uint32) int32 { return l.startEnter(ctx) }
