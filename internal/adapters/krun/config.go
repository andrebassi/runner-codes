// Package krun lança microVMs pela libkrun. Cada VM é um processo próprio
// (`runner.code vmm`), porque krun_start_enter assume o processo e sai com o
// código do workload; a sequência de chamadas da API C mora aqui, atrás de API,
// para ser testada sem a biblioteca.
package krun

import (
	"fmt"

	"runner.code/internal/ports"
)

// diskFormatRaw é KRUN_DISK_FORMAT_RAW em libkrun.h: o ext4 é imagem crua, sem qcow2.
const diskFormatRaw = 0

// tsiHijackInet é KRUN_TSI_HIJACK_INET: liga rede AF_INET/AF_INET6 por TSI.
// Só a VM de build usa; VM de job sempre recebe 0 (sem rede).
const tsiHijackInet = 1

// logLevelError é KRUN_LOG_LEVEL_ERROR: só erro no stderr do processo vmm.
const logLevelError = 1

// gpuVenusFlags são VIRGLRENDERER_VENUS (1<<6) | VIRGLRENDERER_NO_VIRGL (1<<7) em libkrun.h:
// só Vulkan (Venus), sem o virgl de OpenGL, porque o MoltenVK do macOS só traduz Vulkan. É
// a mesma combinação do krunkit e do `--gpu` do smolvm.
const gpuVenusFlags = 1<<6 | 1<<7

// DefaultGPUVRAMMiB é a VRAM do guest quando a spec não diz: 4 GiB, o padrão do smolvm. É
// uma janela de endereço do host, não RAM reservada; o host só aloca o que o guest usa.
const DefaultGPUVRAMMiB = 4096

// API é o subconjunto da libkrun 1.19 que o runner usa. Os retornos seguem a
// convenção C: negativo é -errno, zero ou positivo é sucesso (ou o id do contexto).
type API interface {
	SetLogLevel(level uint32) int32
	CreateCtx() int32
	SetVMConfig(ctx uint32, vcpus uint8, ramMiB uint32) int32
	SetRoot(ctx uint32, path string) int32
	AddDisk2(ctx uint32, blockID, path string, format uint32, readOnly bool) int32
	SetRootDiskRemount(ctx uint32, device, fstype, options string) int32
	DisableImplicitVsock(ctx uint32) int32
	AddVsock(ctx uint32, tsiFeatures uint32) int32
	AddVsockPort2(ctx uint32, port uint32, path string, listen bool) int32
	SetConsoleOutput(ctx uint32, path string) int32
	SetUID(ctx uint32, uid uint32) int32
	SetGID(ctx uint32, gid uint32) int32
	SetExec(ctx uint32, path string, argv, envp []string) int32
	DisableImplicitConsole(ctx uint32) int32
	AddVirtioConsoleDefault(ctx uint32, inFD, outFD, errFD int32) int32
	SetGPUOptions2(ctx uint32, virglFlags uint32, shmSize uint64) int32
	StartEnter(ctx uint32) int32
}

// Spec é a descrição de VM definida na porta (ports.VMSpec); o alias mantém o nome curto aqui.
type Spec = ports.VMSpec

// step é uma chamada C nomeada; o nome vai para a mensagem de erro.
type step struct {
	name string
	call func() int32
}

// Configure aplica a spec num contexto novo e devolve o id dele. Não inicia a VM.
func Configure(api API, s Spec) (uint32, error) {
	if err := s.Validate(); err != nil {
		return 0, err
	}
	api.SetLogLevel(logLevelError)
	id := api.CreateCtx()
	if id < 0 {
		return 0, callErr("krun_create_ctx", id)
	}
	ctx := uint32(id)
	var steps []step
	add := func(name string, call func() int32) { steps = append(steps, step{name, call}) }

	add("krun_set_vm_config", func() int32 { return api.SetVMConfig(ctx, s.CPUs, s.MemoryMiB) })
	if s.RootDisk != "" {
		add("krun_add_disk2", func() int32 { return api.AddDisk2(ctx, "root", s.RootDisk, diskFormatRaw, true) })
		add("krun_set_root_disk_remount", func() int32 { return api.SetRootDiskRemount(ctx, "/dev/vda", "ext4", "ro") })
		if s.DataDisk != "" {
			// Segundo disco = /dev/vdb, gravável: é o disco da máquina persistente.
			add("krun_add_disk2_data", func() int32 { return api.AddDisk2(ctx, "data", s.DataDisk, diskFormatRaw, false) })
		}
	} else {
		add("krun_set_root", func() int32 { return api.SetRoot(ctx, s.RootDir) })
	}
	// Sem desligar o vsock implícito a libkrun liga TSI sozinha e o guest ganha rede.
	tsi := uint32(0)
	if s.Network {
		tsi = tsiHijackInet
	}
	add("krun_disable_implicit_vsock", func() int32 { return api.DisableImplicitVsock(ctx) })
	add("krun_add_vsock", func() int32 { return api.AddVsock(ctx, tsi) })
	if s.VsockSocket != "" {
		// listen=true: o host conecta e a libkrun repassa para o guest que escuta na porta.
		add("krun_add_vsock_port2", func() int32 { return api.AddVsockPort2(ctx, s.VsockPort, s.VsockSocket, true) })
	}
	if s.TTY {
		// Sem o console implícito, o primeiro console explícito é o hvc0. Com os três fds
		// sendo TTY, a libkrun cria uma porta de console só, e o init do guest a usa
		// como terminal do programa (libkrun.h, krun_add_virtio_console_default).
		add("krun_disable_implicit_console", func() int32 { return api.DisableImplicitConsole(ctx) })
		add("krun_add_virtio_console_default", func() int32 { return api.AddVirtioConsoleDefault(ctx, 0, 1, 2) })
	}
	if s.GPU {
		vram := s.GPUVRAMMiB
		if vram == 0 {
			vram = DefaultGPUVRAMMiB
		}
		add("krun_set_gpu_options2", func() int32 { return api.SetGPUOptions2(ctx, gpuVenusFlags, uint64(vram)<<20) })
	}
	if s.Console != "" {
		add("krun_set_console_output", func() int32 { return api.SetConsoleOutput(ctx, s.Console) })
	}
	// GID antes do UID: depois de largar o uid 0 o processo não pode mais trocar o grupo.
	if s.GID != nil {
		add("krun_setgid", func() int32 { return api.SetGID(ctx, *s.GID) })
	}
	if s.UID != nil {
		add("krun_setuid", func() int32 { return api.SetUID(ctx, *s.UID) })
	}
	add("krun_set_exec", func() int32 { return api.SetExec(ctx, s.Exec, s.Args, s.Env) })

	for _, st := range steps {
		if rc := st.call(); rc < 0 {
			return 0, callErr(st.name, rc)
		}
	}
	return ctx, nil
}

// callErr padroniza o erro de uma chamada C que devolveu -errno.
func callErr(name string, rc int32) error {
	return fmt.Errorf("%s failed: errno %d", name, -rc)
}
