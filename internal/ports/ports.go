// Package ports define os contratos entre o núcleo (app) e o mundo de fora: lançar VM,
// falar com o guest, achar a imagem de uma linguagem. Os adapters implementam; o app
// só conhece estas interfaces.
package ports

import (
	"context"
	"errors"
	"fmt"
	"strings"

	"runner.code/internal/domain"
	"runner.code/internal/protocol"
)

// VMSpec descreve uma VM. Vai em JSON do serviço para o processo vmm.
type VMSpec struct {
	CPUs      uint8  `json:"cpus"`
	MemoryMiB uint32 `json:"memory_mib"`
	// RootDisk é um ext4 montado read-only como /. Usado pelas VMs de job.
	RootDisk string `json:"root_disk,omitempty"`
	// RootDir é um diretório compartilhado por virtiofs como /. Só a VM de build usa,
	// porque virtiofs não impede o guest de alcançar o resto do host (doc da libkrun).
	RootDir string `json:"root_dir,omitempty"`
	// DataDisk é o ext4 gravável de uma máquina persistente, visto como /dev/vdb no guest
	// (machine.sh monta o overlay sobre a imagem com ele). Só com RootDisk.
	DataDisk    string `json:"data_disk,omitempty"`
	VsockSocket string `json:"vsock_socket,omitempty"`
	VsockPort   uint32 `json:"vsock_port,omitempty"`
	Network     bool   `json:"network,omitempty"`
	Console     string `json:"console,omitempty"`
	// TTY liga o stdin/stdout/stderr do processo vmm a uma porta de console virtio no
	// guest (/dev/hvc0), que o guest enxerga como terminal: Ctrl+C, job control e
	// tamanho da janela funcionam. Só faz sentido com um terminal de verdade no host.
	TTY bool `json:"tty,omitempty"`
	// GPU liga um virtio-gpu com Venus: o guest vê um device Vulkan servido pela GPU do
	// host (MoltenVK no macOS). GPUVRAMMiB é a janela de memória do host que o guest usa
	// como VRAM; 0 usa o padrão do adapter.
	GPU        bool     `json:"gpu,omitempty"`
	GPUVRAMMiB uint32   `json:"gpu_vram_mib,omitempty"`
	Exec       string   `json:"exec"`
	Args       []string `json:"args,omitempty"`
	Env        []string `json:"env,omitempty"`
	// UID e GID trocam o usuário do processo vmm antes do boot (Linux). Nil mantém o atual.
	UID *uint32 `json:"uid,omitempty"`
	GID *uint32 `json:"gid,omitempty"`
}

// Validate recusa spec que a libkrun aceitaria mas que quebraria em silêncio dentro do guest.
func (s VMSpec) Validate() error {
	if s.CPUs == 0 || s.MemoryMiB == 0 {
		return errors.New("spec: cpus and memory_mib must be greater than zero")
	}
	if (s.RootDisk == "") == (s.RootDir == "") {
		return errors.New("spec: set exactly one of root_disk and root_dir")
	}
	if s.Exec == "" {
		return errors.New("spec: exec is required")
	}
	if s.TTY && s.Console != "" {
		// O console vira o terminal: gravar em arquivo ao mesmo tempo não tem destino.
		return errors.New("spec: tty and console are mutually exclusive")
	}
	if s.DataDisk != "" && s.RootDisk == "" {
		// /dev/vdb só existe ao lado de um root em disco; com virtiofs a ordem dos discos muda.
		return errors.New("spec: data_disk requires root_disk")
	}
	if s.GPUVRAMMiB > 0 && !s.GPU {
		// VRAM sem GPU é erro de quem montou a spec, não algo a ignorar.
		return errors.New("spec: gpu_vram_mib requires gpu")
	}
	if (s.VsockSocket == "") != (s.VsockPort == 0) {
		return errors.New("spec: vsock_socket and vsock_port must be set together")
	}
	// A libkrun leva o argv pela linha de comando do kernel: argumento com espaço
	// chega partido no guest (medido no spike). Comando composto vai por arquivo.
	for _, a := range append([]string{s.Exec}, s.Args...) {
		if a == "" || strings.ContainsAny(a, " \t\n") {
			return fmt.Errorf("spec: argument %q is empty or contains whitespace; pass the command through a file in the image", a)
		}
	}
	return nil
}

// VM é uma microVM viva, um processo por VM.
type VM interface {
	PID() int
	// Done fecha quando o processo da VM termina.
	Done() <-chan struct{}
	// Wait espera o fim e devolve o código de saída do workload.
	Wait() int
	// Kill derruba a VM na hora (SIGKILL no grupo). Idempotente.
	Kill()
}

// VMLauncher cria VMs.
type VMLauncher interface {
	StartVM(spec VMSpec) (VM, error)
}

// GuestClient conversa com o guest-runner de uma VM pelo socket do vsock.
type GuestClient interface {
	Run(ctx context.Context, socket string, job protocol.Job) (protocol.Result, error)
	// RunStream é o Run com a saída entregue a onOutput enquanto o programa roda; o
	// Result final é o mesmo do Run.
	RunStream(ctx context.Context, socket string, job protocol.Job, onOutput func(stream, data string)) (protocol.Result, error)
	WaitReady(ctx context.Context, socket string) error
}

// MachineStore guarda o registro das máquinas persistentes e o diretório de cada uma
// (disco, spec, socket, logs). Erros de ausência e de nome repetido vêm com os códigos
// do domínio (NOT_FOUND, CONFLICT).
type MachineStore interface {
	// Create registra a máquina e dá a ela um disco clonado do modelo vazio.
	Create(m domain.Machine, diskTemplate string) error
	Load(name string) (domain.Machine, error)
	List() ([]domain.Machine, error)
	Delete(name string) error
	// Dir é o diretório da máquina; os arquivos dela moram ali.
	Dir(name string) string
	// PID é o processo vmm registrado (0 se nenhum); SetPID grava ou limpa (0).
	PID(name string) int
	SetPID(name string, pid int) error
}

// DetachedLauncher sobe a VM de uma máquina desligada do processo que pediu.
type DetachedLauncher interface {
	StartDetached(spec VMSpec, specPath, logPath string) (pid int, err error)
}

// Processes olha e mata o processo vmm de uma máquina pelo pid registrado.
type Processes interface {
	Alive(pid int) bool
	// Kill manda SIGKILL ao grupo do processo (o vmm e o que ele criou).
	Kill(pid int)
}

// ImageStore diz onde está o ext4 pronto de uma linguagem.
type ImageStore interface {
	ImagePath(language string) (string, bool)
}
