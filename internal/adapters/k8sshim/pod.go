package k8sshim

import (
	"context"
	"io"

	specs "github.com/opencontainers/runtime-spec/specs-go"
)

// PodSpec descreve a VM de um pod.
type PodSpec struct {
	ID       string
	Netns    string
	Hostname string
	Resources
}

// ContainerFS é o que vira o sistema de arquivos de um container dentro da VM: o rootfs que o
// containerd montou no host e as montagens da spec.
type ContainerFS struct {
	ID     string
	Rootfs string
	Mounts []specs.Mount
}

// Stdio liga um processo às FIFOs do containerd. Stdin nil = sem entrada; com Terminal, toda a
// saída vai para Stdout.
type Stdio struct {
	Stdin  io.Reader
	Stdout io.Writer
	Stderr io.Writer
}

// Proc é um processo vivo dentro da VM.
type Proc interface {
	// Signal manda o sinal ao processo.
	Signal(sig int) error
	// Resize muda o tamanho do TTY.
	Resize(rows, cols uint16) error
	// CloseStdin fecha a entrada (CloseIO do containerd).
	CloseStdin() error
	// Wait bloqueia até o processo sair e devolve o código (128+sinal quando morto por sinal).
	Wait() int
}

// VM é a microVM de um pod.
type VM interface {
	// PrepareContainer copia o rootfs e as montagens bind para o share e monta tudo no guest;
	// devolve o caminho do root do container dentro da VM.
	PrepareContainer(ctx context.Context, fs ContainerFS) (string, error)
	// RemoveContainer desmonta e apaga a cópia do container.
	RemoveContainer(ctx context.Context, id string) error
	// Start roda um processo em chroot no root do container.
	Start(ctx context.Context, root string, p ProcSpec, io Stdio) (Proc, error)
	// Usage é o custo da VM no nó: CPU em microssegundos e memória residente em bytes.
	Usage() (cpuUsec, memBytes uint64)
	// Close desliga a VM e apaga o share do pod.
	Close() error
}

// Booter liga a VM de um pod.
type Booter interface {
	Boot(ctx context.Context, p PodSpec) (VM, error)
}
