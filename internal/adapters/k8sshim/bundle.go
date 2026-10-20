// Package k8sshim é o runtime do rc para Kubernetes: um shim v2 do containerd
// (io.containerd.rc.v2) em que cada pod é uma microVM da libkrun. O container pause (sandbox) liga a
// VM; os containers da app rodam dentro dela, em chroot no rootfs que o containerd preparou, copiado
// para um virtio-fs do pod. A rede do pod (o netns do CNI) chega à VM por TAP + tc mirred.
package k8sshim

import (
	"encoding/json"
	"errors"
	"fmt"
	"os"
	"path/filepath"

	specs "github.com/opencontainers/runtime-spec/specs-go"
)

// Anotações que o CRI do containerd põe na spec OCI de cada container de um pod.
const (
	annContainerType = "io.kubernetes.cri.container-type"
	annSandboxID     = "io.kubernetes.cri.sandbox-id"
	annSandboxNetns  = "io.kubernetes.cri.sandbox-network-ns"
	containerSandbox = "sandbox"
)

// GroupLabels agrupam os containers de um pod num shim só: com eles o containerd reusa o shim do
// sandbox para os containers da app, em vez de abrir um por container (que não acharia a VM).
var GroupLabels = []string{"io.containerd.rc.v2.group", annSandboxID}

// Bundle é o que o shim precisa da spec OCI de um container.
type Bundle struct {
	Spec *specs.Spec
	// Sandbox diz se é o container pause do pod (o que liga a VM). Sem anotação do CRI (ctr run),
	// o próprio container é o seu sandbox.
	Sandbox bool
	// SandboxID é o id do pod; vazio no ctr run.
	SandboxID string
	// Netns é o netns que o CNI configurou para o pod; vazio = VM sem rede.
	Netns string
}

// ReadBundle lê o config.json do bundle.
func ReadBundle(dir string) (Bundle, error) {
	b, err := os.ReadFile(filepath.Join(dir, "config.json"))
	if err != nil {
		return Bundle{}, err
	}
	return ParseBundle(b)
}

// ParseBundle interpreta a spec OCI e classifica o container.
func ParseBundle(data []byte) (Bundle, error) {
	var s specs.Spec
	if err := json.Unmarshal(data, &s); err != nil {
		return Bundle{}, fmt.Errorf("bundle config.json: %w", err)
	}
	if s.Process == nil || len(s.Process.Args) == 0 {
		return Bundle{}, errors.New("bundle config.json without process args")
	}
	b := Bundle{Spec: &s, SandboxID: s.Annotations[annSandboxID]}
	t, annotated := s.Annotations[annContainerType]
	b.Sandbox = !annotated || t == containerSandbox
	b.Netns = s.Annotations[annSandboxNetns]
	if b.Netns == "" && s.Linux != nil {
		for _, ns := range s.Linux.Namespaces {
			if ns.Type == specs.NetworkNamespace && ns.Path != "" {
				b.Netns = ns.Path
			}
		}
	}
	return b, nil
}

// GroupID devolve o id que agrupa este container num shim: o do pod, ou o próprio id.
func GroupID(s *specs.Spec, id string) string {
	for _, l := range GroupLabels {
		if g := s.Annotations[l]; g != "" {
			return g
		}
	}
	return id
}

// ProcSpec é um processo a rodar dentro da VM: o container (init) ou um exec.
type ProcSpec struct {
	Args     []string
	Env      []string
	Cwd      string
	UID, GID uint32
	Groups   []uint32
	Terminal bool
	Rows     uint16
	Cols     uint16
}

// ProcFromOCI traduz o process da spec OCI.
func ProcFromOCI(p *specs.Process) ProcSpec {
	ps := ProcSpec{Args: p.Args, Env: p.Env, Cwd: p.Cwd, UID: p.User.UID, GID: p.User.GID, Groups: p.User.AdditionalGids, Terminal: p.Terminal}
	if p.ConsoleSize != nil {
		ps.Rows, ps.Cols = uint16(p.ConsoleSize.Height), uint16(p.ConsoleSize.Width)
	}
	return ps
}

// Resources são CPU e memória da VM do pod, tirados dos limites do container pause e das anotações
// do pod. Sem limite, o padrão (a memória da libkrun é elástica: o teto não é reservado).
type Resources struct {
	CPUs      uint8
	MemoryMiB uint32
}

// Padrões da VM de um pod: 2 vCPU e 1 GiB de teto, como uma máquina pequena do rc.
const (
	defaultCPUs      = 2
	defaultMemoryMiB = 1024
	// annCPUs e annMemory deixam o pod pedir outro tamanho de VM (a soma dos limites dos containers
	// só chega depois do sandbox, quando a VM já ligou).
	annCPUs   = "runner.codes/cpus"
	annMemory = "runner.codes/memory-mib"
)

// PodResources lê o tamanho da VM das anotações do sandbox; valor inválido ou fora dos limites cai no
// padrão (1 a 16 vCPU, 128 MiB a 64 GiB).
func PodResources(s *specs.Spec) Resources {
	r := Resources{CPUs: defaultCPUs, MemoryMiB: defaultMemoryMiB}
	var n int
	if _, err := fmt.Sscan(s.Annotations[annCPUs], &n); err == nil && n >= 1 && n <= 16 {
		r.CPUs = uint8(n)
	}
	if _, err := fmt.Sscan(s.Annotations[annMemory], &n); err == nil && n >= 128 && n <= 64<<10 {
		r.MemoryMiB = uint32(n)
	}
	return r
}
