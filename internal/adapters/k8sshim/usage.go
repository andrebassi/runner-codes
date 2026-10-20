package k8sshim

import (
	"fmt"
	"os"
	"strconv"
	"strings"

	cg2 "github.com/containerd/cgroups/v3/cgroup2/stats"
	"github.com/containerd/typeurl/v2"
	"google.golang.org/protobuf/types/known/anypb"
)

// clockTicks é o USER_HZ do /proc/<pid>/stat: 100 em todo Linux de x86_64 e arm64 (o kernel fixa o
// valor exposto, independente do HZ interno).
const clockTicks = 100

// pageSize do /proc/<pid>/statm. 4 KiB nas duas arquiteturas suportadas com KVM (o arm64 de 16/64 KiB
// não é alvo da libkrun no Linux).
const pageSize = 4096

// procUsage lê o uso do processo pid em procRoot (o /proc do host): tempo de CPU em microssegundos
// (utime+stime) e memória residente em bytes. É o custo real da VM no nó: vCPUs, virtio e a RAM que o
// guest tocou, já que a memória da libkrun é elástica.
func procUsage(procRoot string, pid int) (cpuUsec, memBytes uint64, err error) {
	stat, err := os.ReadFile(fmt.Sprintf("%s/%d/stat", procRoot, pid))
	if err != nil {
		return 0, 0, err
	}
	// O nome do processo (campo 2) vem entre parênteses e pode ter espaço: os campos contam a partir do
	// último ")". utime e stime são os campos 14 e 15, ou seja, os índices 11 e 12 depois do nome.
	s := string(stat)
	i := strings.LastIndexByte(s, ')')
	if i < 0 {
		return 0, 0, fmt.Errorf("stat of %d: no command field", pid)
	}
	f := strings.Fields(s[i+1:])
	if len(f) < 13 {
		return 0, 0, fmt.Errorf("stat of %d: %d fields", pid, len(f))
	}
	ut, _ := strconv.ParseUint(f[11], 10, 64)
	st, _ := strconv.ParseUint(f[12], 10, 64)
	statm, err := os.ReadFile(fmt.Sprintf("%s/%d/statm", procRoot, pid))
	if err != nil {
		return 0, 0, err
	}
	m := strings.Fields(string(statm))
	if len(m) < 2 {
		return 0, 0, fmt.Errorf("statm of %d: %q", pid, statm)
	}
	rss, _ := strconv.ParseUint(m[1], 10, 64)
	return (ut + st) * (1_000_000 / clockTicks), rss * pageSize, nil
}

// metricsAny embrulha o uso no formato cgroup v2 que o CRI do containerd decodifica. Devolver vazio
// derruba o resumo de estatísticas do kubelet para o nó inteiro ("failed to decode container
// metrics", medido no k3s em 20/10/2026): eviction manager cego, metrics-server sem dado e namespace
// que não termina de apagar.
func metricsAny(cpuUsec, memBytes uint64) (*anypb.Any, error) {
	return typeurl.MarshalAnyToProto(&cg2.Metrics{CPU: &cg2.CPUStat{UsageUsec: cpuUsec}, Memory: &cg2.MemoryStat{Usage: memBytes}})
}
