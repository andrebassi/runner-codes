// Package hoststats mede o host do rc: CPU (/proc/stat, /proc/loadavg), memória (/proc/meminfo), disco
// do diretório de estado (statfs), a presença do /dev/kvm e o consumo do processo de cada máquina
// (/proc/<pid>/stat e status). Alimenta o GET /api/v1/host.
package hoststats

import (
	"bufio"
	"errors"
	"io"
	"os"
	"path/filepath"
	"runtime"
	"strconv"
	"strings"

	"golang.org/x/sys/unix"

	"runner.codes/internal/domain"
)

// clockTicks é o USER_HZ do Linux: as colunas de tempo do /proc estão nessa unidade. É 100 em toda
// arquitetura que o rc roda (x86_64 e arm64); o kernel fixa o valor exposto ao usuário.
const clockTicks = 100

// Reader mede o host; os caminhos são campos para os testes apontarem para arquivos de mentira.
type Reader struct {
	// Dir é o diretório de estado do rc: o disco medido é o do sistema de arquivos dele.
	Dir string
	// ProcRoot é a raiz do /proc (vazio usa /proc); KVM, o /dev/kvm.
	ProcRoot, KVM string
}

// proc devolve o caminho dentro do /proc configurado.
func (r Reader) proc(parts ...string) string {
	root := r.ProcRoot
	if root == "" {
		root = "/proc"
	}
	return filepath.Join(append([]string{root}, parts...)...)
}

// Read mede CPU, memória, disco e KVM. O que o sistema não tem (macOS: nada de /proc) fica em zero,
// que o domínio trata como "não medido"; disco que não dá para medir é erro, porque sem ele não há
// decisão de onde criar máquina.
func (r Reader) Read() (domain.Host, error) {
	h := domain.Host{Cores: runtime.NumCPU()}
	if f, err := os.Open(r.proc("meminfo")); err == nil {
		h.MemTotalMiB, h.MemAvailableMiB = parseMeminfo(f)
		f.Close()
	}
	if f, err := os.Open(r.proc("stat")); err == nil {
		h.CPUBusyTicks, h.CPUTotalTicks = parseStat(f)
		f.Close()
	}
	if b, err := os.ReadFile(r.proc("loadavg")); err == nil {
		if f := strings.Fields(string(b)); len(f) > 0 {
			h.Load1, _ = strconv.ParseFloat(f[0], 64)
		}
	}
	st, err := statfsUp(r.Dir)
	if err != nil {
		return domain.Host{}, err
	}
	// Bsize tem tipo diferente por sistema (int64 no Linux, uint32 no macOS): a conversão serve aos dois.
	bs := uint64(st.Bsize)
	h.DiskTotalMiB = int64(st.Blocks * bs >> 20)
	// Bavail, não Bfree: o que sobra para quem não é root (o reservado do ext4 não conta como livre).
	h.DiskFreeMiB = int64(st.Bavail * bs >> 20)
	kvm := r.KVM
	if kvm == "" {
		kvm = "/dev/kvm"
	}
	_, err = os.Stat(kvm)
	h.KVM = err == nil
	return h, nil
}

// statfsUp mede o sistema de arquivos do diretório, ou do primeiro ancestral que existir. Num host recém-criado o
// diretório de estado só nasce com a primeira máquina, e medir só ele dava "no such file or directory" — o rc
// respondia 500 e quem agenda tratava o servidor novo como fora do ar, sem nunca mandar máquina para ele (medido no
// primeiro worker do superbot, 25/10/2026). O ancestral está no mesmo sistema de arquivos onde o diretório vai nascer.
func statfsUp(dir string) (unix.Statfs_t, error) {
	var st unix.Statfs_t
	for d := filepath.Clean(dir); ; d = filepath.Dir(d) {
		err := unix.Statfs(d, &st)
		if err == nil || !errors.Is(err, os.ErrNotExist) || d == filepath.Dir(d) {
			return st, err
		}
	}
}

// Proc mede o processo de uma máquina: CPU acumulada (utime+stime) e VmRSS. Processo que sumiu entre a
// listagem e a leitura é erro, e quem chama só deixa a máquina sem consumo.
func (r Reader) Proc(pid int) (cpuNanos, rssBytes int64, err error) {
	b, err := os.ReadFile(r.proc(strconv.Itoa(pid), "stat"))
	if err != nil {
		return 0, 0, err
	}
	cpuNanos = parsePidStat(string(b)) * (1e9 / clockTicks)
	if f, err := os.Open(r.proc(strconv.Itoa(pid), "status")); err == nil {
		rssBytes = parseRSS(f)
		f.Close()
	}
	return cpuNanos, rssBytes, nil
}

// parseMeminfo lê MemTotal e MemAvailable (em kB no arquivo) e devolve em MiB. MemAvailable, não
// MemFree: o cache de página volta para quem pede, e o MemFree o contaria como ocupado.
func parseMeminfo(r io.Reader) (total, available int64) {
	sc := bufio.NewScanner(r)
	for sc.Scan() {
		f := strings.Fields(sc.Text())
		if len(f) < 2 {
			continue
		}
		kb, err := strconv.ParseInt(f[1], 10, 64)
		if err != nil {
			continue
		}
		switch f[0] {
		case "MemTotal:":
			total = kb >> 10
		case "MemAvailable:":
			available = kb >> 10
		}
	}
	return total, available
}

// parseStat lê a linha "cpu" do /proc/stat: total é a soma das colunas; ocioso é idle + iowait (a CPU
// esperando disco está livre para outro processo). steal entra no total e conta como ocupado: num
// host do GCP é tempo que a VM não teve.
func parseStat(r io.Reader) (busy, total uint64) {
	sc := bufio.NewScanner(r)
	for sc.Scan() {
		f := strings.Fields(sc.Text())
		if len(f) < 5 || f[0] != "cpu" {
			continue
		}
		var idle uint64
		for i, v := range f[1:] {
			n, err := strconv.ParseUint(v, 10, 64)
			if err != nil {
				return 0, 0
			}
			total += n
			if i == 3 || i == 4 { // idle, iowait
				idle += n
			}
		}
		return total - idle, total
	}
	return 0, 0
}

// parsePidStat soma utime e stime (campos 14 e 15) do /proc/<pid>/stat, em ticks. O nome do processo
// (campo 2) vem entre parênteses e pode ter espaço: a contagem começa depois do último ")".
func parsePidStat(s string) int64 {
	i := strings.LastIndexByte(s, ')')
	if i < 0 {
		return 0
	}
	f := strings.Fields(s[i+1:])
	// Depois do ")" o primeiro campo é o 3 (estado): utime é o índice 11, stime o 12.
	if len(f) < 13 {
		return 0
	}
	u, err1 := strconv.ParseInt(f[11], 10, 64)
	st, err2 := strconv.ParseInt(f[12], 10, 64)
	if err1 != nil || err2 != nil {
		return 0
	}
	return u + st
}

// parseRSS lê o VmRSS (kB) do /proc/<pid>/status e devolve em bytes.
func parseRSS(r io.Reader) int64 {
	sc := bufio.NewScanner(r)
	for sc.Scan() {
		f := strings.Fields(sc.Text())
		if len(f) >= 2 && f[0] == "VmRSS:" {
			kb, _ := strconv.ParseInt(f[1], 10, 64)
			return kb << 10
		}
	}
	return 0
}
