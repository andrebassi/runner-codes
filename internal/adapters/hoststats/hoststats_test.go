package hoststats

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
)

// fakeProc monta um /proc de mentira com os arquivos dados.
func fakeProc(t *testing.T, files map[string]string) string {
	t.Helper()
	root := t.TempDir()
	for name, body := range files {
		p := filepath.Join(root, name)
		os.MkdirAll(filepath.Dir(p), 0o755)
		os.WriteFile(p, []byte(body), 0o644)
	}
	return root
}

// TestRead: memória do meminfo em MiB (MemAvailable, não MemFree), CPU do /proc/stat (idle + iowait
// livres, steal ocupado), load1, disco do diretório medido, KVM pela existência do arquivo.
func TestRead(t *testing.T) {
	proc := fakeProc(t, map[string]string{
		"meminfo": "MemTotal:        8048576 kB\nMemFree:          102400 kB\nMemAvailable:    4194304 kB\nlixo\nSwap: x kB\n",
		// user nice system idle iowait irq softirq steal → total 1000, livre 600+100
		"stat":    "cpu  200 0 50 600 100 10 10 30 0 0\ncpu0 1 2 3 4 5\n",
		"loadavg": "0.42 0.30 0.20 1/123 4567\n",
	})
	dir := t.TempDir()
	kvm := filepath.Join(dir, "kvm")
	os.WriteFile(kvm, nil, 0o644)
	h, err := Reader{Dir: dir, ProcRoot: proc, KVM: kvm}.Read()
	if err != nil {
		t.Fatal(err)
	}
	if h.MemTotalMiB != 7859 || h.MemAvailableMiB != 4096 {
		t.Errorf("memória: %d/%d", h.MemAvailableMiB, h.MemTotalMiB)
	}
	if h.CPUTotalTicks != 1000 || h.CPUBusyTicks != 300 || h.Load1 != 0.42 || h.Cores < 1 {
		t.Errorf("cpu: %d/%d load=%v cores=%d", h.CPUBusyTicks, h.CPUTotalTicks, h.Load1, h.Cores)
	}
	if h.DiskTotalMiB <= 0 || h.DiskFreeMiB <= 0 || h.DiskFreeMiB > h.DiskTotalMiB {
		t.Errorf("disco: %d livre de %d", h.DiskFreeMiB, h.DiskTotalMiB)
	}
	if !h.KVM {
		t.Error("KVM existente lido como ausente")
	}
}

// TestReadMissing: sem /proc a memória e a CPU ficam zero (não medidas) e sem /dev/kvm o KVM é falso.
// Diretório de estado que ainda não existe (host recém-criado, nenhuma máquina) mede o sistema de arquivos do
// primeiro ancestral — antes era erro, o rc respondia 500 e o servidor novo nunca recebia máquina. Caminho que
// atravessa um arquivo não é "ainda não existe": continua erro.
func TestReadMissing(t *testing.T) {
	dir := t.TempDir()
	h, err := Reader{Dir: dir, ProcRoot: filepath.Join(dir, "nada"), KVM: filepath.Join(dir, "nada")}.Read()
	if err != nil || h.MemTotalMiB != 0 || h.CPUTotalTicks != 0 || h.KVM {
		t.Fatalf("ausentes: %+v %v", h, err)
	}
	fresh, err := Reader{Dir: filepath.Join(dir, "ainda", "não", "existe")}.Read()
	if err != nil || fresh.DiskTotalMiB <= 0 || fresh.DiskFreeMiB <= 0 {
		t.Fatalf("diretório ainda não criado: %+v %v", fresh, err)
	}
	file := filepath.Join(dir, "arquivo")
	os.WriteFile(file, nil, 0o644)
	if _, err := (Reader{Dir: filepath.Join(file, "dentro")}).Read(); err == nil {
		t.Fatal("caminho através de um arquivo sem erro")
	}
	// Caminhos padrão: só não pode quebrar (cada sistema tem ou não os arquivos).
	if _, err := (Reader{Dir: dir}).Read(); err != nil {
		t.Fatal(err)
	}
}

// TestProc: utime+stime em nanossegundos (nome com espaço e parênteses não desalinha os campos) e VmRSS
// em bytes; processo sem status fica sem RSS; processo que sumiu é erro.
func TestProc(t *testing.T) {
	// Campos depois do ")": estado(3) ... utime(14)=150 stime(15)=50 → 200 ticks = 2 s.
	stat := "4242 (rc vmm (x)) S 1 2 3 4 5 6 7 8 9 10 150 50 0 0\n"
	proc := fakeProc(t, map[string]string{
		"4242/stat":   stat,
		"4242/status": "Name: rc\nVmRSS:\t  2048 kB\n",
		"7/stat":      stat,
	})
	r := Reader{ProcRoot: proc}
	cpu, rss, err := r.Proc(4242)
	if err != nil || cpu != 2_000_000_000 || rss != 2048<<10 {
		t.Fatalf("proc: cpu=%d rss=%d %v", cpu, rss, err)
	}
	if _, rss, err := r.Proc(7); err != nil || rss != 0 {
		t.Fatalf("sem status: %d %v", rss, err)
	}
	if _, _, err := r.Proc(99); err == nil {
		t.Fatal("processo inexistente sem erro")
	}
}

// TestParsers: linha curta, número ruim e chaves desconhecidas são ignorados em cada leitor.
func TestParsers(t *testing.T) {
	total, avail := parseMeminfo(strings.NewReader("MemTotal: abc kB\nMemAvailable: 2048 kB\n"))
	if total != 0 || avail != 2 {
		t.Errorf("meminfo: %d %d", total, avail)
	}
	if b, tt := parseStat(strings.NewReader("intr 1 2\ncpu 1 x 3 4 5\n")); b != 0 || tt != 0 {
		t.Errorf("stat com número ruim: %d %d", b, tt)
	}
	if b, tt := parseStat(strings.NewReader("cpu 1 2\n")); b != 0 || tt != 0 {
		t.Errorf("stat sem linha cpu válida: %d %d", b, tt)
	}
	for _, s := range []string{"sem parêntese", "1 (x) S 1 2", "1 (x) S 1 2 3 4 5 6 7 8 9 10 a b"} {
		if v := parsePidStat(s); v != 0 {
			t.Errorf("pid stat %q: %d", s, v)
		}
	}
	if v := parseRSS(strings.NewReader("Name: x\n")); v != 0 {
		t.Errorf("sem VmRSS: %d", v)
	}
}
