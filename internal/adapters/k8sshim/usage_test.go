package k8sshim

import (
	"os"
	"path/filepath"
	"testing"

	cg2 "github.com/containerd/cgroups/v3/cgroup2/stats"
	"github.com/containerd/typeurl/v2"
)

// fakeProcDir grava stat e statm de um pid num /proc de mentira.
func fakeProcDir(t *testing.T, stat, statm string) string {
	t.Helper()
	root := t.TempDir()
	os.MkdirAll(filepath.Join(root, "42"), 0o755)
	if stat != "" {
		os.WriteFile(filepath.Join(root, "42", "stat"), []byte(stat), 0o644)
	}
	if statm != "" {
		os.WriteFile(filepath.Join(root, "42", "statm"), []byte(statm), 0o644)
	}
	return root
}

// TestProcUsage: utime+stime em ticks viram microssegundos e o RSS em páginas vira bytes; o nome com
// espaço e parêntese não desloca os campos.
func TestProcUsage(t *testing.T) {
	stat := "42 (rc vmm (x)) S 1 2 3 4 5 6 7 8 9 10 150 50 0 0\n"
	root := fakeProcDir(t, stat, "1000 256 10 1 0 5 0\n")
	cpu, mem, err := procUsage(root, 42)
	if err != nil || cpu != 2_000_000 || mem != 256*4096 {
		t.Fatalf("cpu=%d mem=%d err=%v", cpu, mem, err)
	}
}

// TestProcUsageErrors: processo que não existe, stat sem nome, stat curto, sem statm e statm curto.
func TestProcUsageErrors(t *testing.T) {
	cases := []struct{ stat, statm string }{{"", ""}, {"42 sem nome", "1 2"}, {"42 (x) S 1", "1 2"}, {"42 (x) S 1 2 3 4 5 6 7 8 9 10 1 1", ""},
		{"42 (x) S 1 2 3 4 5 6 7 8 9 10 1 1", "1"}}
	for _, c := range cases {
		if _, _, err := procUsage(fakeProcDir(t, c.stat, c.statm), 42); err == nil {
			t.Fatalf("%+v: sem erro", c)
		}
	}
}

// TestMetricsAny: o Any decodifica de volta para as métricas cgroup v2 que o CRI lê.
func TestMetricsAny(t *testing.T) {
	a, err := metricsAny(7, 9)
	if err != nil {
		t.Fatal(err)
	}
	v, err := typeurl.UnmarshalAny(a)
	if err != nil {
		t.Fatal(err)
	}
	m, ok := v.(*cg2.Metrics)
	if !ok || m.CPU.UsageUsec != 7 || m.Memory.Usage != 9 {
		t.Fatalf("%T %+v", v, v)
	}
}
