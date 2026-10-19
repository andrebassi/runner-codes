package guest

import (
	"bytes"
	"strings"
	"sync"
	"testing"
	"time"
)

// syncBuffer é um console seguro para o -race: o workload escreve de outra goroutine.
type syncBuffer struct {
	mu sync.Mutex
	b  bytes.Buffer
}

// Write guarda sob lock.
func (s *syncBuffer) Write(p []byte) (int, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	return s.b.Write(p)
}

// String lê sob lock.
func (s *syncBuffer) String() string { s.mu.Lock(); defer s.mu.Unlock(); return s.b.String() }

// withConsole troca o console do workload durante o teste.
func withConsole(t *testing.T) *syncBuffer {
	buf := &syncBuffer{}
	old := console
	console = buf
	t.Cleanup(func() { console = old })
	return buf
}

// waitText espera o texto aparecer no console.
func waitText(t *testing.T, buf *syncBuffer, want string) {
	t.Helper()
	for end := time.Now().Add(5 * time.Second); time.Now().Before(end); time.Sleep(20 * time.Millisecond) {
		if strings.Contains(buf.String(), want) {
			return
		}
	}
	t.Fatalf("console never showed %q; got %q", want, buf.String())
}

// TestWorkloadOutputAndExit: cada linha do workload sai com o prefixo (inclusive a última
// sem quebra), env e cwd chegam, e a saída do processo é registrada com o código.
func TestWorkloadOutputAndExit(t *testing.T) {
	buf := withConsole(t)
	s := NewServer(DefaultServerConfig())
	dir := t.TempDir()
	res := s.startWorkload(Job{Argv: []string{"sh", "-c", `echo "one $W"; pwd; printf partial; exit 3`}, Env: []string{"W=env-ok"}, Cwd: dir})
	if res.Status != "success" {
		t.Fatal(res.Error)
	}
	waitText(t, buf, "[workload] exited with code 3")
	out := buf.String()
	if !strings.Contains(out, "[workload] one env-ok\n") || !strings.Contains(out, "[workload] partial\n") || !strings.Contains(out, dir) {
		t.Fatalf("console: %q", out)
	}
}

// TestWorkloadStop: o stop_signal chega ao workload (que trata e sai); um segundo start com
// o primeiro vivo é recusado; quem ignora o sinal leva SIGKILL no prazo.
func TestWorkloadStop(t *testing.T) {
	buf := withConsole(t)
	s := NewServer(DefaultServerConfig())
	s.startWorkload(Job{Argv: []string{"sh", "-c", `trap 'echo SAVED; exit 0' INT; echo up; while :; do sleep 0.1; done`}})
	waitText(t, buf, "[workload] up")
	if res := s.startWorkload(Job{Argv: []string{"true"}}); !strings.Contains(res.Error, "already running") {
		t.Fatalf("second workload: %+v", res)
	}
	s.stopWorkload(2, 5*time.Second) // SIGINT: o trap grava o "estado"
	waitText(t, buf, "[workload] SAVED")

	s.startWorkload(Job{Argv: []string{"sh", "-c", `trap '' TERM; echo stubborn; while :; do sleep 0.1; done`}})
	waitText(t, buf, "[workload] stubborn")
	start := time.Now()
	s.stopWorkload(0, 300*time.Millisecond) // 0 = SIGTERM, ignorado; o prazo vence e vem o SIGKILL
	if time.Since(start) > 3*time.Second {
		t.Fatalf("stop did not kill in time: %v", time.Since(start))
	}
	s.stopWorkload(15, time.Second) // já parado: não faz nada
	if res := s.startWorkload(Job{}); res.Status != "failed" {
		t.Fatal("empty argv must fail")
	}
	if res := s.startWorkload(Job{Argv: []string{"/does/not/exist"}}); res.Status != "failed" {
		t.Fatal("missing program must fail")
	}
	NewServer(DefaultServerConfig()).stopWorkload(15, time.Second) // nunca ligado: nada
}

// TestWorkloadStopOnExit: com stop_on_exit, a saída do workload grava o disco e encerra o
// guest (que desliga a VM).
func TestWorkloadStopOnExit(t *testing.T) {
	withConsole(t)
	synced, exited := make(chan struct{}), make(chan int, 1)
	syncDisks, exitProcess = func() { close(synced) }, func(c int) { exited <- c }
	defer func() { syncDisks, exitProcess = syncDisksDefault, osExit }()
	s := NewServer(DefaultServerConfig())
	s.startWorkload(Job{Argv: []string{"true"}, StopOnExit: true})
	select {
	case <-synced:
	case <-time.After(5 * time.Second):
		t.Fatal("stop_on_exit did not sync")
	}
	if code := <-exited; code != 0 {
		t.Fatalf("exit %d", code)
	}
}
