package krun

import (
	"context"
	"errors"
	"sync"
	"testing"
	"time"

	"runner.codes/internal/protocol"
)

// fakeGuest grava os jobs mandados ao guest; hold segura cada chamada até ser liberada.
type fakeGuest struct {
	mu     sync.Mutex
	jobs   []protocol.Job
	socket string
	hold   chan struct{}
	err    error
	active int
	max    int // maior número de ganchos rodando ao mesmo tempo
}

// run imita o guestproto.Client.Run.
func (f *fakeGuest) run(_ context.Context, socket string, job protocol.Job) (protocol.Result, error) {
	f.mu.Lock()
	f.jobs, f.socket = append(f.jobs, job), socket
	f.active++
	if f.active > f.max {
		f.max = f.active
	}
	hold := f.hold
	f.mu.Unlock()
	if hold != nil {
		<-hold
	}
	f.mu.Lock()
	f.active--
	f.mu.Unlock()
	return protocol.Result{Status: "success"}, f.err
}

// count devolve quantos jobs chegaram.
func (f *fakeGuest) count() int { f.mu.Lock(); defer f.mu.Unlock(); return len(f.jobs) }

// useFakeGuest troca o runGuest pelo falso durante o teste.
func useFakeGuest(t *testing.T, f *fakeGuest) {
	old := runGuest
	runGuest = f.run
	t.Cleanup(func() { runGuest = old })
}

// waitFor espera a condição por até 2 s.
func waitFor(t *testing.T, what string, cond func() bool) {
	t.Helper()
	for end := time.Now().Add(2 * time.Second); time.Now().Before(end); time.Sleep(5 * time.Millisecond) {
		if cond() {
			return
		}
	}
	t.Fatalf("esperando %s", what)
}

// TestGuestResizerHook: o pedido vira um exec do gancho no guest, pelo socket da máquina, com largura e
// altura como argumentos e o teste de existência no próprio shell (sem o gancho, sai 0 sem erro).
func TestGuestResizerHook(t *testing.T) {
	f := &fakeGuest{}
	useFakeGuest(t, f)
	g := newGuestResizer("/m/v.sock")
	g.settle = time.Millisecond
	g.Resize(1512, 794)
	waitFor(t, "o gancho", func() bool { return f.count() == 1 })
	j := f.jobs[0]
	if f.socket != "/m/v.sock" || j.Op != protocol.OpExec || j.Timeout != 15 {
		t.Fatalf("job: socket=%s %+v", f.socket, j)
	}
	want := []string{"sh", "-c", `[ -x "$0" ] || exit 0; exec "$0" "$1" "$2"`, DisplayResizeHook, "1512", "794"}
	if len(j.Argv) != len(want) {
		t.Fatalf("argv: %q", j.Argv)
	}
	for i := range want {
		if j.Argv[i] != want[i] {
			t.Fatalf("argv[%d] = %q, quero %q", i, j.Argv[i], want[i])
		}
	}
}

// TestGuestResizerSettles: vários pedidos seguidos (um arrasto de janela) viram UM gancho, com o último
// tamanho; erro do guest é relatado ao done e não trava o próximo pedido.
func TestGuestResizerSettles(t *testing.T) {
	f := &fakeGuest{err: errors.New("guest fora do ar")}
	useFakeGuest(t, f)
	g := newGuestResizer("/s")
	g.settle = 30 * time.Millisecond
	var mu sync.Mutex
	var got [][2]int
	var errs int
	g.done = func(w, h int, _ protocol.Result, err error) {
		mu.Lock()
		got = append(got, [2]int{w, h})
		if err != nil {
			errs++
		}
		mu.Unlock()
	}
	for w := 800; w <= 1000; w += 20 {
		g.Resize(w, 600)
	}
	waitFor(t, "o gancho do arrasto", func() bool { return f.count() == 1 })
	time.Sleep(60 * time.Millisecond)
	mu.Lock()
	if f.count() != 1 || len(got) != 1 || got[0] != [2]int{1000, 600} || errs != 1 {
		mu.Unlock()
		t.Fatalf("jobs=%d got=%v errs=%d", f.count(), got, errs)
	}
	mu.Unlock()
	g.Resize(1024, 768)
	waitFor(t, "o pedido depois do erro", func() bool { return f.count() == 2 })
}

// TestGuestResizerNoOverlap: pedido que chega com um gancho no ar espera ele acabar e roda depois, com o
// tamanho mais novo; nunca dois ganchos ao mesmo tempo (dois xrandr disputariam o modo da tela).
func TestGuestResizerNoOverlap(t *testing.T) {
	f := &fakeGuest{hold: make(chan struct{})}
	useFakeGuest(t, f)
	g := newGuestResizer("/s")
	g.settle = time.Millisecond
	g.Resize(800, 600)
	waitFor(t, "o primeiro gancho", func() bool { return f.count() == 1 })
	g.Resize(900, 600)
	g.Resize(1000, 700)
	g.fire() // disparo atrasado de um timer antigo, com o gancho ainda no ar: só marca
	close(f.hold)
	waitFor(t, "o gancho seguinte", func() bool { return f.count() == 2 })
	time.Sleep(20 * time.Millisecond)
	f.mu.Lock()
	defer f.mu.Unlock()
	if f.max != 1 || len(f.jobs) != 2 || f.jobs[1].Argv[4] != "1000" || f.jobs[1].Argv[5] != "700" {
		t.Fatalf("max=%d jobs=%d último=%q", f.max, len(f.jobs), f.jobs[len(f.jobs)-1].Argv)
	}
}

// TestGuestResizerSameSize: pedido igual ao que acabou de rodar, chegado durante a execução, não roda de novo.
func TestGuestResizerSameSize(t *testing.T) {
	f := &fakeGuest{hold: make(chan struct{})}
	useFakeGuest(t, f)
	g := newGuestResizer("/s")
	g.settle = time.Millisecond
	g.Resize(800, 600)
	waitFor(t, "o gancho", func() bool { return f.count() == 1 })
	g.Resize(800, 600)
	close(f.hold)
	time.Sleep(30 * time.Millisecond)
	if f.count() != 1 {
		t.Fatalf("rodou %d vezes", f.count())
	}
}
