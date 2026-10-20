package app

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"runner.code/internal/domain"
	"runner.code/internal/protocol"
)

// TestPauseResume: pause congela a máquina ligada e o status diz paused; exec, cp, update e
// start recusam com o motivo; stop retoma antes de desligar; pausa de máquina parada é erro;
// pausar duas vezes e retomar quem roda não são erro; a pausa some se a VM morrer.
func TestPauseResume(t *testing.T) {
	m, st, ps, _, g := machinesFixture(t)
	ctx := context.Background()
	m.Create(ctx, domain.Machine{Name: "p", Stack: "python", Network: domain.Network{Enabled: true}})
	if err := m.Pause("p"); errCode(err) != domain.CodeInvalidState {
		t.Fatalf("pause of a stopped machine: %v", err)
	}
	if err := m.Resume("p"); errCode(err) != domain.CodeInvalidState {
		t.Fatalf("resume of a stopped machine: %v", err)
	}
	m.Start(ctx, "p")
	if err := m.Resume("p"); err != nil {
		t.Fatal("resume of a running machine is a no-op")
	}
	if err := m.Pause("p"); err != nil {
		t.Fatal(err)
	}
	if err := m.Pause("p"); err != nil || len(ps.paused) != 1 {
		t.Fatalf("pausing twice: %v %v", err, ps.paused)
	}
	if s, _ := m.Status("p"); s.State != StatePaused {
		t.Fatalf("state %s", s.State)
	}
	// O start de uma pausada diz que ela está pausada e como sair, não "already running".
	if _, err := m.Start(ctx, "p"); err == nil || !strings.Contains(err.Error(), "is paused; resume it") {
		t.Fatalf("start on a paused machine: %v", err)
	}
	for name, err := range map[string]error{
		"exec":   func() error { _, err := m.Exec(ctx, "p", ExecRequest{Argv: []string{"ls"}}, nil); return err }(),
		"start":  func() error { _, err := m.Start(ctx, "p"); return err }(),
		"update": func() error { _, err := m.Update("p", func(*domain.Egress) {}); return err }(),
		"delete": m.Delete(ctx, "p", false),
	} {
		if errCode(err) != domain.CodeInvalidState {
			t.Fatalf("%s on a paused machine: %v", name, err)
		}
	}
	g.shutdown = func() { ps.mu.Lock(); ps.alive = map[int]bool{}; ps.mu.Unlock() }
	if err := m.Stop(ctx, "p", -1); err != nil || len(ps.resumed) != 1 {
		t.Fatalf("stop must resume first: %v %v", err, ps.resumed)
	}
	if _, err := os.Stat(filepath.Join(st.Dir("p"), machinePaused)); !os.IsNotExist(err) {
		t.Fatal("the pause marker must go with the resume")
	}
	m.Start(ctx, "p")
	m.Pause("p")
	ps.mu.Lock()
	ps.alive = map[int]bool{}
	ps.mu.Unlock()
	if s, _ := m.Status("p"); s.State != StateStopped || m.isPaused("p") {
		t.Fatal("a dead VM is stopped, not paused")
	}
	if err := m.Pause("nope"); errCode(err) != domain.CodeNotFound {
		t.Fatal("missing machine")
	}
	if err := m.Resume("nope"); errCode(err) != domain.CodeNotFound {
		t.Fatal("missing machine")
	}
}

// TestPauseFailures: pause e resume repassam a falha do sinal; delete com force mata a
// máquina pausada.
func TestPauseFailures(t *testing.T) {
	m, _, ps, _, _ := machinesFixture(t)
	ctx := context.Background()
	m.Create(ctx, domain.Machine{Name: "f", Stack: "python"})
	m.Start(ctx, "f")
	m.Pause("f")
	if err := m.Delete(ctx, "f", true); err != nil || len(ps.killed) != 1 {
		t.Fatalf("force delete of a paused machine: %v", err)
	}
	m.Create(ctx, domain.Machine{Name: "g", Stack: "python"})
	m.Start(ctx, "g")
	pid := m.Store.PID("g")
	m.Pause("g")
	// O processo some entre o Status e o sinal: o Resume repassa o erro do kill.
	failing := &failingProcs{procs: ps, failOn: pid}
	m.Procs = failing
	if err := m.Resume("g"); err == nil {
		t.Fatal("a failed SIGCONT must surface")
	}
	m.Procs = ps
	m.Resume("g")
	m.Procs = failing
	if err := m.Pause("g"); err == nil {
		t.Fatal("a failed SIGSTOP must surface")
	}
}

// failingProcs falha pause/resume para um pid, mantendo o resto do fake.
type failingProcs struct {
	*procs
	failOn int
}

// Pause falha no pid escolhido.
func (f *failingProcs) Pause(pid int) error {
	if pid == f.failOn {
		return errors.New("no such process")
	}
	return f.procs.Pause(pid)
}

// Resume falha no pid escolhido.
func (f *failingProcs) Resume(pid int) error {
	if pid == f.failOn {
		return errors.New("no such process")
	}
	return f.procs.Resume(pid)
}

// TestBranch: a filha nasce com o registro da mãe (nome e data novos) e com o disco dela
// como modelo; com a mãe ligada o guest faz sync e a VM congela só durante o clone; nome
// repetido, inválido, mãe inexistente e sync que falha são recusados.
func TestBranch(t *testing.T) {
	m, st, ps, _, g := machinesFixture(t)
	ctx := context.Background()
	m.Create(ctx, domain.Machine{Name: "mom", Stack: "python", Env: []string{"A=1"}, Init: []string{"x"}})
	child, err := m.Branch(ctx, "mom", "kid")
	if err != nil || child.Name != "kid" || child.Env[0] != "A=1" {
		t.Fatalf("branch of a stopped machine: %+v %v", child, err)
	}
	if got := st.tpls[len(st.tpls)-1]; got != filepath.Join(st.Dir("mom"), "disk.ext4") {
		t.Fatalf("the child disk must be cloned from the parent's: %s", got)
	}
	if len(ps.paused) != 0 {
		t.Fatal("a stopped parent is not paused")
	}
	m.Start(ctx, "mom")
	if _, err := m.Branch(ctx, "mom", "kid2"); err != nil {
		t.Fatal(err)
	}
	if len(ps.paused) != 1 || len(ps.resumed) != 1 {
		t.Fatalf("a running parent is frozen only during the clone: %v %v", ps.paused, ps.resumed)
	}
	if j := jobsOf(g, protocol.OpExec); j[len(j)-1].TraceID != "branch-sync" {
		t.Fatalf("the guest must sync before the clone: %+v", j[len(j)-1])
	}
	if _, err := m.Branch(ctx, "mom", "kid"); errCode(err) != domain.CodeConflict {
		t.Fatalf("name taken: %v", err)
	}
	if _, err := m.Branch(ctx, "mom", "Bad Name"); errCode(err) != domain.CodeConfig {
		t.Fatalf("bad name: %v", err)
	}
	if _, err := m.Branch(ctx, "ghost", "x"); errCode(err) != domain.CodeNotFound {
		t.Fatalf("missing parent: %v", err)
	}
	// Mãe pausada: retoma para o sync, congela no clone, solta, e volta a ficar pausada.
	m.Pause("mom")
	np, nr := len(ps.paused), len(ps.resumed)
	if _, err := m.Branch(ctx, "mom", "kid5"); err != nil {
		t.Fatal(err)
	}
	if len(ps.resumed) != nr+2 || len(ps.paused) != np+2 {
		t.Fatalf("paused parent: resume, sync, pause, clone, resume, pause: %v %v", ps.paused[np:], ps.resumed[nr:])
	}
	if s, _ := m.Status("mom"); s.State != StatePaused {
		t.Fatal("the parent must still be paused after the branch")
	}
	m.Resume("mom")
	g.execErr = errors.New("guest gone")
	if _, err := m.Branch(ctx, "mom", "kid3"); err == nil {
		t.Fatal("a failed sync must stop the branch")
	}
	g.execErr = nil
	m.Procs = &failingProcs{procs: ps, failOn: m.Store.PID("mom")}
	if _, err := m.Branch(ctx, "mom", "kid4"); err == nil {
		t.Fatal("a failed freeze must stop the branch")
	}
	m.Procs = ps
	m.Pause("mom")
	m.Procs = &failingProcs{procs: ps, failOn: m.Store.PID("mom")}
	if _, err := m.Branch(ctx, "mom", "kid6"); err == nil {
		t.Fatal("a failed resume of a paused parent must stop the branch")
	}
}
