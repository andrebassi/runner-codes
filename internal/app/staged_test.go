package app

import (
	"context"
	"errors"
	"io"
	"path/filepath"
	"strings"
	"testing"

	"runner.code/internal/domain"
	"runner.code/internal/protocol"
)

// stagedJobs devolve os jobs de exec do guest cujo argv[3] é kind ("stage" ou "sync").
func stagedJobs(g *fileGuest, kind string) []protocol.Job {
	g.mu.Lock()
	defer g.mu.Unlock()
	var out []protocol.Job
	for _, j := range g.jobs {
		if j.Op == protocol.OpExec && len(j.Argv) > 3 && j.Argv[3] == kind {
			out = append(out, j)
		}
	}
	return out
}

// TestStagedStartSyncStop: o volume staged vira device virtio-fs escondido, semeado e ligado
// ao caminho a cada start; o sync espelha a cópia no host e acerta os modos; o stop gracioso
// sincroniza antes e o stop imediato (timeout 0) não; parada não sincroniza.
func TestStagedStartSyncStop(t *testing.T) {
	m, _, _, d, g := machinesFixture(t)
	ctx := context.Background()
	// O create guarda o caminho real (no macOS, /var é /private/var).
	host, _ := filepath.EvalSymlinks(t.TempDir())
	var modes []string
	m.ApplyGuestModes = func(dir string) error { modes = append(modes, dir); return nil }
	if _, err := m.Create(ctx, domain.Machine{Name: "st", Stack: "python", Mounts: []domain.Mount{{Host: host, Guest: "/app", Staged: true}}}); err != nil {
		t.Fatal(err)
	}
	if err := m.Sync(ctx, "st"); errCode(err) != domain.CodeInvalidState {
		t.Fatalf("sync of a stopped machine: %v", err)
	}
	if _, err := m.Start(ctx, "st"); err != nil {
		t.Fatal(err)
	}
	spec := d.specs[len(d.specs)-1]
	if len(spec.Mounts) != 1 || spec.Mounts[0].HostPath != host || spec.Mounts[0].ReadOnly {
		t.Fatalf("the staged host dir is a rw virtio-fs device: %+v", spec.Mounts)
	}
	st := stagedJobs(g, "stage")
	if len(st) != 1 || strings.Join(st[0].Argv[4:], " ") != "/run/rc-staged/"+mountTag(0)+" /workspace/.disk/app /app "+mountTag(0) {
		t.Fatalf("stage job: %+v", st)
	}
	if err := m.Sync(ctx, "st"); err != nil {
		t.Fatal(err)
	}
	sy := stagedJobs(g, "sync")
	if len(sy) != 1 || sy[0].Argv[4] != "/workspace/.disk/app" || sy[0].Argv[5] != "/run/rc-staged/"+mountTag(0) || len(modes) != 1 || modes[0] != host {
		t.Fatalf("sync: %+v modes %v", sy, modes)
	}
	if err := m.Stop(ctx, "st", -1); err != nil || len(stagedJobs(g, "sync")) != 2 {
		t.Fatalf("a graceful stop syncs first: %v", err)
	}
	m.Start(ctx, "st")
	if err := m.Stop(ctx, "st", 0); err != nil || len(stagedJobs(g, "sync")) != 2 {
		t.Fatalf("an immediate stop does not sync: %v", err)
	}
}

// TestStagedFailures: sync que falha deixa a máquina ligada (nada se perde) e diz o volume;
// modo que não se aplica falha o sync; stage que falha falha o start; máquina sem staged não
// sincroniza nada.
func TestStagedFailures(t *testing.T) {
	m, _, _, _, g := machinesFixture(t)
	ctx := context.Background()
	m.Create(ctx, domain.Machine{Name: "bk", Stack: "python", Mounts: []domain.Mount{{Host: t.TempDir(), Guest: "/broken", Staged: true}}})
	m.Start(ctx, "bk")
	if err := m.Stop(ctx, "bk", -1); err == nil || !strings.Contains(err.Error(), "sync /broken: sync broke") {
		t.Fatalf("failed sync: %v", err)
	}
	if s, _ := m.Status("bk"); s.State != StateRunning {
		t.Fatal("a failed sync keeps the machine running")
	}
	m.Create(ctx, domain.Machine{Name: "md", Stack: "python", Mounts: []domain.Mount{{Host: t.TempDir(), Guest: "/app", Staged: true}}})
	m.Start(ctx, "md")
	m.ApplyGuestModes = func(string) error { return errors.New("chmod denied") }
	if err := m.Sync(ctx, "md"); err == nil || !strings.Contains(err.Error(), "chmod denied") {
		t.Fatalf("modes: %v", err)
	}
	m.Create(ctx, domain.Machine{Name: "bs", Stack: "python", Mounts: []domain.Mount{{Host: t.TempDir(), Guest: "/badstage", Staged: true}}})
	if _, err := m.Start(ctx, "bs"); err == nil || !strings.Contains(err.Error(), "stage /badstage: stage broke") {
		t.Fatalf("failed stage: %v", err)
	}
	m.Create(ctx, domain.Machine{Name: "plain", Stack: "python"})
	before := len(g.jobs)
	if err := m.Sync(ctx, "plain"); err != nil || len(g.jobs) != before {
		t.Fatalf("no staged volume, nothing to do: %v", err)
	}
	if err := m.Sync(ctx, "nope"); errCode(err) != domain.CodeNotFound {
		t.Fatalf("unknown machine: %v", err)
	}
}

// TestStagedRunAndBranch: o fim do machine run sincroniza antes de apagar, e a filha de um
// branch herda a cópia como volume de disco comum (não devolve nada ao host).
func TestStagedRunAndBranch(t *testing.T) {
	m, _, _, _, g := machinesFixture(t)
	ctx := context.Background()
	host := t.TempDir()
	if _, _, err := m.Run(ctx, RunRequest{Machine: domain.Machine{Stack: "python", Mounts: []domain.Mount{{Host: host, Guest: "/app", Staged: true}}},
		Command: []string{"true"}}, RunIO{Stdout: io.Discard, Stderr: io.Discard}); err != nil {
		t.Fatal(err)
	}
	if len(stagedJobs(g, "sync")) != 1 {
		t.Fatal("the end of a run syncs the staged copy back")
	}
	m.Create(ctx, domain.Machine{Name: "src", Stack: "python", Mounts: []domain.Mount{{Host: host, Guest: "/app", Staged: true}, {Guest: "/data"}}})
	child, err := m.Branch(ctx, "src", "kid")
	if err != nil {
		t.Fatal(err)
	}
	if len(child.Mounts) != 2 || child.Mounts[0] != (domain.Mount{Guest: "/app"}) || child.Mounts[1] != (domain.Mount{Guest: "/data"}) {
		t.Fatalf("child mounts: %+v", child.Mounts)
	}
	if src, _ := m.Status("src"); !src.Mounts[0].Staged {
		t.Fatal("the source keeps its staged volume")
	}
}
