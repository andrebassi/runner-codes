package app

import (
	"context"
	"io"
	"testing"

	"runner.code/internal/domain"
)

// TestUserRouting: o workload e o exec rodam como o user da máquina; o --user do exec e do
// shell vence; o init roda como root (é ele que cria o usuário).
func TestUserRouting(t *testing.T) {
	m, _, _, _, g := machinesFixture(t)
	ctx := context.Background()
	m.Create(ctx, domain.Machine{Name: "u", Stack: "python", User: "app", Init: []string{"adduser -D app"}, Workload: []string{"serve"}})
	if _, err := m.Start(ctx, "u"); err != nil {
		t.Fatal(err)
	}
	for _, j := range g.jobs {
		switch {
		case j.TraceID == "init-0" && j.User != "":
			t.Fatalf("init must run as root: %q", j.User)
		case j.Op == "workload" && j.User != "app":
			t.Fatalf("workload user: %q", j.User)
		}
	}
	m.Exec(ctx, "u", ExecRequest{Argv: []string{"id"}}, nil)
	if j := g.jobs[len(g.jobs)-1]; j.User != "app" {
		t.Fatalf("exec takes the machine user: %q", j.User)
	}
	m.Exec(ctx, "u", ExecRequest{Argv: []string{"id"}, User: "root"}, nil)
	if j := g.jobs[len(g.jobs)-1]; j.User != "root" {
		t.Fatalf("--user wins: %q", j.User)
	}
	if pick("", "b") != "b" || pick("a", "b") != "a" {
		t.Fatal("pick")
	}
}

// TestRunIsTiedToCaller: o run em primeiro plano prende a VM a quem chamou (se o CLI morrer,
// a VM morre junto — medido: sem isso, um kill -9 no rc deixava código não confiável rodando
// sem dono). start e run -d não prendem: a máquina tem de sobreviver ao comando.
func TestRunIsTiedToCaller(t *testing.T) {
	m, _, _, d, _ := machinesFixture(t)
	ctx := context.Background()
	if _, _, err := m.Run(ctx, RunRequest{Machine: domain.Machine{Stack: "python"}, Command: []string{"true"}}, RunIO{Stdout: io.Discard, Stderr: io.Discard}); err != nil {
		t.Fatal(err)
	}
	if !d.specs[len(d.specs)-1].TiedToCaller {
		t.Fatal("a foreground run must be tied to the caller")
	}
	m.Create(ctx, domain.Machine{Name: "keep", Stack: "python"})
	m.Start(ctx, "keep")
	if d.specs[len(d.specs)-1].TiedToCaller {
		t.Fatal("a persistent machine must outlive the command that started it")
	}
	if _, _, err := m.Run(ctx, RunRequest{Machine: domain.Machine{Name: "bg", Stack: "python"}, Command: []string{"sleep", "9"}, Detach: true}, RunIO{}); err != nil {
		t.Fatal(err)
	}
	if d.specs[len(d.specs)-1].TiedToCaller {
		t.Fatal("run -d leaves the machine running: not tied")
	}
}
