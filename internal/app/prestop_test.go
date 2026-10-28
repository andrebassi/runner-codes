package app

import (
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	"runner.codes/internal/domain"
	"runner.codes/internal/protocol"
)

// preStopJobs devolve os execs do gancho de pre-stop que o guest recebeu.
func preStopJobs(g *fileGuest) []protocol.Job {
	var out []protocol.Job
	for _, j := range jobsOf(g, protocol.OpExec) {
		if len(j.Argv) == 3 && strings.Contains(j.Argv[2], PreStopHook) {
			out = append(out, j)
		}
	}
	return out
}

// TestPreStopRunsOnGracefulStop: o stop gracioso roda o gancho uma vez, com o prazo de metade do stop e teto de
// 5 s, tolerando a ausência do arquivo (o `|| true`). É ele que deixa o desktop anotar as sessões do Claude Code
// antes de a máquina sair do ar numa preempção.
func TestPreStopRunsOnGracefulStop(t *testing.T) {
	cases := []struct {
		name    string
		timeout time.Duration
		want    int // segundos no job (0 = gancho não roda)
	}{
		{"padrão 10 s dá metade, 5 s", -1, 5},
		{"prazo de 14 s bate no teto de 5 s", 14 * time.Second, 5},
		{"drain de 8 s dá 4 s ao gancho", 8 * time.Second, 4},
		{"prazo de 2 s dá 1 s", 2 * time.Second, 1},
		{"prazo curto demais pula o gancho", 1500 * time.Millisecond, 0},
		{"stop imediato não roda gancho", 0, 0},
	}
	for _, tc := range cases {
		t.Run(tc.name, func(t *testing.T) {
			m, _, ps, _, g := machinesFixture(t)
			// O guest desliga no shutdown: sem isso cada stop esperaria o prazo inteiro.
			g.shutdown = func() { ps.mu.Lock(); ps.alive = map[int]bool{}; ps.mu.Unlock() }
			ctx := context.Background()
			m.Create(ctx, domain.Machine{Name: "a", Stack: "python"})
			m.Start(ctx, "a")
			if err := m.Stop(ctx, "a", tc.timeout); err != nil {
				t.Fatal(err)
			}
			jobs := preStopJobs(g)
			if tc.want == 0 {
				if len(jobs) != 0 {
					t.Fatalf("gancho não devia rodar: %+v", jobs)
				}
				return
			}
			if len(jobs) != 1 {
				t.Fatalf("gancho rodou %d vezes", len(jobs))
			}
			j := jobs[0]
			if j.Argv[0] != "sh" || j.Argv[1] != "-c" || !strings.Contains(j.Argv[2], "|| true") || j.Timeout != tc.want {
				t.Fatalf("job do gancho: %+v", j)
			}
		})
	}
}

// TestPreStopFailureDoesNotBlockStop: gancho que falha (guest recusa o exec) não segura o desligamento — a máquina
// sai do ar do mesmo jeito, porque esperar por ele pode estourar o prazo da preempção.
func TestPreStopFailureDoesNotBlockStop(t *testing.T) {
	m, st, ps, _, g := machinesFixture(t)
	ctx := context.Background()
	m.Create(ctx, domain.Machine{Name: "a", Stack: "python"})
	m.Start(ctx, "a")
	g.mu.Lock()
	g.execErr = errors.New("guest recusou")
	g.mu.Unlock()
	if err := m.Stop(ctx, "a", 2*time.Second); err != nil {
		t.Fatalf("stop com gancho falhando: %v", err)
	}
	if st.PID("a") != 0 || len(preStopJobs(g)) != 1 {
		t.Fatalf("pid=%d gancho=%d killed=%v", st.PID("a"), len(preStopJobs(g)), ps.killed)
	}
}
