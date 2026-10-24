package app

import (
	"context"
	"errors"
	"testing"
	"time"

	"runner.codes/internal/domain"
)

// newPlayground monta um playground sobre o dublê de máquinas, com relógio controlado pelo teste.
func newPlayground(t *testing.T, maxRunning int) (*Playground, *Machines, *time.Time) {
	t.Helper()
	m, _, _, _, _ := machinesFixture(t)
	now := time.Date(2026, 10, 20, 12, 0, 0, 0, time.UTC)
	p := &Playground{M: m, Base: domain.Machine{Name: "playground-base", Stack: "python"}, MaxRunning: maxRunning,
		IdleStop: 30 * time.Minute, MaxAge: 24 * time.Hour, StopTimeout: 10 * time.Millisecond, Now: func() time.Time { return now }}
	return p, m, &now
}

// stateOf lê o estado de uma máquina no dublê.
func stateOf(t *testing.T, m *Machines, name string) string {
	t.Helper()
	st, err := m.Status(name)
	if err != nil {
		return "gone"
	}
	return st.State
}

// TestPlaygroundEnsure: a base nasce uma vez e fica parada; cada visitante ganha a sua máquina ligada; o mesmo
// visitante volta para a mesma; visitor id inválido é recusado sem criar nada.
func TestPlaygroundEnsure(t *testing.T) {
	p, m, _ := newPlayground(t, 3)
	ctx := context.Background()
	a, err := p.Ensure(ctx, "visitorAAAAAAAAAAAAA")
	if err != nil {
		t.Fatal(err)
	}
	if stateOf(t, m, "playground-base") != StateStopped || stateOf(t, m, a) != StateRunning {
		t.Fatalf("base %s, visitante %s", stateOf(t, m, "playground-base"), stateOf(t, m, a))
	}
	again, err := p.Ensure(ctx, "visitorAAAAAAAAAAAAA")
	if err != nil || again != a {
		t.Fatalf("mesmo visitante: %q %v", again, err)
	}
	b, _ := p.Ensure(ctx, "visitorBBBBBBBBBBBBB")
	if b == a {
		t.Fatal("dois visitantes na mesma máquina")
	}
	// Visitante que volta com a máquina parada: liga de novo, sem branch novo.
	m.Stop(ctx, a, 10*time.Millisecond)
	if got, err := p.Ensure(ctx, "visitorAAAAAAAAAAAAA"); err != nil || got != a || stateOf(t, m, a) != StateRunning {
		t.Fatalf("religar: %q %v %s", got, err, stateOf(t, m, a))
	}
	var ve domain.ValidationError
	if _, err := p.Ensure(ctx, "../etc/passwd"); !errors.As(err, &ve) {
		t.Fatalf("visitor inválido: %v", err)
	}
}

// TestPlaygroundFull: com o teto de máquinas ligadas atingido, um visitante novo espera; o que já está ligado entra.
func TestPlaygroundFull(t *testing.T) {
	p, _, _ := newPlayground(t, 1)
	ctx := context.Background()
	a, err := p.Ensure(ctx, "visitorAAAAAAAAAAAAA")
	if err != nil {
		t.Fatal(err)
	}
	if _, err := p.Ensure(ctx, "visitorBBBBBBBBBBBBB"); !errors.Is(err, ErrPlaygroundFull) {
		t.Fatalf("acima do teto: %v", err)
	}
	if got, err := p.Ensure(ctx, "visitorAAAAAAAAAAAAA"); err != nil || got != a {
		t.Fatalf("o que já está ligado: %q %v", got, err)
	}
}

// TestPlaygroundReap: parada há mais de IdleStop é desligada; sem uso há mais de MaxAge é apagada; a base e as
// máquinas que não são do playground nunca são tocadas.
func TestPlaygroundReap(t *testing.T) {
	p, m, now := newPlayground(t, 3)
	ctx := context.Background()
	m.Create(ctx, domain.Machine{Name: "agent", Stack: "python"})
	m.Start(ctx, "agent")
	a, _ := p.Ensure(ctx, "visitorAAAAAAAAAAAAA")
	b, _ := p.Ensure(ctx, "visitorBBBBBBBBBBBBB")

	*now = now.Add(31 * time.Minute)
	p.Touch(b) // b usado agora: fica ligado
	got := p.Reap(ctx)
	if stateOf(t, m, a) != StateStopped || stateOf(t, m, b) != StateRunning || len(got) != 1 {
		t.Fatalf("depois de 31 min: a=%s b=%s %v", stateOf(t, m, a), stateOf(t, m, b), got)
	}

	*now = now.Add(25 * time.Hour)
	p.Reap(ctx)
	if stateOf(t, m, a) != "gone" || stateOf(t, m, b) != "gone" {
		t.Fatalf("depois de 25 h: a=%s b=%s", stateOf(t, m, a), stateOf(t, m, b))
	}
	if stateOf(t, m, "playground-base") == "gone" || stateOf(t, m, "agent") != StateRunning {
		t.Fatal("o reaper mexeu na base ou numa máquina fora do playground")
	}
}

// TestPlaygroundReapWithoutUsage: sem registro de uso (playground reiniciado), vale a data de criação da máquina.
func TestPlaygroundReapWithoutUsage(t *testing.T) {
	p, m, now := newPlayground(t, 3)
	ctx := context.Background()
	a, _ := p.Ensure(ctx, "visitorAAAAAAAAAAAAA")
	p.lastUse = nil
	created, _ := m.Status(a)
	*now = created.CreatedAt.Add(25 * time.Hour)
	p.Reap(ctx)
	if stateOf(t, m, a) != "gone" {
		t.Fatalf("máquina antiga sem uso registrado: %s", stateOf(t, m, a))
	}
}

// TestPlaygroundStopTimeoutDefault: sem StopTimeout, o stop do playground dá 30 s ao workload.
func TestPlaygroundStopTimeoutDefault(t *testing.T) {
	if got := (&Playground{}).stopTimeout(); got != 30*time.Second {
		t.Fatalf("padrão %v", got)
	}
}

// TestPlaygroundErrors: base com stack inexistente não cria nada; visitante cuja VM não responde ao start recebe o
// erro; máquina pausada é recusada em vez de ligada por cima.
func TestPlaygroundErrors(t *testing.T) {
	ctx := context.Background()
	m, _, _, _, g := machinesFixture(t)
	bad := &Playground{M: m, Base: domain.Machine{Name: "playground-base", Stack: "nao-existe"}, MaxRunning: 3,
		StopTimeout: 10 * time.Millisecond, Now: time.Now}
	if _, err := bad.Ensure(ctx, "visitorAAAAAAAAAAAAA"); err == nil {
		t.Fatal("base com stack inexistente passou")
	}

	p := &Playground{M: m, Base: domain.Machine{Name: "playground-base", Stack: "python"}, MaxRunning: 3,
		StopTimeout: 10 * time.Millisecond, Now: time.Now}
	a, err := p.Ensure(ctx, "visitorAAAAAAAAAAAAA")
	if err != nil {
		t.Fatal(err)
	}
	if err := m.Pause(a); err != nil {
		t.Fatal(err)
	}
	m.Stop(ctx, a, 10*time.Millisecond) // stop numa pausada retoma e desliga: volta a ficar parada
	m.Start(ctx, a)
	m.Pause(a)
	if _, err := p.Ensure(ctx, "visitorAAAAAAAAAAAAA"); err == nil {
		t.Fatal("máquina pausada foi aceita")
	}

	g.readyErr = errors.New("guest não respondeu")
	if _, err := p.Ensure(ctx, "visitorBBBBBBBBBBBBB"); err == nil {
		t.Fatal("start que falha passou")
	}
}
