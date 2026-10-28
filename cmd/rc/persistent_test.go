package main

import (
	"context"
	"errors"
	"testing"
	"time"
)

// fakeChecker responde a pergunta do SIGTERM com um valor fixo e conta as chamadas.
type fakeChecker struct {
	preempted bool
	err       error
	calls     int
}

// Preempted devolve a resposta combinada.
func (f *fakeChecker) Preempted(context.Context) (bool, error) {
	f.calls++
	return f.preempted, f.err
}

// TestShutdownDrainSkipsPlainRestart: SIGTERM de restart comum (metadata diz FALSE) ou com o metadata fora NÃO drena —
// um deploy do rc não pode suspender as máquinas e soltar os discos. m nil: se drenasse, o teste estouraria.
func TestShutdownDrainSkipsPlainRestart(t *testing.T) {
	for _, c := range []*fakeChecker{{preempted: false}, {err: errors.New("metadata fora")}} {
		if shutdownDrain(c, nil) {
			t.Fatal("drenou num restart comum")
		}
		if c.calls != 1 {
			t.Fatalf("perguntas ao metadata: %d", c.calls)
		}
	}
}

// TestLingerAfterDrain: a API fica 8 s no ar depois da drenagem, mas nunca além dos 28 s do orçamento da preempção.
func TestLingerAfterDrain(t *testing.T) {
	for _, c := range []struct{ elapsed, want time.Duration }{
		{5 * time.Second, drainLinger},
		{25 * time.Second, 3 * time.Second},
		{40 * time.Second, 0},
	} {
		if got := lingerAfterDrain(c.elapsed); got != c.want {
			t.Fatalf("drenagem de %s: espera %s, esperava %s", c.elapsed, got, c.want)
		}
	}
}
