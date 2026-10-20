package app

import (
	"context"
	"errors"
	"strings"
	"sync"
	"time"

	"runner.code/internal/domain"
)

// ErrPlaygroundFull é a recusa quando o teto de máquinas de visitante ligadas ao mesmo tempo já foi atingido.
var ErrPlaygroundFull = errors.New("playground is full: too many machines running, try again in a few minutes")

// Playground dá a cada visitante (identificado pelo visitorId do navegador) uma máquina própria: um branch
// copy-on-write de uma máquina-base já preparada (Claude Code, git, python), então a máquina nova nasce na hora e
// sem refazer o init. A máquina do visitante não tem pasta do host nem agente SSH: o isolamento entre visitantes é a
// VM, não a pasta.
type Playground struct {
	M *Machines
	// Base é a máquina-base, já com nome (ex.: playground-base); criada e preparada no primeiro uso.
	Base domain.Machine
	// MaxRunning é o teto de máquinas de visitante ligadas ao mesmo tempo (cada uma é CPU e memória deste host).
	MaxRunning int
	// IdleStop desliga a máquina sem uso há esse tempo; MaxAge apaga a que ficou esse tempo sem uso.
	IdleStop, MaxAge time.Duration
	// StopTimeout é quanto o workload tem para sair num stop do playground (0 vale 30 s, o stop gracioso padrão).
	StopTimeout time.Duration
	// Now é o relógio (variável para o teste).
	Now func() time.Time

	mu      sync.Mutex
	lastUse map[string]time.Time
}

// Ensure devolve a máquina do visitante ligada: cria a base se faltar, faz o branch se o visitante é novo e liga se
// estiver parada, respeitando MaxRunning. Serializado: duas abas do mesmo visitante não criam duas máquinas.
func (p *Playground) Ensure(ctx context.Context, visitor string) (string, error) {
	name, err := domain.VisitorMachineName(visitor)
	if err != nil {
		return "", err
	}
	p.mu.Lock()
	defer p.mu.Unlock()
	if err := p.ensureBase(ctx); err != nil {
		return "", err
	}
	st, err := p.M.Status(name)
	if isNotFound(err) {
		if _, err := p.M.Branch(ctx, p.Base.Name, name); err != nil {
			return "", err
		}
		st, err = p.M.Status(name)
	}
	if err != nil {
		return "", err
	}
	if st.State != StateRunning {
		if p.runningVisitors() >= p.MaxRunning {
			return "", ErrPlaygroundFull
		}
		if st.State == StatePaused {
			return "", domain.ErrMachineState(name, "is paused")
		}
		if _, err := p.M.Start(ctx, name); err != nil {
			return "", err
		}
	}
	p.touchLocked(name)
	return name, nil
}

// ensureBase cria e prepara a máquina-base uma vez (o init dela instala as ferramentas) e a deixa parada: o branch de
// uma máquina parada copia um disco consistente.
func (p *Playground) ensureBase(ctx context.Context) error {
	_, err := p.M.Status(p.Base.Name)
	if err == nil {
		return nil
	}
	if !isNotFound(err) {
		return err
	}
	if _, err := p.M.Create(ctx, p.Base); err != nil {
		return err
	}
	if _, err := p.M.Start(ctx, p.Base.Name); err != nil {
		return err
	}
	// O init já terminou quando o Start voltou: parar aqui deixa o disco da base consistente para os branches.
	return p.M.Stop(ctx, p.Base.Name, p.stopTimeout())
}

// runningVisitors conta as máquinas de visitante ligadas (a base não conta).
func (p *Playground) runningVisitors() int {
	all, err := p.M.List()
	if err != nil {
		return 0
	}
	n := 0
	for _, st := range all {
		if p.isVisitor(st.Name) && st.State == StateRunning {
			n++
		}
	}
	return n
}

// isVisitor diz se a máquina é de visitante: o reaper e o teto só olham estas, nunca as outras máquinas do host.
func (p *Playground) isVisitor(name string) bool {
	return strings.HasPrefix(name, domain.VisitorPrefix) && name != p.Base.Name
}

// Touch marca uso agora (cada requisição do visitante ao terminal ou às portas dele).
func (p *Playground) Touch(name string) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.touchLocked(name)
}

// touchLocked é o Touch com o lock já tomado.
func (p *Playground) touchLocked(name string) {
	if p.lastUse == nil {
		p.lastUse = map[string]time.Time{}
	}
	p.lastUse[name] = p.Now()
}

// Reap desliga as máquinas de visitante paradas de uso há mais de IdleStop e apaga as sem uso há mais de MaxAge.
// Sem registro de uso (o processo do playground reiniciou), vale a data de criação. Devolve o que fez, para o log.
func (p *Playground) Reap(ctx context.Context) []string {
	all, err := p.M.List()
	if err != nil {
		return nil
	}
	p.mu.Lock()
	defer p.mu.Unlock()
	now := p.Now()
	var done []string
	for _, st := range all {
		if !p.isVisitor(st.Name) {
			continue
		}
		last, ok := p.lastUse[st.Name]
		if !ok {
			last = st.CreatedAt
		}
		idle := now.Sub(last)
		switch {
		case idle > p.MaxAge:
			if err := p.M.Delete(ctx, st.Name, true); err == nil {
				delete(p.lastUse, st.Name)
				done = append(done, "deleted "+st.Name)
			}
		case idle > p.IdleStop && st.State == StateRunning:
			if err := p.M.Stop(ctx, st.Name, p.stopTimeout()); err == nil {
				done = append(done, "stopped "+st.Name)
			}
		}
	}
	return done
}

// stopTimeout é o StopTimeout com o padrão de 30 s.
func (p *Playground) stopTimeout() time.Duration {
	if p.StopTimeout > 0 {
		return p.StopTimeout
	}
	return 30 * time.Second
}

// isNotFound diz se o erro é de máquina inexistente.
func isNotFound(err error) bool {
	var me domain.MachineError
	return errors.As(err, &me) && me.Code == domain.CodeNotFound
}
