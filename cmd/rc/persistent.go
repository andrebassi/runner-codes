package main

import (
	"context"
	"fmt"
	"os"
	"sync"
	"time"

	"runner.codes/internal/adapters/gcedisk"
	"runner.codes/internal/adapters/gcepreempt"
	"runner.codes/internal/app"
)

// enablePersistentDisks liga o disco próprio por máquina (rc serve --persistent-disks). Vazio não faz nada: o Mac e a
// casa do superbot seguem com as máquinas no disco do host. "gce":
//
//  1. cada máquina nova nasce num Persistent Disk só dela, e o create de uma máquina cujo disco já existe o adota;
//  2. remonta o que ficou anexado a este host de um boot anterior (o anexo sobrevive ao reboot, a montagem não);
//  3. ouve o aviso de preempção do spot e, nos ~30 s que o GCE dá, desliga as máquinas e solta os discos.
func enablePersistentDisks(ctx context.Context, mode string, m *app.Machines) error {
	switch mode {
	case "":
		return nil
	case "gce":
	default:
		return fmt.Errorf("--persistent-disks %q: only \"gce\" is supported", mode)
	}
	disks := gcedisk.New()
	m.Persist = disks
	rctx, cancel := context.WithTimeout(ctx, 2*time.Minute)
	n, err := disks.Recover(rctx, m.Store.Dir(""))
	cancel()
	if err != nil {
		// Sem remontar, as máquinas daqui não aparecem — mas o create de cada uma adota o disco de novo. Segue.
		fmt.Fprintf(os.Stderr, "persistent disks: recover: %v\n", err)
	} else if n > 0 {
		fmt.Fprintf(os.Stderr, "persistent disks: %d remounted\n", n)
	}
	go watchPreemption(ctx, preemption, m)
	return nil
}

// preemptionWaiter espera o aviso de preempção (gcepreempt.Watcher).
type preemptionWaiter interface {
	Wait(ctx context.Context) error
}

// preemptionChecker diz se o host está em preempção agora (gcepreempt.Watcher.Preempted).
type preemptionChecker interface {
	Preempted(ctx context.Context) (bool, error)
}

// preemption é o metadata server do GCE: o mesmo para o aviso e para a pergunta do SIGTERM.
var preemption = gcepreempt.New()

// draining segura o fim do processo enquanto a drenagem roda (waitDrain).
var draining sync.WaitGroup

// waitDrain espera a drenagem em andamento terminar, até max: sem esta espera o serve sairia e mataria o Drain no
// meio, com disco ainda preso aqui.
func waitDrain(max time.Duration) {
	done := make(chan struct{})
	go func() { draining.Wait(); close(done) }()
	select {
	case <-done:
	case <-time.After(max):
	}
}

// drainHost suspende (ou desliga) as máquinas e solta os discos, num contexto próprio de 25 s: o do serve já acabou
// ou vai acabar no SIGTERM, justo quando o Drain mais precisa terminar. Chamar de novo durante uma drenagem não refaz
// nada (o Drain é idempotente), e o waitDrain espera as duas.
func drainHost(m *app.Machines, why string) {
	draining.Add(1)
	defer draining.Done()
	fmt.Fprintf(os.Stderr, "%s: draining machines and releasing their disks\n", why)
	start := time.Now()
	dctx, cancel := context.WithTimeout(context.Background(), 25*time.Second)
	defer cancel()
	running, err := m.Drain(dctx)
	fmt.Fprintf(os.Stderr, "preemption drain done in %s: %d were running %v; err=%v\n", time.Since(start).Round(time.Millisecond), len(running), running, err)
}

// watchPreemption espera o aviso e drena o host.
func watchPreemption(ctx context.Context, w preemptionWaiter, m *app.Machines) {
	if err := w.Wait(ctx); err != nil {
		return // o serve acabou antes de qualquer aviso
	}
	drainHost(m, "preemption notice")
}

// shutdownDrain é o SIGTERM do serve com disco persistente. Na preempção do spot o GCE manda o desligamento por ACPI
// JUNTO com o aviso do metadata (medido em 25/10/2026: duas preempções seguidas, e nas duas o HQ viu "STOPPING e o rc
// não responde" — o aviso nunca chegou a drenar): o systemd já está parando o rc quando o watcher acordaria, e o
// contexto cancelado o encerra antes de drenar. Aqui o serve pergunta uma vez ao metadata: em preempção, drena antes
// de sair; num restart comum (deploy do rc), segue como sempre, sem suspender nem soltar nada.
// Devolve se drenou.
func shutdownDrain(p preemptionChecker, m *app.Machines) bool {
	cctx, cancel := context.WithTimeout(context.Background(), 2*time.Second)
	preempted, err := p.Preempted(cctx)
	cancel()
	if err != nil || !preempted {
		return false
	}
	drainHost(m, "shutdown during preemption")
	return true
}

// drainLinger é quanto a API fica no ar depois da drenagem da preempção, para o HQ ler a lista de ligadas: o failover
// lê os hosts a cada 5 s, e 8 s garantem uma leitura inteira (com o prazo de 3 s dela) dentro dos ~30 s do GCE.
const drainLinger = 8 * time.Second

// preemptionBudget é o tempo do SIGTERM até o GCE desligar à força (~30 s), com folga para o próprio fechamento.
const preemptionBudget = 28 * time.Second

// lingerAfterDrain é a espera depois da drenagem: o drainLinger, cortado ao que sobra do orçamento da preempção — uma
// drenagem lenta não pode empurrar o fechamento para depois do desligamento à força.
func lingerAfterDrain(elapsed time.Duration) time.Duration {
	return max(0, min(drainLinger, preemptionBudget-elapsed))
}
