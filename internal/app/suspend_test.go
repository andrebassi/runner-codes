package app

import (
	"context"
	"encoding/base64"
	"errors"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"

	"runner.codes/internal/domain"
	"runner.codes/internal/protocol"
)

// suspendRig é o fixture com suporte a snapshot e uma máquina ligada com workload. O vmm falso, ao receber o sinal,
// grava o manifesto (se writes) e sai.
func suspendRig(t *testing.T, writes bool) (*Machines, *memStore, *procs, *detached, *fileGuest) {
	t.Helper()
	m, st, ps, d, g := machinesFixture(t)
	m.CanSuspend = true
	// Imagem base de verdade: o restore confere o sha256 dela contra o que o snapshot gravou.
	img := filepath.Join(t.TempDir(), "python.ext4")
	os.WriteFile(img, []byte("ext4 da imagem base"), 0o644)
	m.Images = images{"python": img}
	ctx := context.Background()
	if _, err := m.Create(ctx, domain.Machine{Name: "desk", Stack: "python", Workload: []string{"claude", "--continue"}}); err != nil {
		t.Fatal(err)
	}
	if _, err := m.Start(ctx, "desk"); err != nil {
		t.Fatal(err)
	}
	ps.onSuspend = func(pid int) {
		if writes {
			dir := m.snapshotPath("desk")
			os.MkdirAll(dir, 0o700)
			os.WriteFile(filepath.Join(dir, "memory.img"), []byte("ram"), 0o600)
			os.WriteFile(filepath.Join(dir, snapshotManifest), []byte("{}"), 0o600)
		}
		ps.mu.Lock()
		delete(ps.alive, pid)
		ps.mu.Unlock()
	}
	return m, st, ps, d, g
}

// TestSuspendAndRestore: suspender grava o snapshot e desliga (parada, marcada suspensa, sem pid); o start seguinte
// restaura em vez de dar boot (RestoreFrom na spec), NÃO roda o provision (o workload já voltou com o convidado — rodar
// de novo duplicaria o Claude Code) e apaga o snapshot usado. O start depois disso é boot normal, com o provision.
func TestSuspendAndRestore(t *testing.T) {
	m, st, ps, d, g := suspendRig(t, true)
	var log strings.Builder
	m.Log = &log
	ctx := context.Background()
	if err := m.Suspend(ctx, "desk", time.Second); err != nil {
		t.Fatal(err)
	}
	got, _ := m.Status("desk")
	if got.State != StateStopped || !got.Suspended || st.PID("desk") != 0 || len(ps.suspended) != 1 {
		t.Fatalf("depois do suspend: %+v pid=%d", got, st.PID("desk"))
	}
	workloads := len(jobsOf(g, protocol.OpWorkload))
	if _, err := m.Start(ctx, "desk"); err != nil {
		t.Fatal(err)
	}
	spec := d.specs[len(d.specs)-1]
	if spec.RestoreFrom != m.snapshotPath("desk") || spec.SnapshotDir != m.snapshotPath("desk") {
		t.Fatalf("spec do restore: restore=%q snapshot=%q", spec.RestoreFrom, spec.SnapshotDir)
	}
	if n := len(jobsOf(g, protocol.OpWorkload)); n != workloads {
		t.Fatalf("o restore religou o workload (%d → %d)", workloads, n)
	}
	if m.hasSnapshot("desk") {
		t.Fatal("o snapshot usado ficou: o próximo start voltaria no tempo")
	}
	// No worker spot o log é a única pista do que houve na preempção: cada passo deixa a sua linha.
	for _, want := range []string{"machine desk: suspended in", "machine desk: restoring from snapshot", "machine desk: restored from snapshot"} {
		if !strings.Contains(log.String(), want) {
			t.Fatalf("log sem %q:\n%s", want, log.String())
		}
	}
	// O mesmo rastro fica no events.log do diretório da máquina (o disco dela, que viaja na preempção).
	ev, err := os.ReadFile(filepath.Join(m.Store.Dir("desk"), machineEvents))
	if err != nil || !strings.Contains(string(ev), "machine desk: suspended in") || !strings.Contains(string(ev), "machine desk: restored from snapshot") {
		t.Fatalf("events.log: %q %v", ev, err)
	}
	// O relógio do convidado volta parado no instante do suspend: o restore o acerta com a hora do host.
	if c := clockJobs(g); len(c) != 1 || !nearNow(t, c[0]) {
		t.Fatalf("acerto do relógio depois do restore: %+v", c)
	}
	// O gerador aleatório do convidado também volta congelado: o restore mistura 512 bytes novos do host nele.
	if r := tracedJobs(g, reseedTrace); len(r) != 1 || !reseedJob(r[0]) {
		t.Fatalf("entropia depois do restore: %+v", r)
	}
	if got, _ := m.Status("desk"); got.Suspended || got.State != StateRunning {
		t.Fatalf("restaurada: %+v", got)
	}
	// Desliga e liga de novo: sem snapshot, boot normal com o workload.
	m.Stop(ctx, "desk", 0)
	if _, err := m.Start(ctx, "desk"); err != nil {
		t.Fatal(err)
	}
	if spec := d.specs[len(d.specs)-1]; spec.RestoreFrom != "" || spec.SnapshotDir == "" {
		t.Fatalf("boot normal: restore=%q snapshot=%q", spec.RestoreFrom, spec.SnapshotDir)
	}
	if n := len(jobsOf(g, protocol.OpWorkload)); n != workloads+1 {
		t.Fatalf("boot normal sem workload (%d → %d)", workloads, n)
	}
	// Boot do zero tem o relógio certo desde o kernel: nenhum acerto a mais.
	if c := clockJobs(g); len(c) != 1 {
		t.Fatalf("o boot normal acertou o relógio: %d jobs", len(c))
	}
	if r := tracedJobs(g, reseedTrace); len(r) != 1 {
		t.Fatalf("o boot normal mexeu na entropia: %d jobs", len(r))
	}
}

// TestRestoreClockSyncBestEffort: convidado que recusa o acerto do relógio não derruba o Start — a máquina voltou
// inteira — e o acerto é tentado exatamente duas vezes.
func TestRestoreClockSyncBestEffort(t *testing.T) {
	m, _, _, _, g := suspendRig(t, true)
	ctx := context.Background()
	if err := m.Suspend(ctx, "desk", time.Second); err != nil {
		t.Fatal(err)
	}
	g.execErr = errors.New("guest agent gone")
	st, err := m.Start(ctx, "desk")
	if err != nil || st.State != StateRunning {
		t.Fatalf("restore com relógio recusado: %+v %v", st, err)
	}
	if c := clockJobs(g); len(c) != 2 {
		t.Fatalf("tentativas de acerto do relógio: %d", len(c))
	}
	if r := tracedJobs(g, reseedTrace); len(r) != 2 {
		t.Fatalf("tentativas de renovar a entropia: %d", len(r))
	}
}

// TestReseedDiffers: cada restore manda aleatoriedade NOVA — duas máquinas do mesmo dourado divergem.
func TestReseedDiffers(t *testing.T) {
	m, _, _, _, g := suspendRig(t, true)
	m.reseedGuest(context.Background(), "s")
	m.reseedGuest(context.Background(), "s")
	r := tracedJobs(g, reseedTrace)
	if len(r) != 2 || !reseedJob(r[0]) || r[0].Stdin == r[1].Stdin {
		t.Fatalf("entropia repetida: %+v", r)
	}
}

// tracedJobs devolve os exec do convidado com o trace dado.
func tracedJobs(g *fileGuest, trace string) []protocol.Job {
	var out []protocol.Job
	for _, j := range jobsOf(g, protocol.OpExec) {
		if j.TraceID == trace {
			out = append(out, j)
		}
	}
	return out
}

// reseedJob confere o job de entropia: o shell que decodifica para /dev/urandom e 512 bytes em base64 na entrada.
func reseedJob(j protocol.Job) bool {
	in, _ := j.Stdin.(string)
	raw, err := base64.StdEncoding.DecodeString(in)
	return err == nil && len(raw) == reseedBytes && len(j.Argv) == 3 && j.Argv[2] == "base64 -d > /dev/urandom"
}

// TestColdBootDropsStaleSnapshot: host que não sabe restaurar liga do zero e apaga o snapshot que veio no disco —
// senão um start depois, num host que restaura, carregaria uma RAM velha sobre um disco que já mudou.
func TestColdBootDropsStaleSnapshot(t *testing.T) {
	m, _, _, d, _ := suspendRig(t, true)
	ctx := context.Background()
	if err := m.Suspend(ctx, "desk", time.Second); err != nil {
		t.Fatal(err)
	}
	m.CanSuspend = false
	var log strings.Builder
	m.Log = &log
	if _, err := m.Start(ctx, "desk"); err != nil {
		t.Fatal(err)
	}
	if !strings.Contains(log.String(), "machine desk: this host cannot restore; dropping its snapshot") {
		t.Fatalf("log do snapshot velho: %q", log.String())
	}
	if spec := d.specs[len(d.specs)-1]; spec.RestoreFrom != "" {
		t.Fatalf("host sem suporte restaurou: %q", spec.RestoreFrom)
	}
	if _, err := os.Stat(m.snapshotPath("desk")); !os.IsNotExist(err) {
		t.Fatalf("o snapshot velho ficou no disco: %v", err)
	}
}

// TestRestoreRefusesOtherBaseImage: snapshot feito sobre outra imagem base (o host novo construiu a sua, com outro
// sha256) não se restaura — o convidado voltaria com o cache de diretórios de outro disco —, e o snapshot sem a marca
// da imagem (de um rc anterior a ela) também não. Nos dois, boot do zero e o snapshot apagado.
func TestRestoreRefusesOtherBaseImage(t *testing.T) {
	for _, c := range []struct {
		name   string
		tamper func(m *Machines)
		why    string
	}{
		{"imagem reconstruída", func(m *Machines) {
			img, _ := m.Images.ImagePath("python")
			os.WriteFile(img, []byte("outro build do ext4"), 0o644)
			// O arquivo do lado ficou mais velho que a imagem: o digest é refeito.
			old := time.Now().Add(-time.Hour)
			os.Chtimes(img+".sha256", old, old)
		}, "another build of the base image"},
		{"snapshot sem marca", func(m *Machines) {
			os.Remove(filepath.Join(m.snapshotPath("desk"), snapshotBaseImage))
		}, "does not record its base image"},
	} {
		t.Run(c.name, func(t *testing.T) {
			m, _, _, d, _ := suspendRig(t, true)
			ctx := context.Background()
			if err := m.Suspend(ctx, "desk", time.Second); err != nil {
				t.Fatal(err)
			}
			if _, err := os.Stat(filepath.Join(m.snapshotPath("desk"), snapshotBaseImage)); err != nil {
				t.Fatalf("o suspend não gravou a marca da imagem: %v", err)
			}
			c.tamper(m)
			var log strings.Builder
			m.Log = &log
			if _, err := m.Start(ctx, "desk"); err != nil {
				t.Fatal(err)
			}
			if spec := d.specs[len(d.specs)-1]; spec.RestoreFrom != "" {
				t.Fatalf("restaurou sobre outra imagem: %q", spec.RestoreFrom)
			}
			if m.hasSnapshot("desk") || !strings.Contains(log.String(), c.why) {
				t.Fatalf("snapshot=%v log=%q", m.hasSnapshot("desk"), log.String())
			}
		})
	}
}

// TestImageDigestCache: o digest é o sha256 do arquivo, guardado ao lado e reaproveitado enquanto a imagem não muda;
// imagem que muda depois do arquivo do lado tem o digest refeito; imagem que não existe é erro.
func TestImageDigestCache(t *testing.T) {
	img := filepath.Join(t.TempDir(), "x.ext4")
	os.WriteFile(img, []byte("abc"), 0o644)
	a, err := imageDigest(img)
	if err != nil || a != "ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad" {
		t.Fatalf("digest: %q %v", a, err)
	}
	// Arquivo do lado adulterado e mais novo: vale o cache (é ele que poupa a leitura no Suspend).
	os.WriteFile(img+".sha256", []byte(strings.Repeat("0", 64)+"\n"), 0o644)
	if b, _ := imageDigest(img); b != strings.Repeat("0", 64) {
		t.Fatalf("cache ignorado: %q", b)
	}
	old := time.Now().Add(-time.Hour)
	os.Chtimes(img+".sha256", old, old)
	if c, _ := imageDigest(img); c != a {
		t.Fatalf("cache velho reaproveitado: %q", c)
	}
	if _, err := imageDigest(img + ".nada"); err == nil {
		t.Fatal("imagem inexistente sem erro")
	}
}

// clockJobs devolve os jobs de acerto de relógio que o convidado recebeu.
func clockJobs(g *fileGuest) []protocol.Job {
	var out []protocol.Job
	for _, j := range jobsOf(g, protocol.OpExec) {
		if j.TraceID == clockSyncTrace {
			out = append(out, j)
		}
	}
	return out
}

// nearNow confere que o job é `date -u -s @<segundos.nanos>` com a hora de agora (até 5 s de diferença).
func nearNow(t *testing.T, j protocol.Job) bool {
	t.Helper()
	if len(j.Argv) != 4 || j.Argv[0] != "date" || j.Argv[1] != "-u" || j.Argv[2] != "-s" || !strings.HasPrefix(j.Argv[3], "@") {
		return false
	}
	secs, _, _ := strings.Cut(strings.TrimPrefix(j.Argv[3], "@"), ".")
	n, err := strconv.ParseInt(secs, 10, 64)
	if err != nil {
		return false
	}
	d := time.Since(time.Unix(n, 0))
	return d > -5*time.Second && d < 5*time.Second
}

// TestRestoreFallsBackToBoot: snapshot que não volta (o convidado não responde) é descartado e a máquina liga do
// zero — sem o estado da RAM, mas ligada e com o disco inteiro.
func TestRestoreFallsBackToBoot(t *testing.T) {
	m, _, ps, d, g := suspendRig(t, true)
	ctx := context.Background()
	if err := m.Suspend(ctx, "desk", time.Second); err != nil {
		t.Fatal(err)
	}
	g.readyFailures = 1
	var log strings.Builder
	m.Log = &log
	if _, err := m.Start(ctx, "desk"); err != nil {
		t.Fatalf("fallback: %v", err)
	}
	if !strings.Contains(log.String(), "machine desk: restore did not come up") {
		t.Fatalf("log do fallback: %q", log.String())
	}
	n := len(d.specs)
	if d.specs[n-2].RestoreFrom == "" || d.specs[n-1].RestoreFrom != "" || m.hasSnapshot("desk") || len(ps.killed) == 0 {
		t.Fatalf("restore %q, depois %q, snapshot=%v, mortos %v", d.specs[n-2].RestoreFrom, d.specs[n-1].RestoreFrom,
			m.hasSnapshot("desk"), ps.killed)
	}
}

// TestDrainSuspends: no aviso de preempção, a máquina ligada é SUSPENSA (snapshot no disco, nenhum stop pelo guest) e
// o disco é solto com ela congelada dentro; a parada só solta o disco. Suspend que não termina no prazo cai no
// desligamento imediato (kill) e o disco sai do mesmo jeito.
func TestDrainSuspends(t *testing.T) {
	ctx := context.Background()
	old := drainSuspendTimeout
	drainSuspendTimeout = 100 * time.Millisecond
	t.Cleanup(func() { drainSuspendTimeout = old })

	m, st, ps, _, g := suspendRig(t, true)
	p := newFakePersist(st)
	m.Persist = p
	p.mounted[m.Store.Dir("desk")] = true
	if _, err := m.Create(ctx, domain.Machine{Name: "idle", Stack: "python"}); err != nil {
		t.Fatal(err)
	}
	shutdowns := len(jobsOf(g, protocol.OpShutdown))
	running, err := m.Drain(ctx)
	if err != nil || len(running) != 1 || running[0] != "desk" {
		t.Fatalf("drain: %v %v", running, err)
	}
	if !m.hasSnapshot("desk") || len(ps.suspended) != 1 || len(jobsOf(g, protocol.OpShutdown)) != shutdowns {
		t.Fatalf("não suspendeu: snapshot=%v suspensos=%v shutdowns %d→%d", m.hasSnapshot("desk"), ps.suspended,
			shutdowns, len(jobsOf(g, protocol.OpShutdown)))
	}
	if len(p.released) != 2 {
		t.Fatalf("discos soltos: %v", p.released)
	}

	// O vmm não atende o pedido: o drain mata a VM e solta o disco, e o erro do suspend aparece.
	m2, st2, ps2, _, _ := suspendRig(t, true)
	ps2.onSuspend = nil
	p2 := newFakePersist(st2)
	m2.Persist = p2
	p2.mounted[m2.Store.Dir("desk")] = true
	pid := st2.PID("desk")
	if _, err := m2.Drain(ctx); err == nil || !strings.Contains(err.Error(), "suspend") {
		t.Fatalf("drain com suspend falho: %v", err)
	}
	if ps2.alive[pid] || len(ps2.killed) == 0 || len(p2.released) != 1 {
		t.Fatalf("a VM congelada ficou: vivo=%v mortos=%v soltos=%v", ps2.alive[pid], ps2.killed, p2.released)
	}
}

// TestSuspendFailures: só máquina ligada suspende; host sem suporte recusa com ErrSuspendUnsupported (quem chama
// desliga); vmm que sai sem manifesto é erro e o snapshot pela metade some; vmm que não sai no prazo é erro sem
// matar (quem chama decide).
func TestSuspendFailures(t *testing.T) {
	ctx := context.Background()
	t.Run("parada", func(t *testing.T) {
		m, _, _, _, _ := suspendRig(t, true)
		m.Stop(ctx, "desk", 0)
		if err := m.Suspend(ctx, "desk", time.Second); errCode(err) != domain.CodeInvalidState {
			t.Fatalf("parada: %v", err)
		}
	})
	t.Run("sem suporte", func(t *testing.T) {
		m, _, _, _, _ := suspendRig(t, true)
		m.CanSuspend = false
		if err := m.Suspend(ctx, "desk", time.Second); !errors.Is(err, ErrSuspendUnsupported) {
			t.Fatalf("sem suporte: %v", err)
		}
	})
	t.Run("sem manifesto", func(t *testing.T) {
		m, _, _, _, _ := suspendRig(t, false)
		os.MkdirAll(m.snapshotPath("desk"), 0o700)
		os.WriteFile(filepath.Join(m.snapshotPath("desk"), "memory.img"), []byte("pela metade"), 0o600)
		if err := m.Suspend(ctx, "desk", time.Second); err == nil || !strings.Contains(err.Error(), "without a complete snapshot") {
			t.Fatalf("sem manifesto: %v", err)
		}
		if _, err := os.Stat(m.snapshotPath("desk")); !os.IsNotExist(err) {
			t.Fatal("o snapshot pela metade ficou")
		}
	})
	t.Run("não sai no prazo", func(t *testing.T) {
		m, _, ps, _, _ := suspendRig(t, true)
		ps.onSuspend = nil // o vmm não atende: segue vivo
		if err := m.Suspend(ctx, "desk", 50*time.Millisecond); err == nil || !strings.Contains(err.Error(), "not finished") {
			t.Fatalf("prazo: %v", err)
		}
		if len(ps.killed) != 0 {
			t.Fatal("o suspend matou o vmm: quem decide é quem chamou")
		}
	})
	t.Run("contexto cancelado", func(t *testing.T) {
		m, _, ps, _, _ := suspendRig(t, true)
		ps.onSuspend = nil
		cctx, cancel := context.WithCancel(ctx)
		cancel()
		if err := m.Suspend(cctx, "desk", time.Second); !errors.Is(err, context.Canceled) {
			t.Fatalf("cancelado: %v", err)
		}
	})
	t.Run("máquina inexistente", func(t *testing.T) {
		m, _, _, _, _ := suspendRig(t, true)
		if err := m.Suspend(ctx, "nope", time.Second); errCode(err) != domain.CodeNotFound {
			t.Fatalf("inexistente: %v", err)
		}
	})
}

// TestRestoreKeepsMemoryImage: depois do restore o snapshot deixa de ser restaurável (sem manifesto, estado nem marca
// da imagem — voltar a ele seria voltar no tempo), mas a RAM fica: a libkrun a restaurou mapeada, e o suspend seguinte
// grava nela só o que mudou. O suspend seguinte também não a apaga antes de pedir o snapshot.
func TestRestoreKeepsMemoryImage(t *testing.T) {
	m, _, ps, _, _ := suspendRig(t, true)
	ctx := context.Background()
	if err := m.Suspend(ctx, "desk", time.Second); err != nil {
		t.Fatal(err)
	}
	dir := m.snapshotPath("desk")
	os.WriteFile(filepath.Join(dir, snapshotVMState), []byte("v"), 0o600)
	if _, err := m.Start(ctx, "desk"); err != nil {
		t.Fatal(err)
	}
	exists := func(f string) bool { _, err := os.Stat(filepath.Join(dir, f)); return err == nil }
	if !exists("memory.img") || exists(snapshotManifest) || exists(snapshotVMState) || exists(snapshotBaseImage) || m.hasSnapshot("desk") {
		t.Fatalf("depois do restore: memory=%v manifest=%v vmstate=%v base=%v", exists("memory.img"), exists(snapshotManifest),
			exists(snapshotVMState), exists(snapshotBaseImage))
	}
	// O suspend seguinte encontra a RAM no lugar quando pede o snapshot ao vmm.
	sawMemory := false
	prev := ps.onSuspend
	ps.onSuspend = func(pid int) {
		sawMemory = exists("memory.img")
		prev(pid)
	}
	if err := m.Suspend(ctx, "desk", time.Second); err != nil || !sawMemory {
		t.Fatalf("o suspend apagou a RAM antes de pedir o snapshot: %v %v", sawMemory, err)
	}
	if err := m.invalidateSnapshot("nada"); err != nil {
		t.Fatalf("snapshot ausente: %v", err)
	}
}

