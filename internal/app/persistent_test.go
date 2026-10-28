package app

import (
	"context"
	"errors"
	"sort"
	"strings"
	"sync"
	"testing"

	"runner.codes/internal/domain"
)

// fakePersist é o disco próprio de mentira: guarda o que está montado e, para simular um disco que já tem uma
// máquina dentro (ela morava num host que sumiu), põe o registro no store na hora do Ensure.
type fakePersist struct {
	mu        sync.Mutex
	st        *memStore
	mounted   map[string]bool
	onDisk    map[string]domain.Machine // disco existente com a máquina dentro
	ensured   []string
	released  []string
	destroyed []string
	ensureErr error
	// createdErr: o disco nasce e um passo seguinte falha (Ensure devolve true junto com o erro).
	createdErr bool
	relErr     error
}

// newFakePersist devolve o fake ligado ao store do fixture.
func newFakePersist(st *memStore) *fakePersist {
	return &fakePersist{st: st, mounted: map[string]bool{}, onDisk: map[string]domain.Machine{}}
}

// Ensure monta; com a máquina no disco, adota (o registro aparece no store); sem, cria vazio.
func (f *fakePersist) Ensure(_ context.Context, name, dir string, size int) (bool, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.ensured = append(f.ensured, name)
	if f.ensureErr != nil {
		return f.createdErr, f.ensureErr
	}
	f.mounted[dir] = true
	if m, ok := f.onDisk[name]; ok {
		f.st.mu.Lock()
		f.st.ms[name] = m
		f.st.mu.Unlock()
		return false, nil
	}
	return true, nil
}

// Mounted diz se o diretório está montado.
func (f *fakePersist) Mounted(dir string) bool {
	f.mu.Lock()
	defer f.mu.Unlock()
	return f.mounted[dir]
}

// Release desmonta; a máquina some deste host (o registro estava no disco).
func (f *fakePersist) Release(_ context.Context, name, dir string) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.released = append(f.released, name)
	if f.relErr != nil {
		return f.relErr
	}
	delete(f.mounted, dir)
	f.st.mu.Lock()
	delete(f.st.ms, name)
	f.st.mu.Unlock()
	return nil
}

// Destroy desmonta e apaga: a máquina some de vez.
func (f *fakePersist) Destroy(_ context.Context, name, dir string) error {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.destroyed = append(f.destroyed, name)
	delete(f.mounted, dir)
	f.st.mu.Lock()
	delete(f.st.ms, name)
	f.st.mu.Unlock()
	return nil
}

// TestPersistentCreate: máquina nova nasce num disco próprio criado agora (o modelo é clonado para dentro dele); o
// create de uma máquina cujo disco já existe ADOTA — devolve a máquina de lá, sem clonar o modelo por cima (isso
// apagaria o que o usuário tinha). É o pod do StatefulSet renascendo com o mesmo PersistentVolume.
func TestPersistentCreate(t *testing.T) {
	m, st, _, _, _ := machinesFixture(t)
	p := newFakePersist(st)
	m.Persist = p
	ctx := context.Background()
	if _, err := m.Create(ctx, domain.Machine{Name: "dev", Stack: "python", DiskGiB: 12}); err != nil {
		t.Fatal(err)
	}
	if !p.Mounted(st.Dir("dev")) || len(st.tpls) != 1 {
		t.Fatalf("nova: montado=%v modelos=%v", p.Mounted(st.Dir("dev")), st.tpls)
	}
	// A máquina "old" mora num disco que outro host soltou: o create adota.
	p.onDisk["old"] = domain.Machine{Name: "old", Stack: "python", CPUs: 2, DiskGiB: 12}
	got, err := m.Create(ctx, domain.Machine{Name: "old", Stack: "python"})
	if err != nil || got.CPUs != 2 || len(st.tpls) != 1 {
		t.Fatalf("adotar: %+v %v (modelos %v)", got, err, st.tpls)
	}
	if !m.HasPersistentDisks() {
		t.Fatal("HasPersistentDisks")
	}
	if persistentGiB(12) != 13 || persistentGiB(2) != 10 {
		t.Fatal("tamanho do disco")
	}
}

// TestPersistentCreateFailures: o disco não vem → o create falha sem registrar nada; disco criado agora e o create
// falhando depois → o disco é apagado (senão cobra sem dono); disco adotado nunca é apagado por um create que falha.
func TestPersistentCreateFailures(t *testing.T) {
	m, st, _, _, _ := machinesFixture(t)
	p := newFakePersist(st)
	m.Persist = p
	ctx := context.Background()
	p.ensureErr = errors.New("quota")
	if _, err := m.Create(ctx, domain.Machine{Name: "a", Stack: "python"}); err == nil || len(st.ms) != 0 {
		t.Fatalf("sem disco: %v %v", err, st.ms)
	}
	if len(p.destroyed) != 0 {
		t.Fatalf("apagou disco que não nasceu nesta chamada: %v", p.destroyed)
	}
	// O disco nasceu e o anexar falhou: o create apaga o disco recém-criado em vez de deixá-lo cru.
	p.createdErr = true
	if _, err := m.Create(ctx, domain.Machine{Name: "c", Stack: "python"}); err == nil || len(p.destroyed) != 1 || p.destroyed[0] != "c" {
		t.Fatalf("disco cru vazou: %v %v", err, p.destroyed)
	}
	p.createdErr = false
	p.destroyed = nil
	p.ensureErr = nil
	// ruby está no catálogo mas sem imagem: a recusa vem DEPOIS do Ensure.
	if _, err := m.Create(ctx, domain.Machine{Name: "b", Stack: "ruby"}); err == nil {
		t.Fatal("ruby sem imagem criou")
	}
	if len(p.destroyed) != 1 || p.destroyed[0] != "b" {
		t.Fatalf("disco novo não apagado: %v", p.destroyed)
	}
}

// TestPersistentDelete: apagar a máquina num disco próprio apaga o disco (Destroy), não só os arquivos; a máquina
// antiga, de antes dos discos, segue pelo store.
func TestPersistentDelete(t *testing.T) {
	m, st, _, _, _ := machinesFixture(t)
	ctx := context.Background()
	if _, err := m.Create(ctx, domain.Machine{Name: "legacy", Stack: "python"}); err != nil {
		t.Fatal(err)
	}
	p := newFakePersist(st)
	m.Persist = p
	if _, err := m.Create(ctx, domain.Machine{Name: "dev", Stack: "python"}); err != nil {
		t.Fatal(err)
	}
	if err := m.Delete(ctx, "dev", false); err != nil || len(p.destroyed) != 1 {
		t.Fatalf("delete: %v %v", err, p.destroyed)
	}
	if err := m.Delete(ctx, "legacy", false); err != nil || len(p.destroyed) != 1 {
		t.Fatalf("delete antiga: %v %v", err, p.destroyed)
	}
	if _, err := st.Load("legacy"); err == nil {
		t.Fatal("a antiga ficou")
	}
}

// TestPersistentDeleteRunningWaitsExit: apagar com force a máquina ligada num disco próprio espera o vmm sair antes de
// soltar o disco — o desmonte com o processo ainda saindo dava EBUSY e deixava a máquina ligada no registro.
func TestPersistentDeleteRunningWaitsExit(t *testing.T) {
	m, st, ps, _, _ := machinesFixture(t)
	ctx := context.Background()
	m.Persist = newFakePersist(st)
	if _, err := m.Create(ctx, domain.Machine{Name: "trial", Stack: "python"}); err != nil {
		t.Fatal(err)
	}
	if _, err := m.Start(ctx, "trial"); err != nil {
		t.Fatal(err)
	}
	ps.linger = 3
	if err := m.Delete(ctx, "trial", true); err != nil {
		t.Fatal(err)
	}
	if ps.linger != 0 || len(m.Persist.(*fakePersist).destroyed) != 1 {
		t.Fatalf("soltou o disco sem esperar o vmm sair: linger=%d", ps.linger)
	}
}

// TestPersistentRelease: soltar desliga a ligada e solta o disco; sem disco próprio (host sem o recurso, ou máquina
// antiga) é ErrNoPersistentDisk — o orquestrador usa a cópia; máquina desconhecida é NOT_FOUND; erro do release sobe.
func TestPersistentRelease(t *testing.T) {
	m, st, ps, _, _ := machinesFixture(t)
	ctx := context.Background()
	if _, err := m.Create(ctx, domain.Machine{Name: "legacy", Stack: "python"}); err != nil {
		t.Fatal(err)
	}
	if err := m.Release(ctx, "legacy"); !errors.Is(err, ErrNoPersistentDisk) {
		t.Fatalf("sem Persist: %v", err)
	}
	p := newFakePersist(st)
	m.Persist = p
	if err := m.Release(ctx, "legacy"); !errors.Is(err, ErrNoPersistentDisk) {
		t.Fatalf("antiga: %v", err)
	}
	if _, err := m.Create(ctx, domain.Machine{Name: "dev", Stack: "python"}); err != nil {
		t.Fatal(err)
	}
	if _, err := m.Start(ctx, "dev"); err != nil {
		t.Fatal(err)
	}
	pid := st.PID("dev")
	if err := m.Release(ctx, "dev"); err != nil {
		t.Fatal(err)
	}
	if ps.alive[pid] || len(p.released) != 1 {
		t.Fatalf("soltou ligada: vivo=%v soltos=%v", ps.alive[pid], p.released)
	}
	if err := m.Release(ctx, "nada"); errCode(err) != domain.CodeNotFound {
		t.Fatalf("desconhecida: %v", err)
	}
	if _, err := m.Create(ctx, domain.Machine{Name: "e", Stack: "python"}); err != nil {
		t.Fatal(err)
	}
	p.relErr = errors.New("detach")
	if err := m.Release(ctx, "e"); err == nil {
		t.Fatal("erro do release sumiu")
	}
}

// TestDrain: o aviso de preempção desliga as ligadas, solta TODOS os discos e devolve quem estava ligada (publicada
// antes de desligar); o host passa a recusar máquina nova; o segundo aviso não refaz nada.
func TestDrain(t *testing.T) {
	m, st, ps, _, _ := machinesFixture(t)
	p := newFakePersist(st)
	m.Persist = p
	ctx := context.Background()
	for _, n := range []string{"a", "b", "c"} {
		if _, err := m.Create(ctx, domain.Machine{Name: n, Stack: "python"}); err != nil {
			t.Fatal(err)
		}
	}
	m.Start(ctx, "a")
	m.Start(ctx, "c")
	pa, pc := st.PID("a"), st.PID("c")
	running, err := m.Drain(ctx)
	sort.Strings(running)
	if err != nil || len(running) != 2 || running[0] != "a" || running[1] != "c" {
		t.Fatalf("drain: %v %v", running, err)
	}
	if ps.alive[pa] || ps.alive[pc] || len(p.released) != 3 {
		t.Fatalf("ligadas vivas ou discos presos: %v %v soltos %v", ps.alive[pa], ps.alive[pc], p.released)
	}
	active, drained := m.Preemption()
	if !active || len(drained) != 2 {
		t.Fatalf("estado: %v %v", active, drained)
	}
	if _, err := m.Create(ctx, domain.Machine{Name: "d", Stack: "python"}); errCode(err) != domain.CodeInvalidState {
		t.Fatalf("host em preempção aceitou máquina: %v", err)
	}
	again, err := m.Drain(ctx)
	if err != nil || len(again) != 2 || len(p.released) != 3 {
		t.Fatalf("2º aviso refez: %v %v %v", again, err, p.released)
	}
}

// TestDrainFailures: release que falha entra no erro sem impedir o resto; host sem disco próprio só desliga; store
// ilegível devolve o erro.
func TestDrainFailures(t *testing.T) {
	m, st, _, _, _ := machinesFixture(t)
	p := newFakePersist(st)
	m.Persist = p
	ctx := context.Background()
	m.Create(ctx, domain.Machine{Name: "a", Stack: "python"})
	m.Create(ctx, domain.Machine{Name: "b", Stack: "python"})
	p.relErr = errors.New("detach")
	if _, err := m.Drain(ctx); err == nil || len(p.released) != 2 {
		t.Fatalf("erro do release: %v %v", err, p.released)
	}
	// Sem Persist: desliga e nada mais.
	m2, st2, ps2, _, _ := machinesFixture(t)
	m2.Create(ctx, domain.Machine{Name: "x", Stack: "python"})
	m2.Start(ctx, "x")
	pid := st2.PID("x")
	if running, err := m2.Drain(ctx); err != nil || len(running) != 1 || ps2.alive[pid] {
		t.Fatalf("sem Persist: %v %v (vivo %v)", running, err, ps2.alive[pid])
	}
	// Store ilegível: o List falha e o Drain devolve o erro.
	m3, st3, _, _, _ := machinesFixture(t)
	st3.mu.Lock()
	st3.ms["bad"] = domain.Machine{Name: "bad"}
	st3.mu.Unlock()
	m3.Store = failingList{st3}
	if _, err := m3.Drain(ctx); err == nil {
		t.Fatal("store ilegível")
	}
}

// failingList é o store cujo List falha.
type failingList struct{ *memStore }

// List falha sempre.
func (failingList) List() ([]domain.Machine, error) { return nil, errors.New("disk read") }

// TestAdoptBuildsImageHere: a máquina de imagem que chega num host novo (pelo create do HQ ou pelo Adopt do move)
// tem a imagem construída AQUI — o disco traz a máquina, não a imagem. Build que falha: o create devolve o erro e o
// Adopt solta o disco de novo (não fica preso onde não liga). E o Adopt que cria disco e falha depois apaga o disco.
func TestAdoptBuildsImageHere(t *testing.T) {
	m, st, _, _, _ := machinesFixture(t)
	ctx := context.Background()
	var built []string
	var buildErr error
	m.BuildImage = func(_ context.Context, l domain.Language) error {
		built = append(built, l.Image)
		return buildErr
	}
	p := newFakePersist(st)
	m.Persist = p
	moved := domain.Machine{Name: "desk", Stack: ImageStackName("docker.io/sbot/desktop:1"), Image: "docker.io/sbot/desktop:1"}
	p.onDisk["desk"] = moved
	if _, err := m.Create(ctx, domain.Machine{Name: "desk", Image: "docker.io/sbot/desktop:1"}); err != nil || len(built) != 1 || built[0] != moved.Image {
		t.Fatalf("create por adoção não construiu a imagem: %v %v", err, built)
	}
	// Adopt (move): mesma regra; com o build falhando, o disco é solto e o erro aparece.
	p2 := newFakePersist(st)
	m.Persist = p2
	p2.onDisk["desk2"] = domain.Machine{Name: "desk2", Stack: ImageStackName("docker.io/sbot/desktop:2"), Image: "docker.io/sbot/desktop:2"}
	buildErr = errors.New("registry fora")
	if _, err := m.Adopt(ctx, "desk2"); err == nil || !strings.Contains(err.Error(), "registry fora") || len(p2.released) != 1 {
		t.Fatalf("adopt com build falhando: %v soltos %v", err, p2.released)
	}
	// Create por adoção com o build falhando também devolve o erro.
	p3 := newFakePersist(st)
	m.Persist = p3
	p3.onDisk["desk3"] = domain.Machine{Name: "desk3", Stack: ImageStackName("docker.io/sbot/desktop:3"), Image: "docker.io/sbot/desktop:3"}
	if _, err := m.Create(ctx, domain.Machine{Name: "desk3", Image: "docker.io/sbot/desktop:3"}); err == nil {
		t.Fatal("create por adoção com build falhando sem erro")
	}
	buildErr = nil
	// Adopt cujo Ensure cria o disco e falha depois: apaga o disco e devolve o erro do disco.
	p4 := newFakePersist(st)
	m.Persist = p4
	p4.ensureErr, p4.createdErr = errors.New("attach negado"), true
	if _, err := m.Adopt(ctx, "nova"); err == nil || !strings.Contains(err.Error(), "attach negado") || len(p4.destroyed) != 1 {
		t.Fatalf("adopt com disco criado e falho: %v apagados %v", err, p4.destroyed)
	}
}

// TestStartBuildsMissingImage: máquina de imagem cujo ext4 não está neste host (adotada por um rc antigo, ou cache
// podado) é construída no start, em vez de recusar com "not built"; build que falha devolve o erro dele; máquina de
// stack sem imagem segue pedindo o `rc image build`.
func TestStartBuildsMissingImage(t *testing.T) {
	m, st, _, _, _ := machinesFixture(t)
	ctx := context.Background()
	imgs := m.Images.(images)
	ref := "docker.io/sbot/desktop:9"
	stack := ImageStackName(ref)
	var buildErr error
	builds := 0
	m.BuildImage = func(_ context.Context, l domain.Language) error {
		builds++
		if buildErr == nil {
			imgs[l.Name] = "/img/" + l.Name + ".ext4"
		}
		return buildErr
	}
	// O registro chega pelo disco (como na adoção), sem a imagem no host.
	st.mu.Lock()
	st.ms["moved"] = domain.Machine{Name: "moved", Stack: stack, Image: ref}.WithDefaults()
	st.mu.Unlock()
	buildErr = errors.New("registry fora")
	if _, err := m.Start(ctx, "moved"); err == nil || !strings.Contains(err.Error(), "registry fora") {
		t.Fatalf("build falhando: %v", err)
	}
	buildErr = nil
	if _, err := m.Start(ctx, "moved"); err != nil || builds != 2 {
		t.Fatalf("start não construiu a imagem que faltava: %v (builds %d)", err, builds)
	}
	// Stack sem imagem (não reconstruível daqui): a recusa antiga.
	st.mu.Lock()
	st.ms["old"] = domain.Machine{Name: "old", Stack: "ruby"}.WithDefaults()
	st.mu.Unlock()
	if _, err := m.Start(ctx, "old"); err == nil || !strings.Contains(err.Error(), "not built") {
		t.Fatalf("stack sem imagem: %v", err)
	}
}

// TestAdopt: o disco solto com a máquina dentro aparece aqui, desligado; já aqui não chama nada; disco que não existe
// é NOT_FOUND e o disco vazio criado no caminho é apagado (adotar nunca inventa máquina); disco sem registro é solto de
// novo; host sem o recurso ou em preempção recusa.
func TestAdopt(t *testing.T) {
	m, st, _, _, _ := machinesFixture(t)
	ctx := context.Background()
	if _, err := m.Adopt(ctx, "x"); !errors.Is(err, ErrNoPersistentDisk) {
		t.Fatalf("sem Persist: %v", err)
	}
	p := newFakePersist(st)
	m.Persist = p
	p.onDisk["moved"] = domain.Machine{Name: "moved", Stack: "python"}
	got, err := m.Adopt(ctx, "moved")
	if err != nil || got.Name != "moved" || got.State != StateStopped {
		t.Fatalf("adotar: %+v %v", got, err)
	}
	n := len(p.ensured)
	if _, err := m.Adopt(ctx, "moved"); err != nil || len(p.ensured) != n {
		t.Fatalf("já aqui: %v (%d ensures a mais)", err, len(p.ensured)-n)
	}
	if _, err := m.Adopt(ctx, "ghost"); errCode(err) != domain.CodeNotFound || len(p.destroyed) != 1 || p.destroyed[0] != "ghost" {
		t.Fatalf("inexistente: %v destruídos %v", err, p.destroyed)
	}
	// Disco existente sem registro: o fake "adota" mas não põe registro; o Adopt solta de novo.
	p2 := &noRecordPersist{fakePersist: newFakePersist(st)}
	m.Persist = p2
	if _, err := m.Adopt(ctx, "half"); errCode(err) != domain.CodeNotFound || len(p2.released) != 1 {
		t.Fatalf("sem registro: %v soltos %v", err, p2.released)
	}
	p2.ensureErr = errors.New("busy")
	if _, err := m.Adopt(ctx, "half"); err == nil || errCode(err) == domain.CodeNotFound {
		t.Fatalf("erro do disco: %v", err)
	}
	m.Persist = p
	m.Drain(ctx)
	if _, err := m.Adopt(ctx, "other"); errCode(err) != domain.CodeInvalidState {
		t.Fatalf("em preempção: %v", err)
	}
}

// noRecordPersist é o disco que existe mas sem registro de máquina dentro (create interrompido em outro host).
type noRecordPersist struct{ *fakePersist }

// Ensure monta como existente (created=false) sem pôr registro no store.
func (f *noRecordPersist) Ensure(_ context.Context, name, dir string, _ int) (bool, error) {
	f.mu.Lock()
	defer f.mu.Unlock()
	f.ensured = append(f.ensured, name)
	if f.ensureErr != nil {
		return false, f.ensureErr
	}
	f.mounted[dir] = true
	return false, nil
}
