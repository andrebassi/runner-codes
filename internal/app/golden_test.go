package app

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"testing"

	"runner.codes/internal/domain"
)

// fakeGolden é o disco próprio que sabe de snapshot: gold é o que cada snapshot guarda (a máquina de molde, com o nome
// dela, suspensa ou não), e anota cada snapshot tirado.
type fakeGolden struct {
	*fakePersist
	gold     map[string]goldenDisk
	taken    []string
	fromErr  error
	fromNew  bool // EnsureFrom devolve created junto com fromErr
	snapErr  error
	existErr error
	m        *Machines
}

// goldenDisk é o conteúdo de um snapshot: o registro e se a memória suspensa veio junto.
type goldenDisk struct {
	rec       domain.Machine
	suspended bool
}

// EnsureFrom monta um disco novo com o conteúdo do snapshot (ou adota o que já existe, como o Ensure).
func (f *fakeGolden) EnsureFrom(ctx context.Context, name, dir string, size int, snapshot string) (bool, error) {
	if f.fromErr != nil {
		return f.fromNew, f.fromErr
	}
	f.mu.Lock()
	_, onDisk := f.onDisk[name]
	f.mu.Unlock()
	if onDisk {
		return f.Ensure(ctx, name, dir, size)
	}
	g := f.gold[snapshot]
	f.mu.Lock()
	f.mounted[dir] = true
	f.ensured = append(f.ensured, name+"<-"+snapshot)
	f.mu.Unlock()
	f.st.mu.Lock()
	f.st.ms[name] = g.rec
	f.st.mu.Unlock()
	if g.suspended {
		os.MkdirAll(f.m.snapshotPath(name), 0o700)
		os.WriteFile(filepath.Join(f.m.snapshotPath(name), snapshotManifest), []byte("{}"), 0o600)
	}
	return true, nil
}

// Snapshot anota (ou falha) e solta o disco, como o de verdade.
func (f *fakeGolden) Snapshot(ctx context.Context, name, dir, snapshot string) error {
	if f.snapErr != nil {
		return f.snapErr
	}
	f.taken = append(f.taken, name+"->"+snapshot)
	return f.Release(ctx, name, dir)
}

// SnapshotExists diz se o snapshot foi plantado.
func (f *fakeGolden) SnapshotExists(_ context.Context, snapshot string) (bool, error) {
	_, ok := f.gold[snapshot]
	return ok, f.existErr
}

// goldenFixture monta as máquinas com o disco que sabe de snapshot e um dourado "gold-v1": a máquina de molde "mold",
// llama (com GPU, que a tela exige), 2 CPUs, suspensa, com a credencial "claude" e o placeholder dela.
func goldenFixture(t *testing.T) (*Machines, *memStore, *fakeGolden) {
	t.Helper()
	m, st, _, _, _ := machinesFixture(t)
	mold := domain.Machine{Name: "mold", Stack: "llama", GPU: true, CPUs: 2, Network: domain.Network{Credentials: []domain.Credential{
		{Name: "claude", Env: "ANTHROPIC_API_KEY", Hosts: []string{"api.anthropic.com"}, Placeholder: "rc-ph-mold"}}}}.WithDefaults()
	g := &fakeGolden{fakePersist: newFakePersist(st), gold: map[string]goldenDisk{"gold-v1": {rec: mold, suspended: true}}, m: m}
	m.Persist = g
	return m, st, g
}

// want é a máquina pedida que cabe no dourado de goldenFixture, com a porta da tela e a origem do segredo próprias.
func want(name string) domain.Machine {
	return domain.Machine{Name: name, Stack: "llama", GPU: true, CPUs: 2, Display: &domain.Display{Port: 6107},
		Network: domain.Network{Credentials: []domain.Credential{{Name: "claude", Env: "ANTHROPIC_API_KEY", Hosts: []string{"api.anthropic.com"}, SourceEnv: "TOKEN_6107"}}}}
}

// TestCreateFromGolden: o disco nasce do snapshot e o registro é o PEDIDO (nome, porta da tela, origem do segredo),
// com o placeholder do dourado — o que está congelado na memória do guest; o pid do host do molde é limpo; e o Start
// seguinte tem o snapshot para restaurar.
func TestCreateFromGolden(t *testing.T) {
	m, st, g := goldenFixture(t)
	st.pids["trial1"] = 4242
	out, err := m.CreateFromGolden(context.Background(), want("trial1"), "gold-v1")
	if err != nil {
		t.Fatal(err)
	}
	rec, _ := st.Load("trial1")
	if rec.Name != "trial1" || rec.Display == nil || rec.Display.Port != 6107 || rec.CreatedAt.IsZero() || out.Name != "trial1" {
		t.Fatalf("registro: %+v", rec)
	}
	c := rec.Network.Credentials[0]
	if c.Placeholder != "rc-ph-mold" || c.SourceEnv != "TOKEN_6107" {
		t.Fatalf("credencial: %+v", c)
	}
	if st.PID("trial1") != 0 || !m.hasSnapshot("trial1") || g.ensured[0] != "trial1<-gold-v1" || len(g.destroyed) != 0 {
		t.Fatalf("pid=%d snapshot=%v ensured=%v destroyed=%v", st.PID("trial1"), m.hasSnapshot("trial1"), g.ensured, g.destroyed)
	}
}

// TestCreateFromGoldenRefusals: cada caso em que o dourado não serve volta ErrNoGolden (ou o erro do pedido), e o disco
// clonado nunca fica: sem o recurso, snapshot ausente, formato diferente, molde sem a memória suspensa, credencial de
// outro nome, rede nomeada, disco que já existia (adoção, não dourado), host em preempção, nome repetido.
func TestCreateFromGoldenRefusals(t *testing.T) {
	ctx := context.Background()
	plain, _, _, _, _ := machinesFixture(t)
	if _, err := plain.CreateFromGolden(ctx, want("a"), "gold-v1"); !errors.Is(err, ErrNoGolden) {
		t.Fatalf("sem disco próprio: %v", err)
	}
	m, st, g := goldenFixture(t)
	if _, err := m.CreateFromGolden(ctx, want("a"), "gold-v9"); !errors.Is(err, ErrNoGolden) || len(g.ensured) != 0 {
		t.Fatalf("snapshot ausente: %v %v", err, g.ensured)
	}
	big := want("b")
	big.CPUs = 4
	if _, err := m.CreateFromGolden(ctx, big, "gold-v1"); errCode(err) != domain.CodeNotFound || len(g.destroyed) != 1 {
		t.Fatalf("formato: %v destruídos=%v", err, g.destroyed)
	}
	if _, err := st.Load("b"); err == nil {
		t.Fatal("o disco do formato errado ficou")
	}
	g.gold["gold-cold"] = goldenDisk{rec: g.gold["gold-v1"].rec}
	if _, err := m.CreateFromGolden(ctx, want("c"), "gold-cold"); errCode(err) != domain.CodeNotFound || len(g.destroyed) != 2 {
		t.Fatalf("sem memória suspensa: %v", err)
	}
	other := want("d")
	other.Network.Credentials[0].Name = "github"
	if _, err := m.CreateFromGolden(ctx, other, "gold-v1"); errCode(err) != domain.CodeNotFound || len(g.destroyed) != 3 {
		t.Fatalf("credencial de outro nome: %v", err)
	}
	named := want("e")
	named.Network.Name = "lan"
	if _, err := m.CreateFromGolden(ctx, named, "gold-v1"); !errors.Is(err, ErrNoGolden) {
		t.Fatalf("rede nomeada: %v", err)
	}
	g.onDisk["f"] = want("f")
	if _, err := m.CreateFromGolden(ctx, want("f"), "gold-v1"); !errors.Is(err, ErrNoGolden) || g.released[len(g.released)-1] != "f" {
		t.Fatalf("disco que já existia: %v soltos=%v", err, g.released)
	}
	g.fromErr, g.fromNew = errors.New("attach"), true
	if _, err := m.CreateFromGolden(ctx, want("g"), "gold-v1"); err == nil || len(g.destroyed) != 4 {
		t.Fatalf("falha no disco novo: %v destruídos=%v", err, g.destroyed)
	}
	g.fromErr = nil
	g.existErr = errors.New("api fora")
	if _, err := m.CreateFromGolden(ctx, want("h"), "gold-v1"); err == nil || errors.Is(err, ErrNoGolden) {
		t.Fatalf("erro da API vira ErrNoGolden: %v", err)
	}
	g.existErr = nil
	if _, err := m.CreateFromGolden(ctx, want("ok"), "gold-v1"); err != nil {
		t.Fatal(err)
	}
	if _, err := m.CreateFromGolden(ctx, want("ok"), "gold-v1"); errCode(err) != domain.CodeConflict {
		t.Fatalf("repetida: %v", err)
	}
	both := want("i")
	both.Image = "node:22"
	if _, err := m.CreateFromGolden(ctx, both, "gold-v1"); errCode(err) != domain.CodeConfig {
		t.Fatalf("stack e imagem: %v", err)
	}
	bad := want("Bad Name")
	if _, err := m.CreateFromGolden(ctx, bad, "gold-v1"); err == nil {
		t.Fatal("nome inválido aceito")
	}
	m.drain.active = true
	if _, err := m.CreateFromGolden(ctx, want("j"), "gold-v1"); errCode(err) != domain.CodeInvalidState {
		t.Fatalf("em preempção: %v", err)
	}
}

// TestPublishGolden: só a máquina suspensa vira dourado (ligada, ou desligada sem a memória, é recusada); o snapshot
// sai do disco dela, e o disco de trabalho é apagado depois; erro do snapshot sobe sem apagar nada.
func TestPublishGolden(t *testing.T) {
	ctx := context.Background()
	plain, _, _, _, _ := machinesFixture(t)
	if err := plain.PublishGolden(ctx, "x", "gold-v2"); !errors.Is(err, ErrNoGolden) {
		t.Fatalf("sem disco próprio: %v", err)
	}
	m, _, g := goldenFixture(t)
	if err := m.PublishGolden(ctx, "nada", "gold-v2"); errCode(err) != domain.CodeNotFound {
		t.Fatalf("desconhecida: %v", err)
	}
	if _, err := m.Create(ctx, domain.Machine{Name: "mold2", Stack: "python"}); err != nil {
		t.Fatal(err)
	}
	if err := m.PublishGolden(ctx, "mold2", "gold-v2"); errCode(err) != domain.CodeInvalidState {
		t.Fatalf("sem memória suspensa: %v", err)
	}
	os.MkdirAll(m.snapshotPath("mold2"), 0o700)
	os.WriteFile(filepath.Join(m.snapshotPath("mold2"), snapshotManifest), []byte("{}"), 0o600)
	g.snapErr = errors.New("quota")
	if err := m.PublishGolden(ctx, "mold2", "gold-v2"); err == nil || len(g.destroyed) != 0 {
		t.Fatalf("erro do snapshot: %v destruídos=%v", err, g.destroyed)
	}
	g.snapErr = nil
	if err := m.PublishGolden(ctx, "mold2", "gold-v2"); err != nil {
		t.Fatal(err)
	}
	if g.taken[0] != "mold2->gold-v2" || len(g.destroyed) != 1 || g.destroyed[0] != "mold2" {
		t.Fatalf("publicação: tirados=%v destruídos=%v", g.taken, g.destroyed)
	}
	if _, err := m.Start(ctx, "mold3"); err == nil {
		t.Fatal("máquina inexistente ligou")
	}
	if _, err := m.Create(ctx, domain.Machine{Name: "live", Stack: "python"}); err != nil {
		t.Fatal(err)
	}
	if _, err := m.Start(ctx, "live"); err != nil {
		t.Fatal(err)
	}
	if err := m.PublishGolden(ctx, "live", "gold-v3"); errCode(err) != domain.CodeInvalidState {
		t.Fatalf("ligada: %v", err)
	}
}

// TestPublishGoldenNotMounted: máquina suspensa que não mora num disco próprio (de antes dos discos) não tem o que
// virar snapshot.
func TestPublishGoldenNotMounted(t *testing.T) {
	m, st, _ := goldenFixture(t)
	st.ms["old"] = domain.Machine{Name: "old", Stack: "python"}.WithDefaults()
	os.MkdirAll(m.snapshotPath("old"), 0o700)
	os.WriteFile(filepath.Join(m.snapshotPath("old"), snapshotManifest), []byte("{}"), 0o600)
	if err := m.PublishGolden(context.Background(), "old", "gold-v4"); !errors.Is(err, ErrNoPersistentDisk) {
		t.Fatalf("sem disco próprio montado: %v", err)
	}
}

// TestGoldenMismatch: cada diferença de formato tem o motivo dito; igual é "".
func TestGoldenMismatch(t *testing.T) {
	base := domain.Machine{Stack: "python", CPUs: 2, MemoryMiB: 512, DiskGiB: 4}
	if r := goldenMismatch(base, base); r != "" {
		t.Fatalf("igual: %q", r)
	}
	for _, tc := range []func(*domain.Machine){
		func(m *domain.Machine) { m.Stack = "ruby" },
		func(m *domain.Machine) { m.Image = "node:22" },
		func(m *domain.Machine) { m.MemoryMiB = 1024 },
		func(m *domain.Machine) { m.GPU = true },
		func(m *domain.Machine) { m.DiskGiB = 8 },
		func(m *domain.Machine) { m.Network.Credentials = []domain.Credential{{Name: "x"}} },
	} {
		w := base
		tc(&w)
		if goldenMismatch(base, w) == "" {
			t.Errorf("diferença não vista: %+v", w)
		}
	}
}
