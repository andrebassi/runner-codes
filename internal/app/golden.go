package app

import (
	"context"
	"errors"
	"fmt"
	"slices"
	"time"

	"runner.codes/internal/domain"
)

// GoldenDisks é o disco próprio que sabe nascer de um snapshot e virar um: o caminho do snapshot dourado. Uma máquina
// pronta (init feito, área de trabalho de pé) é suspensa uma vez e o disco dela vira um snapshot do GCE; cada máquina
// nova com o mesmo formato nasce de um disco criado desse snapshot (o GCE o entrega sob demanda, em segundos) e o
// Start a RESTAURA — sem boot, sem init, sem instalar nada. Medido em 25/10/2026: a máquina de desktop levava 2 min 37 s
// do agendamento ao pronto, quase tudo apt-get e npm do init num worker de 1 vCPU.
type GoldenDisks interface {
	PersistentDisks
	// EnsureFrom é o Ensure de um disco novo criado do snapshot (nunca formatado); disco que já existe é adotado.
	EnsureFrom(ctx context.Context, name, dir string, sizeGiB int, snapshot string) (created bool, err error)
	// Snapshot solta o disco da máquina e tira dele o snapshot com o nome dado.
	Snapshot(ctx context.Context, name, dir, snapshot string) error
	// SnapshotExists diz se o snapshot existe e está pronto para criar disco.
	SnapshotExists(ctx context.Context, snapshot string) (bool, error)
}

// ErrNoGolden é a recusa de criar do dourado: host sem o recurso, snapshot que não existe (ainda), ou dourado de outro
// formato. Quem pede cria do jeito normal no lugar — o dourado é só o atalho.
var ErrNoGolden = domain.MachineError{Code: domain.CodeNotFound, Msg: "golden snapshot not available for this machine"}

// golden devolve o disco que sabe trabalhar com snapshot, ou ErrNoGolden.
func (m *Machines) golden() (GoldenDisks, error) {
	g, ok := m.Persist.(GoldenDisks) // Persist nil também cai aqui: a asserção sobre interface vazia é !ok
	if !ok {
		return nil, ErrNoGolden
	}
	return g, nil
}

// CreateFromGolden cria a máquina pedida a partir do snapshot dourado: o disco nasce do snapshot (com a máquina
// suspensa dentro) e o registro é o PEDIDO — porta e título da tela, política de rede, credenciais com a origem do
// segredo —, com uma única coisa trazida do dourado: os placeholders das credenciais, que estão congelados na memória
// do guest (regerá-los faria o guest mandar placeholders que o host não reconhece). O segredo de verdade continua só no
// host, trocado por máquina. O Start seguinte restaura (o snapshot e a marca da imagem base vieram no disco).
//
// Dourado de outro formato (imagem, CPU, memória, GPU, tamanho do disco, nomes das credenciais) embrulha ErrNoGolden
// (errors.Is), com o motivo e com o disco clonado apagado: restaurar uma VM num formato diferente é o que a libkrun recusa no meio do boot.
func (m *Machines) CreateFromGolden(ctx context.Context, mc domain.Machine, snapshot string) (out domain.Machine, err error) {
	g, err := m.golden()
	if err != nil {
		return mc, err
	}
	if m.isDraining() {
		return mc, domain.MachineError{Code: domain.CodeInvalidState, Msg: "host is being preempted; create the machine on another host"}
	}
	if mc.Image != "" {
		if mc.Stack != "" {
			return mc, domain.ValidationError{Err: domain.MachineError{Code: domain.CodeConfig, Msg: "set stack or image, not both"}}
		}
		mc.Stack = ImageStackName(mc.Image)
	}
	mc = mc.WithDefaults()
	if err := mc.Validate(); err != nil {
		return mc, err
	}
	if mc.Network.Name != "" || len(mc.Mounts) > 0 {
		// Rede nomeada (IP por máquina) e volume do host não viajam num snapshot: ficam no caminho normal.
		return mc, ErrNoGolden
	}
	if _, err := m.Store.Load(mc.Name); err == nil {
		return mc, domain.ErrMachineExists(mc.Name)
	}
	ok, err := g.SnapshotExists(ctx, snapshot)
	if err != nil {
		return mc, fmt.Errorf("golden snapshot %s: %w", snapshot, err)
	}
	if !ok {
		return mc, ErrNoGolden
	}
	dir := m.Store.Dir(mc.Name)
	created, err := g.EnsureFrom(ctx, mc.Name, dir, persistentGiB(mc.DiskGiB), snapshot)
	if err != nil {
		if created {
			g.Destroy(context.WithoutCancel(ctx), mc.Name, dir)
		}
		return mc, fmt.Errorf("persistent disk from %s: %w", snapshot, err)
	}
	if !created {
		// O disco desta máquina já existia (ela mora num disco solto por outro host): é adoção, não dourado — quem
		// chamou cria pelo caminho normal, que adota.
		g.Release(context.WithoutCancel(ctx), mc.Name, dir)
		return mc, ErrNoGolden
	}
	defer func() {
		if err != nil {
			g.Destroy(context.WithoutCancel(ctx), mc.Name, dir)
		}
	}()
	// O registro que veio no disco é o do dourado (com o nome dele): ele só serve para conferir o formato e dar os
	// placeholders.
	gold, err := m.Store.Load(mc.Name)
	if err != nil {
		return mc, fmt.Errorf("%w: %s has no machine: %v", ErrNoGolden, snapshot, err)
	}
	if reason := goldenMismatch(gold, mc); reason != "" {
		return mc, fmt.Errorf("%w: %s does not fit: %s", ErrNoGolden, snapshot, reason)
	}
	if !m.hasSnapshot(mc.Name) {
		return mc, fmt.Errorf("%w: %s holds no suspended machine", ErrNoGolden, snapshot)
	}
	for i, c := range mc.Network.Credentials {
		for _, gc := range gold.Network.Credentials {
			if gc.Name == c.Name {
				mc.Network.Credentials[i].Placeholder = gc.Placeholder
			}
		}
	}
	mc.CreatedAt = time.Now().UTC().Truncate(time.Second)
	if err := m.Store.Save(mc); err != nil {
		return mc, err
	}
	// O pid gravado é o do vmm do host onde o dourado foi feito: aqui ele é outro processo (ou nenhum).
	if err := m.Store.SetPID(mc.Name, 0); err != nil {
		return mc, err
	}
	if err := m.ensureImage(ctx, mc); err != nil {
		return mc, err
	}
	return mc, nil
}

// goldenMismatch diz por que o dourado não serve para a máquina pedida ("" = serve): o formato que a VM restaurada
// precisa ter igual ao de quando foi suspensa, e as credenciais (pelo nome) que o guest conhece.
func goldenMismatch(gold, want domain.Machine) string {
	switch {
	case gold.Stack != want.Stack || gold.Image != want.Image:
		return fmt.Sprintf("image %q, want %q", gold.Stack, want.Stack)
	case gold.CPUs != want.CPUs || gold.MemoryMiB != want.MemoryMiB:
		return fmt.Sprintf("%d CPUs/%d MiB, want %d/%d", gold.CPUs, gold.MemoryMiB, want.CPUs, want.MemoryMiB)
	case gold.GPU != want.GPU:
		return "GPU differs"
	case gold.DiskGiB != want.DiskGiB:
		return fmt.Sprintf("disk %d GiB, want %d", gold.DiskGiB, want.DiskGiB)
	}
	names := func(m domain.Machine) []string {
		var out []string
		for _, c := range m.Network.Credentials {
			out = append(out, c.Name)
		}
		slices.Sort(out)
		return out
	}
	if !slices.Equal(names(gold), names(want)) {
		return fmt.Sprintf("credentials %v, want %v", names(gold), names(want))
	}
	return ""
}

// PublishGolden faz da máquina suspensa o snapshot dourado: o disco dela vira o snapshot dado e depois é apagado (a
// máquina de molde não fica, o snapshot é o que vale). Máquina ligada, ou desligada sem snapshot da memória, é
// recusada: o dourado tem de nascer restaurável.
func (m *Machines) PublishGolden(ctx context.Context, name, snapshot string) error {
	g, err := m.golden()
	if err != nil {
		return err
	}
	st, err := m.Status(name)
	if err != nil {
		return err
	}
	if st.State != StateStopped || !m.hasSnapshot(name) {
		return domain.ErrMachineState(name, "is not suspended; suspend it first")
	}
	dir := m.Store.Dir(name)
	if !g.Mounted(dir) {
		return ErrNoPersistentDisk
	}
	if err := g.Snapshot(ctx, name, dir, snapshot); err != nil {
		return err
	}
	if err := g.Destroy(context.WithoutCancel(ctx), name, dir); err != nil && !errors.Is(err, context.Canceled) {
		return fmt.Errorf("golden %s published; the work disk was not deleted: %w", snapshot, err)
	}
	return nil
}
