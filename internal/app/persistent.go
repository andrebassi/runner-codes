package app

import (
	"context"
	"errors"
	"fmt"
	"sync"
	"time"

	"runner.codes/internal/domain"
)

// PersistentDisks guarda o diretório de cada máquina num disco de rede só dela — o PersistentVolume do Kubernetes. A
// máquina deixa de morar no disco do host: o host pode sumir (spot preemptado, servidor apagado) e a máquina renasce em
// outro host com o mesmo disco, adotando-o em vez de copiar. O gcedisk.Adapter implementa com Persistent Disk do GCE.
//
// Um disco tem UM dono por vez (gravável por um host só): Ensure nunca toma o disco de um host vivo.
type PersistentDisks interface {
	// Ensure deixa o disco da máquina montado em dir neste host: cria e formata se ele não existe (created=true,
	// diretório vazio), ou adota o que existe (solto, ou preso a um host que já está morrendo). Já montado aqui não é
	// erro.
	Ensure(ctx context.Context, name, dir string, sizeGiB int) (created bool, err error)
	// Mounted diz se dir é o ponto de montagem de um disco (máquina antiga, de antes deles, mora no disco do host).
	Mounted(dir string) bool
	// Release desmonta e solta o disco deste host, deixando-o livre para outro adotar. A máquina já está desligada.
	Release(ctx context.Context, name, dir string) error
	// Destroy solta e apaga o disco (o delete da máquina): ele some de vez.
	Destroy(ctx context.Context, name, dir string) error
}

// persistentGiB é o tamanho do disco de uma máquina: o disco gravável dela (DiskGiB, esparso dentro dele) mais 1 GiB
// para o registro, os logs e a folga do ext4. Persistent Disk começa em 10 GB: menos que isso o GCE recusa.
func persistentGiB(diskGiB int) int {
	return max(diskGiB+1, 10)
}

// ErrNoPersistentDisk é a recusa de soltar uma máquina que não mora num disco próprio (o host não tem o recurso, ou
// ela é de antes dele): quem orquestra usa a cópia (export/import) no lugar.
var ErrNoPersistentDisk = domain.MachineError{Code: domain.CodeConfig, Msg: "machine has no persistent disk on this host"}

// drainStopTimeout é o prazo do desligamento de cada máquina no aviso de preempção. O spot dá ~30 s até o ACPI
// desligar o host: 8 s para o guest gravar o disco e sair deixam ~20 s para desmontar e soltar os discos (a API do
// GCE leva 5–10 s por detach, em paralelo). Mais que isso e o host morre com disco preso, e o próximo dono espera o
// GCE soltar sozinho.
const drainStopTimeout = 8 * time.Second

// drainSuspendTimeout é o prazo de suspender cada máquina ligada no aviso de preempção. 15 s gravam a RAM usada de um
// desktop com folga, e deixam o resto dos ~30 s para o desligamento de reserva e para soltar o disco. Variável só
// para o teste encurtar.
var drainSuspendTimeout = 15 * time.Second

// drainState é o estado da preempção deste host, lido pelo GET /api/v1/host.
type drainState struct {
	mu      sync.Mutex
	active  bool
	drained []string
}

// HasPersistentDisks diz se as máquinas novas deste host nascem num disco próprio.
func (m *Machines) HasPersistentDisks() bool { return m.Persist != nil }

// Preemption diz se o host está em preempção e quais máquinas estavam ligadas no aviso.
func (m *Machines) Preemption() (bool, []string) {
	m.drain.mu.Lock()
	defer m.drain.mu.Unlock()
	return m.drain.active, append([]string(nil), m.drain.drained...)
}

// Release desliga a máquina (se ligada) e solta o disco dela, para outro host adotar — a mudança de host sem copiar.
// Sem disco próprio é ErrNoPersistentDisk.
func (m *Machines) Release(ctx context.Context, name string) error {
	if _, err := m.Status(name); err != nil {
		return err
	}
	dir := m.Store.Dir(name)
	if m.Persist == nil || !m.Persist.Mounted(dir) {
		return ErrNoPersistentDisk
	}
	if err := m.Stop(ctx, name, -1); err != nil {
		return fmt.Errorf("stop before release: %w", err)
	}
	return m.Persist.Release(ctx, name, dir)
}

// Adopt traz para este host a máquina cujo disco próprio existe solto (ou preso a um host que está morrendo), sem
// ligar: é o fim da mudança de host — o desktop movido desligado aparece no destino na hora, em vez de só na próxima
// abertura. Disco que não existe é NOT_FOUND, e o disco vazio que o Ensure criaria é apagado: adotar nunca inventa
// máquina.
func (m *Machines) Adopt(ctx context.Context, name string) (MachineStatus, error) {
	if st, err := m.Status(name); err == nil {
		return st, nil // já está aqui
	}
	if m.Persist == nil {
		return MachineStatus{}, ErrNoPersistentDisk
	}
	if m.isDraining() {
		return MachineStatus{}, domain.MachineError{Code: domain.CodeInvalidState, Msg: "host is being preempted; adopt the machine on another host"}
	}
	dir := m.Store.Dir(name)
	created, err := m.Persist.Ensure(ctx, name, dir, persistentGiB(0))
	if created {
		// Adotar nunca cria: disco que nasceu aqui é de máquina que não existe (ou de um Ensure que falhou depois de
		// criar). Apaga, com ou sem erro, para não ficar cobrando.
		m.Persist.Destroy(context.WithoutCancel(ctx), name, dir)
		if err != nil {
			return MachineStatus{}, fmt.Errorf("persistent disk: %w", err)
		}
		return MachineStatus{}, domain.ErrMachineNotFound(name)
	}
	if err != nil {
		return MachineStatus{}, fmt.Errorf("persistent disk: %w", err)
	}
	st, err := m.Status(name)
	if err != nil {
		// Disco sem registro (create interrompido no meio em outro host): solta de novo, não fica montado à toa.
		m.Persist.Release(context.WithoutCancel(ctx), name, dir)
		return MachineStatus{}, err
	}
	// A imagem é do host: o host novo pode nunca ter rodado a dessa máquina. Se ela não fica pronta aqui, o disco é
	// solto de novo — preso a um host onde a máquina não liga, ele só atrapalharia a próxima tentativa em outro.
	if err := m.ensureImage(ctx, st.Machine); err != nil {
		m.Persist.Release(context.WithoutCancel(ctx), name, dir)
		return MachineStatus{}, err
	}
	return st, nil
}

// Drain é a resposta ao aviso de preempção: marca o host (nenhuma máquina nova deve vir), desliga as ligadas e solta
// todos os discos, tudo em paralelo para caber nos ~30 s. Devolve as que estavam ligadas — quem orquestra religa essas
// em outro host. Chamar de novo não refaz nada: o aviso é um só.
func (m *Machines) Drain(ctx context.Context) ([]string, error) {
	m.drain.mu.Lock()
	if m.drain.active {
		out := append([]string(nil), m.drain.drained...)
		m.drain.mu.Unlock()
		return out, nil
	}
	m.drain.active = true
	m.drain.mu.Unlock()

	all, err := m.List()
	if err != nil {
		return nil, err
	}
	var running []string
	for _, st := range all {
		if st.State == StateRunning || st.State == StatePaused {
			running = append(running, st.Name)
		}
	}
	// Publica as ligadas ANTES de desligar: o orquestrador pode ler o host no meio da drenagem e já religar em outro.
	m.drain.mu.Lock()
	m.drain.drained = running
	m.drain.mu.Unlock()

	var wg sync.WaitGroup
	errs := make([]error, len(all))
	for i, st := range all {
		wg.Add(1)
		go func(i int, name string, state string) {
			defer wg.Done()
			// Ligada e o host sabe suspender: suspende — o disco sai com a máquina congelada dentro, e o host novo a
			// devolve no mesmo ponto. Suspend que falha cai no desligamento: imediato se a VM pode ter ficado congelada
			// no meio do snapshot (esperar o guest de uma VM parada só gastaria o prazo), normal se nem começou.
			stopTimeout := drainStopTimeout
			if state == StateRunning && m.CanSuspend {
				err := m.Suspend(ctx, name, drainSuspendTimeout)
				if err == nil {
					stopTimeout = -1
				} else {
					errs[i] = fmt.Errorf("%s: suspend: %w", name, err)
					if !errors.Is(err, ErrSuspendUnsupported) {
						stopTimeout = 0
					}
					m.logf(name, "drain: suspend failed (%v); stopping", err)
				}
			} else if state == StateRunning {
				m.logf(name, "drain: this host cannot suspend; stopping")
			}
			// Desligar (o guest grava o disco), depois soltar. Erro no stop não impede o release: o vmm é morto no fim
			// do prazo de qualquer jeito, e um disco preso aqui atrasa o failover inteiro.
			if stopTimeout >= 0 {
				if err := m.Stop(ctx, name, stopTimeout); err != nil {
					errs[i] = errors.Join(errs[i], fmt.Errorf("%s: stop: %w", name, err))
				}
			}
			if m.Persist == nil || !m.Persist.Mounted(m.Store.Dir(name)) {
				return
			}
			// O rastro vai ANTES de soltar: depois do release o diretório já não é o disco da máquina.
			m.logf(name, "drain: releasing the disk")
			if err := m.Persist.Release(ctx, name, m.Store.Dir(name)); err != nil {
				errs[i] = errors.Join(errs[i], fmt.Errorf("%s: release: %w", name, err))
			}
		}(i, st.Name, st.State)
	}
	wg.Wait()
	return running, errors.Join(errs...)
}
