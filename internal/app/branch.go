package app

import (
	"context"
	"os"
	"path/filepath"
	"time"

	"runner.code/internal/domain"
	"runner.code/internal/protocol"
)

// StatePaused é a máquina ligada com as vCPUs congeladas (machine pause).
const StatePaused = "paused"

// machinePaused é o marcador de pausa no diretório da máquina: o processo congelado não diz
// que está congelado, então o estado mora aqui.
const machinePaused = "paused"

// isPaused diz se a máquina foi pausada (e não retomada).
func (m *Machines) isPaused(name string) bool {
	_, err := os.Stat(filepath.Join(m.Store.Dir(name), machinePaused))
	return err == nil
}

// Pause congela uma máquina ligada: processos, memória e conexões ficam onde estão até o
// Resume. A rede para junto (o gateway vive no mesmo processo).
func (m *Machines) Pause(name string) error {
	st, err := m.Status(name)
	if err != nil {
		return err
	}
	switch st.State {
	case StatePaused:
		return nil
	case StateStopped:
		return domain.ErrMachineState(name, "is not running")
	}
	if err := m.Procs.Pause(st.PID); err != nil {
		return err
	}
	return os.WriteFile(filepath.Join(m.Store.Dir(name), machinePaused), nil, 0o600)
}

// Resume continua uma máquina pausada; máquina já rodando não é erro.
func (m *Machines) Resume(name string) error {
	st, err := m.Status(name)
	if err != nil {
		return err
	}
	if st.State != StatePaused {
		if st.State == StateStopped {
			return domain.ErrMachineState(name, "is not running")
		}
		return nil
	}
	if err := m.Procs.Resume(st.PID); err != nil {
		return err
	}
	return os.Remove(filepath.Join(m.Store.Dir(name), machinePaused))
}

// Branch cria a máquina `to` como cópia copy-on-write do disco de `from` (clonefile no APFS,
// FICLONE no Linux): tudo o que foi instalado e gravado vem junto, e o init não roda de novo.
// Com a origem ligada, o guest grava o cache no disco e a VM fica congelada só durante o
// clone — o disco sai consistente como depois de uma queda de energia. Sem snapshot de
// memória na libkrun upstream, é cópia do disco, não da execução.
func (m *Machines) Branch(ctx context.Context, from, to string) (domain.Machine, error) {
	st, err := m.Status(from)
	if err != nil {
		return domain.Machine{}, err
	}
	child := st.Machine
	child.Name, child.CreatedAt = to, time.Now().UTC().Truncate(time.Second)
	// A cópia staged vai junto no disco, mas a filha não devolve nada ao host:
	// vira um volume de disco comum no mesmo caminho, que é onde a cópia já mora.
	child.Mounts = append([]domain.Mount(nil), st.Mounts...)
	for i, mt := range child.Mounts {
		if mt.Staged {
			child.Mounts[i] = domain.Mount{Guest: mt.Guest}
		}
	}
	if err := child.Validate(); err != nil {
		return child, err
	}
	if st.State == StatePaused {
		// Congelada, a VM não grava o cache no disco: o que ela escreveu por último ficaria fora
		// do clone (medido: o arquivo recém-gravado não estava na filha). Retoma só para o sync
		// e volta a pausar no fim, depois do clone.
		if err := m.Procs.Resume(st.PID); err != nil {
			return child, err
		}
		defer m.Procs.Pause(st.PID)
		st.State = StateRunning
	}
	if st.State == StateRunning {
		sock := filepath.Join(m.Store.Dir(from), machineSocket)
		if _, err := m.Guest.Run(ctx, sock, protocol.Job{TraceID: "branch-sync", Op: protocol.OpExec, Argv: []string{"sync"}}); err != nil {
			return child, err
		}
		if err := m.Procs.Pause(st.PID); err != nil {
			return child, err
		}
		defer m.Procs.Resume(st.PID)
	}
	if err := m.Store.Create(child, filepath.Join(m.Store.Dir(from), "disk.ext4")); err != nil {
		return child, err
	}
	// A filha herda placeholders (no registro) e a CA: o disco clonado já confia nela.
	if err := m.copyCredentialCA(from, to); err != nil {
		m.Store.Delete(to)
		return child, err
	}
	return child, nil
}
