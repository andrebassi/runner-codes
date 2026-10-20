package app

import (
	"context"
	"fmt"
	"path/filepath"

	"runner.code/internal/domain"
	"runner.code/internal/protocol"
)

// stagedHidden é onde o diretório do host de um volume staged fica montado no guest, fora do
// caminho do workload: só o seed e o sync o usam.
func stagedHidden(i int) string { return "/run/rc-staged/" + mountTag(i) }

// stageScript monta o diretório do host escondido, semeia a cópia no disco da máquina na
// primeira vez (a marca ao lado diz que já semeou: um start seguinte não pisa no que o
// workload mudou) e liga a cópia ao caminho do guest. $1 escondido, $2 cópia, $3 guest, $4 tag.
const stageScript = `set -e
mkdir -p "$1" "$2" "$3"
mount -t virtiofs -o rw "$4" "$1"
if [ ! -e "$2.rc-seeded" ]; then cp -a "$1/." "$2/"; touch "$2.rc-seeded"; fi
mount --bind "$2" "$3"`

// syncScript espelha a cópia ($1) no diretório do host ($2): apaga do host o que não está na
// cópia e copia o resto por cima (edição feita no host depois do
// primeiro start é sobrescrita).
const syncScript = `set -e
cd "$2"
find . -mindepth 1 -depth | while IFS= read -r p; do
  [ -e "$1/$p" ] || [ -h "$1/$p" ] || rm -rf "./$p"
done
cp -a "$1/." "$2/"`

// stage prepara o volume staged i a cada start.
func (m *Machines) stage(ctx context.Context, name, sock string, i int, mt domain.Mount) error {
	res, err := m.Guest.Run(ctx, sock, protocol.Job{TraceID: "stage-" + mountTag(i), Op: protocol.OpExec,
		Argv: []string{"sh", "-c", stageScript, "stage", stagedHidden(i), mt.DiskPath(), mt.Guest, mountTag(i)}})
	if err == nil && res.ExitCode != 0 {
		err = fmt.Errorf("%s", asString(res.Stderr))
	}
	if err != nil {
		return fmt.Errorf("machine %s: stage %s: %w", name, mt.Guest, err)
	}
	return nil
}

// Sync devolve ao host as cópias staged de uma máquina ligada (rc machine sync). Máquina sem
// volume staged não faz nada; parada não tem o que devolver (a cópia está no disco dela e
// volta no próximo sync com ela ligada).
func (m *Machines) Sync(ctx context.Context, name string) error {
	st, err := m.Status(name)
	if err != nil {
		return err
	}
	var staged []int
	for i, mt := range st.Mounts {
		if mt.Staged {
			staged = append(staged, i)
		}
	}
	if len(staged) == 0 {
		return nil
	}
	if st.State != StateRunning {
		return domain.ErrMachineState(name, "is not running; start it to sync its staged volumes")
	}
	sock := filepath.Join(m.Store.Dir(name), machineSocket)
	for _, i := range staged {
		mt := st.Mounts[i]
		res, err := m.Guest.Run(ctx, sock, protocol.Job{TraceID: "sync-" + mountTag(i), Op: protocol.OpExec,
			Argv: []string{"sh", "-c", syncScript, "sync", mt.DiskPath(), stagedHidden(i)}})
		if err == nil && res.ExitCode != 0 {
			err = fmt.Errorf("%s", asString(res.Stderr))
		}
		if err == nil && m.ApplyGuestModes != nil {
			err = m.ApplyGuestModes(mt.Host)
		}
		if err != nil {
			return fmt.Errorf("machine %s: sync %s: %w", name, mt.Guest, err)
		}
	}
	return nil
}
