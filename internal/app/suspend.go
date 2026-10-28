package app

import (
	"context"
	"crypto/rand"
	"crypto/sha256"
	"encoding/base64"
	"encoding/hex"
	"errors"
	"fmt"
	"io"
	"os"
	"path/filepath"
	"strings"
	"time"

	"runner.codes/internal/domain"
	"runner.codes/internal/protocol"
)

// machineSnapshot é o diretório do snapshot dentro do diretório da máquina. Mora no disco próprio dela: suspensa num
// host, a máquina viaja com o disco e volta no mesmo ponto no host que adotar o disco.
const machineSnapshot = "snapshot"

// snapshotManifest é o arquivo que a libkrun grava por último no snapshot (depois do fsync da RAM e do estado): é a
// marca de snapshot completo. Diretório sem ele é gravação interrompida, que não se restaura.
const snapshotManifest = "manifest.json"

// DefaultSuspendTimeout é quanto o Suspend espera o processo vmm gravar o snapshot e sair. A RAM vai esparsa (só as
// páginas usadas), e um desktop de 4 GiB grava em poucos segundos num pd-balanced; 20 s cabem nos ~30 s do aviso de
// preempção com folga para soltar o disco depois.
const DefaultSuspendTimeout = 20 * time.Second

// ErrSuspendUnsupported é a recusa do Suspend quando a libkrun deste host não sabe suspender (sem o fork do superbot):
// quem chama desliga pelo caminho normal.
var ErrSuspendUnsupported = errors.New("this host cannot suspend machines (libkrun without snapshot support)")

// clockSyncTrace é o trace do job que acerta o relógio do convidado depois do restore.
const clockSyncTrace = "restore-clock"

// syncGuestClock acerta o relógio de parede do convidado com a hora do host, logo depois do restore. O snapshot traz o
// relógio do instante do suspend, e o kernel do convidado não o corrige sozinho: medido em 25/10/2026 no
// sbot-host-14, o desktop voltou marcando a hora do suspend, atrasado o tempo todo em que ficou parado — depois de
// uma preempção seriam minutos ou horas, e TLS, tokens com validade e logs quebram com o relógio no passado.
//
// É melhor esforço, com duas tentativas: a máquina já voltou inteira, e falhar o Start por causa do relógio faria
// quem chama tratar como perdido um desktop que está no ar. O `@segundos.nanos` é do date do GNU (o convidado do
// desktop é Debian).
func (m *Machines) syncGuestClock(ctx context.Context, sock string) {
	for range 2 {
		now := time.Now()
		res, err := m.Guest.Run(ctx, sock, protocol.Job{TraceID: clockSyncTrace, Op: protocol.OpExec,
			Argv: []string{"date", "-u", "-s", fmt.Sprintf("@%d.%09d", now.Unix(), now.Nanosecond())}})
		if err == nil && res.ExitCode == 0 {
			return
		}
	}
}

// reseedTrace é o trace do job que renova a entropia do convidado depois do restore.
const reseedTrace = "restore-reseed"

// reseedBytes é quanto de aleatoriedade do host entra no gerador do convidado: 512 bytes são 4096 bits, o tamanho do
// pool de entrada do kernel — mais que isso não muda nada.
const reseedBytes = 512

// reseedGuest mistura aleatoriedade nova do host no gerador do kernel do convidado, logo depois do restore. A memória
// restaurada traz o estado do gerador do instante do snapshot: com o snapshot dourado, TODAS as máquinas nascidas dele
// voltariam com o mesmo estado, e os primeiros números "aleatórios" (chave de sessão TLS, nonce, token) sairiam iguais
// entre elas. Escrever em /dev/urandom mistura no pool sem creditar entropia — é o bastante para os estados divergirem,
// e não depende de nada além do base64 do coreutils no convidado. Melhor esforço, como o relógio.
func (m *Machines) reseedGuest(ctx context.Context, sock string) {
	buf := make([]byte, reseedBytes)
	if _, err := rand.Read(buf); err != nil {
		return
	}
	for range 2 {
		res, err := m.Guest.Run(ctx, sock, protocol.Job{TraceID: reseedTrace, Op: protocol.OpExec,
			Argv: []string{"sh", "-c", "base64 -d > /dev/urandom"}, Stdin: base64.StdEncoding.EncodeToString(buf)})
		if err == nil && res.ExitCode == 0 {
			return
		}
	}
}

// machineEvents é o log de decisões de suspend e restore no diretório da máquina — que, com disco persistente, é o
// disco dela. Ele viaja com o disco: o worker spot preemptado some com o journal em ~15 s (o failover apaga a
// instância), e foi assim que a primeira preempção com suspend, em 25/10/2026, voltou por boot do zero sem deixar
// pista de qual lado falhou. No host que adota, o arquivo conta o que o anterior fez.
const machineEvents = "events.log"

// logf registra uma decisão sobre a máquina: no Log (o journal do rc) e no events.log dela, com data e hora UTC.
// Falha ao gravar o arquivo não muda nada no fluxo — é rastro, não estado.
func (m *Machines) logf(name, format string, args ...any) {
	line := fmt.Sprintf("machine %s: "+format, append([]any{name}, args...)...)
	if m.Log != nil {
		fmt.Fprintln(m.Log, line)
	}
	if f, err := os.OpenFile(filepath.Join(m.Store.Dir(name), machineEvents), os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0o600); err == nil {
		fmt.Fprintf(f, "%s %s\n", time.Now().UTC().Format(time.RFC3339), line)
		f.Close()
	}
}

// snapshotVMState é o estado das vCPUs e dos devices no snapshot (o vmstate da libkrun): sem ele e sem o manifesto, o
// que sobra no diretório é só a RAM.
const snapshotVMState = "vmstate"

// invalidateSnapshot tira do snapshot o que o faz restaurável — o manifesto (a marca de completo), o estado e a marca
// da imagem base — e mantém a RAM (memory.img). Depois de um restore, a RAM da máquina é o mapeamento privado desse
// arquivo (restore sob demanda): apagá-lo obrigaria o próximo suspend a ler a RAM inteira para regravá-la, e num
// desktop nascido do snapshot dourado isso estourou os 15 s do dreno da preempção (25/10/2026) — com o arquivo no
// lugar, a libkrun grava nele só as páginas escritas. Arquivo ausente não é erro.
func (m *Machines) invalidateSnapshot(name string) error {
	dir := m.snapshotPath(name)
	for _, f := range []string{snapshotManifest, snapshotVMState, snapshotBaseImage} {
		if err := os.Remove(filepath.Join(dir, f)); err != nil && !errors.Is(err, os.ErrNotExist) {
			return err
		}
	}
	return nil
}

// snapshotBaseImage guarda, dentro do snapshot, o sha256 da imagem base (o ext4 só leitura do root) com que a máquina
// estava ligada. O restore só vale sobre a MESMA imagem: o kernel do convidado volta com o cache de diretórios e
// inodes dela, e cada host constrói a sua (o mkfs.ext4 sorteia UUID e hash seed, e a tag da imagem anda). Medido em
// 25/10/2026: um desktop restaurado noutro worker passou a acusar "Directory block failed checksum" no vda e
// "Bad message" ao listar /usr/local/lib/node_modules — o claude sumiu do PATH.
const snapshotBaseImage = "base-image.sha256"

// imageDigest é o sha256 do arquivo da imagem, guardado ao lado dela (<imagem>.sha256) para o próximo pedido: a
// conta lê o arquivo inteiro (segundos para um desktop), e o Suspend da preempção não tem esses segundos. Vale
// enquanto o arquivo do lado for mais novo que a imagem.
func imageDigest(path string) (string, error) {
	side := path + ".sha256"
	img, err := os.Stat(path)
	if err != nil {
		return "", err
	}
	if st, err := os.Stat(side); err == nil && !st.ModTime().Before(img.ModTime()) {
		if b, err := os.ReadFile(side); err == nil && len(strings.TrimSpace(string(b))) == 64 {
			return strings.TrimSpace(string(b)), nil
		}
	}
	f, err := os.Open(path)
	if err != nil {
		return "", err
	}
	defer f.Close()
	h := sha256.New()
	if _, err := io.Copy(h, f); err != nil {
		return "", err
	}
	sum := hex.EncodeToString(h.Sum(nil))
	// Grava por rename: um Start concorrente nunca lê o arquivo pela metade.
	tmp := side + ".tmp"
	if err := os.WriteFile(tmp, []byte(sum+"\n"), 0o644); err == nil {
		os.Rename(tmp, side)
	}
	return sum, nil
}

// snapshotMatchesImage diz se o snapshot foi feito sobre esta imagem. Snapshot sem a marca (de um rc anterior a ela)
// não se confere e não se restaura.
func (m *Machines) snapshotMatchesImage(name, image string) (bool, string) {
	saved, err := os.ReadFile(filepath.Join(m.snapshotPath(name), snapshotBaseImage))
	if err != nil {
		return false, "the snapshot does not record its base image"
	}
	local, err := imageDigest(image)
	if err != nil {
		return false, fmt.Sprintf("base image unreadable here: %v", err)
	}
	if strings.TrimSpace(string(saved)) != local {
		return false, "the snapshot was taken over another build of the base image"
	}
	return true, ""
}

// snapshotPath é o diretório do snapshot da máquina.
func (m *Machines) snapshotPath(name string) string {
	return filepath.Join(m.Store.Dir(name), machineSnapshot)
}

// hasSnapshot diz se a máquina tem um snapshot completo para restaurar.
func (m *Machines) hasSnapshot(name string) bool {
	_, err := os.Stat(filepath.Join(m.snapshotPath(name), snapshotManifest))
	return err == nil
}

// Suspend grava o estado inteiro da máquina ligada (vCPUs, devices e RAM) no disco dela e a desliga: o próximo Start
// a traz de volta no mesmo ponto — o Claude Code no meio da tarefa, a janela aberta, o cursor onde estava. É a
// resposta à preempção do spot: o disco sai do host que vai sumir com a máquina suspensa dentro.
//
// Só conta como suspensa com o manifesto no lugar: processo que sai sem ele (snapshot recusado, disco cheio) ou que
// não sai no prazo é erro, e o snapshot pela metade é apagado — quem chama desliga pelo caminho normal.
func (m *Machines) Suspend(ctx context.Context, name string, timeout time.Duration) error {
	st, err := m.Status(name)
	if err != nil {
		return err
	}
	if st.State != StateRunning {
		return domain.ErrMachineState(name, "is not running; only a running machine can be suspended")
	}
	if !m.CanSuspend {
		return ErrSuspendUnsupported
	}
	if timeout <= 0 {
		timeout = DefaultSuspendTimeout
	}
	snap := m.snapshotPath(name)
	// Snapshot velho fora antes do pedido: o manifesto que aparecer depois é deste suspend, não de um anterior. A RAM
	// do arquivo fica (invalidateSnapshot): a máquina restaurada sob demanda está mapeada sobre ela, e a libkrun grava
	// ali só as páginas que mudaram.
	if err := m.invalidateSnapshot(name); err != nil {
		return fmt.Errorf("suspend %s: clear old snapshot: %w", name, err)
	}
	if err := m.Procs.Suspend(st.PID); err != nil {
		return fmt.Errorf("suspend %s: %w", name, err)
	}
	started := time.Now()
	for end := time.Now().Add(timeout); m.Procs.Alive(st.PID) && time.Now().Before(end); {
		select {
		case <-ctx.Done():
			return ctx.Err()
		case <-time.After(20 * time.Millisecond):
		}
	}
	if m.Procs.Alive(st.PID) {
		// Ainda gravando (ou travado): a VM pode estar congelada no meio do snapshot. Não mata aqui — quem chama
		// decide (o Drain desliga pelo caminho normal, que mata no prazo).
		m.logf(name, "snapshot not finished in %s", timeout)
		return fmt.Errorf("suspend %s: snapshot not finished in %s", name, timeout)
	}
	m.leaveFabric(st.Machine)
	if err := m.Store.SetPID(name, 0); err != nil {
		return err
	}
	if !m.hasSnapshot(name) {
		os.RemoveAll(snap)
		m.logf(name, "stopped without a complete snapshot")
		return fmt.Errorf("suspend %s: the machine stopped without a complete snapshot", name)
	}
	// A marca da imagem base entra por último, já com o snapshot completo: sem ela o restore recusa. O digest está
	// em cache desde o Start (imageDigest), então aqui é só ler um arquivo.
	if image, ok := m.Images.ImagePath(st.Stack); ok {
		if sum, err := imageDigest(image); err == nil {
			os.WriteFile(filepath.Join(snap, snapshotBaseImage), []byte(sum+"\n"), 0o600)
		}
	}
	m.logf(name, "suspended in %s", time.Since(started).Round(time.Millisecond))
	return nil
}
