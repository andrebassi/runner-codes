package app

import (
	"context"
	"crypto/sha256"
	"encoding/hex"
	"errors"
	"fmt"
	"io"
	"net"
	"os"
	"path/filepath"
	"strings"
	"sync"
	"time"

	"runner.codes/internal/domain"
	"runner.codes/internal/ports"
	"runner.codes/internal/protocol"
)

// Nomes dos arquivos de runtime no diretório de uma máquina (o store cuida do registro,
// do disco e do pid).
const (
	machineSocket  = "v.sock"
	machineConsole = "console.log"
	machineVMMLog  = "vmm.log"
	machineSpec    = "spec.json"
	// machineDenials é o log de auditoria da rede (uma linha por conexão ou nome negado);
	// machineEgressSocket é onde o gateway da máquina ligada aceita política nova.
	machineDenials      = "egress-denials.log"
	machineEgressSocket = "egress.sock"
)

// DefaultStopTimeout é a espera do stop pelo desligamento limpo antes do SIGKILL: 10 s.
// O sync do guest leva milissegundos; o prazo cobre disco lento.
const DefaultStopTimeout = 10 * time.Second

// DiskTemplates entrega o modelo vazio do disco de máquina (oci.Builder implementa).
type DiskTemplates interface {
	DiskTemplate(ctx context.Context, gib int) (string, error)
}

// Machines cuida das máquinas persistentes: registro, ciclo de vida da VM destacada,
// exec, cópia de arquivo e logs. Independente do Pool: máquina não é job.
type Machines struct {
	Store    ports.MachineStore
	Disks    DiskTemplates
	Images   ports.ImageStore
	Catalog  Catalog
	Launcher ports.DetachedLauncher
	Procs    ports.Processes
	Guest    ports.GuestClient
	// CanSuspend diz que a libkrun deste host sabe suspender e restaurar (o fork do superbot): o Start passa a
	// restaurar o snapshot que a máquina tiver, e o Suspend fica disponível. Falso: só boot e desligamento.
	CanSuspend bool
	// Log recebe as decisões de suspend e restore (restaurou, caiu no boot do zero, apagou snapshot velho), uma por
	// linha — no worker spot elas são a única pista do que aconteceu numa preempção, porque o host some junto. O rc
	// serve aponta para o stderr (journal); nil não escreve.
	Log io.Writer
	// ReadyTimeout limita a espera do guest no start; 0 usa o ReadyTimeout do pool.
	ReadyTimeout time.Duration
	// BuildImage constrói o ext4 de uma imagem OCI qualquer (`image` no runner.code); nil
	// recusa máquina por imagem. Recebe a linguagem sintética com o nome do ext4.
	BuildImage func(ctx context.Context, l domain.Language) error
	// buildMu deixa um build de imagem (e um preparo de modelo de disco) por vez. O builder usa um diretório de trabalho só (o mesmo inject/ e o mesmo
	// .tar) e sobe uma VM de build por chamada: dois builds juntos sobrescrevem os arquivos um do outro ("build VM
	// exited with code 1") e seis VMs de build juntas num host de 8 GB derrubaram o próprio rc (medido no teste de 25
	// desktops do superbot, 25/10/2026, num host novo sem a imagem em cache). Quem espera encontra a imagem pronta:
	// o build seguinte é só a conferência do cache.
	buildMu sync.Mutex
	// UpdateEgress manda a política nova ao gateway de uma máquina ligada (o socket de
	// controle do processo vmm); nil recusa a troca com a máquina ligada.
	UpdateEgress func(socket string, e domain.Egress) error
	// NetworksDir guarda as redes nomeadas (<cache>/networks); vazio recusa rede nomeada.
	NetworksDir string
	// NewCredentialCA cria a CA de uma máquina com credenciais (certificado e chave em PEM),
	// restrita aos hosts dados; nil recusa credencial.
	NewCredentialCA func(hosts []string) (cert, key []byte, err error)
	// ApplyGuestModes acerta no host o modo dos arquivos que o guest criou num volume (a
	// libkrun no macOS os cria 0600 e guarda o modo do guest em xattr); nil não mexe.
	ApplyGuestModes func(dir string) error
	// Persist põe o diretório de cada máquina nova num disco de rede só dela (persistent.go); nil mantém tudo no
	// disco do host, como sempre foi (Mac, casa do superbot).
	Persist PersistentDisks
	// EgressProxy e EgressCA são o proxy de saída do host e a CA dele (ports.VMNet); vazios, a máquina sai direto.
	EgressProxy string
	EgressCA    string
	// drain é o estado da preempção do host (Drain).
	drain drainState
}

// fabricDir é o diretório de uma rede nomeada: o hash do nome, e não o nome, porque o
// caminho do socket Unix no macOS tem teto de 104 bytes e um nome de 63 caracteres estouraria.
func (m *Machines) fabricDir(network string) string {
	h := sha256.Sum256([]byte(network))
	return filepath.Join(m.NetworksDir, hex.EncodeToString(h[:6]))
}

// Update troca a política de saída de uma máquina: grava no registro (vale no próximo
// start) e, com ela ligada, entrega ao gateway, que aplica na próxima conexão ou consulta
// e esquece os IPs de nomes revogados.
func (m *Machines) Update(name string, change func(*domain.Egress)) (domain.Machine, error) {
	st, err := m.Status(name)
	if err != nil {
		return domain.Machine{}, err
	}
	mc := st.Machine
	change(&mc.Network.Egress)
	if err := mc.Validate(); err != nil {
		return st.Machine, err
	}
	if st.State == StatePaused {
		// O gateway está congelado junto com a VM: o pedido ao socket de controle não volta.
		return st.Machine, domain.ErrMachineState(name, "is paused; resume it first")
	}
	if st.State == StateRunning {
		if !st.Network.Attached() {
			// Sem placa no boot não há gateway para avisar: só vale depois de um restart.
			return st.Machine, domain.ErrMachineState(name, "has no network attached; stop it, update, and start again")
		}
		if m.UpdateEgress == nil {
			return st.Machine, domain.ErrMachineState(name, "is running and live updates are not available")
		}
		if err := m.UpdateEgress(filepath.Join(m.Store.Dir(name), machineEgressSocket), mc.Network.Egress); err != nil {
			return st.Machine, err
		}
	}
	if err := m.Store.Save(mc); err != nil {
		return st.Machine, err
	}
	return mc, nil
}

// initMarker marca, no disco da máquina, que o init já rodou: fica na raiz persistente, então
// sobrevive ao stop/start e some com o delete (que leva o disco).
const initMarker = "/var/lib/runner/init-done"

// stopGrace é a folga do host sobre o stop_timeout do workload: depois do sinal e da espera,
// o guest ainda faz sync antes de sair.
const stopGrace = 5 * time.Second

// ImageStackName dá o nome do ext4 de uma máquina criada por imagem: "img-" e 12 hex do
// sha256 da referência. Mesmo ref, mesmo nome: a segunda máquina reaproveita o build.
func ImageStackName(ref string) string {
	sum := sha256.Sum256([]byte(ref))
	return "img-" + hex.EncodeToString(sum[:6])
}

// MachineStatus é a máquina com o estado de agora.
type MachineStatus struct {
	domain.Machine
	State string `json:"state"`
	PID   int    `json:"pid,omitempty"`
	// Suspended: parada com um snapshot completo no disco — o próximo start volta no ponto em que ela parou.
	Suspended bool `json:"suspended,omitempty"`
}

// Estados de uma máquina.
const (
	StateRunning = "running"
	StateStopped = "stopped"
)

// ExecRequest é um comando numa máquina ligada.
type ExecRequest struct {
	// User sobrepõe o user da máquina neste comando ("root" para administrar).
	User    string
	Argv    []string
	Env     []string
	Cwd     string
	Stdin   string
	Timeout time.Duration // 0 = sem prazo
}

// Create valida, confere a stack (imagem construída e GPU, se pedida) e registra a
// máquina com um disco novo. Não liga: o start é separado.
func (m *Machines) Create(ctx context.Context, mc domain.Machine) (domain.Machine, error) {
	return m.CreateFrom(ctx, mc, "")
}

// CreateFrom é o Create com o disco de partida dado (o de um pack feito de uma máquina, com
// o que foi instalado nela): clonado como o modelo vazio seria. Vazio usa o modelo novo.
func (m *Machines) CreateFrom(ctx context.Context, mc domain.Machine, disk string) (out domain.Machine, err error) {
	if m.isDraining() {
		// Host em preempção: a máquina nasceria num disco que vai ser solto em segundos.
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
	for i, mt := range mc.Mounts {
		if _, _, ok := mt.S3(); ok || mt.Disk() {
			continue // bucket ou disco da máquina: nada no disco do host para conferir
		}
		// Guarda o caminho real: link simbólico trocado depois do create não redireciona o
		// volume para outro lugar do host.
		real, err := filepath.EvalSymlinks(mt.Host)
		if fi, serr := os.Stat(real); err != nil || serr != nil || !fi.IsDir() {
			return mc, domain.ValidationError{Err: domain.MachineError{Code: domain.CodeConfig,
				Msg: fmt.Sprintf("volume %s: host path must be an existing directory", mt)}}
		}
		mc.Mounts[i].Host = real
	}
	if _, err := m.Store.Load(mc.Name); err == nil {
		return mc, domain.ErrMachineExists(mc.Name)
	}
	if m.Persist != nil {
		// O disco próprio vem antes de tudo: se ele já existe (a máquina morava num host que sumiu, ou foi solta para
		// mudar de host), adotar é o create inteiro — o registro e o disco gravável já estão lá dentro. É o pod do
		// StatefulSet renascendo em outro node com o mesmo PersistentVolume.
		dir := m.Store.Dir(mc.Name)
		created, perr := m.Persist.Ensure(ctx, mc.Name, dir, persistentGiB(mc.DiskGiB))
		if perr != nil {
			// O disco nasceu nesta chamada e um passo seguinte falhou (anexar, mkfs): apaga, senão ele fica cobrando
			// sem dono e cru, e a próxima tentativa o adota como se estivesse pronto.
			if created {
				m.Persist.Destroy(context.WithoutCancel(ctx), mc.Name, dir)
			}
			return mc, fmt.Errorf("persistent disk: %w", perr)
		}
		if adopted, lerr := m.Store.Load(mc.Name); lerr == nil {
			// A imagem é do host, não do disco: o host novo pode nunca ter rodado essa imagem. Sem isto a máquina
			// adotada não ligava ("image … not built", medido em 25/10/2026 no desktop que mudou de worker depois da
			// preempção do spot).
			if err := m.ensureImage(ctx, adopted); err != nil {
				return adopted, err
			}
			return adopted, nil
		}
		if created {
			// Disco novo e o create falhou no meio: apaga o disco, senão ele fica cobrando sem dono.
			defer func() {
				if err != nil {
					m.Persist.Destroy(context.WithoutCancel(ctx), mc.Name, dir)
				}
			}()
		}
	}
	if mc.Network.Name != "" {
		if m.NetworksDir == "" {
			return mc, domain.ValidationError{Err: domain.MachineError{Code: domain.CodeConfig, Msg: "named networks are not available here"}}
		}
		// IP na rede: o primeiro livre entre as máquinas já registradas nela.
		all, err := m.Store.List()
		if err != nil {
			return mc, err
		}
		var used []string
		for _, o := range all {
			if o.Network.Name == mc.Network.Name {
				used = append(used, o.Network.Address)
			}
		}
		a, err := domain.NextFabricAddress(used)
		if err != nil {
			return mc, domain.ValidationError{Err: domain.MachineError{Code: domain.CodeConfig, Msg: err.Error()}}
		}
		mc.Network.Address = a.String()
	}
	if err := m.ensureImage(ctx, mc); err != nil {
		return mc, err
	}
	tpl := disk
	if tpl == "" {
		// O modelo de disco usa o mesmo diretório de trabalho e a mesma VM de build das imagens: mesma trava. Seis
		// criações juntas num host novo disputavam o rename do mesmo arquivo ("move disk template: rename … no such
		// file", medido no 2º teste de 25 desktops, 25/10/2026).
		m.buildMu.Lock()
		var err error
		tpl, err = m.Disks.DiskTemplate(ctx, mc.DiskGiB)
		m.buildMu.Unlock()
		if err != nil {
			return mc, fmt.Errorf("disk template: %w", err)
		}
	}
	if mc.CreatedAt.IsZero() {
		mc.CreatedAt = time.Now().UTC().Truncate(time.Second)
	}
	if len(mc.Network.Credentials) > 0 {
		if m.NewCredentialCA == nil {
			return mc, domain.ValidationError{Err: domain.MachineError{Code: domain.CodeConfig, Msg: "credentials are not available here"}}
		}
		mintPlaceholders(&mc)
	}
	if err := m.Store.Create(mc, tpl); err != nil {
		return mc, err
	}
	if len(mc.Network.Credentials) > 0 {
		if err := m.writeCredentialCA(mc); err != nil {
			// Máquina com credencial e sem CA não teria como ligar: sai inteira.
			m.Store.Delete(mc.Name)
			return mc, err
		}
	}
	return mc, nil
}

// Status lê o registro e diz se o vmm registrado está vivo.
func (m *Machines) Status(name string) (MachineStatus, error) {
	mc, err := m.Store.Load(name)
	if err != nil {
		return MachineStatus{}, err
	}
	st := MachineStatus{Machine: mc, State: StateStopped}
	if pid := m.Store.PID(name); m.Procs.Alive(pid) {
		st.State, st.PID = StateRunning, pid
		if m.isPaused(name) {
			st.State = StatePaused
		}
	} else {
		// Pausa de uma VM que morreu (kill, reboot do host) não vale mais.
		os.Remove(filepath.Join(m.Store.Dir(name), machinePaused))
		st.Suspended = m.hasSnapshot(name)
	}
	return st, nil
}

// List devolve todas as máquinas com o estado de cada uma.
func (m *Machines) List() ([]MachineStatus, error) {
	ms, err := m.Store.List()
	if err != nil {
		return nil, err
	}
	out := make([]MachineStatus, 0, len(ms))
	for _, mc := range ms {
		st, err := m.Status(mc.Name)
		if err != nil {
			return nil, err
		}
		out = append(out, st)
	}
	return out, nil
}

// Start liga a VM da máquina destacada do processo atual e espera o guest responder.
// Se o guest não sobe no prazo, a VM é morta e o erro traz o fim do console.
func (m *Machines) Start(ctx context.Context, name string) (MachineStatus, error) {
	return m.start(ctx, name, false)
}

// vmmPoll é o intervalo da vigia do vmm durante a espera de prontidão: curto o bastante para
// a recusa da configuração aparecer na hora, longo o bastante para não pesar (kill -0).
const vmmPoll = 50 * time.Millisecond

// watchVMM devolve um contexto que acaba quando o processo do vmm morre: sem isso, uma VM que
// nem chegou a bootar fazia o start esperar o prazo de prontidão inteiro (30 s, medido).
func (m *Machines) watchVMM(ctx context.Context, pid int) context.Context {
	watched, stop := context.WithCancel(ctx)
	go func() {
		t := time.NewTicker(vmmPoll)
		defer t.Stop()
		for {
			select {
			case <-watched.Done():
				return
			case <-t.C:
				if !m.Procs.Alive(pid) {
					stop()
					return
				}
			}
		}
	}()
	return watched
}

// fileSize devolve o tamanho do arquivo, 0 se ele não existe.
func fileSize(path string) int64 {
	st, err := os.Stat(path)
	if err != nil {
		return 0
	}
	return st.Size()
}

// readFrom lê o arquivo a partir de off, até 64 KiB (o fim de um log de vmm cabe nisso).
func readFrom(path string, off int64) string {
	f, err := os.Open(path)
	if err != nil {
		return ""
	}
	defer f.Close()
	f.Seek(off, io.SeekStart)
	b, _ := io.ReadAll(io.LimitReader(f, 64<<10))
	return string(b)
}

// lastLine devolve a última linha não vazia de um texto, ou "(no log)".
func lastLine(s string) string {
	lines := strings.Split(strings.TrimRight(s, "\n"), "\n")
	if l := strings.TrimSpace(lines[len(lines)-1]); l != "" && l != "(no console)" {
		return l
	}
	return "(no log)"
}

// start liga a máquina; tied prende a VM ao processo que chamou (o run em primeiro plano).
func (m *Machines) start(ctx context.Context, name string, tied bool) (MachineStatus, error) {
	st, err := m.Status(name)
	if err != nil {
		return st, err
	}
	if st.State == StatePaused {
		// Pausada não é parada: o start não religa nada, e "already running" mandava procurar
		// o processo errado. Diz o estado e as duas saídas.
		return st, domain.ErrMachineState(name, "is paused; resume it (rc machine resume) or stop it first")
	}
	if st.State == StateRunning {
		return st, domain.ErrMachineState(name, "is already running")
	}
	image, ok := m.Images.ImagePath(st.Stack)
	if !ok && st.Image != "" {
		// Máquina de imagem cujo ext4 não está neste host: chegou de outro host pelo disco próprio (adotada antes de
		// o create construir a imagem) ou o cache foi podado. A imagem é reconstruível, então constrói em vez de
		// recusar; só stack sem imagem continua pedindo o `rc image build`.
		if err := m.ensureImage(ctx, st.Machine); err != nil {
			return st, err
		}
		image, ok = m.Images.ImagePath(st.Stack)
	}
	if !ok {
		return st, fmt.Errorf("image for %s not built (run `rc image build --stack %s`)", st.Stack, st.Stack)
	}
	l, _ := m.Catalog.Get(st.Stack)
	dir := m.Store.Dir(name)
	sock := filepath.Join(dir, machineSocket)
	// Socket que ficou de uma VM morta faria a libkrun falhar ao criar o novo.
	os.Remove(sock)
	spec := ports.VMSpec{
		TiedToCaller: tied,
		CPUs:         st.CPUs, MemoryMiB: st.MemoryMiB, RootDisk: image,
		DataDisk:    filepath.Join(dir, "disk.ext4"),
		VsockSocket: sock, VsockPort: protocol.DefaultPort,
		Console: filepath.Join(dir, machineConsole), Exec: "/opt/runner/machine.sh",
	}
	for i, mt := range st.Mounts {
		if _, _, ok := mt.S3(); ok || mt.Disk() {
			continue // montado pelo guest (mount-s3 ou bind no disco), não é device virtio-fs
		}
		// O diretório pode ter sumido desde o create: falhar aqui, com o caminho, é melhor que
		// a libkrun recusar o device com um errno sem contexto.
		if fi, err := os.Stat(mt.Host); err != nil || !fi.IsDir() {
			return st, domain.ValidationError{Err: domain.MachineError{Code: domain.CodeConfig,
				Msg: fmt.Sprintf("volume %s: host directory %s is not available", mt, mt.Host)}}
		}
		spec.Mounts = append(spec.Mounts, ports.VMMount{Tag: mountTag(i), HostPath: mt.Host, ReadOnly: mt.ReadOnly})
	}
	if st.Network.Attached() {
		os.Remove(filepath.Join(dir, machineEgressSocket))
		netw := st.Network
		netw.Credentials, _ = bindingSources(st.Machine)
		spec.Net = &ports.VMNet{Network: netw, DenialsLog: filepath.Join(dir, machineDenials),
			ControlSocket: filepath.Join(dir, machineEgressSocket), Machine: name,
			EgressProxy: m.EgressProxy, EgressCA: m.EgressCA}
		if st.Network.Name != "" {
			spec.Net.FabricDir = m.fabricDir(st.Network.Name)
		}
		if len(st.Network.Credentials) > 0 {
			spec.Net.CACert, spec.Net.CAKey = filepath.Join(dir, machineCredCACert), filepath.Join(dir, machineCredCAKey)
		}
	}
	if st.SSHAgent {
		sock := os.Getenv("SSH_AUTH_SOCK")
		if sock == "" {
			return st, domain.ValidationError{Err: domain.MachineError{Code: domain.CodeConfig,
				Msg: "ssh_agent needs SSH_AUTH_SOCK set where the machine starts (ssh-add -l to check the agent)"}}
		}
		spec.SSHAgent = sock
	}
	if st.DockerSocket {
		spec.Sockets = append(spec.Sockets, ports.VMSocket{Host: filepath.Join(dir, MachineDockerSocket), Guest: "/var/run/docker.sock"}) // contract:ok — caminho do Docker
	}
	for _, e := range st.ExposeSockets {
		host := e.Host
		if host == "" {
			host = filepath.Join(dir, filepath.Base(e.Guest))
		}
		spec.Sockets = append(spec.Sockets, ports.VMSocket{Host: host, Guest: e.Guest})
	}
	for i, ms := range st.MountSockets {
		// Conferido no start: a libkrun só disca o socket quando o guest conecta, e um
		// caminho errado viraria "connection refused" lá dentro, sem dizer qual.
		if fi, err := os.Stat(ms.Host); err != nil || fi.Mode()&os.ModeSocket == 0 {
			return st, domain.ValidationError{Err: domain.MachineError{Code: domain.CodeConfig,
				Msg: fmt.Sprintf("mount socket: %s is not a unix socket on this host", ms.Host)}}
		}
		spec.HostSockets = append(spec.HostSockets, ports.VMSocket{Host: ms.Host, Guest: ms.Guest, Port: uint32(protocol.MountSocketPortBase + i)})
	}
	if st.Display != nil {
		spec.Display = &ports.VMDisplay{Width: st.Display.Width, Height: st.Display.Height,
			Listen: fmt.Sprintf("127.0.0.1:%d", st.Display.Port), Title: st.Display.Title}
	}
	if st.GPU {
		spec.GPU, spec.GPUVRAMMiB = true, l.GPUVRAMMiB
		if st.GPUVRAMMiB > 0 {
			spec.GPUVRAMMiB = st.GPUVRAMMiB
		}
	}
	spec.Nested = st.Nested
	// Com suporte a snapshot, todo start arma o suspender; com snapshot no disco, a VM volta por ele em vez do boot.
	restore := false
	if m.CanSuspend {
		spec.SnapshotDir = m.snapshotPath(name)
		if m.hasSnapshot(name) {
			if ok, why := m.snapshotMatchesImage(name, image); ok {
				spec.RestoreFrom, restore = spec.SnapshotDir, true
				m.logf(name, "restoring from snapshot")
			} else {
				// Outra imagem base: restaurar entregaria um convidado com o cache de outro disco. Boot do zero.
				m.logf(name, "not restoring: %s; booting from scratch", why)
				os.RemoveAll(m.snapshotPath(name))
			}
		} else {
			// Digest em cache desde já: o Suspend da preempção só lê o arquivo do lado.
			imageDigest(image)
		}
	} else {
		// Host que não sabe restaurar liga do zero, e o disco muda a partir daqui: o snapshot que veio com ele (a
		// máquina suspensa num host com o fork e adotada aqui) fica velho na hora. Deixado no disco, o próximo start
		// num host que restaura carregaria aquela RAM por cima de um sistema de arquivos que já mudou — páginas de
		// cache e journal de outro tempo, e o ext4 corrompido. Fora antes do boot.
		if m.hasSnapshot(name) {
			m.logf(name, "this host cannot restore; dropping its snapshot and booting from scratch")
		}
		os.RemoveAll(m.snapshotPath(name))
	}
	// O log do vmm é aberto em append: guarda onde ele termina agora, para o erro deste start
	// citar o que este vmm escreveu, e não a linha de um start anterior.
	vmmLog := filepath.Join(dir, machineVMMLog)
	logFrom := fileSize(vmmLog)
	pid, err := m.Launcher.StartDetached(spec, filepath.Join(dir, machineSpec), vmmLog)
	if err != nil {
		return st, err
	}
	if err := m.Store.SetPID(name, pid); err != nil {
		m.Procs.Kill(pid)
		return st, err
	}
	timeout := m.ReadyTimeout
	if timeout <= 0 {
		timeout = ReadyTimeout
	}
	readyCtx, cancel := context.WithTimeout(ctx, timeout)
	defer cancel()
	if err := m.Guest.WaitReady(m.watchVMM(readyCtx, pid), sock); err != nil {
		if restore {
			// Snapshot que não voltou (outra forma de VM, libkrun recusou, convidado travado): fora com ele, e boot do
			// zero — a máquina liga sem o estado da RAM, mas com o disco inteiro. Melhor que não ligar.
			m.logf(name, "restore did not come up (%v; vmm: %s); booting from scratch", err,
				lastLine(readFrom(vmmLog, logFrom)))
			m.Procs.Kill(pid)
			m.Store.SetPID(name, 0)
			os.RemoveAll(m.snapshotPath(name))
			return m.Start(ctx, name)
		}
		if !m.Procs.Alive(pid) {
			// O vmm saiu antes de o guest responder: a configuração foi recusada (nested sem
			// suporte, libkrun sem um recurso, disco faltando). O motivo é a última linha
			// do log dele, e esperar o prazo inteiro não acrescentaria nada.
			m.Store.SetPID(name, 0)
			return st, fmt.Errorf("machine %s failed to start: %s", name, lastLine(readFrom(vmmLog, logFrom)))
		}
		m.Procs.Kill(pid)
		m.Store.SetPID(name, 0)
		return st, fmt.Errorf("machine %s did not become ready: %w; console: %s; vmm: %s", name, err,
			tailFile(filepath.Join(dir, machineConsole), 400), lastLine(readFrom(vmmLog, logFrom)))
	}
	if restore {
		// A RAM já foi carregada: o snapshot não serve mais (voltar a ele de novo seria voltar no tempo). E nada de
		// provision — montagens, segredos e o workload voltaram com o convidado; rodar de novo duplicaria o workload.
		// A RAM do arquivo fica: a libkrun a restaura sob demanda (mapeada), e o próximo suspend grava nela só o que
		// mudou (invalidateSnapshot).
		m.invalidateSnapshot(name)
		m.syncGuestClock(ctx, sock)
		m.reseedGuest(ctx, sock)
		m.logf(name, "restored from snapshot")
		return m.Status(name)
	}
	if err := m.provision(ctx, st.Machine, sock); err != nil {
		// A VM fica ligada: dá para entrar com `rc machine shell` e ver o que falhou. O init
		// não marcado roda de novo no próximo start.
		return st, err
	}
	return m.Status(name)
}

// MachineDockerSocket é o nome do socket do Docker no diretório da máquina (docker_socket).
const MachineDockerSocket = "docker.sock"

// mountTag é a tag virtio-fs da i-ésima montagem; o guest monta por ela.
func mountTag(i int) string { return fmt.Sprintf("vol%d", i) }

// provision roda o init (uma vez por disco) e liga o workload, os dois com o env e o
// workdir da máquina.
func (m *Machines) provision(ctx context.Context, mc domain.Machine, sock string) error {
	// Segredos primeiro: o init já pode precisar deles (um token para clonar, por exemplo).
	if err := m.sendSecrets(ctx, mc, sock); err != nil {
		return err
	}
	// A CA de credenciais antes de tudo: o init e o workload já podem chamar os hosts.
	if len(mc.Network.Credentials) > 0 {
		if err := m.installCredentialCA(ctx, mc, sock); err != nil {
			return err
		}
	}
	// Volumes primeiro: o workdir pode ser um deles, e o init pode precisar do que está lá.
	// Montagem não persiste entre starts, então roda a cada start.
	for i, mt := range mc.Mounts {
		if mt.Disk() {
			// Diretório no disco ext4 da máquina (o /workspace é ele), ligado ao caminho a cada
			// boot: fora do overlay e persistente.
			src := mt.DiskPath()
			res, err := m.Guest.Run(ctx, sock, protocol.Job{TraceID: "mount-disk-" + mountTag(i), Op: protocol.OpExec,
				Argv: []string{"sh", "-c", `mkdir -p "$1" "$2" && mount --bind "$1" "$2"`, "mount", src, mt.Guest}})
			if err != nil {
				return fmt.Errorf("machine %s: mount %s: %w", mc.Name, mt.Guest, err)
			}
			if res.ExitCode != 0 {
				return fmt.Errorf("machine %s: mount %s: %s", mc.Name, mt.Guest, asString(res.Stderr))
			}
			continue
		}
		if mt.Staged {
			if err := m.stage(ctx, mc.Name, sock, i, mt); err != nil {
				return err
			}
			continue
		}
		if bucket, prefix, ok := mt.S3(); ok {
			// O env da máquina leva as credenciais AWS_*: elas ficam no guest.
			res, err := m.Guest.Run(ctx, sock, protocol.Job{TraceID: "mount-s3-" + mountTag(i), Op: protocol.OpMountS3,
				Path: mt.Guest, Env: mc.Env, S3: &protocol.S3Mount{Bucket: bucket, Prefix: prefix, ReadOnly: mt.ReadOnly}})
			if err != nil {
				return fmt.Errorf("machine %s: mount %s: %w", mc.Name, mt.Guest, err)
			}
			if res.Status != "success" {
				return fmt.Errorf("machine %s: mount %s: %s", mc.Name, mt.Guest, res.Error)
			}
			continue
		}
		opts := "rw"
		if mt.ReadOnly {
			opts = "ro"
		}
		res, err := m.Guest.Run(ctx, sock, protocol.Job{TraceID: "mount-" + mountTag(i), Op: protocol.OpExec,
			Argv: []string{"sh", "-c", `mkdir -p "$1" && mount -t virtiofs -o "$3" "$2" "$1"`, "mount", mt.Guest, mountTag(i), opts}})
		if err != nil {
			return fmt.Errorf("machine %s: mount %s: %w", mc.Name, mt.Guest, err)
		}
		if res.ExitCode != 0 {
			return fmt.Errorf("machine %s: mount %s: %s", mc.Name, mt.Guest, asString(res.Stderr))
		}
	}
	// O workdir é o cwd do init e do workload: no disco novo ele ainda não existe, e o exec
	// com cwd ausente falharia antes do primeiro comando.
	if mc.Workdir != "" && (len(mc.Init) > 0 || len(mc.Workload) > 0) {
		res, err := m.Guest.Run(ctx, sock, protocol.Job{TraceID: "workdir", Op: protocol.OpExec, Argv: []string{"mkdir", "-p", mc.Workdir}})
		if err != nil {
			return fmt.Errorf("machine %s: create workdir: %w", mc.Name, err)
		}
		if res.ExitCode != 0 {
			return fmt.Errorf("machine %s: create workdir %s: %s", mc.Name, mc.Workdir, asString(res.Stderr))
		}
	}
	if len(mc.Init) > 0 {
		res, err := m.Guest.Run(ctx, sock, protocol.Job{TraceID: "init-check", Op: protocol.OpExec, Argv: []string{"test", "-f", initMarker}})
		if err != nil {
			return fmt.Errorf("machine %s: check init: %w", mc.Name, err)
		}
		if res.ExitCode != 0 {
			for i, cmd := range mc.Init {
				res, err := m.Guest.Run(ctx, sock, protocol.Job{TraceID: fmt.Sprintf("init-%d", i), Op: protocol.OpExec,
					Argv: []string{"sh", "-c", cmd}, Env: jobEnv(mc), Cwd: mc.Workdir})
				if err != nil {
					return fmt.Errorf("machine %s: init %q: %w", mc.Name, cmd, err)
				}
				if res.ExitCode != 0 {
					return fmt.Errorf("machine %s: init %q exited with %d: %s", mc.Name, cmd, res.ExitCode, asString(res.Stderr))
				}
			}
			mark := "mkdir -p " + filepath.Dir(initMarker) + " && touch " + initMarker
			if _, err := m.Guest.Run(ctx, sock, protocol.Job{TraceID: "init-done", Op: protocol.OpExec, Argv: []string{"sh", "-c", mark}}); err != nil {
				return fmt.Errorf("machine %s: mark init done: %w", mc.Name, err)
			}
		}
	}
	if len(mc.Workload) > 0 {
		res, err := m.Guest.Run(ctx, sock, protocol.Job{TraceID: "workload", Op: protocol.OpWorkload,
			Argv: mc.Workload, Env: jobEnv(mc), Cwd: mc.Workdir, StopOnExit: mc.StopOnExit, User: mc.User})
		if err != nil {
			return fmt.Errorf("machine %s: start workload: %w", mc.Name, err)
		}
		if res.Status != "success" {
			return fmt.Errorf("machine %s: start workload: %s", mc.Name, res.Error)
		}
	}
	return nil
}

// Stop pede ao guest para gravar o disco e sair; se a VM não terminar no prazo, mata.
// Máquina já parada não é erro: o estado pedido já vale.
func (m *Machines) Stop(ctx context.Context, name string, timeout time.Duration) error {
	st, err := m.Status(name)
	if err != nil {
		return err
	}
	if st.State == StatePaused {
		// Congelada, a VM não responde ao shutdown: continua antes, para o stop ser limpo.
		if err := m.Resume(name); err != nil {
			return err
		}
		st.State = StateRunning
	}
	if st.State != StateRunning {
		m.leaveFabric(st.Machine)
		return m.Store.SetPID(name, 0)
	}
	// Negativo = o stop_timeout da máquina (10 s se ela não declara); explícito vale só
	// para este stop (o `machine stop --timeout`).
	if timeout < 0 {
		timeout = DefaultStopTimeout
		if st.StopTimeout > 0 {
			timeout = time.Duration(st.StopTimeout) * time.Second
		}
	}
	if timeout > 0 {
		// Stop gracioso devolve as cópias staged ao host antes de desligar; se o sync falha a
		// máquina fica ligada, para nada se perder (a cópia segue no disco dela).
		if err := m.Sync(ctx, name); err != nil {
			return err
		}
		m.preStop(ctx, name, timeout)
		sig, _ := domain.SignalNumber(st.StopSignal) // validado no create
		wait := timeout
		if len(st.Workload) > 0 {
			// Com workload, o prazo é dele; o sync do guest vem depois e ganha a folga. Sem
			// workload o prazo inteiro já é para o sync, e um --timeout curto continua curto.
			wait += stopGrace
		}
		sctx, cancel := context.WithTimeout(ctx, wait)
		// Erro aqui (guest travado, conexão caiu quando o processo saiu) não importa: o que
		// decide é o processo terminar, conferido abaixo.
		m.Guest.Run(sctx, filepath.Join(m.Store.Dir(name), machineSocket), protocol.Job{TraceID: "stop", Op: protocol.OpShutdown,
			Signal: sig, Timeout: int(timeout / time.Second)})
		cancel()
		for end := time.Now().Add(wait); time.Now().Before(end) && m.Procs.Alive(st.PID); {
			time.Sleep(20 * time.Millisecond)
		}
	}
	if m.Procs.Alive(st.PID) {
		m.Procs.Kill(st.PID)
	}
	m.leaveFabric(st.Machine)
	return m.Store.SetPID(name, 0)
}

// ensureImage deixa a imagem da máquina pronta NESTE host: com imagem (OCI ou pack), constrói (o build tem cache, e
// no host que já a tem é instantâneo); com stack, confere que ela existe no catálogo, tem ext4 e aceita o pedido de
// GPU. Vale para o create e para a adoção de um disco vindo de outro host.
func (m *Machines) ensureImage(ctx context.Context, mc domain.Machine) error {
	if mc.Image != "" {
		// Imagem qualquer: a GPU é responsabilidade de quem escolheu a imagem (precisa do
		// Mesa com Venus); o ext4 sai do mesmo build das stacks, sem warmup.
		if m.BuildImage == nil {
			return domain.ValidationError{Err: domain.MachineError{Code: domain.CodeConfig, Msg: "machines from an image are not available here"}}
		}
		m.buildMu.Lock()
		err := m.BuildImage(ctx, domain.Language{Name: mc.Stack, Image: mc.Image, CPUs: mc.CPUs, MemoryMiB: mc.MemoryMiB})
		m.buildMu.Unlock()
		if err != nil {
			return fmt.Errorf("build image %s: %w", mc.Image, err)
		}
		return nil
	}
	l, ok := m.Catalog.Get(mc.Stack)
	if !ok {
		return domain.ValidationError{Err: domain.ErrUnsupportedLanguage{Name: mc.Stack}}
	}
	if _, ok := m.Images.ImagePath(mc.Stack); !ok {
		return domain.ValidationError{Err: domain.ErrUnsupportedLanguage{Name: mc.Stack}}
	}
	return (domain.Job{GPU: mc.GPU}).CheckGPU(l)
}

// PreStopHook é o caminho, dentro da máquina, do gancho que roda antes de todo desligamento gracioso — o preStop do
// Kubernetes. É onde a máquina anota o que estava aberto para reabrir na volta (o desktop do superbot guarda as
// sessões do Claude Code e os apps), seja a volta depois de dormir, de mudar de host ou de uma preempção do spot.
const PreStopHook = "/etc/rc/pre-stop"

// preStopMax é o teto do gancho: ele rouba tempo do desligamento, e no aviso de preempção o desligamento inteiro tem
// 8 s (drainStopTimeout). Metade do prazo do stop, até 5 s.
const preStopMax = 5 * time.Second

// preStop roda o gancho da máquina, se ela tiver um. Erro, ausência ou prazo estourado não impedem o stop: o gancho é
// para lembrar o que estava aberto, e um desligamento que espera por ele pode perder o prazo da preempção.
func (m *Machines) preStop(ctx context.Context, name string, timeout time.Duration) {
	limit := min(timeout/2, preStopMax)
	if limit < time.Second {
		return
	}
	hctx, cancel := context.WithTimeout(ctx, limit)
	defer cancel()
	m.Exec(hctx, name, ExecRequest{Argv: []string{"sh", "-c", "[ -x " + PreStopHook + " ] && exec " + PreStopHook + " || true"},
		Timeout: limit}, nil)
}

// leaveFabric tira a máquina da rede nomeada: o gateway já faz isso ao fechar, mas um
// SIGKILL (stop que estourou o prazo, delete --force, queda) não deixa o Close rodar, e o
// nome velho seguiria resolvendo — para quem herdar o IP depois do delete.
func (m *Machines) leaveFabric(mc domain.Machine) {
	if mc.Network.Name == "" || m.NetworksDir == "" {
		return
	}
	dir := m.fabricDir(mc.Network.Name)
	os.Remove(filepath.Join(dir, mc.Name+".ip"))
	os.Remove(filepath.Join(dir, mc.Network.Address+".sock"))
	os.Remove(dir) // só sai se vazio: a última máquina da rede leva o diretório junto
}

// Delete apaga a máquina e o disco dela. Ligada, só com force (que mata sem esperar).
func (m *Machines) Delete(ctx context.Context, name string, force bool) error {
	st, err := m.Status(name)
	if err != nil {
		return err
	}
	if st.State == StateRunning || st.State == StatePaused {
		if !force {
			return domain.ErrMachineState(name, "is running; stop it first or use force")
		}
		m.Procs.Kill(st.PID) // SIGKILL vale também para processo parado por SIGSTOP
		m.waitExit(st.PID, killWait)
	}
	m.leaveFabric(st.Machine)
	if m.Persist != nil && m.Persist.Mounted(m.Store.Dir(name)) {
		// O diretório é o ponto de montagem: apagar os arquivos deixaria o disco cobrando vazio. Destroy desmonta,
		// solta e apaga o disco inteiro.
		return m.Persist.Destroy(ctx, name, m.Store.Dir(name))
	}
	return m.Store.Delete(name)
}

// killWait é quanto o Delete com force espera o grupo do vmm sair depois do SIGKILL. O sinal só pede a morte: a VM
// devolve gigas de memória e fecha os arquivos do diretório da máquina antes de sumir, e soltar o disco antes disso
// dá EBUSY no desmonte. Medido em 25/10/2026 numa n1-standard-1 (1 vCPU): o fim do trial apagou a VM ligada, o
// desmonte falhou com "device or resource busy" e a máquina ficou registrada como ligada, segurando a única vaga do
// servidor por 5 min. 30 s cobre a saída mais lenta medida com folga; passou disso, o desmonte reporta o erro.
var killWait = 30 * time.Second

// waitExit espera o processo sair, até o prazo.
func (m *Machines) waitExit(pid int, limit time.Duration) {
	for end := time.Now().Add(limit); time.Now().Before(end) && m.Procs.Alive(pid); {
		time.Sleep(vmmPoll)
	}
}

// isDraining diz se o host recebeu o aviso de preempção.
func (m *Machines) isDraining() bool {
	m.drain.mu.Lock()
	defer m.drain.mu.Unlock()
	return m.drain.active
}

// running devolve o socket do guest de uma máquina ligada, ou INVALID_STATE.
func (m *Machines) running(name string) (string, error) {
	st, err := m.Status(name)
	if err != nil {
		return "", err
	}
	if st.State == StatePaused {
		return "", domain.ErrMachineState(name, "is paused; resume it first")
	}
	if st.State != StateRunning {
		return "", domain.ErrMachineState(name, "is not running; start it first")
	}
	return filepath.Join(m.Store.Dir(name), machineSocket), nil
}

// Exec roda o argv na máquina. Com onOutput, a saída chega enquanto o programa roda.
func (m *Machines) Exec(ctx context.Context, name string, req ExecRequest, onOutput func(stream, data string)) (domain.Result, error) {
	if len(req.Argv) == 0 || req.Argv[0] == "" {
		return domain.Result{}, domain.ValidationError{Err: domain.MachineError{Code: domain.CodeConfig, Msg: "exec needs a command"}}
	}
	sock, err := m.running(name)
	if err != nil {
		return domain.Result{}, err
	}
	mc, _ := m.Store.Load(name)
	job := protocol.Job{TraceID: "exec-" + name, Op: protocol.OpExec, Argv: req.Argv, Env: append(jobEnv(mc), req.Env...), Cwd: req.Cwd,
		User: pick(req.User, mc.User)}
	if job.Cwd == "" {
		job.Cwd = mc.Workdir
	}
	if req.Stdin != "" {
		job.Stdin = req.Stdin
	}
	runCtx := ctx
	if req.Timeout > 0 {
		job.Timeout = int((req.Timeout + time.Second - 1) / time.Second)
		var cancel context.CancelFunc
		runCtx, cancel = context.WithTimeout(ctx, req.Timeout+timeoutGrace)
		defer cancel()
	}
	start := time.Now()
	var res protocol.Result
	if onOutput != nil {
		res, err = m.Guest.RunStream(runCtx, sock, job, onOutput)
	} else {
		res, err = m.Guest.Run(runCtx, sock, job)
	}
	if err != nil {
		return domain.Result{}, fmt.Errorf("exec in machine %s: %w", name, err)
	}
	return fromProtocol(job.TraceID, res, time.Since(start)), nil
}

// CopyIn grava r em guestPath na máquina, em pedaços de 1 MiB. O guest só troca o
// arquivo no último pedaço, então uma cópia interrompida nunca deixa o destino pela metade.
func (m *Machines) CopyIn(ctx context.Context, name, guestPath string, r io.Reader, mode os.FileMode) (int64, error) {
	p, err := domain.GuestPath(guestPath)
	if err != nil {
		return 0, err
	}
	sock, err := m.running(name)
	if err != nil {
		return 0, err
	}
	// Lê um pedaço à frente: só assim se sabe que o pedaço atual é o último (Final).
	cur := make([]byte, protocol.ChunkBytes)
	n, rerr := io.ReadFull(r, cur)
	var off int64
	for {
		if rerr != nil && rerr != io.EOF && rerr != io.ErrUnexpectedEOF {
			return off, rerr
		}
		final := rerr != nil
		var next []byte
		var nn int
		if !final {
			next = make([]byte, protocol.ChunkBytes)
			nn, rerr = io.ReadFull(r, next)
			final = nn == 0 && (rerr == io.EOF || rerr == io.ErrUnexpectedEOF)
		}
		if off+int64(n) > protocol.MaxCopyBytes {
			return off, fmt.Errorf("copy: total_size exceeds maximum of %d bytes", int64(protocol.MaxCopyBytes))
		}
		res, err := m.Guest.Run(ctx, sock, protocol.Job{TraceID: "put", Op: protocol.OpPut, Path: p,
			Offset: off, Data: cur[:n], Final: final, Mode: uint32(mode.Perm())})
		if err != nil {
			return off, fmt.Errorf("copy to %s:%s: %w", name, p, err)
		}
		if res.Status != "success" {
			return off, errors.New(res.Error)
		}
		off += int64(n)
		if final {
			return off, nil
		}
		cur, n = next, nn
	}
}

// CopyOut escreve em w o arquivo guestPath da máquina, pedindo pedaços até o tamanho
// que o guest informa.
func (m *Machines) CopyOut(ctx context.Context, name, guestPath string, w io.Writer) (int64, error) {
	p, err := domain.GuestPath(guestPath)
	if err != nil {
		return 0, err
	}
	sock, err := m.running(name)
	if err != nil {
		return 0, err
	}
	var off int64
	for {
		res, err := m.Guest.Run(ctx, sock, protocol.Job{TraceID: "get", Op: protocol.OpGet, Path: p, Offset: off})
		if err != nil {
			return off, fmt.Errorf("copy from %s:%s: %w", name, p, err)
		}
		if res.Status != "success" {
			return off, errors.New(res.Error)
		}
		if _, err := w.Write(res.Data); err != nil {
			return off, err
		}
		off += int64(len(res.Data))
		// Pedaço vazio antes do fim = arquivo encolheu no meio da cópia; parar evita laço.
		if off >= res.Size || len(res.Data) == 0 {
			return off, nil
		}
	}
}

// ShellRequest é uma sessão de terminal numa máquina ligada. Argv vazio abre o shell
// padrão da imagem (bash se houver, senão sh).
type ShellRequest struct {
	User string
	Argv []string
	Env  []string
	Cwd  string
	Size ports.Winsize
}

// Shell liga um terminal (PTY no guest) a in/out até o programa sair, repassando cada
// resize; devolve o código de saída do programa.
func (m *Machines) Shell(ctx context.Context, name string, req ShellRequest, in io.Reader, out io.Writer, resize <-chan ports.Winsize) (int, error) {
	sock, err := m.running(name)
	if err != nil {
		return 0, err
	}
	mc, _ := m.Store.Load(name)
	job := protocol.Job{TraceID: "shell-" + name, Argv: req.Argv, Env: append(jobEnv(mc), req.Env...), Cwd: req.Cwd, User: pick(req.User, mc.User),
		Rows: req.Size.Rows, Cols: req.Size.Cols}
	if job.Cwd == "" {
		job.Cwd = mc.Workdir
	}
	code, err := m.Guest.Interactive(ctx, sock, job, in, out, resize)
	if err != nil {
		return 0, fmt.Errorf("shell in machine %s: %w", name, err)
	}
	return code, nil
}

// ListeningPorts lista as portas TCP em LISTEN no guest que o host alcança pelo loopback de lá (o terminal web as
// encaminha). Lê /proc/net/tcp e tcp6 com cat, que existe em qualquer imagem; o tcp6 ausente (kernel sem IPv6) não é erro.
func (m *Machines) ListeningPorts(ctx context.Context, name string) ([]uint16, error) {
	res, err := m.Exec(ctx, name, ExecRequest{User: "root", Argv: []string{"sh", "-c", "cat /proc/net/tcp /proc/net/tcp6 2>/dev/null"}}, nil)
	if err != nil {
		return nil, err
	}
	return domain.ParseListeningPorts(res.Stdout), nil
}

// DialPort abre uma conexão até 127.0.0.1:port dentro da máquina ligada, pelo vsock (sem precisar de rede).
func (m *Machines) DialPort(ctx context.Context, name string, port uint16) (net.Conn, error) {
	sock, err := m.running(name)
	if err != nil {
		return nil, err
	}
	return m.Guest.DialPort(ctx, sock, port)
}

// Fontes do console aceitas pelo Logs: tudo, ou só o workload do runner.code.
const (
	LogSourceAll      = "all"
	LogSourceWorkload = "workload"
)

// workloadPrefix é a marca que o guest põe em cada linha do workload (internal/guest).
const workloadPrefix = "[workload] "

// Logs devolve as últimas linhas do console da máquina (o guest e o que o machine.sh
// escreve), legível com a máquina parada. tail 0 devolve tudo. source "workload" deixa só
// as linhas do workload, sem a marca; o tail conta depois do filtro.
func (m *Machines) Logs(name string, tail int, source string) (string, error) {
	if source != "" && source != LogSourceAll && source != LogSourceWorkload {
		return "", domain.ValidationError{Err: domain.MachineError{Code: domain.CodeConfig,
			Msg: fmt.Sprintf("unknown log source %q (use all or workload)", source)}}
	}
	if _, err := m.Store.Load(name); err != nil {
		return "", err
	}
	raw, err := os.ReadFile(filepath.Join(m.Store.Dir(name), machineConsole))
	if errors.Is(err, os.ErrNotExist) {
		return "", nil
	}
	if err != nil {
		return "", err
	}
	lines := strings.SplitAfter(string(raw), "\n")
	if lines[len(lines)-1] == "" {
		lines = lines[:len(lines)-1]
	}
	if source == LogSourceWorkload {
		kept := lines[:0]
		for _, l := range lines {
			if strings.HasPrefix(l, workloadPrefix) {
				// O console da VM é um terminal: a quebra chega como \r\n. Quem pede só o
				// workload quer as linhas como o programa as escreveu.
				l = strings.TrimPrefix(l, workloadPrefix)
				if strings.HasSuffix(l, "\r\n") {
					l = strings.TrimSuffix(l, "\r\n") + "\n"
				}
				kept = append(kept, l)
			}
		}
		lines = kept
	}
	if tail <= 0 {
		return strings.Join(lines, ""), nil
	}
	if len(lines) > tail {
		lines = lines[len(lines)-tail:]
	}
	return strings.Join(lines, ""), nil
}

// pick devolve o primeiro valor não vazio (o do pedido vence o da máquina).
func pick(a, b string) string {
	if a != "" {
		return a
	}
	return b
}
