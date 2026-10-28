// Package ports define os contratos entre o núcleo (app) e o mundo de fora: lançar VM,
// falar com o guest, achar a imagem de uma linguagem. Os adapters implementam; o app
// só conhece estas interfaces.
package ports

import (
	"context"
	"errors"
	"fmt"
	"io"
	"net"
	"strings"
	"syscall"

	"runner.codes/internal/domain"
	"runner.codes/internal/protocol"
)

// VMSocket liga um socket Unix do host (Host, no diretório da máquina) a um socket Unix de
// dentro do guest (Guest): quem conecta no host fala com quem escuta no guest, pelo vsock.
type VMSocket struct {
	Host  string `json:"host"`
	Guest string `json:"guest"`
	// Port é a porta vsock de um socket do host montado no guest (HostSockets); 0 nos
	// sockets do guest expostos no host (Sockets), que vão pelo socket do vsock da máquina.
	Port uint32 `json:"port,omitempty"`
}

// VMSpec descreve uma VM. Vai em JSON do serviço para o processo vmm.
type VMSpec struct {
	// TiedToCaller prende a VM de uma máquina ao processo que a ligou (pipe de vigia): se ele
	// morrer, até por SIGKILL, a VM morre junto. É o `machine run` em primeiro plano — código
	// não confiável não pode seguir rodando sem dono. Máquina persistente não usa. Só o
	// launcher lê; não vai para o arquivo da spec.
	TiedToCaller bool `json:"-"`
	// Sockets são as pontes de socket Unix host → guest (o docker.sock).
	Sockets []VMSocket `json:"sockets,omitempty"`
	// HostSockets são os sockets Unix do host que o guest alcança (--mount-socket): a libkrun
	// disca Host a cada conexão que o guest abre na porta vsock Port.
	HostSockets []VMSocket `json:"host_sockets,omitempty"`
	CPUs        uint8      `json:"cpus"`
	MemoryMiB   uint32     `json:"memory_mib"`
	// RootDisk é um ext4 montado read-only como /. Usado pelas VMs de job.
	RootDisk string `json:"root_disk,omitempty"`
	// RootDir é um diretório compartilhado por virtiofs como /. Só a VM de build usa,
	// porque virtiofs não impede o guest de alcançar o resto do host (doc da libkrun).
	RootDir string `json:"root_dir,omitempty"`
	// DataDisk é o ext4 gravável de uma máquina persistente, visto como /dev/vdb no guest
	// (machine.sh monta o overlay sobre a imagem com ele). Só com RootDisk.
	DataDisk    string `json:"data_disk,omitempty"`
	VsockSocket string `json:"vsock_socket,omitempty"`
	VsockPort   uint32 `json:"vsock_port,omitempty"`
	// SSHAgent é o SSH_AUTH_SOCK do host (de quem deu o start): a libkrun liga a porta vsock
	// SSHAgentPort do guest a ele, e o guest repassa um socket Unix de dentro por ali. As
	// chaves ficam no agente do host; o guest só pede assinaturas.
	SSHAgent string `json:"ssh_agent,omitempty"`
	Network  bool   `json:"network,omitempty"`
	Console  string `json:"console,omitempty"`
	// TTY liga o stdin/stdout/stderr do processo vmm a uma porta de console virtio no
	// guest (/dev/hvc0), que o guest enxerga como terminal: Ctrl+C, job control e
	// tamanho da janela funcionam. Só faz sentido com um terminal de verdade no host.
	TTY bool `json:"tty,omitempty"`
	// GPU liga um virtio-gpu com Venus: o guest vê um device Vulkan servido pela GPU do
	// host (MoltenVK no macOS). GPUVRAMMiB é a janela de memória do host que o guest usa
	// como VRAM; 0 usa o padrão do adapter.
	GPU        bool   `json:"gpu,omitempty"`
	GPUVRAMMiB uint32 `json:"gpu_vram_mib,omitempty"`
	// Nested expõe as extensões de virtualização do host ao guest (/dev/kvm lá dentro).
	// Precisa de KVM aninhado no Linux ou de um Mac M3+ com macOS 15+.
	Nested bool `json:"nested,omitempty"`
	// Mounts são diretórios do host expostos por virtio-fs, cada um com a sua tag; o guest
	// monta pela tag. Só as máquinas usam (o job efêmero não vê nada do host).
	Mounts []VMMount `json:"mounts,omitempty"`
	// Net liga uma virtio-net ao gateway em userspace do processo vmm (internal/adapters/
	// netgw): NAT com a política de saída e as portas publicadas. Exclui Network (TSI, que
	// só a VM de build usa, porque o TSI não aplica filtro nenhum).
	Net *VMNet `json:"net,omitempty"`
	// PodNetns é o netns de um pod do Kubernetes (o caminho que o CNI criou, ex.
	// /var/run/netns/cni-...): o processo vmm liga a virtio-net a um TAP ali dentro e o guest
	// assume o IP e o MAC da eth0 do pod (internal/adapters/podnet). Exclui Net e Network.
	PodNetns string `json:"pod_netns,omitempty"`
	// NetMAC é o MAC da virtio-net quando não é o do gateway (o da eth0 do pod). Só o processo
	// vmm preenche, depois de ler o netns; não vai para o arquivo da spec.
	NetMAC []byte `json:"-"`
	// Display liga uma saída de vídeo ao virtio-gpu (exige GPU) e um visualizador no
	// navegador servido pelo processo vmm.
	Display *VMDisplay `json:"display,omitempty"`
	Exec    string     `json:"exec"`
	Args    []string   `json:"args,omitempty"`
	Env     []string   `json:"env,omitempty"`
	// UID e GID trocam o usuário do processo vmm antes do boot (Linux). Nil mantém o atual.
	UID *uint32 `json:"uid,omitempty"`
	GID *uint32 `json:"gid,omitempty"`
	// SnapshotDir é onde o processo vmm grava o snapshot da VM ao receber SIGUSR2 (suspender): vCPUs, devices e
	// RAM. Fica no diretório da máquina, que mora no disco próprio dela — o snapshot viaja com o disco para o host
	// novo. Vazio: a VM não suspende (só desliga).
	SnapshotDir string `json:"snapshot_dir,omitempty"`
	// RestoreFrom é um snapshot a restaurar em vez de dar boot: a VM volta no ponto em que foi suspensa. A forma
	// (CPUs, memória, devices) tem de ser a mesma do snapshot — a libkrun recusa outra.
	RestoreFrom string `json:"restore_from,omitempty"`
}

// VMNet é a rede de uma máquina: a declaração do domínio mais onde o gateway grava o log
// de recusas e escuta a troca de política com a máquina ligada.
type VMNet struct {
	Network domain.Network `json:"network"`
	// DenialsLog recebe uma linha por conexão ou nome negado.
	DenialsLog string `json:"denials_log,omitempty"`
	// ControlSocket aceita uma política nova (JSON de domain.Egress) com a máquina ligada.
	ControlSocket string `json:"control_socket,omitempty"`
	// FabricDir é o diretório da rede nomeada (sockets e nomes das máquinas ligadas nela);
	// Machine é o nome desta máquina, publicado no DNS da rede.
	FabricDir string `json:"fabric_dir,omitempty"`
	Machine   string `json:"machine,omitempty"`
	// CACert e CAKey são a CA de credenciais da máquina (arquivos no diretório dela): o
	// gateway termina o TLS dos hosts das bindings com certificados assinados por ela.
	CACert string `json:"ca_cert,omitempty"`
	CAKey  string `json:"ca_key,omitempty"`
	// EgressProxy é o proxy de saída do host (host:porta, RC_EGRESS_PROXY do rc): toda conexão TCP permitida vai a
	// ele com um header PROXY v2 (destino e máquina), e ele registra e repassa. Vazio: o gateway disca o destino
	// direto, como sempre.
	EgressProxy string `json:"egress_proxy,omitempty"`
	// EgressCA é o certificado (PEM) da CA do proxy de saída: com credenciais, o gateway troca o token falando TLS com
	// o proxy, que apresenta um certificado dessa CA — sem ela no cofre, a troca recusaria o proxy.
	EgressCA string `json:"egress_ca,omitempty"`
}

// VMDisplay é a tela de uma máquina: tamanho e onde o visualizador escuta (host:porta,
// sempre no loopback por padrão).
type VMDisplay struct {
	Width  uint32 `json:"width"`
	Height uint32 `json:"height"`
	Listen string `json:"listen"`
	// Title é o nome na barra da página do visualizador; vazio usa o padrão.
	Title string `json:"title,omitempty"`
}

// VMMount é um device virtio-fs: Tag identifica no guest, HostPath é o diretório exposto e
// ReadOnly faz a libkrun recusar escrita no próprio device.
type VMMount struct {
	Tag      string `json:"tag"`
	HostPath string `json:"host_path"`
	ReadOnly bool   `json:"read_only,omitempty"`
}

// Validate recusa spec que a libkrun aceitaria mas que quebraria em silêncio dentro do guest.
func (s VMSpec) Validate() error {
	if s.CPUs == 0 || s.MemoryMiB == 0 {
		return errors.New("spec: cpus and memory_mib must be greater than zero")
	}
	if (s.RootDisk == "") == (s.RootDir == "") {
		return errors.New("spec: set exactly one of root_disk and root_dir")
	}
	if s.Exec == "" {
		return errors.New("spec: exec is required")
	}
	if s.TTY && s.Console != "" {
		// O console vira o terminal: gravar em arquivo ao mesmo tempo não tem destino.
		return errors.New("spec: tty and console are mutually exclusive")
	}
	if s.DataDisk != "" && s.RootDisk == "" {
		// /dev/vdb só existe ao lado de um root em disco; com virtiofs a ordem dos discos muda.
		return errors.New("spec: data_disk requires root_disk")
	}
	if s.GPUVRAMMiB > 0 && !s.GPU {
		// VRAM sem GPU é erro de quem montou a spec, não algo a ignorar.
		return errors.New("spec: gpu_vram_mib requires gpu")
	}
	if (s.VsockSocket == "") != (s.VsockPort == 0) {
		return errors.New("spec: vsock_socket and vsock_port must be set together")
	}
	if s.PodNetns != "" && (s.Net != nil || s.Network || !strings.HasPrefix(s.PodNetns, "/")) {
		// Duas virtio-net (ou TSI junto) dariam ao guest uma saída fora da rede do pod.
		return errors.New("spec: pod_netns needs an absolute path and excludes net and network")
	}
	if s.Net != nil {
		if s.Network {
			return errors.New("spec: net (gateway) and network (TSI) are mutually exclusive")
		}
		if err := s.Net.Network.Validate(); err != nil {
			return fmt.Errorf("spec: %w", err)
		}
	}
	if s.Display != nil {
		if !s.GPU {
			// A saída de vídeo é do virtio-gpu: sem GPU a libkrun nem cria o device.
			return errors.New("spec: display requires gpu")
		}
		if s.Display.Width == 0 || s.Display.Height == 0 || s.Display.Width > 7680 || s.Display.Height > 4320 || s.Display.Listen == "" {
			return fmt.Errorf("spec: display %dx%d on %q is invalid (up to 7680x4320, listen required)", s.Display.Width, s.Display.Height, s.Display.Listen)
		}
	}
	tags := map[string]bool{}
	for _, m := range s.Mounts {
		// Tag repetida faria o guest montar o diretório errado sem erro nenhum.
		if m.Tag == "" || tags[m.Tag] || !strings.HasPrefix(m.HostPath, "/") {
			return fmt.Errorf("spec: mount %+v needs a unique tag and an absolute host path", m)
		}
		tags[m.Tag] = true
	}
	// A libkrun leva o argv pela linha de comando do kernel: argumento com espaço
	// chega partido no guest (medido no spike). Comando composto vai por arquivo.
	for _, a := range append([]string{s.Exec}, s.Args...) {
		if a == "" || strings.ContainsAny(a, " \t\n") {
			return fmt.Errorf("spec: argument %q is empty or contains whitespace; pass the command through a file in the image", a)
		}
	}
	return nil
}

// VM é uma microVM viva, um processo por VM.
type VM interface {
	PID() int
	// Done fecha quando o processo da VM termina.
	Done() <-chan struct{}
	// Wait espera o fim e devolve o código de saída do workload.
	Wait() int
	// Kill derruba a VM na hora (SIGKILL no grupo). Idempotente.
	Kill()
}

// VMLauncher cria VMs.
type VMLauncher interface {
	StartVM(spec VMSpec) (VM, error)
}

// GuestClient conversa com o guest-runner de uma VM pelo socket do vsock.
type GuestClient interface {
	Run(ctx context.Context, socket string, job protocol.Job) (protocol.Result, error)
	// RunStream é o Run com a saída entregue a onOutput enquanto o programa roda; o
	// Result final é o mesmo do Run.
	RunStream(ctx context.Context, socket string, job protocol.Job, onOutput func(stream, data string)) (protocol.Result, error)
	// Interactive abre uma sessão de terminal (OpPTY): repassa in ao programa, escreve a
	// saída em out, manda cada tamanho de resize e devolve o código de saída do programa.
	Interactive(ctx context.Context, socket string, job protocol.Job, in io.Reader, out io.Writer, resize <-chan Winsize) (int, error)
	WaitReady(ctx context.Context, socket string) error
	// DialPort abre uma conexão TCP até 127.0.0.1:port dentro do guest, pelo vsock: funciona sem a máquina ter rede.
	DialPort(ctx context.Context, socket string, port uint16) (net.Conn, error)
}

// Winsize é o tamanho de um terminal em linhas e colunas.
type Winsize struct{ Rows, Cols uint16 }

// MachineStore guarda o registro das máquinas persistentes e o diretório de cada uma
// (disco, spec, socket, logs). Erros de ausência e de nome repetido vêm com os códigos
// do domínio (NOT_FOUND, CONFLICT).
type MachineStore interface {
	// Create registra a máquina e dá a ela um disco clonado do modelo vazio.
	Create(m domain.Machine, diskTemplate string) error
	Load(name string) (domain.Machine, error)
	// Save regrava o registro de uma máquina que já existe (ex.: política de rede nova).
	Save(m domain.Machine) error
	List() ([]domain.Machine, error)
	Delete(name string) error
	// Dir é o diretório da máquina; os arquivos dela moram ali.
	Dir(name string) string
	// PID é o processo vmm registrado (0 se nenhum); SetPID grava ou limpa (0).
	PID(name string) int
	SetPID(name string, pid int) error
}

// DetachedLauncher sobe a VM de uma máquina desligada do processo que pediu.
type DetachedLauncher interface {
	StartDetached(spec VMSpec, specPath, logPath string) (pid int, err error)
}

// Processes olha e mata o processo vmm de uma máquina pelo pid registrado.
type Processes interface {
	Alive(pid int) bool
	// Kill manda SIGKILL ao grupo do processo (o vmm e o que ele criou).
	Kill(pid int)
	// Pause congela o grupo do processo (as vCPUs param onde estão); Resume continua.
	Pause(pid int) error
	Resume(pid int) error
	// Suspend pede ao processo vmm (só a ele, não ao grupo) para gravar o snapshot da VM e sair: SuspendSignal.
	Suspend(pid int) error
}

// SuspendSignal é o sinal que pede ao processo vmm para suspender a VM (gravar o snapshot e sair). SIGUSR2: o SIGTERM
// já é "desligar" e o SIGSTOP é o Pause, que congela sem gravar nada.
const SuspendSignal = syscall.SIGUSR2

// ImageStore diz onde está o ext4 pronto de uma linguagem.
type ImageStore interface {
	ImagePath(language string) (string, bool)
}
