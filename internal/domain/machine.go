package domain

import (
	"errors"
	"fmt"
	"path"
	"regexp"
	"strconv"
	"strings"
	"time"
)

// Padrões de uma máquina persistente quando o pedido não diz, com disco
// pequeno (10 GiB) porque a imagem da stack já vem pronta e read-only.
const (
	// 4 vCPUs e 8 GiB: a memória da libkrun é elástica (o host só compromete o
	// que o guest toca) e vCPU ociosa dorme no hypervisor, então sobrar custa quase nada, e
	// faltar faz o pip/npm/cargo do init rodar em câmera lenta.
	DefaultMachineCPUs      = 4
	DefaultMachineMemoryMiB = 8192
	DefaultMachineDiskGiB   = 10
	MaxMachineDiskGiB       = 1024
)

// machineName casa nome de máquina: minúsculas, dígitos e hífen, começando por letra ou
// dígito, até 63 caracteres. Vira nome de diretório e aparece em URL, então nada de
// barra, ponto ou espaço.
var machineName = regexp.MustCompile(`^[a-z0-9][a-z0-9-]{0,62}$`)

// userSpec é um nome de usuário do passwd (letras, dígitos, _ e -, com $ no fim para contas
// de máquina do Samba) ou "uid[:gid]" numérico.
var userSpec = regexp.MustCompile(`^([a-z_][a-z0-9_-]{0,31}\$?|[0-9]+(:[0-9]+)?)$`)

// ValidateName confere um nome de máquina sozinho (o pack deriva o nome do arquivo de saída).
func ValidateName(n string) error {
	if !machineName.MatchString(n) {
		return fmt.Errorf("invalid machine name %q (lowercase letters, digits and '-', up to 63)", n)
	}
	return nil
}

// Machine é uma máquina persistente: VM com nome, disco próprio e ciclo de vida
// (create, start, exec, stop, delete), sobre a imagem read-only de uma stack.
type Machine struct {
	Name      string    `json:"name"`
	Stack     string    `json:"stack"`
	CPUs      uint8     `json:"cpus"`
	MemoryMiB uint32    `json:"memory_mib"`
	DiskGiB   int       `json:"disk_gib"`
	GPU       bool      `json:"gpu,omitempty"`
	CreatedAt time.Time `json:"created_at"`

	// Image é a imagem OCI de origem quando a máquina não vem de uma stack do catálogo
	// (`image` no runner.code); o Stack passa a ser o nome da imagem construída a partir dela.
	Image string `json:"image,omitempty"`
	// Env e Workdir valem para o workload, o init e o exec/shell da máquina.
	Env     []string `json:"env,omitempty"`
	Workdir string   `json:"workdir,omitempty"`
	// User é quem roda o workload, o exec e o shell: nome do passwd do guest ou "uid[:gid]".
	// O init roda sempre como root (é ele que costuma criar o usuário).
	User string `json:"user,omitempty"`
	// Init roda uma vez, como root, no primeiro start (cada item via `sh -c`).
	Init []string `json:"init,omitempty"`
	// Workload é o processo que a máquina mantém rodando desde o start (entrypoint + cmd).
	Workload []string `json:"workload,omitempty"`
	// StopSignal vai ao workload no stop; StopTimeout é quanto ele tem para sair antes do
	// SIGKILL; StopOnExit desliga a máquina quando o workload sai.
	StopSignal  string `json:"stop_signal,omitempty"`
	StopTimeout int    `json:"stop_timeout,omitempty"`
	StopOnExit  bool   `json:"stop_on_exit,omitempty"`
	// DockerSocket liga o /var/run/docker.sock do guest a <diretório da máquina>/docker.sock no
	// host (pelo vsock): o docker do host fala com o daemon de dentro, e os contêineres ficam
	// no kernel do guest. Não é montar o socket do host na máquina, que entregaria o host.
	DockerSocket bool `json:"docker_socket,omitempty"`
	// ExposeSockets publicam no host um socket Unix que o workload escuta (--expose-socket);
	// MountSockets põem no guest um socket do host (--mount-socket). Só pela CLI: pela API
	// seriam caminhos arbitrários no disco do servidor.
	ExposeSockets []SocketBridge `json:"expose_sockets,omitempty"`
	MountSockets  []SocketBridge `json:"mount_sockets,omitempty"`
	// SSHAgent encaminha o agente SSH de quem dá o start (SSH_AUTH_SOCK): git e ssh na
	// máquina usam as chaves do host, que nunca entram nela.
	SSHAgent bool `json:"ssh_agent,omitempty"`
	// Nested dá ao guest as extensões de virtualização do host, para rodar um hypervisor
	// lá dentro. Desligado por padrão: o guest aninhado é bem mais lento.
	Nested bool `json:"nested,omitempty"`
	// Secrets são os segredos resolvidos no host a cada start (nunca gravados no registro com o
	// valor: aqui só a fonte).
	Secrets []Secret `json:"secrets,omitempty"`
	// GPUVRAMMiB é a VRAM do guest (gpu_vram); 0 usa a da stack no catálogo.
	GPUVRAMMiB uint32 `json:"gpu_vram_mib,omitempty"`
	// Mounts são diretórios do host montados na máquina por virtiofs a cada start (a chave
	// `volumes` do runner.code e da API).
	Mounts []Mount `json:"volumes,omitempty"` // contract:ok — chave do runner.code e da API
	// Network é a rede da máquina: desligada por padrão; com rede, portas ou política, a
	// máquina ganha uma virtio-net atrás do gateway do host.
	Network Network `json:"network,omitempty"`
	// Display é a tela da máquina, vista no navegador (exige GPU); nil sem tela.
	Display *Display `json:"display,omitempty"`
}

// Display é a saída de vídeo de uma máquina: tamanho e a porta do visualizador no
// 127.0.0.1 do host.
type Display struct {
	Width  uint32 `json:"width"`
	Height uint32 `json:"height"`
	Port   uint16 `json:"port"`
	// Title é o nome na barra da página do visualizador (e da aba); vazio usa o padrão do visualizador.
	Title string `json:"title,omitempty"`
}

// DefaultDisplayPort é a porta padrão do visualizador (a do noVNC, conhecida de quem já
// abriu um desktop remoto no navegador).
const DefaultDisplayPort = 6080

// ParseDisplay lê "LARGURAxALTURA" (ex. 1280x800).
func ParseDisplay(s string) (Display, error) {
	w, h, ok := strings.Cut(strings.ToLower(s), "x")
	wn, err1 := strconv.ParseUint(w, 10, 32)
	hn, err2 := strconv.ParseUint(h, 10, 32)
	if !ok || err1 != nil || err2 != nil || wn < 320 || hn < 200 || wn > 7680 || hn > 4320 {
		return Display{}, fmt.Errorf("display %q must be WIDTHxHEIGHT between 320x200 and 7680x4320", s)
	}
	return Display{Width: uint32(wn), Height: uint32(hn), Port: DefaultDisplayPort}, nil
}

// Mount é um diretório do host visto dentro da máquina. Host chega absoluto (quem lê a
// declaração resolve "./" e "~" antes); ReadOnly é imposto pela libkrun no device, não por
// uma opção de montagem que o root do guest poderia desfazer.
type Mount struct {
	Host     string `json:"host"`
	Guest    string `json:"guest"`
	ReadOnly bool   `json:"read_only,omitempty"`
	// Staged copia Host para o disco da máquina no primeiro start e trabalha na cópia, sem ida
	// e volta de virtio-fs a cada arquivo; `rc machine sync` e o stop gracioso devolvem a
	// cópia ao host (o `:staged`).
	Staged bool `json:"staged,omitempty"`
}

// MaxMounts limita as montagens por máquina: cada uma é um device virtio-fs com a sua fila,
// e 16 cobre qualquer exemplo real com folga sem deixar uma declaração esgotar os slots.
const MaxMounts = 16

// reservedGuestPaths são caminhos que a máquina precisa para funcionar: montar por cima
// deles esconde o guest (/opt/runner), o kernel (/proc, /sys, /dev) ou o próprio disco.
var reservedGuestPaths = []string{"/", "/proc", "/sys", "/dev", "/opt/runner", "/persist", "/tmp"}

// ParseMount lê "host:guest", "host:guest:ro" ou "host:guest:rw", o formato
// do Docker. Não confere o host no disco: isso é do adapter.
func ParseMount(s string) (Mount, error) {
	// Só um caminho absoluto do guest: volume de disco (no disco da máquina, fora do overlay),
	// como `-v /var/lib/docker`.
	if strings.HasPrefix(s, "/") && !strings.Contains(s, ":") {
		return Mount{Guest: s}, nil
	}
	// O "s3://" tem dois-pontos próprios: sai antes do corte e volta no Host.
	scheme := ""
	if strings.HasPrefix(s, s3Scheme) {
		scheme, s = s3Scheme, s[len(s3Scheme):]
	}
	parts := strings.Split(s, ":")
	var v Mount
	switch {
	case len(parts) == 3 && (parts[2] == "ro" || parts[2] == "rw"):
		v.ReadOnly = parts[2] == "ro"
	case len(parts) == 3 && parts[2] == "staged" && scheme == "":
		v.Staged = true
	case len(parts) != 2:
		return v, fmt.Errorf("mount %q must be HOST:GUEST[:ro|:rw|:staged]", s)
	}
	v.Host, v.Guest = scheme+parts[0], parts[1]
	if parts[0] == "" || v.Guest == "" {
		return v, fmt.Errorf("mount %q must be HOST:GUEST[:ro|:rw|:staged]", s)
	}
	return v, nil
}

// s3Scheme marca o volume que é um bucket (S3, R2, MinIO) em vez de um diretório do host.
const s3Scheme = "s3://"

// bucketName é a regra de nome de bucket do S3: 3 a 63 caracteres, minúsculas, dígitos,
// ponto e hífen, começando e terminando em letra ou dígito.
var bucketName = regexp.MustCompile(`^[a-z0-9][a-z0-9.-]{1,61}[a-z0-9]$`)

// S3 diz se a montagem é um bucket (s3://BUCKET[/PREFIX]) e devolve bucket e prefixo.
func (v Mount) S3() (bucket, prefix string, ok bool) {
	rest, ok := strings.CutPrefix(v.Host, s3Scheme)
	if !ok {
		return "", "", false
	}
	bucket, prefix, _ = strings.Cut(rest, "/")
	return bucket, strings.Trim(prefix, "/"), true
}

// Disk diz se é um volume de disco: um diretório no disco ext4 da máquina, ligado ao
// caminho do guest a cada boot. Sobrevive a stop/start e fica fora do overlay (o Docker
// precisa de um ext4 de verdade em /var/lib/docker).
func (v Mount) Disk() bool { return v.Host == "" }

// DiskPath é onde um volume de disco (e a cópia de um volume staged) mora no disco da
// máquina: o mesmo lugar para os dois, e assim a filha de um branch herda a cópia staged como
// um volume de disco comum.
func (v Mount) DiskPath() string { return "/workspace/.disk" + path.Clean(v.Guest) }

// String devolve a montagem no formato de entrada.
func (v Mount) String() string {
	if v.Disk() {
		return v.Guest
	}
	s := v.Host + ":" + v.Guest
	if v.ReadOnly {
		s += ":ro"
	}
	if v.Staged {
		s += ":staged"
	}
	return s
}

// validateMounts recusa montagem com caminho relativo, destino reservado, destino repetido
// ou acima do teto.
func validateMounts(vs []Mount) error {
	if len(vs) > MaxMounts {
		return fmt.Errorf("at most %d mounts", MaxMounts)
	}
	seen := map[string]bool{}
	for _, v := range vs {
		if bucket, _, ok := v.S3(); ok {
			if !bucketName.MatchString(bucket) {
				return fmt.Errorf("mount %s: %q is not a valid bucket name", v, bucket)
			}
		} else if !v.Disk() && !path.IsAbs(v.Host) {
			return fmt.Errorf("mount %s: host and guest paths must be absolute", v)
		}
		if !path.IsAbs(v.Guest) {
			return fmt.Errorf("mount %s: host and guest paths must be absolute", v)
		}
		g := path.Clean(v.Guest)
		for _, r := range reservedGuestPaths {
			if g == r || (r != "/" && strings.HasPrefix(g, r+"/")) {
				return fmt.Errorf("mount %s: %s is reserved inside the machine", v, r)
			}
		}
		if seen[g] {
			return fmt.Errorf("mount %s: %s is mounted twice", v, g)
		}
		seen[g] = true
	}
	return nil
}

// MaxStopTimeout é o teto do stop_timeout (300 s): o stop não pode
// prender a máquina indefinidamente.
const MaxStopTimeout = 300

// stopSignals são os nomes de sinal aceitos no stop_signal, com a numeração do Linux (o
// guest), que não é a do host macOS: a tabela é fixa aqui e nunca sai da libc de quem valida.
// Número também vale (1–64).
var stopSignals = map[string]int{
	"SIGHUP": 1, "SIGINT": 2, "SIGQUIT": 3, "SIGILL": 4, "SIGTRAP": 5, "SIGABRT": 6, "SIGBUS": 7,
	"SIGFPE": 8, "SIGKILL": 9, "SIGUSR1": 10, "SIGSEGV": 11, "SIGUSR2": 12, "SIGPIPE": 13,
	"SIGALRM": 14, "SIGTERM": 15, "SIGSTKFLT": 16, "SIGCHLD": 17, "SIGCONT": 18, "SIGSTOP": 19,
	"SIGTSTP": 20, "SIGTTIN": 21, "SIGTTOU": 22, "SIGURG": 23, "SIGXCPU": 24, "SIGXFSZ": 25,
	"SIGVTALRM": 26, "SIGPROF": 27, "SIGWINCH": 28, "SIGIO": 29, "SIGPWR": 30, "SIGSYS": 31,
}

// sigRTMin é o primeiro sinal de tempo real como a glibc numera (34): é o que o
// `STOPSIGNAL SIGRTMIN+3` de uma imagem com systemd quer dizer (37).
const sigRTMin, sigRTMax = 34, 64

// SignalNumber traduz o stop_signal ("SIGINT", "INT", "2") para o número do sinal no
// guest Linux; vazio é SIGTERM.
func SignalNumber(s string) (int, error) {
	if s == "" {
		return 15, nil
	}
	name := strings.ToUpper(strings.TrimSpace(s))
	if !strings.HasPrefix(name, "SIG") {
		name = "SIG" + name
	}
	if n, ok := stopSignals[name]; ok {
		return n, nil
	}
	if off, ok := strings.CutPrefix(name, "SIGRTMIN"); ok {
		n := sigRTMin
		if off != "" {
			// Só "+N" em decimal: "SIGRTMIN3" e "SIGRTMIN-1" são erro de digitação, não sinal.
			k, err := strconv.Atoi(strings.TrimPrefix(off, "+"))
			if !strings.HasPrefix(off, "+") || err != nil || k < 0 {
				return 0, fmt.Errorf("unknown stop_signal %q (use SIGRTMIN+N)", s)
			}
			n += k
		}
		if n <= sigRTMax {
			return n, nil
		}
	}
	if n, err := strconv.Atoi(s); err == nil && n >= 1 && n <= 64 {
		return n, nil
	}
	return 0, fmt.Errorf("unknown stop_signal %q (use a name like SIGTERM or SIGRTMIN+3, or a number 1-64)", s)
}

// WithDefaults preenche CPU, memória e disco que vieram zerados.
func (m Machine) WithDefaults() Machine {
	if !m.GPU {
		// gpu_vram sem gpu é ignorado (um runner.code que desliga a GPU não
		// precisa apagar a linha da janela de memória).
		m.GPUVRAMMiB = 0
	}
	if m.CPUs == 0 {
		m.CPUs = DefaultMachineCPUs
	}
	if m.MemoryMiB == 0 {
		m.MemoryMiB = DefaultMachineMemoryMiB
	}
	if m.DiskGiB == 0 {
		m.DiskGiB = DefaultMachineDiskGiB
	}
	return m
}

// SocketBridge liga um socket Unix do host a um do guest. Host vazio num ExposeSockets é
// <diretório da máquina>/<nome do socket do guest>.
type SocketBridge struct {
	Host  string `json:"host,omitempty"`
	Guest string `json:"guest"`
}

// MaxSocketBridges é o teto de pontes de socket por máquina (64), somando as
// duas direções: cada uma é um listener no host ou uma porta vsock.
const MaxSocketBridges = 64

// ParseExposeSocket lê GUEST[:HOST] (o --expose-socket).
func ParseExposeSocket(s string) (SocketBridge, error) {
	guest, host, _ := strings.Cut(s, ":")
	b := SocketBridge{Guest: guest, Host: host}
	if !path.IsAbs(guest) || (host != "" && !path.IsAbs(host)) {
		return b, fmt.Errorf("expose socket %q must be /GUEST[:/HOST] with absolute paths", s)
	}
	return b, nil
}

// ParseMountSocket lê HOST:GUEST (o --mount-socket).
func ParseMountSocket(s string) (SocketBridge, error) {
	host, guest, ok := strings.Cut(s, ":")
	b := SocketBridge{Host: host, Guest: guest}
	if !ok || !path.IsAbs(host) || !path.IsAbs(guest) {
		return b, fmt.Errorf("mount socket %q must be /HOST:/GUEST with absolute paths", s)
	}
	return b, nil
}

// validateSockets confere o teto e que nenhum caminho do guest se repete: dois listeners no
// mesmo caminho, um apagaria o outro.
func validateSockets(m Machine) error {
	if n := len(m.ExposeSockets) + len(m.MountSockets); n > MaxSocketBridges {
		return fmt.Errorf("%d socket bridges, at most %d", n, MaxSocketBridges)
	}
	seen := map[string]bool{}
	for _, b := range append(append([]SocketBridge{}, m.ExposeSockets...), m.MountSockets...) {
		if !path.IsAbs(b.Guest) || (b.Host != "" && !path.IsAbs(b.Host)) {
			return fmt.Errorf("socket bridge %s:%s needs absolute paths", b.Host, b.Guest)
		}
		if seen[b.Guest] {
			return fmt.Errorf("guest socket %s is used twice", b.Guest)
		}
		seen[b.Guest] = true
	}
	return nil
}

// Validate recusa máquina que não pode existir; toda recusa vem como ValidationError com
// código CONFIG_ERROR.
func (m Machine) Validate() error {
	var err error
	switch {
	case ValidateName(m.Name) != nil:
		err = ValidateName(m.Name)
	case m.Stack == "":
		err = errors.New("machine needs a stack")
	case m.CPUs == 0 || m.MemoryMiB < 128:
		err = fmt.Errorf("invalid resources (cpus=%d memory_mib=%d; minimum 1 and 128)", m.CPUs, m.MemoryMiB)
	case m.DiskGiB < 1 || m.DiskGiB > MaxMachineDiskGiB:
		err = fmt.Errorf("disk of %d GiB out of range [1, %d]", m.DiskGiB, MaxMachineDiskGiB)
	case m.StopTimeout < 0 || m.StopTimeout > MaxStopTimeout:
		err = fmt.Errorf("stop_timeout %d out of range [0, %d]", m.StopTimeout, MaxStopTimeout)
	case m.StopOnExit && len(m.Workload) == 0:
		err = errors.New("stop_on_exit needs a workload (entrypoint or cmd)")
	case m.User != "" && !userSpec.MatchString(m.User):
		err = fmt.Errorf("user %q must be a passwd name or uid[:gid]", m.User)
	case m.Workdir != "" && !path.IsAbs(m.Workdir):
		err = fmt.Errorf("workdir %q must be absolute", m.Workdir)
	}
	if err == nil {
		_, err = SignalNumber(m.StopSignal)
	}
	if err == nil {
		err = validateMounts(m.Mounts)
	}
	if err == nil {
		err = validateSockets(m)
	}
	if err == nil {
		err = m.Network.Validate()
	}
	if err == nil {
		err = validateSecrets(m.Secrets)
	}
	if err == nil && !m.Network.Enabled && m.Network.Egress.Empty() && len(m.Network.Credentials) == 0 {
		for _, v := range m.Mounts {
			if _, _, ok := v.S3(); ok {
				// Quem fala com o bucket é o guest, pela rede da máquina.
				err = fmt.Errorf("mount %s: an s3 volume needs net = true", v)
				break
			}
		}
	}
	if err == nil && m.Display != nil && !m.GPU {
		err = errors.New("display needs gpu = true (the screen is a virtio-gpu output)")
	}
	if err == nil {
		for _, e := range m.Env {
			if !strings.Contains(e, "=") || strings.HasPrefix(e, "=") {
				err = fmt.Errorf("env %q must be KEY=VALUE", e)
				break
			}
		}
	}
	if err != nil {
		return ValidationError{Err: MachineError{Code: CodeConfig, Msg: err.Error()}}
	}
	return nil
}

// Códigos estáveis de erro das máquinas, nomes fixos: quem chama decide pelo
// código, não pela mensagem.
const (
	CodeNotFound     = "NOT_FOUND"
	CodeConflict     = "CONFLICT"
	CodeInvalidState = "INVALID_STATE"
	CodeConfig       = "CONFIG_ERROR"
)

// MachineError é uma recusa com código estável.
type MachineError struct {
	Code string
	Msg  string
}

// Error junta código e mensagem.
func (e MachineError) Error() string { return e.Code + ": " + e.Msg }

// ErrMachineNotFound, ErrMachineExists e ErrMachineState montam os erros mais comuns.
func ErrMachineNotFound(name string) error {
	return MachineError{Code: CodeNotFound, Msg: "machine " + name + " not found"}
}

// ErrMachineExists é o CONFLICT de criar uma máquina com nome já usado.
func ErrMachineExists(name string) error {
	return MachineError{Code: CodeConflict, Msg: "machine " + name + " already exists"}
}

// ErrMachineState é o INVALID_STATE de uma operação que não cabe no estado atual.
func ErrMachineState(name, msg string) error {
	return MachineError{Code: CodeInvalidState, Msg: "machine " + name + " " + msg}
}

// GuestPath valida o caminho de um arquivo dentro da máquina: absoluto e já limpo (sem
// `..` que escape da raiz depois de normalizado).
func GuestPath(p string) (string, error) {
	if p == "" || !path.IsAbs(p) {
		return "", ValidationError{Err: MachineError{Code: CodeConfig, Msg: fmt.Sprintf("guest path %q must be absolute", p)}}
	}
	return path.Clean(p), nil
}
