// Package runfile lê o runner.code: a declaração de uma máquina persistente em TOML.
// Um arquivo versionado com stack (ou imagem), recursos,
// ambiente, init e workload, em vez de uma fila de flags.
package runfile

import (
	"bytes"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"sort"
	"strings"

	"github.com/pelletier/go-toml/v2"

	"runner.codes/internal/domain"
)

// DefaultName é o arquivo que `rc machine create` procura quando não recebe -f.
const DefaultName = "runner.code"

// file espelha o TOML. Ponteiros nos números distinguem "não declarado" de zero, para a
// precedência (flag > arquivo > padrão) não confundir os dois.
type file struct {
	Stack        string   `toml:"stack"`
	Image        string   `toml:"image"`
	Entrypoint   []string `toml:"entrypoint"`
	Cmd          []string `toml:"cmd"`
	Env          []string `toml:"env"`
	Workdir      string   `toml:"workdir"`
	User         string   `toml:"user"`
	DockerSocket bool     `toml:"docker_socket"`
	// Secrets é a tabela [secrets]: NOME = { from_env = "..." } ou { from_file = "/abs" }.
	Secrets     map[string]secretSource `toml:"secrets"`
	Auth        *auth                   `toml:"auth"`
	Init        []string                `toml:"init"`
	StopSignal  string                  `toml:"stop_signal"`
	StopTimeout *int                    `toml:"stop_timeout"`
	StopOnExit  bool                    `toml:"stop_on_exit"`
	CPUs        *uint8                  `toml:"cpus"`
	Memory      *uint32                 `toml:"memory"`
	Storage     *int                    `toml:"storage"`
	GPU         bool                    `toml:"gpu"`
	GPUVRAM     *uint32                 `toml:"gpu_vram"`
	Mounts      []string                `toml:"volumes"` // contract:ok — chave do runner.code e da API
	Net         bool                    `toml:"net"`
	NetBackend  string                  `toml:"net_backend"`
	Ports       []string                `toml:"ports"`
	Network     *network                `toml:"network"`
	// Display e DisplayPort são do rc.
	Display     string  `toml:"display"`
	DisplayPort *uint16 `toml:"display_port"`
	Dev         *dev    `toml:"dev"`
}

// auth é a tabela [auth] do runner.code.
type auth struct {
	SSHAgent bool `toml:"ssh_agent"`
}

// secretSource é a fonte de um segredo do [secrets].
type secretSource struct {
	FromEnv  string `toml:"from_env"`
	FromFile string `toml:"from_file"`
}

// credential é uma [[network.credentials]] do runner.code.
type credential struct {
	Name      string   `toml:"name"`
	Env       string   `toml:"environment_variable"`
	Hosts     []string `toml:"allowed_hosts"`
	Methods   []string `toml:"methods"`
	SetHeader string   `toml:"set_header"`
	// Injection é o injection_location do runner.code: header (padrão) e, com body, o corpo de
	// formulário.
	Injection struct {
		Header bool `toml:"header"`
		Body   bool `toml:"body"`
	} `toml:"injection_location"`
}

// network é a tabela [network] do runner.code: a política de saída.
type network struct {
	// Name junta a máquina a uma rede nomeada (o mesmo que --network).
	Name string `toml:"name"`
	// Credentials são as [[network.credentials]] do runner.code.
	Credentials       []credential `toml:"credentials"`
	AllowHosts        []string     `toml:"allow_hosts"`
	AllowHostPatterns []string     `toml:"allow_host_patterns"`
	AllowCIDRs        []string     `toml:"allow_cidrs"`
	DenyCIDRs         []string     `toml:"deny_cidrs"`
	BlockPrivate      bool         `toml:"block_private"`
	AllowPrivate      []string     `toml:"allow_private"`
}

// dev é o perfil [dev]: env soma ao de cima; workdir e init substituem.
type dev struct {
	Env     []string `toml:"env"`
	Workdir string   `toml:"workdir"`
	User    string   `toml:"user"`
	Init    []string `toml:"init"`
	Mounts  []string `toml:"volumes"` // contract:ok — chave do runner.code e da API; substitui a de cima
	Ports   []string `toml:"ports"`   // substitui a de cima
}

// unsupported são chaves do formato que o runner.code reconhece mas ainda não implementa.
// Recusar com o motivo é melhor que ignorar: `net = false` ignorado seria inofensivo, mas
// `[network] allow_hosts` ignorado faria alguém achar que a máquina tem filtro de rede.
var unsupported = map[string]string{
	"allow_system_mounts": "mounting host system directories",
	"overlay":             "a separate overlay disk (the machine disk holds the overlay; size it with storage)",
	"health":              "health checks",
	"restart":             "restart policies",
	"artifact":            "pack/artifact profiles",
	"pack":                "pack/artifact profiles",
	"rosetta":             "Rosetta",
	"cuda":                "CUDA (needs a Linux host with an NVIDIA GPU)",
	"auto_graph":          "auto_graph",
	"block_io":            "block I/O limits",
	"disk_durability":     "disk durability modes",
	"branch":              "machine branches",
	"fork":                "machine branches",
	"service":             "service tables",
	// Aninhar é decisão de quem liga, não de um arquivo que veio com o repo.
	"nested":      "nested virtualization in a file (pass --nested on the command line)",
	"nested_virt": "nested virtualization in a file (pass --nested on the command line)",
}

// devUnsupported são as chaves do [dev] na mesma situação.
var devUnsupported = map[string]string{}

// Parse lê o TOML e devolve a máquina declarada (sem nome: o nome vem da CLI ou da API).
// Chave do formato ainda não suportada é erro com o motivo; chave desconhecida é erro.
func Parse(data []byte) (domain.Machine, error) {
	var m domain.Machine
	var raw map[string]any
	if err := toml.Unmarshal(data, &raw); err != nil {
		return m, fmt.Errorf("runner.code: %w", err)
	}
	if err := refuseUnsupported(raw, unsupported, ""); err != nil {
		return m, err
	}
	if d, ok := raw["dev"].(map[string]any); ok {
		if err := refuseUnsupported(d, devUnsupported, "dev."); err != nil {
			return m, err
		}
	}
	var f file
	dec := toml.NewDecoder(bytes.NewReader(data))
	dec.DisallowUnknownFields()
	if err := dec.Decode(&f); err != nil {
		var sm *toml.StrictMissingError
		if errors.As(err, &sm) {
			return m, fmt.Errorf("runner.code: unknown key(s): %s", strictKeys(sm))
		}
		return m, fmt.Errorf("runner.code: %w", err)
	}
	if f.Stack != "" && f.Image != "" {
		return m, errors.New("runner.code: set stack or image, not both")
	}
	m = domain.Machine{Stack: f.Stack, Image: f.Image, GPU: f.GPU, Env: f.Env, Workdir: f.Workdir, User: f.User, DockerSocket: f.DockerSocket, Init: f.Init,
		StopSignal: f.StopSignal, StopOnExit: f.StopOnExit}
	m.Workload = append(append([]string{}, f.Entrypoint...), f.Cmd...)
	if len(m.Workload) == 0 {
		m.Workload = nil
	}
	if f.StopTimeout != nil {
		m.StopTimeout = *f.StopTimeout
	}
	if f.CPUs != nil {
		m.CPUs = *f.CPUs
	}
	if f.Memory != nil {
		m.MemoryMiB = *f.Memory
	}
	if f.Storage != nil {
		m.DiskGiB = *f.Storage
	}
	if f.GPUVRAM != nil {
		m.GPUVRAMMiB = *f.GPUVRAM
	}
	// net_backend: só existe o gateway (virtio-net). "tsi" é recusado com o motivo: o TSI
	// não aplica política nenhuma, e o runner não liga rede sem o piso de bloqueio.
	switch f.NetBackend {
	case "", "virtio-net":
	case "tsi":
		return domain.Machine{}, errors.New("runner.code: net_backend \"tsi\" is not supported: every machine network goes through the filtering gateway (virtio-net)")
	default:
		return domain.Machine{}, fmt.Errorf("runner.code: net_backend %q must be virtio-net", f.NetBackend)
	}
	if f.Display != "" {
		d, err := domain.ParseDisplay(f.Display)
		if err != nil {
			return domain.Machine{}, fmt.Errorf("runner.code: %w", err)
		}
		if f.DisplayPort != nil {
			d.Port = *f.DisplayPort
		}
		m.Display = &d
	}
	names := make([]string, 0, len(f.Secrets))
	for n := range f.Secrets {
		names = append(names, n)
	}
	sort.Strings(names) // ordem estável: o mapa do TOML não tem ordem
	for _, n := range names {
		m.Secrets = append(m.Secrets, domain.Secret{Name: n, FromEnv: f.Secrets[n].FromEnv, FromFile: f.Secrets[n].FromFile})
	}
	if f.Auth != nil {
		m.SSHAgent = f.Auth.SSHAgent
	}
	m.Network.Enabled = f.Net
	if f.Network != nil {
		m.Network.Egress = domain.Egress{AllowHosts: f.Network.AllowHosts, AllowHostPatterns: f.Network.AllowHostPatterns,
			AllowCIDRs: f.Network.AllowCIDRs, DenyCIDRs: f.Network.DenyCIDRs, BlockPrivate: f.Network.BlockPrivate,
			AllowPrivate: f.Network.AllowPrivate}
		m.Network.Name = f.Network.Name
		for _, c := range f.Network.Credentials {
			m.Network.Credentials = append(m.Network.Credentials, domain.Credential{Name: c.Name, Env: c.Env, Hosts: c.Hosts,
				Methods: c.Methods, SetHeader: c.SetHeader, Body: c.Injection.Body})
		}
	}
	portSpecs := f.Ports
	if f.Dev != nil && len(f.Dev.Ports) > 0 {
		portSpecs = f.Dev.Ports
	}
	for _, s := range portSpecs {
		ps, err := domain.ParsePorts(s)
		if err != nil {
			return domain.Machine{}, fmt.Errorf("runner.code: %w", err)
		}
		m.Network.Ports = append(m.Network.Ports, ps...)
	}
	// A lista do [dev], se não vazia, substitui a de cima.
	specs := f.Mounts
	if f.Dev != nil && len(f.Dev.Mounts) > 0 {
		specs = f.Dev.Mounts
	}
	for _, s := range specs {
		mt, err := domain.ParseMount(s)
		if err != nil {
			return domain.Machine{}, fmt.Errorf("runner.code: %w", err)
		}
		m.Mounts = append(m.Mounts, mt)
	}
	if f.Dev != nil {
		m.Env = append(m.Env, f.Dev.Env...)
		if f.Dev.Workdir != "" {
			m.Workdir = f.Dev.Workdir
		}
		if f.Dev.User != "" {
			m.User = f.Dev.User
		}
		if f.Dev.Init != nil {
			m.Init = f.Dev.Init
		}
	}
	return m, nil
}

// Load lê o arquivo do caminho dado.
func Load(path string) (domain.Machine, error) {
	data, err := os.ReadFile(path)
	if err != nil {
		return domain.Machine{}, err
	}
	m, err := Parse(data)
	if err != nil {
		return m, fmt.Errorf("%s: %s", path, strings.TrimPrefix(err.Error(), "runner.code: "))
	}
	// Abs só falha se o Getwd falhar, e o ReadFile acima acabou de ler o mesmo caminho
	// relativo a partir do cwd: o erro não tem como acontecer aqui.
	abs, _ := filepath.Abs(path)
	// Relativo ao arquivo, como no docker-compose: o mesmo runner.code funciona de qualquer
	// diretório.
	m.Mounts, err = ResolveMounts(m.Mounts, filepath.Dir(abs))
	return m, err
}

// ResolveMounts deixa o lado do host de cada montagem absoluto: "~" vira o HOME e caminho
// relativo é resolvido contra base. O resto da validação (existe? é diretório?) é do create.
func ResolveMounts(ms []domain.Mount, base string) ([]domain.Mount, error) {
	out := make([]domain.Mount, 0, len(ms))
	for _, mt := range ms {
		if _, _, ok := mt.S3(); ok || mt.Disk() {
			out = append(out, mt) // bucket ou disco da máquina: não é caminho do host
			continue
		}
		h := mt.Host
		if h == "~" || strings.HasPrefix(h, "~/") {
			home, err := os.UserHomeDir()
			if err != nil {
				return nil, fmt.Errorf("volume %s: %w", mt, err)
			}
			h = filepath.Join(home, strings.TrimPrefix(h, "~"))
		}
		if !filepath.IsAbs(h) {
			h = filepath.Join(base, h)
		}
		mt.Host = filepath.Clean(h)
		out = append(out, mt)
	}
	return out, nil
}

// Override põe por cima da máquina do arquivo os campos que a linha de comando declarou:
// a flag vence o arquivo, e o arquivo vence o padrão. set diz se a flag de nome key foi
// passada (cmd.Flags().Changed): só ela distingue "--cpus 0" de "não disse nada".
// Exceções deliberadas: env soma (a flag acrescenta ao do arquivo, e a última definição de
// uma chave vence dentro da VM); stack e image se excluem, então declarar uma limpa a outra.
func Override(base, flags domain.Machine, set func(key string) bool) domain.Machine {
	if set("stack") {
		base.Stack, base.Image = flags.Stack, ""
	}
	if set("image") {
		base.Image, base.Stack = flags.Image, ""
	}
	if set("cpus") {
		base.CPUs = flags.CPUs
	}
	if set("memory") {
		base.MemoryMiB = flags.MemoryMiB
	}
	if set("disk-gib") {
		base.DiskGiB = flags.DiskGiB
	}
	if set("gpu") {
		base.GPU = flags.GPU
	}
	if set("gpu-vram") {
		base.GPUVRAMMiB = flags.GPUVRAMMiB
	}
	if set("env") {
		base.Env = append(append([]string{}, base.Env...), flags.Env...)
	}
	if set("workdir") {
		base.Workdir = flags.Workdir
	}
	if set("user") {
		base.User = flags.User
	}
	if set("docker-socket") {
		base.DockerSocket = flags.DockerSocket
	}
	if set("ssh-agent") {
		base.SSHAgent = flags.SSHAgent
	}
	if set("nested") {
		base.Nested = flags.Nested
	}
	// As pontes de socket só existem na CLI (o arquivo não as declara): a lista da flag soma.
	if set("expose-socket") {
		base.ExposeSockets = append(base.ExposeSockets, flags.ExposeSockets...)
	}
	if set("mount-socket") {
		base.MountSockets = append(base.MountSockets, flags.MountSockets...)
	}
	if set("init") {
		base.Init = flags.Init
	}
	if set("stop-signal") {
		base.StopSignal = flags.StopSignal
	}
	if set("stop-timeout") {
		base.StopTimeout = flags.StopTimeout
	}
	if set("stop-on-exit") {
		base.StopOnExit = flags.StopOnExit
	}
	if set("net") {
		base.Network.Enabled = flags.Network.Enabled
	}
	if set("network") {
		base.Network.Name = flags.Network.Name
	}
	// Segredos e credenciais da CLI se somam aos do arquivo.
	base.Secrets = append(append([]domain.Secret{}, base.Secrets...), flags.Secrets...)
	base.Network.Credentials = append(append([]domain.Credential{}, base.Network.Credentials...), flags.Network.Credentials...)
	// Portas e regras de saída da CLI se somam às do arquivo.
	if set("port") {
		base.Network.Ports = append(append([]domain.PortMap{}, base.Network.Ports...), flags.Network.Ports...)
	}
	be, fe := &base.Network.Egress, flags.Network.Egress
	be.AllowHosts = append(append([]string{}, be.AllowHosts...), fe.AllowHosts...)
	be.AllowHostPatterns = append(append([]string{}, be.AllowHostPatterns...), fe.AllowHostPatterns...)
	be.AllowCIDRs = append(append([]string{}, be.AllowCIDRs...), fe.AllowCIDRs...)
	be.DenyCIDRs = append(append([]string{}, be.DenyCIDRs...), fe.DenyCIDRs...)
	be.AllowPrivate = append(append([]string{}, be.AllowPrivate...), fe.AllowPrivate...)
	if set("block-private") {
		be.BlockPrivate = fe.BlockPrivate
	}
	if flags.Display != nil {
		base.Display = flags.Display
	}
	if set("volume") {
		// Como o env: a flag acrescenta ao que o arquivo declarou.
		base.Mounts = append(append([]domain.Mount{}, base.Mounts...), flags.Mounts...)
	}
	if len(flags.Workload) > 0 {
		base.Workload = flags.Workload
	}
	base.Name = flags.Name
	return base
}

// refuseUnsupported devolve o erro da primeira chave (em ordem alfabética, para a mensagem
// ser estável) que está na lista de não suportadas.
func refuseUnsupported(raw map[string]any, list map[string]string, prefix string) error {
	var found []string
	for k := range raw {
		if _, ok := list[k]; ok {
			found = append(found, k)
		}
	}
	if len(found) == 0 {
		return nil
	}
	sort.Strings(found)
	return fmt.Errorf("runner.code: key %q is not supported yet: %s", prefix+found[0], list[found[0]])
}

// strictKeys lista as chaves desconhecidas que o decoder estrito encontrou.
func strictKeys(e *toml.StrictMissingError) string {
	var keys []string
	for _, d := range e.Errors {
		keys = append(keys, strings.Join(d.Key(), "."))
	}
	return strings.Join(keys, ", ")
}
