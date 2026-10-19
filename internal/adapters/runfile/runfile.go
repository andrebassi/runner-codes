// Package runfile lê o runner.code: a declaração de uma máquina persistente em TOML, o
// equivalente do runner.code. Um arquivo versionado com stack (ou imagem), recursos,
// ambiente, init e workload, em vez de uma fila de flags.
package runfile

import (
	"bytes"
	"errors"
	"fmt"
	"os"
	"sort"
	"strings"

	"github.com/pelletier/go-toml/v2"

	"runner.code/internal/domain"
)

// DefaultName é o arquivo que `rc machine create` procura quando não recebe -f.
const DefaultName = "runner.code"

// file espelha o TOML. Ponteiros nos números distinguem "não declarado" de zero, para a
// precedência (flag > arquivo > padrão) não confundir os dois.
type file struct {
	Stack       string   `toml:"stack"`
	Image       string   `toml:"image"`
	Entrypoint  []string `toml:"entrypoint"`
	Cmd         []string `toml:"cmd"`
	Env         []string `toml:"env"`
	Workdir     string   `toml:"workdir"`
	Init        []string `toml:"init"`
	StopSignal  string   `toml:"stop_signal"`
	StopTimeout *int     `toml:"stop_timeout"`
	StopOnExit  bool     `toml:"stop_on_exit"`
	CPUs        *uint8   `toml:"cpus"`
	Memory      *uint32  `toml:"memory"`
	Storage     *int     `toml:"storage"`
	GPU         bool     `toml:"gpu"`
	GPUVRAM     *uint32  `toml:"gpu_vram"`
	Dev         *dev     `toml:"dev"`
}

// dev é o perfil [dev]: env soma ao de cima; workdir e init substituem.
type dev struct {
	Env     []string `toml:"env"`
	Workdir string   `toml:"workdir"`
	Init    []string `toml:"init"`
}

// unsupported são chaves do runner.code que o runner.code reconhece mas ainda não implementa.
// Recusar com o motivo é melhor que ignorar: `net = false` ignorado seria inofensivo, mas
// `[network] allow_hosts` ignorado faria alguém achar que a máquina tem filtro de rede.
var unsupported = map[string]string{
	"net":                 "networking (planned: phase C)",
	"network":             "egress filtering (planned: phase C)",
	"ports":               "port forwarding (planned: phase C)",
	"volumes":             "host directory mounts are not supported (copy with `rc machine cp`)",
	"allow_system_mounts": "host directory mounts are not supported",
	"auth":                "SSH agent forwarding (planned: phase B)",
	"secrets":             "secrets (planned: phase B)",
	"user":                "running the workload as another user",
	"overlay":             "a separate overlay disk (the machine disk holds the overlay; size it with storage)",
	"health":              "health checks",
	"restart":             "restart policies",
	"artifact":            "pack/artifact profiles",
	"pack":                "pack/artifact profiles",
	"net_backend":         "networking (planned: phase C)",
	"rosetta":             "Rosetta",
	"cuda":                "CUDA (needs a Linux host with an NVIDIA GPU)",
	"auto_graph":          "auto_graph",
	"docker_socket":       "a Docker socket",
	"block_io":            "block I/O limits",
	"disk_durability":     "disk durability modes",
	"branch":              "machine branches",
	"fork":                "machine branches",
	"service":             "service tables",
}

// devUnsupported são as chaves do [dev] na mesma situação.
var devUnsupported = map[string]string{
	"volumes": unsupported["volumes"],
	"ports":   unsupported["ports"],
	"user":    unsupported["user"],
}

// Parse lê o TOML e devolve a máquina declarada (sem nome: o nome vem da CLI ou da API).
// Chave do runner.code ainda não suportada é erro com o motivo; chave desconhecida é erro.
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
	m = domain.Machine{Stack: f.Stack, Image: f.Image, GPU: f.GPU, Env: f.Env, Workdir: f.Workdir, Init: f.Init,
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
	if f.Dev != nil {
		m.Env = append(m.Env, f.Dev.Env...)
		if f.Dev.Workdir != "" {
			m.Workdir = f.Dev.Workdir
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
	return m, nil
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
