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

// Padrões de uma máquina persistente quando o pedido não diz:, com disco
// menor (10 GiB em vez de 20) porque a imagem da stack já vem pronta e read-only.
const (
	DefaultMachineCPUs      = 2
	DefaultMachineMemoryMiB = 2048
	DefaultMachineDiskGiB   = 10
	MaxMachineDiskGiB       = 1024
)

// machineName casa nome de máquina: minúsculas, dígitos e hífen, começando por letra ou
// dígito, até 63 caracteres. Vira nome de diretório e aparece em URL, então nada de
// barra, ponto ou espaço.
var machineName = regexp.MustCompile(`^[a-z0-9][a-z0-9-]{0,62}$`)

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
	// Init roda uma vez, como root, no primeiro start (cada item via `sh -c`).
	Init []string `json:"init,omitempty"`
	// Workload é o processo que a máquina mantém rodando desde o start (entrypoint + cmd).
	Workload []string `json:"workload,omitempty"`
	// StopSignal vai ao workload no stop; StopTimeout é quanto ele tem para sair antes do
	// SIGKILL; StopOnExit desliga a máquina quando o workload sai.
	StopSignal  string `json:"stop_signal,omitempty"`
	StopTimeout int    `json:"stop_timeout,omitempty"`
	StopOnExit  bool   `json:"stop_on_exit,omitempty"`
	// GPUVRAMMiB é a VRAM do guest (gpu_vram); 0 usa a da stack no catálogo.
	GPUVRAMMiB uint32 `json:"gpu_vram_mib,omitempty"`
}

// MaxStopTimeout é o teto do stop_timeout (300 s): o stop não pode
// prender a máquina indefinidamente.
const MaxStopTimeout = 300

// stopSignals são os nomes de sinal aceitos no stop_signal; número também vale (1–64).
var stopSignals = map[string]int{
	"SIGHUP": 1, "SIGINT": 2, "SIGQUIT": 3, "SIGKILL": 9, "SIGUSR1": 10, "SIGUSR2": 12, "SIGTERM": 15,
}

// SignalNumber traduz o stop_signal ("SIGINT", "INT", "2") para o número do sinal no
// guest Linux; vazio é SIGTERM.
func SignalNumber(s string) (int, error) {
	if s == "" {
		return 15, nil
	}
	name := strings.ToUpper(s)
	if !strings.HasPrefix(name, "SIG") {
		name = "SIG" + name
	}
	if n, ok := stopSignals[name]; ok {
		return n, nil
	}
	if n, err := strconv.Atoi(s); err == nil && n >= 1 && n <= 64 {
		return n, nil
	}
	return 0, fmt.Errorf("unknown stop_signal %q (use a name like SIGTERM or a number 1-64)", s)
}

// WithDefaults preenche CPU, memória e disco que vieram zerados.
func (m Machine) WithDefaults() Machine {
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

// Validate recusa máquina que não pode existir; toda recusa vem como ValidationError com
// código CONFIG_ERROR.
func (m Machine) Validate() error {
	var err error
	switch {
	case !machineName.MatchString(m.Name):
		err = fmt.Errorf("invalid machine name %q (lowercase letters, digits and '-', up to 63)", m.Name)
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
	case m.GPUVRAMMiB > 0 && !m.GPU:
		err = errors.New("gpu_vram needs gpu = true")
	case m.Workdir != "" && !path.IsAbs(m.Workdir):
		err = fmt.Errorf("workdir %q must be absolute", m.Workdir)
	}
	if err == nil {
		_, err = SignalNumber(m.StopSignal)
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

// Códigos estáveis de erro das máquinas, os mesmos nomes: quem chama decide pelo
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
