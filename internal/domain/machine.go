package domain

import (
	"errors"
	"fmt"
	"path"
	"regexp"
	"time"
)

// Padrões de uma máquina persistente quando o pedido não diz: os do smolvm, com disco
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
	}
	if err != nil {
		return ValidationError{Err: MachineError{Code: CodeConfig, Msg: err.Error()}}
	}
	return nil
}

// Códigos estáveis de erro das máquinas, os mesmos nomes do smolvm: quem chama decide pelo
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
