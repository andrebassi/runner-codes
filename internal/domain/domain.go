// Package domain guarda as regras do runner que não dependem de VM, HTTP nem OCI:
// o que é uma linguagem, um job e um resultado, e o que torna um job aceitável.
package domain

import (
	"errors"
	"fmt"
	"strings"
	"time"
)

// MaxStdinBytes é o teto de stdin herdado do contrato da API (E005).
const MaxStdinBytes = 1_000_000

// MaxFiles limita arquivos por job; mais que isso não é código de LLM, é abuso.
const MaxFiles = 32

// MaxSourceBytes soma o conteúdo de todos os arquivos; o tmpfs do guest tem 256 MiB
// e o código-fonte precisa caber com folga para o build e a saída.
const MaxSourceBytes = 4 << 20

// DefaultTimeout vale quando o job não diz; MaxTimeout é o teto contra job que prende VM.
const (
	DefaultTimeout = 10 * time.Second
	MaxTimeout     = 300 * time.Second
)

// Códigos de erro do contrato atual da API (pkg/api), mantidos para os clientes.
var (
	ErrTimeout     = errors.New("E001: operation timed out")
	ErrStdinTooBig = errors.New("E005: stdin too long")
)

// ErrUnsupportedLanguage é devolvido para linguagem fora do catálogo deste host.
type ErrUnsupportedLanguage struct{ Name string }

// Error segue o formato E006 do contrato.
func (e ErrUnsupportedLanguage) Error() string { return "E006: unsupported language " + e.Name }

// Language descreve como uma linguagem roda: imagem OCI de origem, teto de recursos
// (contra abuso, não por concorrência: a memória da libkrun é elástica) e o warmup
// gravado na imagem no build.
type Language struct {
	Name      string `yaml:"name"`
	Image     string `yaml:"image"`
	CPUs      uint8  `yaml:"cpus"`
	MemoryMiB uint32 `yaml:"memory_mib"`
	Warmup    string `yaml:"warmup"`
}

// File é um arquivo-fonte do job.
type File struct {
	Name    string
	Content string
}

// Job é um pedido de execução já no vocabulário do domínio.
type Job struct {
	TraceID  string
	Language string
	Code     string
	Files    []File
	Stdin    string
	Timeout  time.Duration
}

// Result é o desfecho de um job. TimedOut distingue timeout de falha do programa.
type Result struct {
	TraceID   string
	Status    string
	Stdout    string
	Stderr    string
	Exception string
	ExitCode  int
	Duration  time.Duration
	TimedOut  bool
	Error     string
}

// Normalize aplica o timeout padrão e devolve o job pronto para validar.
func (j Job) Normalize() Job {
	if j.Timeout == 0 {
		j.Timeout = DefaultTimeout
	}
	return j
}

// ValidationError marca recusa do job por regra de domínio (vira HTTP 400), em oposição
// a falha de infraestrutura. Unwrap preserva errors.Is/As para os códigos E005/E006.
type ValidationError struct{ Err error }

// Error repassa a mensagem do erro embrulhado.
func (e ValidationError) Error() string { return e.Err.Error() }

// Unwrap expõe o erro original.
func (e ValidationError) Unwrap() error { return e.Err }

// Validate decide se o job pode ir para uma VM; toda recusa vem como ValidationError.
func (j Job) Validate(known func(string) bool) error {
	if err := j.validate(known); err != nil {
		return ValidationError{Err: err}
	}
	return nil
}

// validate aplica as regras. known diz se a linguagem tem imagem
// neste host. As regras de nome de arquivo fecham escrita fora do diretório do job
// dentro do guest (path traversal), mesmo a VM sendo descartável.
func (j Job) validate(known func(string) bool) error {
	if !known(j.Language) {
		return ErrUnsupportedLanguage{Name: j.Language}
	}
	if len(j.Stdin) > MaxStdinBytes {
		return ErrStdinTooBig
	}
	if j.Timeout <= 0 || j.Timeout > MaxTimeout {
		return fmt.Errorf("timeout %v fora do intervalo (0, %v]", j.Timeout, MaxTimeout)
	}
	if j.Code == "" && len(j.Files) == 0 {
		return errors.New("job sem código e sem arquivos")
	}
	if len(j.Files) > MaxFiles {
		return fmt.Errorf("%d arquivos; o limite é %d", len(j.Files), MaxFiles)
	}
	total := len(j.Code)
	seen := map[string]bool{}
	for _, f := range j.Files {
		if err := validFileName(f.Name); err != nil {
			return err
		}
		if seen[f.Name] {
			return fmt.Errorf("arquivo %q repetido", f.Name)
		}
		seen[f.Name] = true
		total += len(f.Content)
	}
	if total > MaxSourceBytes {
		return fmt.Errorf("código com %d bytes; o limite é %d", total, MaxSourceBytes)
	}
	return nil
}

// validFileName aceita só nome simples: sem barra, sem "..", sem oculto, sem byte nulo.
func validFileName(name string) error {
	switch {
	case name == "", name == ".", name == "..":
		return fmt.Errorf("nome de arquivo inválido %q", name)
	case strings.ContainsAny(name, "/\\\x00"):
		return fmt.Errorf("nome de arquivo %q não pode ter separador nem byte nulo", name)
	case strings.HasPrefix(name, "."):
		return fmt.Errorf("nome de arquivo %q não pode começar com ponto", name)
	case len(name) > 255:
		return fmt.Errorf("nome de arquivo com %d bytes; o limite é 255", len(name))
	}
	return nil
}

// ValidateLanguage confere uma entrada do catálogo antes de ela virar imagem.
func (l Language) ValidateLanguage() error {
	switch {
	case l.Name == "" || strings.ContainsAny(l.Name, "/ ."):
		return fmt.Errorf("linguagem com nome inválido %q", l.Name)
	case l.Image == "":
		return fmt.Errorf("linguagem %s sem imagem", l.Name)
	case l.CPUs == 0 || l.MemoryMiB < 128:
		return fmt.Errorf("linguagem %s com recursos inválidos (cpus=%d memory_mib=%d; mínimo 1 e 128)", l.Name, l.CPUs, l.MemoryMiB)
	}
	return nil
}
