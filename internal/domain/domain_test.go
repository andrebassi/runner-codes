package domain

import (
	"errors"
	"strings"
	"testing"
	"time"
)

// known aceita só python, como um host com uma imagem construída.
func known(name string) bool { return name == "python" }

// validJob é um job aceitável, base para as mutações dos testes.
func validJob() Job {
	return Job{Language: "python", Code: "print(1)", Timeout: time.Second}
}

// TestNormalizeDefaultTimeout: job sem timeout ganha o padrão; com timeout, mantém.
func TestNormalizeDefaultTimeout(t *testing.T) {
	if got := (Job{}).Normalize().Timeout; got != DefaultTimeout {
		t.Fatalf("timeout padrão %v, quer %v", got, DefaultTimeout)
	}
	if got := (Job{Timeout: 3 * time.Second}).Normalize().Timeout; got != 3*time.Second {
		t.Fatalf("timeout explícito alterado: %v", got)
	}
}

// TestValidateAccepts: job simples e job com arquivos legítimos passam.
func TestValidateAccepts(t *testing.T) {
	if err := validJob().Validate(known); err != nil {
		t.Fatal(err)
	}
	j := Job{Language: "python", Timeout: MaxTimeout, Files: []File{{Name: "main.py", Content: "x"}, {Name: "util.py"}}}
	if err := j.Validate(known); err != nil {
		t.Fatal(err)
	}
}

// TestValidateRejects varre cada regra; cada caso diz qual ataque ou erro ele fecha.
func TestValidateRejects(t *testing.T) {
	mut := func(f func(*Job)) Job { j := validJob(); f(&j); return j }
	many := make([]File, MaxFiles+1)
	for i := range many {
		many[i] = File{Name: strings.Repeat("a", i+1)}
	}
	cases := map[string]Job{
		"linguagem desconhecida": mut(func(j *Job) { j.Language = "cobol" }),
		"stdin grande":           mut(func(j *Job) { j.Stdin = strings.Repeat("x", MaxStdinBytes+1) }),
		"timeout zero":           mut(func(j *Job) { j.Timeout = 0 }),
		"timeout acima do teto":  mut(func(j *Job) { j.Timeout = MaxTimeout + time.Second }),
		"sem código":             mut(func(j *Job) { j.Code = "" }),
		"arquivos demais":        mut(func(j *Job) { j.Files = many }),
		"traversal com barra":    mut(func(j *Job) { j.Files = []File{{Name: "../etc/passwd"}} }),
		"barra invertida":        mut(func(j *Job) { j.Files = []File{{Name: `a\b`}} }),
		"byte nulo":              mut(func(j *Job) { j.Files = []File{{Name: "a\x00b"}} }),
		"ponto ponto":            mut(func(j *Job) { j.Files = []File{{Name: ".."}} }),
		"ponto":                  mut(func(j *Job) { j.Files = []File{{Name: "."}} }),
		"vazio":                  mut(func(j *Job) { j.Files = []File{{Name: ""}} }),
		"oculto":                 mut(func(j *Job) { j.Files = []File{{Name: ".bashrc"}} }),
		"nome longo":             mut(func(j *Job) { j.Files = []File{{Name: strings.Repeat("a", 256)}} }),
		"repetido":               mut(func(j *Job) { j.Files = []File{{Name: "a.py"}, {Name: "a.py"}} }),
		"código grande":          mut(func(j *Job) { j.Files = []File{{Name: "a.py", Content: strings.Repeat("x", MaxSourceBytes)}} }),
	}
	for name, j := range cases {
		t.Run(name, func(t *testing.T) {
			if err := j.Validate(known); err == nil {
				t.Fatal("esperava recusa")
			}
		})
	}
}

// TestErrorCodes trava os códigos do contrato da API.
func TestErrorCodes(t *testing.T) {
	err := mutStdin().Validate(known)
	if !errors.Is(err, ErrStdinTooBig) || !strings.HasPrefix(err.Error(), "E005") {
		t.Fatalf("stdin grande deveria ser E005, veio %v", err)
	}
	err = Job{Language: "zig", Code: "x", Timeout: time.Second}.Validate(known)
	var ul ErrUnsupportedLanguage
	if !errors.As(err, &ul) || err.Error() != "E006: unsupported language zig" {
		t.Fatalf("linguagem desconhecida deveria ser E006, veio %v", err)
	}
	if !strings.HasPrefix(ErrTimeout.Error(), "E001") {
		t.Fatal("timeout deveria ser E001")
	}
	// Toda recusa vem como ValidationError: é o que a API usa para responder 400 e não 500.
	var ve ValidationError
	if !errors.As(err, &ve) || !errors.As(mutStdin().Validate(known), &ve) {
		t.Fatal("recusa de validação deveria ser ValidationError")
	}
}

// mutStdin devolve um job válido com stdin acima do teto.
func mutStdin() Job {
	j := validJob()
	j.Stdin = strings.Repeat("x", MaxStdinBytes+1)
	return j
}

// TestValidateLanguage cobre a validação das entradas do catálogo.
func TestValidateLanguage(t *testing.T) {
	ok := Language{Name: "python", Image: "python:3.12-alpine", CPUs: 1, MemoryMiB: 512}
	if err := ok.ValidateLanguage(); err != nil {
		t.Fatal(err)
	}
	bad := map[string]Language{
		"sem nome":        {Image: "x", CPUs: 1, MemoryMiB: 512},
		"nome com barra":  {Name: "a/b", Image: "x", CPUs: 1, MemoryMiB: 512},
		"nome com ponto":  {Name: "a.b", Image: "x", CPUs: 1, MemoryMiB: 512},
		"sem imagem":      {Name: "a", CPUs: 1, MemoryMiB: 512},
		"sem cpu":         {Name: "a", Image: "x", MemoryMiB: 512},
		"memória pequena": {Name: "a", Image: "x", CPUs: 1, MemoryMiB: 64},
	}
	for name, l := range bad {
		t.Run(name, func(t *testing.T) {
			if err := l.ValidateLanguage(); err == nil {
				t.Fatal("esperava recusa")
			}
		})
	}
}
