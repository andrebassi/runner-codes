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
		t.Fatalf("default timeout %v, want %v", got, DefaultTimeout)
	}
	if got := (Job{Timeout: 3 * time.Second}).Normalize().Timeout; got != 3*time.Second {
		t.Fatalf("explicit timeout changed: %v", got)
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
		"unknown language":      mut(func(j *Job) { j.Language = "cobol" }),
		"large stdin":           mut(func(j *Job) { j.Stdin = strings.Repeat("x", MaxStdinBytes+1) }),
		"timeout zero":          mut(func(j *Job) { j.Timeout = 0 }),
		"timeout above ceiling": mut(func(j *Job) { j.Timeout = MaxTimeout + time.Second }),
		"no code":               mut(func(j *Job) { j.Code = "" }),
		"too many files":        mut(func(j *Job) { j.Files = many }),
		"traversal with slash":  mut(func(j *Job) { j.Files = []File{{Name: "../etc/passwd"}} }),
		"backslash":             mut(func(j *Job) { j.Files = []File{{Name: `a\b`}} }),
		"null byte":             mut(func(j *Job) { j.Files = []File{{Name: "a\x00b"}} }),
		"dot dot":               mut(func(j *Job) { j.Files = []File{{Name: ".."}} }),
		"dot":                   mut(func(j *Job) { j.Files = []File{{Name: "."}} }),
		"empty":                 mut(func(j *Job) { j.Files = []File{{Name: ""}} }),
		"hidden":                mut(func(j *Job) { j.Files = []File{{Name: ".bashrc"}} }),
		"long name":             mut(func(j *Job) { j.Files = []File{{Name: strings.Repeat("a", 256)}} }),
		"duplicate":             mut(func(j *Job) { j.Files = []File{{Name: "a.py"}, {Name: "a.py"}} }),
		"large code":            mut(func(j *Job) { j.Files = []File{{Name: "a.py", Content: strings.Repeat("x", MaxSourceBytes)}} }),
	}
	for name, j := range cases {
		t.Run(name, func(t *testing.T) {
			if err := j.Validate(known); err == nil {
				t.Fatal("expected rejection")
			}
		})
	}
}

// TestErrorCodes trava os códigos do contrato da API.
func TestErrorCodes(t *testing.T) {
	err := mutStdin().Validate(known)
	if !errors.Is(err, ErrStdinTooBig) || !strings.HasPrefix(err.Error(), "E005") {
		t.Fatalf("large stdin should be E005, got %v", err)
	}
	err = Job{Language: "zig", Code: "x", Timeout: time.Second}.Validate(known)
	var ul ErrUnsupportedLanguage
	if !errors.As(err, &ul) || err.Error() != "E006: unsupported language zig" {
		t.Fatalf("unknown language should be E006, got %v", err)
	}
	if !strings.HasPrefix(ErrTimeout.Error(), "E001") {
		t.Fatal("timeout should be E001")
	}
	// Toda recusa vem como ValidationError: é o que a API usa para responder 400 e não 500.
	var ve ValidationError
	if !errors.As(err, &ve) || !errors.As(mutStdin().Validate(known), &ve) {
		t.Fatal("validation rejection should be ValidationError")
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
		"no name":         {Image: "x", CPUs: 1, MemoryMiB: 512},
		"name with slash": {Name: "a/b", Image: "x", CPUs: 1, MemoryMiB: 512},
		"name with dot":   {Name: "a.b", Image: "x", CPUs: 1, MemoryMiB: 512},
		"no image":        {Name: "a", CPUs: 1, MemoryMiB: 512},
		"no cpu":          {Name: "a", Image: "x", MemoryMiB: 512},
		"small memory":    {Name: "a", Image: "x", CPUs: 1, MemoryMiB: 64},
	}
	for name, l := range bad {
		t.Run(name, func(t *testing.T) {
			if err := l.ValidateLanguage(); err == nil {
				t.Fatal("expected rejection")
			}
		})
	}
}
