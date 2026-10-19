package domain

import (
	"errors"
	"strings"
	"testing"
)

// TestMachineDefaultsAndValidate: zeros viram os padrões, e cada recusa sai como
// ValidationError com o código CONFIG_ERROR.
func TestMachineDefaultsAndValidate(t *testing.T) {
	m := Machine{Name: "dev-1", Stack: "python"}.WithDefaults()
	if m.CPUs != DefaultMachineCPUs || m.MemoryMiB != DefaultMachineMemoryMiB || m.DiskGiB != DefaultMachineDiskGiB {
		t.Fatalf("defaults: %+v", m)
	}
	if err := m.Validate(); err != nil {
		t.Fatal(err)
	}
	kept := Machine{Name: "a", Stack: "go", CPUs: 4, MemoryMiB: 512, DiskGiB: 3}.WithDefaults()
	if kept.CPUs != 4 || kept.MemoryMiB != 512 || kept.DiskGiB != 3 {
		t.Fatalf("explicit values must stay: %+v", kept)
	}
	bad := map[string]Machine{
		"empty name":     {Stack: "python"},
		"upper case":     {Name: "Dev", Stack: "python"},
		"slash":          {Name: "a/b", Stack: "python"},
		"dot":            {Name: "a.b", Stack: "python"},
		"leading hyphen": {Name: "-a", Stack: "python"},
		"too long":       {Name: strings.Repeat("a", 64), Stack: "python"},
		"no stack":       {Name: "a"},
		"small memory":   {Name: "a", Stack: "python", MemoryMiB: 64},
		"huge disk":      {Name: "a", Stack: "python", DiskGiB: MaxMachineDiskGiB + 1},
	}
	for name, m := range bad {
		t.Run(name, func(t *testing.T) {
			err := m.WithDefaults().Validate()
			var ve ValidationError
			var me MachineError
			if !errors.As(err, &ve) || !errors.As(err, &me) || me.Code != CodeConfig {
				t.Fatalf("expected CONFIG_ERROR validation, got %v", err)
			}
		})
	}
	if err := (Machine{Name: "a", Stack: "python", CPUs: 1, MemoryMiB: 512}).Validate(); err == nil {
		t.Fatal("zero disk without defaults must be refused")
	}
}

// TestMachineWorkloadRules: workload, init e política de parada válidos passam; cada
// regra quebrada é CONFIG_ERROR.
func TestMachineWorkloadRules(t *testing.T) {
	ok := Machine{Name: "a", Stack: "python", Env: []string{"A=1", "EMPTY="}, Workdir: "/app",
		Init: []string{"echo hi"}, Workload: []string{"python3", "-m", "http.server"},
		StopSignal: "SIGINT", StopTimeout: 60, StopOnExit: true}.WithDefaults()
	if err := ok.Validate(); err != nil {
		t.Fatal(err)
	}
	mut := func(f func(*Machine)) Machine { m := ok; f(&m); return m }
	for name, m := range map[string]Machine{
		"timeout too big":      mut(func(m *Machine) { m.StopTimeout = MaxStopTimeout + 1 }),
		"negative timeout":     mut(func(m *Machine) { m.StopTimeout = -1 }),
		"stop_on_exit no work": mut(func(m *Machine) { m.Workload = nil }),
		"relative workdir":     mut(func(m *Machine) { m.Workdir = "app" }),
		"bad signal":           mut(func(m *Machine) { m.StopSignal = "SIGNOPE" }),
		"env without equals":   mut(func(m *Machine) { m.Env = []string{"JUSTKEY"} }),
		"env with empty key":   mut(func(m *Machine) { m.Env = []string{"=x"} }),
		"vram without gpu":     mut(func(m *Machine) { m.GPUVRAMMiB = 2048 }),
	} {
		var me MachineError
		if err := m.Validate(); !errors.As(err, &me) || me.Code != CodeConfig {
			t.Fatalf("%s: %v", name, err)
		}
	}
}

// TestSignalNumber: nome com e sem SIG, minúsculo, número, vazio (SIGTERM) e inválidos.
func TestSignalNumber(t *testing.T) {
	for in, want := range map[string]int{"": 15, "SIGTERM": 15, "int": 2, "SIGQUIT": 3, "9": 9, "64": 64, "SIGUSR1": 10} {
		if got, err := SignalNumber(in); err != nil || got != want {
			t.Fatalf("%q: %d %v", in, got, err)
		}
	}
	for _, bad := range []string{"SIGFOO", "0", "65", "abc"} {
		if _, err := SignalNumber(bad); err == nil {
			t.Fatalf("%q must be refused", bad)
		}
	}
}

// TestMachineErrors trava os códigos estáveis: é por eles que a API e a CLI decidem.
func TestMachineErrors(t *testing.T) {
	cases := map[string]error{
		CodeNotFound:     ErrMachineNotFound("x"),
		CodeConflict:     ErrMachineExists("x"),
		CodeInvalidState: ErrMachineState("x", "is running"),
	}
	for code, err := range cases {
		var me MachineError
		if !errors.As(err, &me) || me.Code != code || !strings.HasPrefix(err.Error(), code+": machine x") {
			t.Fatalf("%s: %v", code, err)
		}
	}
}

// TestGuestPath: só caminho absoluto, normalizado (o `..` não sobe acima da raiz).
func TestGuestPath(t *testing.T) {
	if p, err := GuestPath("/workspace/../etc//x"); err != nil || p != "/etc/x" {
		t.Fatalf("clean: %q %v", p, err)
	}
	if p, _ := GuestPath("/../../x"); p != "/x" {
		t.Fatalf("cannot escape the root: %q", p)
	}
	for _, bad := range []string{"", "rel/x", "./x"} {
		var ve ValidationError
		if _, err := GuestPath(bad); !errors.As(err, &ve) {
			t.Fatalf("%q must be refused: %v", bad, err)
		}
	}
}
