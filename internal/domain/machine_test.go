package domain

import (
	"errors"
	"fmt"
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
		"bad network":          mut(func(m *Machine) { m.Network.Egress.AllowCIDRs = []string{"x"} }),
	} {
		var me MachineError
		if err := m.Validate(); !errors.As(err, &me) || me.Code != CodeConfig {
			t.Fatalf("%s: %v", name, err)
		}
	}
}

// TestDisplay: LxA válido vira a tela com a porta padrão; fora da faixa ou malformado é
// recusado; display sem GPU é CONFIG_ERROR.
func TestDisplay(t *testing.T) {
	d, err := ParseDisplay("1280X800")
	if err != nil || d != (Display{Width: 1280, Height: 800, Port: DefaultDisplayPort}) {
		t.Fatalf("%+v %v", d, err)
	}
	for _, bad := range []string{"", "1280", "x800", "100x100", "9000x800", "1280x5000", "axb"} {
		if _, err := ParseDisplay(bad); err == nil {
			t.Fatalf("%q must fail", bad)
		}
	}
	m := Machine{Name: "d", Stack: "python", Display: &d}.WithDefaults()
	var me MachineError
	if err := m.Validate(); !errors.As(err, &me) || !strings.Contains(err.Error(), "gpu") {
		t.Fatalf("display without gpu: %v", err)
	}
	m.GPU = true
	if err := m.Validate(); err != nil {
		t.Fatal(err)
	}
}

// TestParseMount: os três formatos aceitos e os malformados.
func TestParseMount(t *testing.T) {
	for in, want := range map[string]Mount{
		"/h:/g":     {Host: "/h", Guest: "/g"},
		"/h:/g:ro":  {Host: "/h", Guest: "/g", ReadOnly: true},
		"./h:/g:rw": {Host: "./h", Guest: "/g"},
	} {
		got, err := ParseMount(in)
		if err != nil || got != want {
			t.Fatalf("%q: %+v %v", in, got, err)
		}
	}
	if (Mount{Host: "/h", Guest: "/g", ReadOnly: true}).String() != "/h:/g:ro" || (Mount{Host: "/h", Guest: "/g"}).String() != "/h:/g" {
		t.Fatal("String must round-trip")
	}
	// "/h" sozinho é volume de disco (TestDiskMount); "h" relativo continua inválido.
	for _, bad := range []string{"h", "/h:/g:rx", ":/g", "/h:", "/a:/b:ro:x"} {
		if _, err := ParseMount(bad); err == nil {
			t.Fatalf("%q must fail", bad)
		}
	}
}

// TestMountRules: caminho relativo, destino reservado (inclusive subdiretório), destino
// repetido e acima do teto são recusados; o resto passa.
func TestMountRules(t *testing.T) {
	base := Machine{Name: "a", Stack: "python"}.WithDefaults()
	base.Mounts = []Mount{{Host: "/h", Guest: "/data"}, {Host: "/h2", Guest: "/var/www", ReadOnly: true}}
	if err := base.Validate(); err != nil {
		t.Fatal(err)
	}
	many := make([]Mount, MaxMounts+1)
	for i := range many {
		many[i] = Mount{Host: "/h", Guest: fmt.Sprintf("/m%d", i)}
	}
	for name, ms := range map[string][]Mount{
		"relative host":  {{Host: "h", Guest: "/g"}},
		"relative guest": {{Host: "/h", Guest: "g"}},
		"root":           {{Host: "/h", Guest: "/"}},
		"proc":           {{Host: "/h", Guest: "/proc"}},
		"under runner":   {{Host: "/h", Guest: "/opt/runner/x"}},
		"tmp":            {{Host: "/h", Guest: "/tmp/"}},
		"twice":          {{Host: "/h", Guest: "/g"}, {Host: "/i", Guest: "/g/"}},
		"too many":       many,
	} {
		m := base
		m.Mounts = ms
		var me MachineError
		if err := m.Validate(); !errors.As(err, &me) || me.Code != CodeConfig {
			t.Fatalf("%s: %v", name, err)
		}
	}
	ok := base
	ok.Mounts = []Mount{{Host: "/h", Guest: "/opt/runners"}, {Host: "/h", Guest: "/procfs"}}
	if err := ok.Validate(); err != nil {
		t.Fatalf("a prefix that is not a subdirectory must pass: %v", err)
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

// TestVRAMWithoutGPU: gpu_vram sem gpu é ignorado, não recusado.
func TestVRAMWithoutGPU(t *testing.T) {
	m := Machine{Name: "v", Stack: "python", GPUVRAMMiB: 2048}.WithDefaults()
	if m.GPUVRAMMiB != 0 || m.Validate() != nil {
		t.Fatalf("vram without gpu: %d %v", m.GPUVRAMMiB, m.Validate())
	}
	if g := (Machine{Name: "g", Stack: "llama", GPU: true, GPUVRAMMiB: 2048}).WithDefaults(); g.GPUVRAMMiB != 2048 {
		t.Fatal("with gpu the window stays")
	}
}

// TestSignalNumberTable: a numeração é a do Linux (o guest), inclusive onde o macOS difere
// (SIGUSR1 é 30 lá), e SIGRTMIN+N segue a glibc.
func TestSignalNumberTable(t *testing.T) {
	for in, want := range map[string]int{"SIGUSR1": 10, "chld": 17, "SIGWINCH": 28, "SIGSYS": 31, "SIGRTMIN": 34,
		"SIGRTMIN+3": 37, "rtmin+30": 64, " SIGHUP ": 1, "64": 64} {
		if n, err := SignalNumber(in); err != nil || n != want {
			t.Errorf("%q = %d %v, want %d", in, n, err, want)
		}
	}
	for _, bad := range []string{"SIGRTMIN+31", "SIGRTMIN3", "SIGRTMIN-1", "SIGRTMIN+x", "SIGNOPE", "0", "65"} {
		if _, err := SignalNumber(bad); err == nil {
			t.Errorf("%q must be refused", bad)
		}
	}
}

// TestParsePorts: faixa de mesmo tamanho vira uma porta por número; lados desiguais, faixa
// invertida e mais de MaxPorts são recusados; porta solta segue como antes.
func TestParsePorts(t *testing.T) {
	ps, err := ParsePorts("5173-5175:6173-6175/udp")
	if err != nil || len(ps) != 3 || ps[0] != (PortMap{5173, 6173, true}) || ps[2] != (PortMap{5175, 6175, true}) {
		t.Fatalf("range: %v %v", ps, err)
	}
	if ps, err := ParsePorts("8000-8001"); err != nil || len(ps) != 2 || ps[1] != (PortMap{8001, 8001, false}) {
		t.Fatalf("same-side range: %v %v", ps, err)
	}
	if ps, err := ParsePorts("8080:80/tcp"); err != nil || len(ps) != 1 || ps[0] != (PortMap{8080, 80, false}) {
		t.Fatalf("single: %v %v", ps, err)
	}
	if ps, err := ParsePorts("1-64"); err != nil || len(ps) != MaxPorts {
		t.Fatalf("exactly the ceiling: %d %v", len(ps), err)
	}
	for _, bad := range []string{"5173-5180:5173-5179", "10-5", "1-65", "1-65535", "a-b", "8000-", "8000-8001:x", "0-3"} {
		if _, err := ParsePorts(bad); err == nil {
			t.Errorf("%q must be refused", bad)
		}
	}
}

// TestSocketBridges: GUEST[:HOST] e HOST:GUEST com caminhos absolutos; repetir o caminho do
// guest, passar de 64 ou caminho relativo recusa.
func TestSocketBridges(t *testing.T) {
	if b, err := ParseExposeSocket("/run/app.sock"); err != nil || b != (SocketBridge{Guest: "/run/app.sock"}) {
		t.Fatalf("expose default host: %+v %v", b, err)
	}
	if b, err := ParseExposeSocket("/run/app.sock:/tmp/app.sock"); err != nil || b.Host != "/tmp/app.sock" {
		t.Fatalf("expose host: %+v %v", b, err)
	}
	if b, err := ParseMountSocket("/run/user/1000/podman.sock:/run/podman.sock"); err != nil || b.Guest != "/run/podman.sock" {
		t.Fatalf("mount: %+v %v", b, err)
	}
	for _, bad := range []string{"run/app.sock", "/run/app.sock:tmp/x"} {
		if _, err := ParseExposeSocket(bad); err == nil {
			t.Errorf("expose %q must be refused", bad)
		}
	}
	for _, bad := range []string{"/h.sock", "h.sock:/g.sock", "/h.sock:g.sock"} {
		if _, err := ParseMountSocket(bad); err == nil {
			t.Errorf("mount %q must be refused", bad)
		}
	}
	m := Machine{Name: "s", Stack: "python"}.WithDefaults()
	m.ExposeSockets = []SocketBridge{{Guest: "/run/a.sock"}}
	m.MountSockets = []SocketBridge{{Host: "/h.sock", Guest: "/run/b.sock"}}
	if err := m.Validate(); err != nil {
		t.Fatal(err)
	}
	dup := m
	dup.MountSockets = []SocketBridge{{Host: "/h.sock", Guest: "/run/a.sock"}}
	rel := m
	rel.MountSockets = []SocketBridge{{Host: "h.sock", Guest: "/run/b.sock"}}
	many := m
	many.ExposeSockets = nil
	for i := 0; i < MaxSocketBridges; i++ {
		many.ExposeSockets = append(many.ExposeSockets, SocketBridge{Guest: fmt.Sprintf("/run/%d.sock", i)})
	}
	for name, bad := range map[string]Machine{"duplicate guest": dup, "relative host": rel, "65 bridges": many} {
		if err := bad.Validate(); err == nil {
			t.Errorf("%s must be refused", name)
		}
	}
	many.MountSockets = nil
	if err := many.Validate(); err != nil {
		t.Fatalf("exactly %d bridges: %v", MaxSocketBridges, err)
	}
}

// TestParseMountStaged: ":staged" liga a cópia; não vale para bucket nem para volume de disco,
// e volta igual no String.
func TestParseMountStaged(t *testing.T) {
	v, err := ParseMount("/src:/app:staged")
	if err != nil || !v.Staged || v.ReadOnly || v.String() != "/src:/app:staged" {
		t.Fatalf("staged: %+v %v", v, err)
	}
	if v.DiskPath() != "/workspace/.disk/app" || (Mount{Guest: "/var/lib/x/"}).DiskPath() != "/workspace/.disk/var/lib/x" {
		t.Fatalf("disk path: %s", v.DiskPath())
	}
	for _, bad := range []string{"s3://bucket:/data:staged", "/src:/app:copy"} {
		if _, err := ParseMount(bad); err == nil {
			t.Errorf("%q must be refused", bad)
		}
	}
}
