package domain

import (
	"slices"
	"strings"
	"testing"
)

// TestParseAndroid trava o formato aceito (com e sem @DPI) e os limites de tela e densidade.
func TestParseAndroid(t *testing.T) {
	ok := []struct {
		in   string
		want Android
	}{
		{"720x1280@320", Android{720, 1280, 320}},
		{" 1080X1920@480 ", Android{1080, 1920, 480}},
		{"720x1280", Android{720, 1280, DefaultAndroid.DPI}},
		{"320x320@120", Android{320, 320, 120}},
		{"4096x4096@640", Android{4096, 4096, 640}},
	}
	for _, c := range ok {
		got, err := ParseAndroid(c.in)
		if err != nil || got != c.want {
			t.Errorf("ParseAndroid(%q) = %+v, %v; want %+v", c.in, got, err, c.want)
		}
	}
	bad := map[string]string{
		"":              "WIDTHxHEIGHT",
		"720":           "WIDTHxHEIGHT",
		"axb":           "WIDTHxHEIGHT",
		"720x1280@x":    "is not a number",
		"319x1280":      "between 320x320",
		"720x4097":      "between 320x320",
		"720x1280@119":  "dpi 119",
		"720x1280@641":  "dpi 641",
		"720x-1280@320": "WIDTHxHEIGHT",
	}
	for in, want := range bad {
		if _, err := ParseAndroid(in); err == nil || !strings.Contains(err.Error(), want) {
			t.Errorf("ParseAndroid(%q) err = %v; want contendo %q", in, err, want)
		}
	}
}

// TestAndroidBootArgs trava os parâmetros que o /init do Redroid precisa: o perfil de contêiner, a renderização por
// software e a tela pedida.
func TestAndroidBootArgs(t *testing.T) {
	got := Android{Width: 1080, Height: 1920, DPI: 480}.BootArgs()
	want := []string{"qemu=1", "androidboot.hardware=redroid", "androidboot.redroid_gpu_mode=guest",
		"androidboot.redroid_width=1080", "androidboot.redroid_height=1920", "androidboot.redroid_dpi=480",
		"androidboot.redroid_net_ndns=2", "androidboot.redroid_net_dns1=8.8.8.8", "androidboot.redroid_net_dns2=1.1.1.1"}
	if !slices.Equal(got, want) {
		t.Fatalf("BootArgs = %v; want %v", got, want)
	}
}

// TestValidateAndroid confere que a máquina Android válida passa e que cada recurso que depende do agente do rc (ou
// da virtio-gpu) é recusado com uma mensagem que diz o porquê.
func TestValidateAndroid(t *testing.T) {
	base := func() Machine {
		a := DefaultAndroid
		return Machine{Name: "a1", Stack: "redroid", Image: "redroid/redroid:14.0.0_64only-latest", Android: &a}.WithDefaults()
	}
	if err := base().Validate(); err != nil {
		t.Fatalf("android válido recusado: %v", err)
	}
	if !base().IsAndroid() || (Machine{}).IsAndroid() {
		t.Fatal("IsAndroid não segue o campo Android")
	}
	cases := map[string]func(m *Machine){
		"needs image":             func(m *Machine) { m.Image = "" },
		"drop display and gpu":    func(m *Machine) { m.GPU = true },
		"init, workload and user": func(m *Machine) { m.Workload = []string{"sh"} },
		"not available":           func(m *Machine) { m.SSHAgent = true },
		"dpi 10":                  func(m *Machine) { m.Android.DPI = 10 },
	}
	for want, mutate := range cases {
		m := base()
		mutate(&m)
		if err := m.Validate(); err == nil || !strings.Contains(err.Error(), want) {
			t.Errorf("%s: err = %v; want contendo %q", want, err, want)
		}
	}
	// Display sem GPU já cai na regra antiga; com GPU, a do Android diz o motivo certo.
	m := base()
	d := Display{Width: 800, Height: 600, Port: DefaultDisplayPort}
	m.Display, m.GPU = &d, true
	if err := m.Validate(); err == nil || !strings.Contains(err.Error(), "drop display and gpu") {
		t.Errorf("display+gpu: err = %v", err)
	}
	m = base()
	m.Mounts = []Mount{{Host: "/tmp", Guest: "/data/x"}}
	if err := m.Validate(); err == nil || !strings.Contains(err.Error(), "volumes") {
		t.Errorf("mounts: err = %v", err)
	}
}
