package runfile

import (
	"os"
	"path/filepath"
	"reflect"
	"strings"
	"testing"

	"runner.code/internal/domain"
)

// full é um runner.code com tudo o que é suportado, inclusive o perfil [dev].
const full = `
stack = "python"
entrypoint = ["python3", "-m"]
cmd = ["http.server", "8000"]
env = ["A=1"]
workdir = "/app"
init = ["mkdir -p /app", "echo ready > /app/ok"]
stop_signal = "SIGINT"
stop_timeout = 30
stop_on_exit = true
cpus = 4
memory = 4096
storage = 20
gpu = true
gpu_vram = 2048

[dev]
env = ["MODE=dev"]
workdir = "/app/src"
init = ["echo dev"]
`

// TestParseFull: cada chave vai para o campo certo; entrypoint+cmd viram o workload; o
// [dev] soma env e substitui workdir e init.
func TestParseFull(t *testing.T) {
	m, err := Parse([]byte(full))
	if err != nil {
		t.Fatal(err)
	}
	if m.Stack != "python" || m.CPUs != 4 || m.MemoryMiB != 4096 || m.DiskGiB != 20 || !m.GPU || m.GPUVRAMMiB != 2048 {
		t.Fatalf("resources: %+v", m)
	}
	if !reflect.DeepEqual(m.Workload, []string{"python3", "-m", "http.server", "8000"}) {
		t.Fatalf("workload: %v", m.Workload)
	}
	if !reflect.DeepEqual(m.Env, []string{"A=1", "MODE=dev"}) || m.Workdir != "/app/src" || !reflect.DeepEqual(m.Init, []string{"echo dev"}) {
		t.Fatalf("dev merge: env=%v workdir=%s init=%v", m.Env, m.Workdir, m.Init)
	}
	if m.StopSignal != "SIGINT" || m.StopTimeout != 30 || !m.StopOnExit {
		t.Fatalf("stop policy: %+v", m)
	}
}

// TestParseMinimal: só a imagem; nada mais é inventado (zeros ficam para os padrões).
func TestParseMinimal(t *testing.T) {
	m, err := Parse([]byte(`image = "redis:7-alpine"`))
	if err != nil || m.Image != "redis:7-alpine" || m.Stack != "" || m.CPUs != 0 || m.Workload != nil {
		t.Fatalf("%+v %v", m, err)
	}
	m, err = Parse([]byte("stack = \"go\"\n[dev]\nenv = [\"X=1\"]\n"))
	if err != nil || m.Workdir != "" || m.Init != nil || m.Env[0] != "X=1" {
		t.Fatalf("dev without workdir/init must keep the top level: %+v %v", m, err)
	}
}

// TestParseRefusals: chave do runner.code ainda não suportada sai com o motivo (rede, montagem,
// segredos); chave desconhecida sai com o nome; TOML quebrado e stack+image também são erro.
func TestParseRefusals(t *testing.T) {
	cases := map[string]string{
		"net = true\nstack = \"python\"":                           `"net" is not supported yet: networking`,
		"stack = \"python\"\n[network]\nallow_hosts = [\"x.com\"]": `"network" is not supported yet: egress`,
		"stack = \"python\"\nvolumes = [\"./a:/a\"]":               `"volumes" is not supported yet`,
		"stack = \"python\"\n[secrets]\nA = {from_env = \"B\"}":    `"secrets" is not supported yet`,
		"stack = \"python\"\n[dev]\nports = [\"80\"]":              `"dev.ports" is not supported yet`,
		"stack = \"python\"\ncpu = 2":                              "unknown key(s): cpu",
		"stack = \"python\"\n[dev]\nenvs = [\"A=1\"]":              "unknown key(s): dev.envs",
		"stack = ":                               "runner.code:",
		"stack = \"python\"\nimage = \"alpine\"": "stack or image, not both",
		"stack = \"python\"\ncpus = \"four\"":    "runner.code:",
	}
	for in, want := range cases {
		if _, err := Parse([]byte(in)); err == nil || !strings.Contains(err.Error(), want) {
			t.Fatalf("%q: want %q, got %v", in, want, err)
		}
	}
}

// TestLoad: lê do disco e prefixa o erro com o caminho; arquivo ausente é erro.
func TestLoad(t *testing.T) {
	dir := t.TempDir()
	p := filepath.Join(dir, DefaultName)
	os.WriteFile(p, []byte(full), 0o600)
	if m, err := Load(p); err != nil || m.Stack != "python" {
		t.Fatalf("%+v %v", m, err)
	}
	bad := filepath.Join(dir, "bad.code")
	os.WriteFile(bad, []byte("cpu = 1"), 0o600)
	if _, err := Load(bad); err == nil || !strings.HasPrefix(err.Error(), bad+": unknown key") {
		t.Fatalf("error must name the file: %v", err)
	}
	if _, err := Load(filepath.Join(dir, "none")); err == nil {
		t.Fatal("missing file must fail")
	}
}

// TestOverride: só a flag passada vence o arquivo (zero explícito inclusive); env soma;
// stack e image se excluem; o workload da CLI (depois do --) substitui o do arquivo.
func TestOverride(t *testing.T) {
	base, _ := Parse([]byte(full))
	flags := domain.Machine{Name: "web", Env: []string{"B=2"}, Image: "alpine:3.20", Workload: []string{"sleep", "1"},
		MemoryMiB: 512, DiskGiB: 5, Workdir: "/w", Init: []string{"true"}, StopSignal: "SIGTERM"}
	got := Override(base, flags, func(string) bool { return true })
	if got.Name != "web" || got.CPUs != 0 || got.Stack != "" || got.Image != "alpine:3.20" || got.MemoryMiB != 512 || got.DiskGiB != 5 ||
		got.GPU || got.GPUVRAMMiB != 0 || got.Workdir != "/w" || got.Init[0] != "true" || got.StopSignal != "SIGTERM" ||
		got.StopTimeout != 0 || got.StopOnExit || got.Workload[0] != "sleep" {
		t.Fatalf("every flag set must win: %+v", got)
	}
	if !reflect.DeepEqual(got.Env, []string{"A=1", "MODE=dev", "B=2"}) {
		t.Fatalf("env must extend: %v", got.Env)
	}
	none := Override(base, domain.Machine{Name: "web"}, func(string) bool { return false })
	if none.Name != "web" || none.Stack != "python" || none.CPUs != 4 || none.Workload[0] != "python3" || none.StopTimeout != 30 {
		t.Fatalf("no flag set must keep the file: %+v", none)
	}
	st := Override(domain.Machine{Image: "redis"}, domain.Machine{Stack: "go"}, func(k string) bool { return k == "stack" })
	if st.Stack != "go" || st.Image != "" {
		t.Fatalf("stack must clear image: %+v", st)
	}
}
