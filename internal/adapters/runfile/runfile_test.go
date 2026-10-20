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
		"stack = \"python\"\nnet_backend = \"tsi\"":               `"tsi" is not supported`,
		"stack = \"python\"\nnet_backend = \"vmnet\"":             `must be virtio-net`,
		"stack = \"python\"\nports = [\"x\"]":                     `port "x"`,
		"stack = \"python\"\n[network]\nallow_host = [\"x.com\"]": "unknown key(s): network.allow_host",
		"stack = \"python\"\nvolumes = [\"./a\"]":                 "HOST:GUEST",
		"stack = \"python\"\nallow_system_mounts = true":          `"allow_system_mounts" is not supported yet`,
		"stack = \"python\"\n[health]\ncmd = [\"true\"]":          `"health" is not supported yet`,
		"stack = \"python\"\n[dev]\nusers = \"x\"":                "unknown key(s): dev.users",
		"stack = \"python\"\ncpu = 2":                             "unknown key(s): cpu",
		"stack = \"python\"\n[dev]\nenvs = [\"A=1\"]":             "unknown key(s): dev.envs",
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

// TestDockerSocketAndDiskVolumes: docker_socket e volume de disco vêm do arquivo; o volume de
// disco não é resolvido contra a pasta do arquivo (não é caminho do host).
func TestDockerSocketAndDiskVolumes(t *testing.T) {
	dir := t.TempDir()
	os.WriteFile(filepath.Join(dir, "runner.code"), []byte("stack = \"python\"\ndocker_socket = true\nvolumes = [\"/var/lib/docker\"]\n"), 0o600)
	m, err := Load(filepath.Join(dir, "runner.code"))
	if err != nil || !m.DockerSocket || len(m.Mounts) != 1 || !m.Mounts[0].Disk() || m.Mounts[0].Guest != "/var/lib/docker" {
		t.Fatalf("docker machine: %+v %v", m, err)
	}
	if o := Override(m, domain.Machine{}, func(k string) bool { return k == "docker-socket" }); o.DockerSocket {
		t.Fatal("--docker-socket=false must win")
	}
}

// TestAuthTable: [auth] ssh_agent liga o agente; --ssh-agent da CLI vence.
func TestAuthTable(t *testing.T) {
	m, err := Parse([]byte("stack = \"python\"\n[auth]\nssh_agent = true\n"))
	if err != nil || !m.SSHAgent {
		t.Fatalf("auth: %+v %v", m, err)
	}
	if o := Override(m, domain.Machine{}, func(k string) bool { return k == "ssh-agent" }); o.SSHAgent {
		t.Fatal("--ssh-agent=false wins")
	}
}

// TestSecretsTable: [secrets] vira a lista em ordem de nome; --secret-* da CLI soma.
func TestSecretsTable(t *testing.T) {
	m, err := Parse([]byte("stack = \"python\"\n[secrets]\nZ = { from_file = \"/run/z\" }\nA = { from_env = \"HOST_A\" }\n"))
	if err != nil || len(m.Secrets) != 2 || m.Secrets[0] != (domain.Secret{Name: "A", FromEnv: "HOST_A"}) || m.Secrets[1].FromFile != "/run/z" {
		t.Fatalf("secrets: %+v %v", m.Secrets, err)
	}
	o := Override(m, domain.Machine{Secrets: []domain.Secret{{Name: "B", FromEnv: "HB"}}}, func(string) bool { return false })
	if len(o.Secrets) != 3 || len(m.Secrets) != 2 {
		t.Fatal("CLI secrets add to the file's, without touching it")
	}
}

// TestUserKey: user no topo e no [dev] (que vence); --user da CLI vence os dois.
func TestUserKey(t *testing.T) {
	m, err := Parse([]byte("stack = \"python\"\nuser = \"app\"\n[dev]\nuser = \"dev\"\n"))
	if err != nil || m.User != "dev" {
		t.Fatalf("user: %q %v", m.User, err)
	}
	m, _ = Parse([]byte("stack = \"python\"\nuser = \"app\"\n"))
	if m.User != "app" {
		t.Fatalf("top-level user: %q", m.User)
	}
	if o := Override(m, domain.Machine{User: "root"}, func(k string) bool { return k == "user" }); o.User != "root" {
		t.Fatalf("--user wins: %q", o.User)
	}
}

// TestCredentialKeys: [[network.credentials]] do runner.code vira binding, com injection_location
// body; --credential da CLI soma às do arquivo.
func TestCredentialKeys(t *testing.T) {
	m, err := Parse([]byte(`
stack = "python"
[[network.credentials]]
name = "github"
environment_variable = "GITHUB_TOKEN"
allowed_hosts = ["api.github.com"]
methods = ["GET", "HEAD"]
set_header = "authorization"

[[network.credentials]]
name = "refresh"
environment_variable = "REFRESH_TOKEN"
allowed_hosts = ["auth.example.com"]
injection_location = { header = true, body = true }
`))
	if err != nil {
		t.Fatal(err)
	}
	cs := m.Network.Credentials
	if len(cs) != 2 || cs[0].Env != "GITHUB_TOKEN" || cs[0].SetHeader != "authorization" || strings.Join(cs[0].Methods, ",") != "GET,HEAD" || cs[0].Body || !cs[1].Body {
		t.Fatalf("credentials: %+v", cs)
	}
	over := Override(m, domain.Machine{Network: domain.Network{Credentials: []domain.Credential{{Name: "x"}}}}, func(string) bool { return false })
	if len(over.Network.Credentials) != 3 || len(m.Network.Credentials) != 2 {
		t.Fatal("CLI credentials add to the file's, without touching it")
	}
}

// TestNetworkKeys: net, ports (o [dev] substitui), net_backend virtio-net e a tabela
// [network] inteira vão para a rede da máquina.
func TestNetworkKeys(t *testing.T) {
	m, err := Parse([]byte(`
stack = "python"
net = true
net_backend = "virtio-net"
ports = ["8080:80", "53/udp"]
[network]
name = "lab"
allow_hosts = ["example.com"]
allow_host_patterns = ["*.cdn.net"]
allow_cidrs = ["1.1.1.1"]
deny_cidrs = ["10.0.0.0/8"]
block_private = true
allow_private = ["192.168.1.0/24"]
[dev]
ports = ["3000"]
`))
	if err != nil {
		t.Fatal(err)
	}
	n := m.Network
	e := n.Egress
	if !n.Enabled || len(n.Ports) != 1 || n.Ports[0] != (domain.PortMap{Host: 3000, Guest: 3000}) ||
		e.AllowHosts[0] != "example.com" || e.AllowHostPatterns[0] != "*.cdn.net" || e.AllowCIDRs[0] != "1.1.1.1" ||
		e.DenyCIDRs[0] != "10.0.0.0/8" || !e.BlockPrivate || e.AllowPrivate[0] != "192.168.1.0/24" || n.Name != "lab" {
		t.Fatalf("network: %+v", n)
	}
	m, _ = Parse([]byte("stack = \"python\"\nports = [\"8080:80\", \"53/udp\"]\n"))
	if len(m.Network.Ports) != 2 || !m.Network.Ports[1].UDP || m.Network.Enabled {
		t.Fatalf("ports without [dev]: %+v", m.Network)
	}
	over := Override(m, domain.Machine{Network: domain.Network{Enabled: true, Ports: []domain.PortMap{{Host: 1, Guest: 1}},
		Egress: domain.Egress{AllowHosts: []string{"a.com"}, BlockPrivate: true}}}, func(k string) bool { return k == "net" || k == "port" || k == "block-private" })
	if !over.Network.Enabled || len(over.Network.Ports) != 3 || over.Network.Egress.AllowHosts[0] != "a.com" || !over.Network.Egress.BlockPrivate {
		t.Fatalf("CLI network must add to the file: %+v", over.Network)
	}
	// --network troca a rede nomeada do arquivo; sem a flag, a do arquivo fica.
	m.Network.Name = "lab"
	if o := Override(m, domain.Machine{Network: domain.Network{Name: "other"}}, func(k string) bool { return k == "network" }); o.Network.Name != "other" {
		t.Fatalf("--network must win: %q", o.Network.Name)
	}
	if o := Override(m, domain.Machine{Network: domain.Network{Name: "other"}}, func(string) bool { return false }); o.Network.Name != "lab" {
		t.Fatalf("without --network the file stays: %q", o.Network.Name)
	}
}

// TestDisplayKeys: display vira a tela com a porta padrão ou a declarada; tamanho inválido
// é recusado.
func TestDisplayKeys(t *testing.T) {
	m, err := Parse([]byte("stack = \"python\"\ngpu = true\ndisplay = \"1280x800\"\n"))
	if err != nil || m.Display == nil || m.Display.Width != 1280 || m.Display.Port != domain.DefaultDisplayPort {
		t.Fatalf("%+v %v", m.Display, err)
	}
	m, _ = Parse([]byte("stack = \"python\"\ndisplay = \"800x600\"\ndisplay_port = 7000\n"))
	if m.Display.Port != 7000 {
		t.Fatalf("port: %+v", m.Display)
	}
	if _, err := Parse([]byte("stack = \"python\"\ndisplay = \"big\"\n")); err == nil {
		t.Fatal("bad display must fail")
	}
	over := Override(m, domain.Machine{Display: &domain.Display{Width: 1024, Height: 768, Port: 1}}, func(string) bool { return false })
	if over.Display.Width != 1024 {
		t.Fatal("--display must win over the file")
	}
}

// TestMounts: volumes do topo e do [dev] somam; o Load resolve "./" contra a pasta do
// arquivo e "~" contra o HOME; o que já é absoluto fica como está.
func TestMounts(t *testing.T) {
	dir := t.TempDir()
	p := filepath.Join(dir, DefaultName)
	os.WriteFile(p, []byte("stack = \"python\"\nvolumes = [\"./www:/var/www:ro\", \"/abs:/abs\"]\n[dev]\nvolumes = [\"~/models:/models\"]\n"), 0o600)
	m, err := Load(p)
	if err != nil {
		t.Fatal(err)
	}
	home, _ := os.UserHomeDir()
	if !reflect.DeepEqual(m.Mounts, []domain.Mount{{Host: filepath.Join(home, "models"), Guest: "/models"}}) {
		t.Fatalf("[dev] volumes must replace the top level: %+v", m.Mounts)
	}
	os.WriteFile(p, []byte("stack = \"python\"\nvolumes = [\"./www:/var/www:ro\", \"/abs:/abs\"]\n[dev]\nenv = [\"A=1\"]\n"), 0o600)
	m, _ = Load(p)
	want := []domain.Mount{{Host: filepath.Join(dir, "www"), Guest: "/var/www", ReadOnly: true}, {Host: "/abs", Guest: "/abs"}}
	if !reflect.DeepEqual(m.Mounts, want) {
		t.Fatalf("mounts: %+v", m.Mounts)
	}
	got, _ := ResolveMounts([]domain.Mount{{Host: "~", Guest: "/h"}, {Host: "a/../b", Guest: "/b"}, {Host: "s3://bkt/x", Guest: "/s"}}, "/base")
	if got[0].Host != home || got[1].Host != "/base/b" || got[2].Host != "s3://bkt/x" {
		t.Fatalf("resolve: %+v", got)
	}
	t.Setenv("HOME", "")
	if _, err := ResolveMounts([]domain.Mount{{Host: "~/x", Guest: "/x"}}, "/"); err == nil {
		t.Fatal("~ without HOME must fail")
	}
	over := Override(domain.Machine{Mounts: want[:1]}, domain.Machine{Mounts: want[1:]}, func(k string) bool { return k == "volume" })
	if len(over.Mounts) != 2 {
		t.Fatalf("--volume must add to the file: %+v", over.Mounts)
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

// TestNested: --nested chega à máquina pelo Override (sem isso a flag se perdia calada e a VM
// subia sem /dev/kvm), e o arquivo não liga nested: recusa nomeando a flag.
func TestNested(t *testing.T) {
	if o := Override(domain.Machine{Stack: "python"}, domain.Machine{Nested: true}, func(k string) bool { return k == "nested" }); !o.Nested {
		t.Fatal("--nested must reach the machine")
	}
	if o := Override(domain.Machine{Stack: "python"}, domain.Machine{Nested: true}, func(string) bool { return false }); o.Nested {
		t.Fatal("an unset flag must not turn nested on")
	}
	for _, k := range []string{"nested", "nested_virt"} {
		if _, err := Parse([]byte("stack = \"python\"\n" + k + " = true\n")); err == nil || !strings.Contains(err.Error(), "--nested") {
			t.Fatalf("%s in a file: %v", k, err)
		}
	}
}

// TestSocketBridgesOverride: as pontes de socket da CLI chegam à máquina pelo Override.
func TestSocketBridgesOverride(t *testing.T) {
	flags := domain.Machine{ExposeSockets: []domain.SocketBridge{{Guest: "/run/app.sock"}},
		MountSockets: []domain.SocketBridge{{Host: "/h.sock", Guest: "/run/h.sock"}}}
	o := Override(domain.Machine{Stack: "python"}, flags, func(k string) bool { return k == "expose-socket" || k == "mount-socket" })
	if len(o.ExposeSockets) != 1 || len(o.MountSockets) != 1 {
		t.Fatalf("bridges: %+v", o)
	}
	if o := Override(domain.Machine{Stack: "python"}, flags, func(string) bool { return false }); len(o.ExposeSockets)+len(o.MountSockets) != 0 {
		t.Fatal("unset flags add nothing")
	}
}
