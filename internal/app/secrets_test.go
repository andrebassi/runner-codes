package app

import (
	"context"
	"encoding/json"
	"net"
	"os"
	"path/filepath"
	"runner.codes/internal/ports"
	"strings"
	"testing"
	"time"

	"runner.codes/internal/domain"
	"runner.codes/internal/protocol"
)

// TestSSHAgentAtStart: com ssh_agent, a spec leva o SSH_AUTH_SOCK de quem dá o start, o guest
// abre o socket do agente e todo comando recebe o SSH_AUTH_SOCK de dentro; sem agente no host,
// o start é recusado com o motivo.
func TestSSHAgentAtStart(t *testing.T) {
	m, _, _, d, g := machinesFixture(t)
	ctx := context.Background()
	t.Setenv("SSH_AUTH_SOCK", "/tmp/host-agent.sock")
	m.Create(ctx, domain.Machine{Name: "a", Stack: "python", SSHAgent: true})
	if _, err := m.Start(ctx, "a"); err != nil {
		t.Fatal(err)
	}
	if d.specs[len(d.specs)-1].SSHAgent != "/tmp/host-agent.sock" {
		t.Fatal("the spec carries the host agent socket")
	}
	var agent, env bool
	for _, j := range g.jobs {
		agent = agent || (j.Op == protocol.OpSSHAgent && j.Path == protocol.GuestSSHAgentSocket && j.Port == protocol.SSHAgentPort)
		env = env || (j.Op == protocol.OpSetEnv && strings.Join(j.Env, " ") == "SSH_AUTH_SOCK="+protocol.GuestSSHAgentSocket)
	}
	if !agent || !env {
		t.Fatalf("agent job %v, env %v", agent, env)
	}
	m.Stop(ctx, "a", 0)
	t.Setenv("SSH_AUTH_SOCK", "")
	if _, err := m.Start(ctx, "a"); errCode(err) != domain.CodeConfig {
		t.Fatalf("no host agent: %v", err)
	}
}

// TestSecretsAtStart: no start, os segredos resolvidos (variável do host e arquivo) vão ao
// guest num set-env antes do init; o registro guarda só a fonte; o segredo de mesmo nome de
// uma credencial alimenta a binding (na spec) e não entra no guest; variável ausente e
// arquivo ilegível falham o start com o motivo.
func TestSecretsAtStart(t *testing.T) {
	m, st, _, d, g := machinesFixture(t)
	ctx := context.Background()
	var hosts []string
	m.NewCredentialCA = fakeCA(&hosts)
	file := filepath.Join(t.TempDir(), "tok")
	os.WriteFile(file, []byte("do-arquivo\n"), 0o600)
	t.Setenv("RC_TEST_HOST_API", "do-ambiente")
	mc := domain.Machine{Name: "s", Stack: "python", Init: []string{"true"},
		Secrets: []domain.Secret{{Name: "API", FromEnv: "RC_TEST_HOST_API"}, {Name: "TOK", FromFile: file}, {Name: "GH", FromFile: file}},
		Network: domain.Network{Credentials: []domain.Credential{{Name: "gh", Env: "GH", Hosts: []string{"api.github.com"}}}}}
	if _, err := m.Create(ctx, mc); err != nil {
		t.Fatal(err)
	}
	// O registro é o que vai para o machine.json: serializado, não pode ter valor nenhum.
	if b, _ := json.Marshal(st.ms["s"]); strings.Contains(string(b), "do-ambiente") || strings.Contains(string(b), "do-arquivo") || !strings.Contains(string(b), "RC_TEST_HOST_API") {
		t.Fatalf("the record holds only the source: %s", b)
	}
	if _, err := m.Start(ctx, "s"); err != nil {
		t.Fatal(err)
	}
	setEnv, initJob := -1, -1
	for i, j := range g.jobs {
		switch {
		case j.Op == protocol.OpSetEnv:
			setEnv = i
			if strings.Join(j.Env, " ") != "API=do-ambiente TOK=do-arquivo" {
				t.Fatalf("guest secrets (the binding's one stays out): %v", j.Env)
			}
		case j.TraceID == "init-0":
			initJob = i
		}
	}
	if setEnv < 0 || initJob < 0 || setEnv > initJob {
		t.Fatalf("secrets must reach the guest before init: set-env=%d init=%d", setEnv, initJob)
	}
	c := d.specs[len(d.specs)-1].Net.Network.Credentials[0]
	if c.SourceFile != file || c.Placeholder == "" {
		t.Fatalf("the same-named secret feeds the binding: %+v", c)
	}
	if saved, _ := st.Load("s"); saved.Network.Credentials[0].SourceFile != "" {
		t.Fatal("the source goes in the spec, not in the record")
	}
	m.Stop(ctx, "s", 0)
	os.Unsetenv("RC_TEST_HOST_API")
	if _, err := m.Start(ctx, "s"); err == nil || !strings.Contains(err.Error(), "resolve secret 'API': env_unset") {
		t.Fatalf("unset host variable: %v", err)
	}
	m.Create(ctx, domain.Machine{Name: "f", Stack: "python", Secrets: []domain.Secret{{Name: "X", FromFile: "/nonexistent"}}})
	if _, err := m.Start(ctx, "f"); err == nil || !strings.Contains(err.Error(), "file_unreadable") {
		t.Fatalf("unreadable file: %v", err)
	}
}

// TestNestedSpec: a máquina com Nested liga o nested na spec, e só ela.
func TestNestedSpec(t *testing.T) {
	m, _, _, d, _ := machinesFixture(t)
	ctx := context.Background()
	m.Create(ctx, domain.Machine{Name: "n", Stack: "python", Nested: true})
	m.Create(ctx, domain.Machine{Name: "plain", Stack: "python"})
	if _, err := m.Start(ctx, "n"); err != nil || !d.specs[len(d.specs)-1].Nested {
		t.Fatalf("nested machine: %v", err)
	}
	if _, err := m.Start(ctx, "plain"); err != nil || d.specs[len(d.specs)-1].Nested {
		t.Fatalf("plain machine: %v", err)
	}
}

// TestStartVMMDies: o vmm que morre antes de o guest responder encerra a espera na hora e o
// erro traz a última linha do log dele (o motivo); o que fica vivo sem responder espera o prazo
// e diz o que o console e o vmm mostraram.
func TestStartVMMDies(t *testing.T) {
	m, st, _, d, g := machinesFixture(t)
	ctx := context.Background()
	m.ReadyTimeout = 10 * time.Second
	g.hang = true
	m.Create(ctx, domain.Machine{Name: "n", Stack: "python", Nested: true})
	d.dieLog = "Error: nested virtualization is not supported on this host"
	began := time.Now()
	_, err := m.Start(ctx, "n")
	if err == nil || !strings.Contains(err.Error(), "failed to start: Error: nested virtualization is not supported") {
		t.Fatalf("dead vmm: %v", err)
	}
	if took := time.Since(began); took > 2*time.Second {
		t.Fatalf("a dead vmm must end the wait at once, took %s", took)
	}
	if st.PID("n") != 0 {
		t.Fatal("the pid of a dead vmm is cleared")
	}
	d.dieLog = ""
	m.ReadyTimeout = 200 * time.Millisecond
	_, err = m.Start(ctx, "n")
	if err == nil || !strings.Contains(err.Error(), "did not become ready") || !strings.Contains(err.Error(), "vmm: (no log)") {
		t.Fatalf("live vmm that never answers: %v", err)
	}
}

// TestLastLine: a última linha não vazia; sem nada, "(no log)".
func TestLastLine(t *testing.T) {
	for in, want := range map[string]string{"a\nb\n\n": "b", "": "(no log)", "(no console)": "(no log)", "  x  ": "x", readFrom("/nope", 0): "(no log)"} {
		if got := lastLine(in); got != want {
			t.Errorf("lastLine(%q) = %q, want %q", in, got, want)
		}
	}
}

// TestSocketBridgesAtStart: --expose-socket sem HOST cai no diretório da máquina; o
// --mount-socket confere que o socket existe no host, vai na spec com a sua porta vsock e
// manda o relay ao guest; caminho que não é socket recusa com o nome.
func TestSocketBridgesAtStart(t *testing.T) {
	m, st, _, d, g := machinesFixture(t)
	ctx := context.Background()
	hostSock := filepath.Join(t.TempDir(), "h.sock")
	l, err := net.Listen("unix", hostSock)
	if err != nil {
		t.Fatal(err)
	}
	defer l.Close()
	hostSock2 := filepath.Join(t.TempDir(), "h2.sock")
	l2, err := net.Listen("unix", hostSock2)
	if err != nil {
		t.Fatal(err)
	}
	defer l2.Close()
	m.Create(ctx, domain.Machine{Name: "sb", Stack: "python",
		ExposeSockets: []domain.SocketBridge{{Guest: "/run/app.sock"}, {Guest: "/run/x.sock", Host: "/srv/x.sock"}},
		MountSockets:  []domain.SocketBridge{{Host: hostSock, Guest: "/run/h.sock"}, {Host: hostSock2, Guest: "/run/h2.sock"}}})
	if _, err := m.Start(ctx, "sb"); err != nil {
		t.Fatal(err)
	}
	spec := d.specs[len(d.specs)-1]
	if len(spec.Sockets) != 2 || spec.Sockets[0].Host != filepath.Join(st.Dir("sb"), "app.sock") || spec.Sockets[1].Host != "/srv/x.sock" {
		t.Fatalf("exposed: %+v", spec.Sockets)
	}
	if len(spec.HostSockets) != 2 || spec.HostSockets[0] != (ports.VMSocket{Host: hostSock, Guest: "/run/h.sock", Port: protocol.MountSocketPortBase}) ||
		spec.HostSockets[1].Port != protocol.MountSocketPortBase+1 {
		t.Fatalf("mounted: %+v", spec.HostSockets)
	}
	var relay, relay2 bool
	for _, j := range g.jobs {
		relay = relay || (j.Op == protocol.OpSSHAgent && j.Path == "/run/h.sock" && j.Port == protocol.MountSocketPortBase)
		relay2 = relay2 || (j.Op == protocol.OpSSHAgent && j.Path == "/run/h2.sock" && j.Port == protocol.MountSocketPortBase+1)
	}
	if !relay || !relay2 {
		t.Fatal("the guest relay for the mounted socket")
	}
	m.Stop(ctx, "sb", 0)
	m.Create(ctx, domain.Machine{Name: "gone", Stack: "python", MountSockets: []domain.SocketBridge{{Host: "/nope.sock", Guest: "/run/h.sock"}}})
	if _, err := m.Start(ctx, "gone"); errCode(err) != domain.CodeConfig || !strings.Contains(err.Error(), "/nope.sock") {
		t.Fatalf("missing host socket: %v", err)
	}
}
