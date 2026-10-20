package app

import (
	"context"
	"errors"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"runner.code/internal/domain"
	"runner.code/internal/protocol"
)

// fakeCA devolve um par fixo e guarda os hosts pedidos.
func fakeCA(hosts *[]string) func([]string) ([]byte, []byte, error) {
	return func(h []string) ([]byte, []byte, error) {
		*hosts = h
		return []byte("CERT"), []byte("KEY"), nil
	}
}

// credMachine é uma máquina com duas bindings (uma em dois hosts).
func credMachine(name string) domain.Machine {
	return domain.Machine{Name: name, Stack: "python", Env: []string{"A=1", "SSL_CERT_FILE=/mine.pem"}, Network: domain.Network{Credentials: []domain.Credential{
		{Name: "notion", Env: "NOTION_API_KEY", Hosts: []string{"api.notion.com"}},
		{Name: "gh", Env: "GH_TOKEN", Hosts: []string{"api.github.com", "api.notion.com"}},
	}}}
}

// TestCredentialCreate: o create dá um placeholder estável a cada binding, grava a CA da
// máquina (chave 0600) restrita aos hosts sem repetição; sem gerador de CA a credencial é
// recusada, e CA que falha não deixa a máquina pela metade.
func TestCredentialCreate(t *testing.T) {
	m, st, _, _, _ := machinesFixture(t)
	ctx := context.Background()
	if _, err := m.Create(ctx, credMachine("no")); errCode(err) != domain.CodeConfig {
		t.Fatalf("without a CA generator: %v", err)
	}
	var hosts []string
	m.NewCredentialCA = fakeCA(&hosts)
	mc, err := m.Create(ctx, credMachine("c"))
	if err != nil {
		t.Fatal(err)
	}
	for _, c := range mc.Network.Credentials {
		if !strings.HasPrefix(c.Placeholder, domain.PlaceholderPrefix+strings.ToUpper(c.Name)+"_") || len(c.Placeholder) < 40 {
			t.Fatalf("placeholder: %q", c.Placeholder)
		}
	}
	if mc.Network.Credentials[0].Placeholder == mc.Network.Credentials[1].Placeholder {
		t.Fatal("placeholders must differ")
	}
	if saved, _ := st.Load("c"); saved.Network.Credentials[0].Placeholder != mc.Network.Credentials[0].Placeholder {
		t.Fatal("the placeholder is stored with the machine")
	}
	if strings.Join(hosts, " ") != "api.notion.com api.github.com" {
		t.Fatalf("CA hosts: %v", hosts)
	}
	fi, err := os.Stat(filepath.Join(st.Dir("c"), machineCredCAKey))
	if err != nil || fi.Mode().Perm() != 0o600 {
		t.Fatalf("the CA key must be 0600: %v", err)
	}
	m.NewCredentialCA = func([]string) ([]byte, []byte, error) { return nil, nil, errors.New("no entropy") }
	if _, err := m.Create(ctx, credMachine("broken")); err == nil {
		t.Fatal("a failed CA must fail the create")
	}
	if _, err := st.Load("broken"); err == nil {
		t.Fatal("a failed CA must not leave the machine behind")
	}
}

// TestCredentialStart: o start leva a CA à spec, instala ca.pem e o pacote no guest antes do
// resto, e todo comando recebe o placeholder e as variáveis de TLS (o env declarado vence).
func TestCredentialStart(t *testing.T) {
	m, st, _, d, g := machinesFixture(t)
	ctx := context.Background()
	var hosts []string
	m.NewCredentialCA = fakeCA(&hosts)
	mc := credMachine("s")
	mc.Init = []string{"echo init"}
	mc, _ = m.Create(ctx, mc)
	if _, err := m.Start(ctx, "s"); err != nil {
		t.Fatal(err)
	}
	net := d.specs[len(d.specs)-1].Net
	if net == nil || net.CACert != filepath.Join(st.Dir("s"), machineCredCACert) || net.CAKey != filepath.Join(st.Dir("s"), machineCredCAKey) {
		t.Fatalf("spec CA: %+v", net)
	}
	var install, init protocol.Job
	for _, j := range g.jobs {
		switch j.TraceID {
		case "credential-ca":
			install = j
		case "init-0":
			init = j
		}
	}
	if len(install.Argv) != 7 || install.Argv[4] != "CERT" || install.Argv[5] != guestCredCA || install.Argv[6] != guestCredBundle {
		t.Fatalf("CA install job: %+v", install.Argv)
	}
	env := strings.Join(init.Env, " ")
	ph := mc.Network.Credentials[0].Placeholder
	if !strings.Contains(env, "NOTION_API_KEY="+ph) || !strings.Contains(env, "NODE_EXTRA_CA_CERTS="+guestCredCA) ||
		!strings.HasSuffix(env, "A=1 SSL_CERT_FILE=/mine.pem") {
		t.Fatalf("init env: %s", env)
	}
	res, err := m.Exec(ctx, "s", ExecRequest{Argv: []string{"env"}, Env: []string{"B=2"}}, nil)
	if err != nil || res.ExitCode != 0 {
		t.Fatal(err)
	}
	if last := g.jobs[len(g.jobs)-1]; !strings.Contains(strings.Join(last.Env, " "), "GH_TOKEN=") || last.Env[len(last.Env)-1] != "B=2" {
		t.Fatalf("exec env: %v", last.Env)
	}
	os.Remove(filepath.Join(st.Dir("s"), machineCredCACert))
	m.Stop(ctx, "s", 0)
	if _, err := m.Start(ctx, "s"); err == nil || !strings.Contains(err.Error(), "credential CA") {
		t.Fatalf("a missing CA certificate fails the start: %v", err)
	}
}

// TestCredentialBranch: a filha herda placeholders e a CA da mãe; mãe sem credencial não
// copia nada.
func TestCredentialBranch(t *testing.T) {
	m, st, _, _, _ := machinesFixture(t)
	ctx := context.Background()
	var hosts []string
	m.NewCredentialCA = fakeCA(&hosts)
	mom, _ := m.Create(ctx, credMachine("mom"))
	kid, err := m.Branch(ctx, "mom", "kid")
	if err != nil {
		t.Fatal(err)
	}
	if kid.Network.Credentials[0].Placeholder != mom.Network.Credentials[0].Placeholder {
		t.Fatal("the child keeps the parent's placeholders")
	}
	for _, f := range []string{machineCredCACert, machineCredCAKey} {
		if b, _ := os.ReadFile(filepath.Join(st.Dir("kid"), f)); len(b) == 0 {
			t.Fatalf("the child must have the parent's %s", f)
		}
	}
	if fi, _ := os.Stat(filepath.Join(st.Dir("kid"), machineCredCAKey)); fi.Mode().Perm() != 0o600 {
		t.Fatal("the copied key stays 0600")
	}
	m.Create(ctx, domain.Machine{Name: "plain", Stack: "python"})
	if _, err := m.Branch(ctx, "plain", "plain2"); err != nil {
		t.Fatal(err)
	}
	if _, err := os.Stat(filepath.Join(st.Dir("plain2"), machineCredCACert)); !os.IsNotExist(err) {
		t.Fatal("no CA to copy")
	}
	os.Chmod(filepath.Join(st.Dir("mom"), machineCredCAKey), 0o000)
	if _, err := m.Branch(ctx, "mom", "kid2"); err == nil {
		t.Fatal("an unreadable CA key must fail the branch")
	}
	if _, err := st.Load("kid2"); err == nil {
		t.Fatal("a failed branch must not leave the child")
	}
}
