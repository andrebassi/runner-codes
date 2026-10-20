package app

import (
	"context"
	"crypto/rand"
	"errors"
	"fmt"
	"os"
	"path/filepath"
	"strings"

	"runner.code/internal/domain"
	"runner.code/internal/protocol"
)

// Arquivos da CA de credenciais no diretório da máquina (só no host): o certificado vai ao
// guest, a chave nunca sai daqui.
const (
	machineCredCACert = "cred-ca.pem"
	machineCredCAKey  = "cred-ca.key"
)

// Onde o guest encontra a CA e o pacote de raízes (as do sistema + a da máquina).
const (
	guestCredCA     = "/run/rc/credentials/ca.pem"
	guestCredBundle = "/run/rc/ca-bundle.pem"
)

// mintPlaceholders dá a cada binding sem placeholder um novo, aleatório: o placeholder é o
// único dado da credencial que o guest vê, e fica estável enquanto a máquina existir.
func mintPlaceholders(mc *domain.Machine) {
	for i, c := range mc.Network.Credentials {
		if c.Placeholder == "" {
			var b [16]byte
			rand.Read(b[:])
			mc.Network.Credentials[i].Placeholder = domain.MintPlaceholder(c.Name, b)
		}
	}
}

// credentialHosts junta os hosts de todas as bindings (a restrição de nome da CA).
func credentialHosts(cs []domain.Credential) []string {
	var hosts []string
	seen := map[string]bool{}
	for _, c := range cs {
		for _, h := range c.Hosts {
			if !seen[h] {
				seen[h] = true
				hosts = append(hosts, h)
			}
		}
	}
	return hosts
}

// writeCredentialCA grava a CA da máquina no diretório dela (chave 0600).
func (m *Machines) writeCredentialCA(mc domain.Machine) error {
	cert, key, err := m.NewCredentialCA(credentialHosts(mc.Network.Credentials))
	if err != nil {
		return fmt.Errorf("credential CA: %w", err)
	}
	dir := m.Store.Dir(mc.Name)
	if err := os.WriteFile(filepath.Join(dir, machineCredCAKey), key, 0o600); err != nil {
		return err
	}
	return os.WriteFile(filepath.Join(dir, machineCredCACert), cert, 0o644)
}

// copyCredentialCA leva a CA da mãe para a filha do branch: o disco clonado já confia nela,
// e processos que a filha herdar continuam funcionando.
func (m *Machines) copyCredentialCA(from, to string) error {
	for _, f := range []string{machineCredCACert, machineCredCAKey} {
		b, err := os.ReadFile(filepath.Join(m.Store.Dir(from), f))
		if os.IsNotExist(err) {
			return nil // mãe sem credencial
		}
		if err != nil {
			return err
		}
		mode := os.FileMode(0o644)
		if f == machineCredCAKey {
			mode = 0o600
		}
		if err := os.WriteFile(filepath.Join(m.Store.Dir(to), f), b, mode); err != nil {
			return err
		}
	}
	return nil
}

// jobEnv é o ambiente de todo comando da máquina: placeholders e as variáveis que apontam as
// bibliotecas TLS para o pacote com a CA (curl, Python, Node, Deno, Git e OpenSSL funcionam
// sem mudança), e por último o env declarado, que vence.
func jobEnv(mc domain.Machine) []string {
	if len(mc.Network.Credentials) == 0 {
		// Cópia: quem chama acrescenta o env do pedido, e escrever na capacidade sobrando do
		// mc.Env misturaria o env de dois exec concorrentes.
		return append([]string{}, mc.Env...)
	}
	env := []string{"SSL_CERT_FILE=" + guestCredBundle, "CURL_CA_BUNDLE=" + guestCredBundle, "REQUESTS_CA_BUNDLE=" + guestCredBundle,
		"GIT_SSL_CAINFO=" + guestCredBundle, "NODE_EXTRA_CA_CERTS=" + guestCredCA, "DENO_CERT=" + guestCredCA}
	for _, c := range mc.Network.Credentials {
		env = append(env, c.Env+"="+c.Placeholder)
	}
	return append(env, mc.Env...)
}

// installCredentialCA põe a CA no guest e monta o pacote de raízes: as do sistema da imagem
// (o primeiro caminho que existir, Alpine, Debian ou Fedora) e a da máquina por último.
// Roda a cada start: /run não persiste.
func (m *Machines) installCredentialCA(ctx context.Context, mc domain.Machine, sock string) error {
	cert, err := os.ReadFile(filepath.Join(m.Store.Dir(mc.Name), machineCredCACert))
	if err != nil {
		return fmt.Errorf("machine %s: credential CA: %w", mc.Name, err)
	}
	script := `mkdir -p /run/rc/credentials && printf '%s' "$1" > "$2" && ` +
		`{ for f in /etc/ssl/certs/ca-certificates.crt /etc/ssl/cert.pem /etc/pki/tls/certs/ca-bundle.crt; do ` +
		`[ -f "$f" ] && { cat "$f"; break; }; done; cat "$2"; } > "$3"`
	res, err := m.Guest.Run(ctx, sock, protocol.Job{TraceID: "credential-ca", Op: protocol.OpExec,
		Argv: []string{"sh", "-c", script, "install-ca", string(cert), guestCredCA, guestCredBundle}})
	if err != nil {
		return fmt.Errorf("machine %s: install credential CA: %w", mc.Name, err)
	}
	if res.ExitCode != 0 {
		return fmt.Errorf("machine %s: install credential CA: %s", mc.Name, asString(res.Stderr))
	}
	return nil
}

// bindingSources liga cada credencial ao segredo de mesmo nome (a variável da binding), se
// houver: a binding passa a ler daquela fonte, e o segredo não entra no guest. Devolve as
// credenciais para a spec (cópia) e os segredos que vão para o guest.
func bindingSources(mc domain.Machine) ([]domain.Credential, []domain.Secret) {
	feeds := map[string]domain.Secret{}
	for _, s := range mc.Secrets {
		feeds[s.Name] = s
	}
	creds := append([]domain.Credential(nil), mc.Network.Credentials...)
	used := map[string]bool{}
	for i, c := range creds {
		if s, ok := feeds[c.Env]; ok {
			creds[i].SourceEnv, creds[i].SourceFile = s.FromEnv, s.FromFile
			used[s.Name] = true
		}
	}
	var guest []domain.Secret
	for _, s := range mc.Secrets {
		if !used[s.Name] {
			guest = append(guest, s)
		}
	}
	return creds, guest
}

// resolveSecrets lê o valor de cada segredo no host (ambiente de quem deu o start, ou
// arquivo), com o motivo quando falta: o start falha em vez de subir sem a chave.
func resolveSecrets(ss []domain.Secret) ([]string, error) {
	var env []string
	for _, s := range ss {
		if s.FromFile != "" {
			b, err := os.ReadFile(s.FromFile)
			if err != nil {
				return nil, fmt.Errorf("resolve secret '%s': file_unreadable (%s)", s.Name, s.FromFile)
			}
			env = append(env, s.Name+"="+strings.TrimRight(string(b), "\r\n"))
			continue
		}
		v, ok := os.LookupEnv(s.FromEnv)
		if !ok {
			return nil, fmt.Errorf("resolve secret '%s': env_unset (%s is not set where the machine starts)", s.Name, s.FromEnv)
		}
		env = append(env, s.Name+"="+v)
	}
	return env, nil
}

// sendSecrets abre o agente SSH no guest (se pedido) e manda os segredos ao guest (só
// memória), com o SSH_AUTH_SOCK junto; sem nada disso, não faz nada.
func (m *Machines) sendSecrets(ctx context.Context, mc domain.Machine, sock string) error {
	_, guest := bindingSources(mc)
	env, err := resolveSecrets(guest)
	if err != nil {
		return fmt.Errorf("machine %s: %w", mc.Name, err)
	}
	if mc.SSHAgent {
		res, err := m.Guest.Run(ctx, sock, protocol.Job{TraceID: "ssh-agent", Op: protocol.OpSSHAgent,
			Path: protocol.GuestSSHAgentSocket, Port: protocol.SSHAgentPort})
		if err != nil {
			return fmt.Errorf("machine %s: ssh agent: %w", mc.Name, err)
		}
		if res.Status != "success" {
			return fmt.Errorf("machine %s: ssh agent: %s", mc.Name, res.Error)
		}
		env = append(env, "SSH_AUTH_SOCK="+protocol.GuestSSHAgentSocket)
	}
	for i, ms := range mc.MountSockets {
		// O mesmo relay do agente, um por socket do host, cada um na sua porta vsock.
		res, err := m.Guest.Run(ctx, sock, protocol.Job{TraceID: "mount-socket", Op: protocol.OpSSHAgent,
			Path: ms.Guest, Port: uint16(protocol.MountSocketPortBase + i)})
		if err == nil && res.Status != "success" {
			err = errors.New(res.Error)
		}
		if err != nil {
			return fmt.Errorf("machine %s: mount socket %s: %w", mc.Name, ms.Guest, err)
		}
	}
	if len(env) == 0 {
		return nil
	}
	res, err := m.Guest.Run(ctx, sock, protocol.Job{TraceID: "secrets", Op: protocol.OpSetEnv, Env: env})
	if err != nil {
		return fmt.Errorf("machine %s: secrets: %w", mc.Name, err)
	}
	if res.Status != "success" {
		return fmt.Errorf("machine %s: secrets: %s", mc.Name, res.Error)
	}
	return nil
}
