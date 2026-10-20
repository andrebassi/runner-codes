package domain

import (
	"encoding/hex"
	"fmt"
	"regexp"
	"strings"
)

// Credential é uma credencial que a máquina usa sem ler: o guest recebe só um placeholder
// na variável Env, e o gateway troca o placeholder pelo valor real nas requisições HTTPS
// para Hosts — dentro de um cabeçalho (ou do corpo de formulário, com Body). SetHeader, em
// vez de troca, define o cabeçalho inteiro com o valor. O valor nunca entra na máquina, no
// registro dela nem no disco: o gateway o lê do ambiente de quem deu o start.
type Credential struct {
	Name      string   `json:"name"`
	Env       string   `json:"environment_variable"` // contract:ok — chave do runner.code e da API
	Hosts     []string `json:"allowed_hosts"`
	Methods   []string `json:"methods,omitempty"`
	SetHeader string   `json:"set_header,omitempty"`
	Body      bool     `json:"body,omitempty"`
	// Placeholder é dado no create e fica estável enquanto a máquina existir.
	Placeholder string `json:"placeholder,omitempty"`
	// SourceEnv e SourceFile vêm de um segredo de mesmo nome (o start preenche na spec):
	// o valor sai dessa variável do host, ou desse arquivo lido a cada requisição.
	SourceEnv  string `json:"source_env,omitempty"`
	SourceFile string `json:"source_file,omitempty"`
}

// PlaceholderPrefix começa todo placeholder: o gateway recusa um que ele não emitiu.
const PlaceholderPrefix = "RC_PLACEHOLDER_"

// credentialName é o nome da binding: minúsculas, dígitos, - e _, até 32.
var credentialName = regexp.MustCompile(`^[a-z][a-z0-9_-]{0,31}$`)

// envName é o nome de variável de ambiente.
var envName = regexp.MustCompile(`^[A-Za-z_][A-Za-z0-9_]*$`)

// dnsLabel é um rótulo de nome DNS em minúsculas.
var dnsLabel = regexp.MustCompile(`^[a-z0-9]([a-z0-9-]{0,61}[a-z0-9])?$`)

// headerName é um nome de cabeçalho HTTP em minúsculas.
var headerName = regexp.MustCompile(`^[a-z0-9][a-z0-9-]*$`)

// credentialMethods são os métodos que uma binding pode limitar.
var credentialMethods = map[string]bool{"GET": true, "HEAD": true, "POST": true, "PUT": true, "PATCH": true, "DELETE": true, "OPTIONS": true}

// forbiddenHeaders são os cabeçalhos de roteamento, enquadramento e sessão: definir um deles
// desviaria a requisição (host), quebraria o enquadramento (content-length) ou misturaria
// sessões (cookie).
var forbiddenHeaders = map[string]bool{"host": true, "content-length": true, "transfer-encoding": true, "connection": true,
	"cookie": true, "upgrade": true, "te": true, "trailer": true, "keep-alive": true, "proxy-connection": true, "proxy-authorization": true}

// ForbiddenHeader diz se o cabeçalho não pode levar nem receber credencial.
func ForbiddenHeader(h string) bool { return forbiddenHeaders[strings.ToLower(h)] }

// ParseCredential lê NAME=ENV_VAR@HOST[,HOST...][:body][:set-header=HEADER].
// Só a sintaxe; as regras são do Validate.
func ParseCredential(s string) (Credential, error) {
	var c Credential
	name, rest, ok := strings.Cut(s, "=")
	env, rest2, ok2 := strings.Cut(rest, "@")
	if !ok || !ok2 {
		return c, fmt.Errorf("credential %q must be NAME=ENV_VAR@HOST[,HOST...][:body][:set-header=HEADER]", s)
	}
	parts := strings.Split(rest2, ":")
	c.Name, c.Env, c.Hosts = name, env, strings.Split(parts[0], ",")
	for _, opt := range parts[1:] {
		switch {
		case opt == "body":
			c.Body = true
		case strings.HasPrefix(opt, "set-header="):
			c.SetHeader = strings.TrimPrefix(opt, "set-header=")
		default:
			return c, fmt.Errorf("credential %q: unknown option %q (body, set-header=HEADER)", s, opt)
		}
	}
	return c, nil
}

// validCredentialHost aceita nome DNS em minúsculas com ao menos dois rótulos, ou *.nome
// (todo subdomínio, não o próprio nome) com ao menos dois rótulos depois do *. — "*.com"
// mandaria a credencial para qualquer site .com.
func validCredentialHost(h string) bool {
	name := strings.TrimPrefix(h, "*.")
	labels := strings.Split(name, ".")
	// TLD nunca é só dígito: isso também recusa IP escrito como nome (1.2.3.4).
	if len(labels) < 2 || strings.Trim(labels[len(labels)-1], "0123456789") == "" {
		return false
	}
	for _, l := range labels {
		if !dnsLabel.MatchString(l) {
			return false
		}
	}
	return true
}

// HostMatches diz se host cai em pattern: igual, ou subdomínio de um "*.x".
func HostMatches(pattern, host string) bool {
	host = strings.ToLower(strings.TrimSuffix(host, "."))
	if base, ok := strings.CutPrefix(pattern, "*."); ok {
		return strings.HasSuffix(host, "."+base)
	}
	return host == pattern
}

// Allows diz se a binding vale para host e método.
func (c Credential) Allows(host, method string) bool {
	hostOK := false
	for _, h := range c.Hosts {
		if HostMatches(h, host) {
			hostOK = true
			break
		}
	}
	if !hostOK {
		return false
	}
	if len(c.Methods) == 0 {
		return true
	}
	for _, m := range c.Methods {
		if m == method {
			return true
		}
	}
	return false
}

// validate confere uma binding sozinha.
func (c Credential) validate() error {
	switch {
	case !credentialName.MatchString(c.Name):
		return fmt.Errorf("credential name %q must be lowercase letters, digits, - and _ (up to 32)", c.Name)
	case !envName.MatchString(c.Env):
		return fmt.Errorf("credential %s: %q is not an environment variable name", c.Name, c.Env)
	case len(c.Hosts) == 0:
		return fmt.Errorf("credential %s: allowed_hosts must not be empty (a credential is never sent anywhere by default)", c.Name)
	case c.SetHeader != "" && (!headerName.MatchString(c.SetHeader) || forbiddenHeaders[c.SetHeader]):
		return fmt.Errorf("credential %s: set_header %q must be a lowercase header name, not a routing, framing or cookie header", c.Name, c.SetHeader)
	}
	for _, h := range c.Hosts {
		if !validCredentialHost(h) {
			return fmt.Errorf("credential %s: host %q must be a lowercase DNS name or *.name (no IP, scheme, port or *.tld)", c.Name, h)
		}
	}
	for _, m := range c.Methods {
		if !credentialMethods[m] {
			return fmt.Errorf("credential %s: method %q is not one of GET, HEAD, POST, PUT, PATCH, DELETE, OPTIONS", c.Name, m)
		}
	}
	return nil
}

// validateCredentials confere as bindings juntas: nome e variável únicos, dois set_header
// iguais em hosts que se cruzam recusados, e — com lista de hosts na política — todo host de
// credencial dentro dela (credencial nunca alarga a rede, e rede nunca alarga credencial).
func validateCredentials(cs []Credential, e Egress) error {
	names, envs := map[string]bool{}, map[string]bool{}
	var pol *Policy
	if len(e.AllowHosts)+len(e.AllowHostPatterns) > 0 {
		pol, _ = e.Compile() // a política já foi validada antes
	}
	for i, c := range cs {
		if err := c.validate(); err != nil {
			return err
		}
		if names[c.Name] || envs[c.Env] {
			return fmt.Errorf("credential %s: name and environment variable must be unique in a machine", c.Name)
		}
		names[c.Name], envs[c.Env] = true, true
		for _, h := range c.Hosts {
			probe := h
			if base, ok := strings.CutPrefix(h, "*."); ok {
				probe = "sub." + base
			}
			if pol != nil && !pol.NameAllowed(probe) {
				return fmt.Errorf("credential %s: host %s is outside the machine's allow_hosts", c.Name, h)
			}
		}
		for _, o := range cs[:i] {
			if c.SetHeader != "" && c.SetHeader == o.SetHeader && hostsOverlap(c.Hosts, o.Hosts) {
				return fmt.Errorf("credentials %s and %s both set %s for overlapping hosts", o.Name, c.Name, c.SetHeader)
			}
		}
	}
	return nil
}

// hostsOverlap diz se alguma requisição casaria com as duas listas.
func hostsOverlap(a, b []string) bool {
	for _, x := range a {
		for _, y := range b {
			xp, yp := strings.TrimPrefix(x, "*."), strings.TrimPrefix(y, "*.")
			if x == y || HostMatches(x, yp) || HostMatches(y, xp) {
				return true
			}
		}
	}
	return false
}

// MintPlaceholder monta o placeholder de uma binding a partir de 16 bytes aleatórios.
func MintPlaceholder(name string, random [16]byte) string {
	return PlaceholderPrefix + strings.ToUpper(strings.NewReplacer("-", "_").Replace(name)) + "_" + strings.ToUpper(hex.EncodeToString(random[:]))
}

// Secret é um segredo da máquina: o valor sai do host no start (variável de ambiente de quem
// deu o start, ou arquivo absoluto) e entra no ambiente de todo comando do guest, só em
// memória. Com o mesmo nome da variável de uma credencial, ele alimenta a binding e não entra
// no guest (a tabela [secrets]).
type Secret struct {
	Name     string `json:"name"`
	FromEnv  string `json:"from_env,omitempty"`
	FromFile string `json:"from_file,omitempty"`
}

// ParseSecret lê GUEST=SOURCE do --secret-env (source é variável do host) ou do
// --secret-file (source é arquivo absoluto do host).
func ParseSecret(s string, file bool) (Secret, error) {
	name, src, ok := strings.Cut(s, "=")
	if !ok || name == "" || src == "" {
		return Secret{}, fmt.Errorf("secret %q must be GUEST_VAR=SOURCE", s)
	}
	if file {
		return Secret{Name: name, FromFile: src}, nil
	}
	return Secret{Name: name, FromEnv: src}, nil
}

// validateSecrets confere nome único, uma fonte só e arquivo absoluto.
func validateSecrets(ss []Secret) error {
	seen := map[string]bool{}
	for _, s := range ss {
		switch {
		case !envName.MatchString(s.Name):
			return fmt.Errorf("secret %q is not an environment variable name", s.Name)
		case seen[s.Name]:
			return fmt.Errorf("secret %s is declared twice", s.Name)
		case (s.FromEnv == "") == (s.FromFile == ""):
			return fmt.Errorf("secret %s needs exactly one of from_env and from_file", s.Name)
		case s.FromEnv != "" && !envName.MatchString(s.FromEnv):
			return fmt.Errorf("secret %s: from_env %q is not an environment variable name", s.Name, s.FromEnv)
		case s.FromFile != "" && !strings.HasPrefix(s.FromFile, "/"):
			return fmt.Errorf("secret %s: from_file %q must be an absolute path", s.Name, s.FromFile)
		}
		seen[s.Name] = true
	}
	return nil
}
