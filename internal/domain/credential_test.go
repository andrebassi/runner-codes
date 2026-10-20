package domain

import (
	"strings"
	"testing"
)

// TestParseCredential: a forma completa, com :body e :set-header em qualquer ordem, e as
// formas quebradas.
func TestParseCredential(t *testing.T) {
	c, err := ParseCredential("notion=NOTION_API_KEY@api.notion.com")
	if err != nil || c.Name != "notion" || c.Env != "NOTION_API_KEY" || len(c.Hosts) != 1 || c.Body || c.SetHeader != "" {
		t.Fatalf("plain: %+v %v", c, err)
	}
	for _, s := range []string{"gh=GH@github.com,*.github.com:body:set-header=authorization", "gh=GH@github.com,*.github.com:set-header=authorization:body"} {
		c, err := ParseCredential(s)
		if err != nil || !c.Body || c.SetHeader != "authorization" || strings.Join(c.Hosts, " ") != "github.com *.github.com" {
			t.Fatalf("%s: %+v %v", s, c, err)
		}
	}
	for _, bad := range []string{"x", "x=Y", "x@h.com", "x=Y@h.com:gzip"} {
		if _, err := ParseCredential(bad); err == nil {
			t.Fatalf("%q must fail", bad)
		}
	}
}

// TestCredentialMatching: host exato, curinga só de subdomínio, método limitado ou livre.
func TestCredentialMatching(t *testing.T) {
	c := Credential{Hosts: []string{"api.x.com", "*.y.io"}}
	for host, want := range map[string]bool{"api.x.com": true, "API.X.COM.": true, "x.com": false, "a.y.io": true, "a.b.y.io": true, "y.io": false, "evily.io": false} {
		if c.Allows(host, "POST") != want {
			t.Fatalf("%s: want %v", host, want)
		}
	}
	c.Methods = []string{"GET"}
	if !c.Allows("api.x.com", "GET") || c.Allows("api.x.com", "POST") {
		t.Fatal("methods limit the binding")
	}
	if !ForbiddenHeader("Cookie") || ForbiddenHeader("authorization") {
		t.Fatal("forbidden headers")
	}
}

// TestCredentialRules: cada recusa do create, a regra de allow_hosts (credencial dentro da
// política) e a implicação de rede.
func TestCredentialRules(t *testing.T) {
	ok := Credential{Name: "gh", Env: "GH_TOKEN", Hosts: []string{"api.github.com"}}
	n := Network{Credentials: []Credential{ok}}
	if err := n.Validate(); err != nil {
		t.Fatal(err)
	}
	if !n.Attached() {
		t.Fatal("a credential attaches a network card")
	}
	if p, _ := n.OutboundPolicy(); p.denyAll {
		t.Fatal("a credential implies net")
	}
	for name, c := range map[string]Credential{
		"name":       {Name: "Bad", Env: "A", Hosts: []string{"a.com"}},
		"env":        {Name: "a", Env: "1A", Hosts: []string{"a.com"}},
		"no hosts":   {Name: "a", Env: "A"},
		"ip":         {Name: "a", Env: "A", Hosts: []string{"1.2.3.4"}},
		"tld":        {Name: "a", Env: "A", Hosts: []string{"*.com"}},
		"single":     {Name: "a", Env: "A", Hosts: []string{"localhost"}},
		"scheme":     {Name: "a", Env: "A", Hosts: []string{"https://a.com"}},
		"upper":      {Name: "a", Env: "A", Hosts: []string{"A.com"}},
		"method":     {Name: "a", Env: "A", Hosts: []string{"a.com"}, Methods: []string{"get"}},
		"header":     {Name: "a", Env: "A", Hosts: []string{"a.com"}, SetHeader: "Authorization"},
		"cookie hdr": {Name: "a", Env: "A", Hosts: []string{"a.com"}, SetHeader: "cookie"},
	} {
		if err := (Network{Credentials: []Credential{c}}).Validate(); err == nil {
			t.Fatalf("%s must be refused", name)
		}
	}
	dup := Network{Credentials: []Credential{ok, {Name: "gh", Env: "OTHER", Hosts: []string{"a.com"}}}}
	if err := dup.Validate(); err == nil {
		t.Fatal("duplicated name")
	}
	dupEnv := Network{Credentials: []Credential{ok, {Name: "b", Env: "GH_TOKEN", Hosts: []string{"a.com"}}}}
	if err := dupEnv.Validate(); err == nil {
		t.Fatal("duplicated env")
	}
	h1 := Credential{Name: "a", Env: "A", Hosts: []string{"*.github.com"}, SetHeader: "authorization"}
	h2 := Credential{Name: "b", Env: "B", Hosts: []string{"api.github.com"}, SetHeader: "authorization"}
	if err := (Network{Credentials: []Credential{h1, h2}}).Validate(); err == nil {
		t.Fatal("two bindings setting the same header on overlapping hosts")
	}
	h2.Hosts = []string{"gitlab.com"}
	if err := (Network{Credentials: []Credential{h1, h2}}).Validate(); err != nil {
		t.Fatalf("same header on disjoint hosts is fine: %v", err)
	}
	inside := Network{Egress: Egress{AllowHosts: []string{"github.com"}}, Credentials: []Credential{ok, {Name: "w", Env: "W", Hosts: []string{"*.github.com"}}}}
	if err := inside.Validate(); err != nil {
		t.Fatalf("credential hosts under allow_hosts: %v", err)
	}
	outside := Network{Egress: Egress{AllowHosts: []string{"github.com"}}, Credentials: []Credential{{Name: "n", Env: "N", Hosts: []string{"api.notion.com"}}}}
	if err := outside.Validate(); err == nil || !strings.Contains(err.Error(), "outside the machine's allow_hosts") {
		t.Fatalf("a credential must not widen the network: %v", err)
	}
	if !hostsOverlap([]string{"a.com"}, []string{"a.com"}) || hostsOverlap([]string{"a.com"}, []string{"b.com"}) {
		t.Fatal("overlap")
	}
}

// TestUserSpec: nome do passwd e uid[:gid] passam; o resto é recusado com o motivo.
func TestUserSpec(t *testing.T) {
	base := Machine{Name: "a", Stack: "python"}.WithDefaults()
	for _, u := range []string{"app", "_svc", "web-1", "smb$", "1000", "501:20"} {
		m := base
		m.User = u
		if err := m.Validate(); err != nil {
			t.Fatalf("%q: %v", u, err)
		}
	}
	for _, u := range []string{"App", "a b", "1000:", ":20", "../x", "a:b"} {
		m := base
		m.User = u
		if err := m.Validate(); err == nil || !strings.Contains(err.Error(), "uid[:gid]") {
			t.Fatalf("%q must be refused: %v", u, err)
		}
	}
}

// TestSecrets: GUEST=SOURCE das duas flags; regras de nome, fonte única e arquivo absoluto.
func TestSecrets(t *testing.T) {
	if s, err := ParseSecret("API=HOST_API", false); err != nil || s != (Secret{Name: "API", FromEnv: "HOST_API"}) {
		t.Fatalf("env: %+v %v", s, err)
	}
	if s, _ := ParseSecret("CRED=/run/c.json", true); s != (Secret{Name: "CRED", FromFile: "/run/c.json"}) {
		t.Fatalf("file: %+v", s)
	}
	for _, bad := range []string{"API", "=X", "API="} {
		if _, err := ParseSecret(bad, false); err == nil {
			t.Fatalf("%q must fail", bad)
		}
	}
	base := Machine{Name: "a", Stack: "python"}.WithDefaults()
	for name, ss := range map[string][]Secret{
		"name":      {{Name: "1A", FromEnv: "X"}},
		"twice":     {{Name: "A", FromEnv: "X"}, {Name: "A", FromEnv: "Y"}},
		"no source": {{Name: "A"}},
		"both":      {{Name: "A", FromEnv: "X", FromFile: "/f"}},
		"bad env":   {{Name: "A", FromEnv: "1X"}},
		"relative":  {{Name: "A", FromFile: "f"}},
	} {
		m := base
		m.Secrets = ss
		if err := m.Validate(); err == nil {
			t.Fatalf("%s must be refused", name)
		}
	}
	base.Secrets = []Secret{{Name: "A", FromEnv: "X"}, {Name: "B", FromFile: "/f"}}
	if err := base.Validate(); err != nil {
		t.Fatal(err)
	}
}

// TestValidateName: o nome sozinho, com a mesma regra do Validate.
func TestValidateName(t *testing.T) {
	if ValidateName("py312") != nil || ValidateName("Py312") == nil || ValidateName("a.b") == nil {
		t.Fatal("name rule")
	}
}

// TestMintPlaceholder: prefixo fixo, nome em maiúsculas com _ no lugar de -, e 32 dígitos hexa.
func TestMintPlaceholder(t *testing.T) {
	p := MintPlaceholder("my-key", [16]byte{0xab, 1})
	if p != "RC_PLACEHOLDER_MY_KEY_AB010000000000000000000000000000" {
		t.Fatalf("placeholder: %s", p)
	}
}
