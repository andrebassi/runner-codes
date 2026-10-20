package credca

import (
	"crypto/x509"
	"encoding/pem"
	"strings"
	"testing"
	"time"
)

// newAuthority cria e carrega uma CA para hosts.
func newAuthority(t *testing.T, hosts ...string) (*Authority, *x509.CertPool) {
	t.Helper()
	c, k, err := New(hosts, time.Now())
	if err != nil {
		t.Fatal(err)
	}
	a, err := Load(c, k)
	if err != nil {
		t.Fatal(err)
	}
	pool := x509.NewCertPool()
	pool.AppendCertsFromPEM(c)
	return a, pool
}

// TestIssueVerifies: o certificado de um host permitido valida contra a CA (como o cliente
// do guest vai validar), inclusive subdomínio de um curinga; é reaproveitado do cache e
// reemitido perto de vencer.
func TestIssueVerifies(t *testing.T) {
	a, pool := newAuthority(t, "api.notion.com", "*.github.com")
	for _, host := range []string{"api.notion.com", "api.github.com"} {
		c, err := a.Issue(host)
		if err != nil {
			t.Fatal(err)
		}
		if _, err := c.Leaf.Verify(x509.VerifyOptions{Roots: pool, DNSName: host}); err != nil {
			t.Fatalf("%s must verify: %v", host, err)
		}
	}
	first, _ := a.Issue("api.notion.com")
	if again, _ := a.Issue("api.notion.com"); again != first {
		t.Fatal("the leaf must come from the cache")
	}
	a.now = func() time.Time { return time.Now().Add(leafValidity) }
	if renewed, _ := a.Issue("api.notion.com"); renewed == first {
		t.Fatal("a leaf near expiry must be issued again")
	}
}

// TestNameConstraints: a CA não serve para outro domínio — o certificado que ela assinar
// fora dos hosts das bindings é recusado pelo cliente (o curinga não cobre o próprio apex).
func TestNameConstraints(t *testing.T) {
	a, pool := newAuthority(t, "api.notion.com", "*.github.com")
	for _, host := range []string{"google.com", "github.com", "notion.com"} {
		c, err := a.Issue(host)
		if err != nil {
			t.Fatal(err)
		}
		if _, err := c.Leaf.Verify(x509.VerifyOptions{Roots: pool, DNSName: host}); err == nil || !strings.Contains(err.Error(), "constraint") {
			t.Fatalf("%s must be refused by the name constraints: %v", host, err)
		}
	}
	cb, _ := pem.Decode(mustPEM(t))
	ca, _ := x509.ParseCertificate(cb.Bytes)
	if !ca.IsCA || !ca.MaxPathLenZero || !ca.PermittedDNSDomainsCritical {
		t.Fatalf("CA flags: %+v", ca)
	}
}

// mustPEM devolve o certificado PEM de uma CA nova.
func mustPEM(t *testing.T) []byte {
	c, _, err := New([]string{"a.com"}, time.Now())
	if err != nil {
		t.Fatal(err)
	}
	return c
}

// TestErrors: CA sem host, PEM quebrado e conteúdo que não é certificado nem chave.
func TestErrors(t *testing.T) {
	if _, _, err := New(nil, time.Now()); err == nil {
		t.Fatal("a CA needs hosts")
	}
	c, k, _ := New([]string{"a.com"}, time.Now())
	junk := pem.EncodeToMemory(&pem.Block{Type: "CERTIFICATE", Bytes: []byte("x")})
	for name, pair := range map[string][2][]byte{
		"not pem":  {[]byte("x"), k},
		"bad cert": {junk, k},
		"bad key":  {c, junk},
	} {
		if _, err := Load(pair[0], pair[1]); err == nil {
			t.Fatalf("%s must fail", name)
		}
	}
}
