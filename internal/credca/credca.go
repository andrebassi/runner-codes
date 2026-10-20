// Package credca é a autoridade certificadora de uma máquina com credenciais: o gateway
// termina o TLS das requisições aos hosts das bindings com certificados assinados por ela, e
// o guest confia nela (ca.pem). A CA é restrita por name constraints aos hosts das bindings:
// mesmo vazada, ela não serve para se passar por outro domínio, porque o cliente recusa o
// certificado fora da restrição.
package credca

import (
	"crypto/ecdsa"
	"crypto/elliptic"
	"crypto/rand"
	"crypto/tls"
	"crypto/x509"
	"crypto/x509/pkix"
	"encoding/pem"
	"errors"
	"math/big"
	"strings"
	"sync"
	"time"
)

// caValidity é a vida da CA: a da máquina, na prática (ela nasce no create e só morre com o
// delete). Dez anos dispensam rotação; a chave fica só no host, 0600.
const caValidity = 10 * 365 * 24 * time.Hour

// leafValidity é a vida do certificado de um host: curta, e emitido de novo depois disso.
const leafValidity = 24 * time.Hour

// New cria a CA para hosts (os allowed_hosts das bindings) e devolve certificado e chave
// em PEM. "*.x.com" vira a restrição ".x.com" (só subdomínios), como na binding.
func New(hosts []string, now time.Time) (certPEM, keyPEM []byte, err error) {
	if len(hosts) == 0 {
		return nil, nil, errors.New("credca: a CA needs at least one host")
	}
	key, err := ecdsa.GenerateKey(elliptic.P256(), rand.Reader)
	if err != nil {
		return nil, nil, err
	}
	var permitted []string
	for _, h := range hosts {
		if base, ok := strings.CutPrefix(h, "*."); ok {
			permitted = append(permitted, "."+base)
		} else {
			permitted = append(permitted, h)
		}
	}
	tmpl := &x509.Certificate{
		SerialNumber:                serial(),
		Subject:                     pkix.Name{CommonName: "runner.code credentials CA"},
		NotBefore:                   now.Add(-time.Hour),
		NotAfter:                    now.Add(caValidity),
		KeyUsage:                    x509.KeyUsageCertSign | x509.KeyUsageCRLSign,
		BasicConstraintsValid:       true,
		IsCA:                        true,
		MaxPathLenZero:              true,
		PermittedDNSDomainsCritical: true,
		PermittedDNSDomains:         permitted,
	}
	der, err := x509.CreateCertificate(rand.Reader, tmpl, tmpl, &key.PublicKey, key)
	if err != nil {
		return nil, nil, err
	}
	kder, _ := x509.MarshalECPrivateKey(key) // chave P-256 recém-gerada: não tem como falhar
	return pem.EncodeToMemory(&pem.Block{Type: "CERTIFICATE", Bytes: der}),
		pem.EncodeToMemory(&pem.Block{Type: "EC PRIVATE KEY", Bytes: kder}), nil
}

// serial é um número de série aleatório de 128 bits.
func serial() *big.Int {
	n, _ := rand.Int(rand.Reader, new(big.Int).Lsh(big.NewInt(1), 128))
	return n
}

// Authority emite certificados de host assinados pela CA, com cache por nome.
type Authority struct {
	cert *x509.Certificate
	key  *ecdsa.PrivateKey
	now  func() time.Time
	mu   sync.Mutex
	// leaves guarda o certificado de cada host até perto de vencer.
	leaves map[string]*tls.Certificate
}

// Load lê a CA em PEM (a gravada no create).
func Load(certPEM, keyPEM []byte) (*Authority, error) {
	cb, _ := pem.Decode(certPEM)
	kb, _ := pem.Decode(keyPEM)
	if cb == nil || kb == nil {
		return nil, errors.New("credca: CA certificate or key is not PEM")
	}
	cert, err := x509.ParseCertificate(cb.Bytes)
	if err != nil {
		return nil, err
	}
	key, err := x509.ParseECPrivateKey(kb.Bytes)
	if err != nil {
		return nil, err
	}
	return &Authority{cert: cert, key: key, now: time.Now, leaves: map[string]*tls.Certificate{}}, nil
}

// Issue devolve o certificado de host (SAN = host), assinado pela CA.
func (a *Authority) Issue(host string) (*tls.Certificate, error) {
	a.mu.Lock()
	defer a.mu.Unlock()
	now := a.now()
	if c, ok := a.leaves[host]; ok && now.Add(time.Hour).Before(c.Leaf.NotAfter) {
		return c, nil
	}
	key, err := ecdsa.GenerateKey(elliptic.P256(), rand.Reader)
	if err != nil {
		return nil, err
	}
	tmpl := &x509.Certificate{
		SerialNumber: serial(),
		Subject:      pkix.Name{CommonName: host},
		DNSNames:     []string{host},
		NotBefore:    now.Add(-time.Hour),
		NotAfter:     now.Add(leafValidity),
		KeyUsage:     x509.KeyUsageDigitalSignature,
		ExtKeyUsage:  []x509.ExtKeyUsage{x509.ExtKeyUsageServerAuth},
	}
	der, err := x509.CreateCertificate(rand.Reader, tmpl, a.cert, &key.PublicKey, a.key)
	if err != nil {
		return nil, err
	}
	leaf, _ := x509.ParseCertificate(der) // acabou de ser criado pela própria biblioteca
	c := &tls.Certificate{Certificate: [][]byte{der}, PrivateKey: key, Leaf: leaf}
	a.leaves[host] = c
	return c, nil
}
