package domain

import (
	"net/netip"
	"strings"
	"testing"
)

// TestParsePort: os formatos aceitos (porta única, par, /udp, /tcp) e as recusas.
func TestParsePort(t *testing.T) {
	for in, want := range map[string]PortMap{
		"8080":        {Host: 8080, Guest: 8080},
		"18789:18789": {Host: 18789, Guest: 18789},
		"3000:80":     {Host: 3000, Guest: 80},
		"5353:53/udp": {Host: 5353, Guest: 53, UDP: true},
		"9000/tcp":    {Host: 9000, Guest: 9000},
		"65535:1":     {Host: 65535, Guest: 1},
	} {
		got, err := ParsePort(in)
		if err != nil || got != want {
			t.Fatalf("%q: %+v %v", in, got, err)
		}
	}
	if (PortMap{Host: 1, Guest: 2, UDP: true}).String() != "1:2/udp" || (PortMap{Host: 1, Guest: 2}).String() != "1:2" {
		t.Fatal("String must round-trip")
	}
	for _, bad := range []string{"", "0", "70000", "a:b", "80:", ":80", "80:80:80", "80/sctp", "-1"} {
		if _, err := ParsePort(bad); err == nil {
			t.Fatalf("%q must fail", bad)
		}
	}
}

// ip é um atalho para os testes.
func ip(s string) netip.Addr { return netip.MustParseAddr(s) }

// TestPolicyOpen: sem regra, a internet é aberta, mas o piso vale sempre (metadata,
// link-local, loopback do host), inclusive em IPv4 mapeado em IPv6.
func TestPolicyOpen(t *testing.T) {
	p, err := Egress{}.Compile()
	if err != nil || p.FiltersNames() || !p.NameAllowed("anything.example") {
		t.Fatalf("open policy: %v", err)
	}
	for _, a := range []string{"1.1.1.1", "10.0.0.5", "2606:4700::1111"} {
		if ok, _ := p.IPAllowed(ip(a), false); !ok {
			t.Fatalf("%s must be allowed on an open policy", a)
		}
	}
	for _, a := range []string{"169.254.169.254", "fe80::1", "127.0.0.1", "::1", "0.0.0.0", "::ffff:169.254.169.254", "100.100.100.200"} {
		if ok, why := p.IPAllowed(ip(a), true); ok || why == "" {
			t.Fatalf("%s must hit the floor (learned or not): %v %q", a, ok, why)
		}
	}
}

// TestPolicyHosts: as três formas de nome (apex+sub, =exato, *.sub) e os padrões; o IP
// só passa se aprendido pelo DNS de um nome permitido; loopback aprendido nunca passa
// (DNS rebinding); nome com esquema ou porta é recusado na compilação.
// TestPolicyBlockPage: a página de bloqueio é opção da política (desligada por padrão), e a lista que ela mostra é a
// declarada, nomes e padrões, na ordem.
func TestPolicyBlockPage(t *testing.T) {
	p, err := Egress{AllowHosts: []string{"claude.ai", "=api.x.io"}, AllowHostPatterns: []string{"*.cdn.net"}, BlockPage: true}.Compile()
	if err != nil {
		t.Fatal(err)
	}
	if !p.BlockPage() || strings.Join(p.AllowedHosts(), ",") != "claude.ai,=api.x.io,*.cdn.net" {
		t.Fatalf("block page %v, allowed %v", p.BlockPage(), p.AllowedHosts())
	}
	off, _ := Egress{AllowHosts: []string{"claude.ai"}}.Compile()
	if off.BlockPage() {
		t.Fatal("block page must be off unless asked")
	}
}

// TestPolicyPaths: a regra de caminho sai normalizada (host minúsculo), restringe a política, faz o DNS filtrar nomes
// (o host dela cai no sinkhole, não resolve livre) e exige a página de bloqueio; cada forma que o proxy não conseguiria
// comparar com segurança é recusada na compilação.
func TestPolicyPaths(t *testing.T) {
	e := Egress{AllowPaths: []string{"CDNJS.Cloudflare.com./polyfill/", "x.io/a/b.js"}, BlockPage: true}
	p, err := e.Compile()
	if err != nil {
		t.Fatal(err)
	}
	if got := strings.Join(p.AllowedPaths(), ","); got != "cdnjs.cloudflare.com/polyfill/,x.io/a/b.js" {
		t.Fatalf("paths %q", got)
	}
	if !e.Restricted() || !p.FiltersNames() || p.NameAllowed("cdnjs.cloudflare.com") {
		t.Fatal("a path rule must restrict, filter names, and keep its host on the sinkhole")
	}
	if ok, _ := p.IPAllowed(netip.MustParseAddr("1.1.1.1"), false); ok {
		t.Fatal("a path rule must not open IPs")
	}
	if _, err := (Egress{AllowPaths: []string{"x.io/a"}}).Compile(); err == nil || !strings.Contains(err.Error(), "block_page") {
		t.Fatalf("without block_page: %v", err)
	}
	for _, bad := range []string{"x.io", "x.io/", "/a", "=x.io/a", "*.x.io/a", "x.io:443/a", "x.io//a", "x.io/a/../b",
		"x.io/./a", "x.io/a%2Fb", "x.io/a?b=1", "x.io/a#b", "x.io/a,b", "x.io/a b", "x.io/a\\b"} {
		if _, err := (Egress{AllowPaths: []string{bad}, BlockPage: true}).Compile(); err == nil {
			t.Errorf("%q must be refused", bad)
		}
	}
}

// TestPolicyHosts: nome casa subdomínios, "=" só o nome, "*." só subdomínios; IP só passa se aprendido de nome liberado.
func TestPolicyHosts(t *testing.T) {
	p, err := Egress{AllowHosts: []string{"Example.com.", "=api.only.io", "*.sub.dev"}, AllowHostPatterns: []string{"registry.npmjs.org", "*.cdn.net"}}.Compile()
	if err != nil {
		t.Fatal(err)
	}
	allowed := []string{"example.com", "www.example.com", "API.example.com.", "api.only.io", "x.sub.dev", "registry.npmjs.org", "a.cdn.net"}
	denied := []string{"notexample.com", "x.api.only.io", "sub.dev", "www.registry.npmjs.org", "cdn.net", "google.com"}
	for _, n := range allowed {
		if !p.NameAllowed(n) {
			t.Fatalf("%s must resolve", n)
		}
	}
	for _, n := range denied {
		if p.NameAllowed(n) {
			t.Fatalf("%s must not resolve", n)
		}
	}
	if got := strings.Join(p.StaticHosts(), ","); got != "example.com,api.only.io,registry.npmjs.org" {
		t.Fatalf("static hosts: %s", got)
	}
	if ok, why := p.IPAllowed(ip("93.184.216.34"), false); ok || why != "not in the allow list" {
		t.Fatal("an IP not learned from an allowed name must be refused")
	}
	if ok, _ := p.IPAllowed(ip("93.184.216.34"), true); !ok {
		t.Fatal("an IP learned from an allowed name must pass")
	}
	if ok, _ := p.IPAllowed(ip("127.0.0.1"), true); ok {
		t.Fatal("a learned loopback address must never pass")
	}
	// Só CIDRs: o DNS não filtra, então todo IP sairia "aprendido". Aprendido não pode abrir
	// nada aqui — era o furo que deixava allow_cidrs = ["127.0.0.1/32"] alcançar a internet.
	cidrs, _ := Egress{AllowCIDRs: []string{"127.0.0.1/32"}}.Compile()
	if ok, why := cidrs.IPAllowed(ip("93.184.216.34"), true); ok || why != "not in the allow list" {
		t.Fatalf("with only allow_cidrs a learned IP must be refused: %v %q", ok, why)
	}
	if ok, _ := cidrs.IPAllowed(ip("127.0.0.1"), false); !ok {
		t.Fatal("the allowed CIDR itself still passes")
	}
	for _, bad := range []Egress{
		{AllowHosts: []string{"https://x.com"}}, {AllowHosts: []string{"x.com:443"}}, {AllowHosts: []string{""}},
		{AllowHosts: []string{"*."}}, {AllowHostPatterns: []string{"=x.com"}}, {AllowHostPatterns: []string{"a b"}},
	} {
		if _, err := bad.Compile(); err == nil {
			t.Fatalf("%+v must be refused", bad)
		}
	}
}

// TestPolicyCIDRs: allow_cidrs abre (inclusive IP solto e o loopback, só por ele);
// deny vence allow; block_private fecha as faixas privadas e allow_private reabre uma;
// CIDR inválido é erro com o nome da chave.
func TestPolicyCIDRs(t *testing.T) {
	p, err := Egress{AllowCIDRs: []string{"1.1.1.1", "10.0.0.0/8", "127.0.0.1/32"}, DenyCIDRs: []string{"10.9.0.0/16"}}.Compile()
	if err != nil {
		t.Fatal(err)
	}
	if p.FiltersNames() || !p.NameAllowed("anything") {
		t.Fatal("with only CIDRs, names resolve and the connection decides")
	}
	cases := map[string]bool{"1.1.1.1": true, "1.1.1.2": false, "10.1.2.3": true, "10.9.1.1": false, "127.0.0.1": true, "8.8.8.8": false}
	for a, want := range cases {
		if ok, _ := p.IPAllowed(ip(a), false); ok != want {
			t.Fatalf("%s: got %v", a, ok)
		}
	}
	bp, _ := Egress{AllowPrivate: []string{"192.168.1.0/24"}}.Compile()
	for a, want := range map[string]bool{"192.168.1.10": true, "192.168.2.10": false, "172.20.0.1": false, "100.64.0.1": false, "fd12::1": false, "8.8.8.8": true} {
		if ok, why := bp.IPAllowed(ip(a), false); ok != want || (!ok && why != "block_private") {
			t.Fatalf("block_private %s: %v %q", a, ok, why)
		}
	}
	for key, e := range map[string]Egress{"allow_cidrs": {AllowCIDRs: []string{"nope"}}, "deny_cidrs": {DenyCIDRs: []string{"1.2.3.4/99"}}, "allow_private": {AllowPrivate: []string{"x"}}} {
		if _, err := e.Compile(); err == nil || !strings.Contains(err.Error(), key) {
			t.Fatalf("%s: %v", key, err)
		}
	}
}

// TestNamedNetwork: nome válido liga a placa; endereço fora da faixa é recusado; o
// próximo IP pula os usados, o .0 e o .255, e a faixa cheia é erro.
func TestNamedNetwork(t *testing.T) {
	n := Network{Name: "lab", Address: "100.98.0.10"}
	if !n.Attached() || n.Validate() != nil {
		t.Fatal("a named network attaches a NIC")
	}
	for _, bad := range []Network{{Name: "Lab!"}, {Address: "10.0.0.1"}, {Address: "x"}} {
		if bad.Validate() == nil {
			t.Fatalf("%+v must be refused", bad)
		}
	}
	a, err := NextFabricAddress([]string{"100.98.0.10", "100.98.0.11", "junk"})
	if err != nil || a.String() != "100.98.0.12" {
		t.Fatalf("next: %v %v", a, err)
	}
	if a, _ := NextFabricAddress([]string{"100.98.0.254"}); a.String() != "100.98.0.10" {
		t.Fatalf("first free: %v", a)
	}
	var used []string
	for ip := netip.MustParseAddr("100.98.0.10"); FabricPrefix.Contains(ip); ip = ip.Next() {
		used = append(used, ip.String())
	}
	if a, _ := NextFabricAddress(append(used[:244:244], "100.98.0.254")); a.String() != "100.98.1.1" {
		t.Fatalf("must skip .255 and .0: %v", a)
	}
	if _, err := NextFabricAddress(used); err == nil {
		t.Fatal("a full network must fail")
	}
}

// TestNetwork: placa só quando há rede, porta ou política; porta sem rede de saída nega
// toda saída; política implica rede; portas repetidas, zeradas ou demais são recusadas.
func TestNetwork(t *testing.T) {
	if (Network{}).Attached() {
		t.Fatal("no network by default")
	}
	ports := Network{Ports: []PortMap{{Host: 8080, Guest: 80}}}
	if !ports.Attached() {
		t.Fatal("a published port needs a NIC")
	}
	p, _ := ports.OutboundPolicy()
	if ok, why := p.IPAllowed(ip("1.1.1.1"), true); ok || !strings.Contains(why, "net = false") || p.NameAllowed("x.com") {
		t.Fatal("ports without net must deny every outbound connection and every lookup")
	}
	pol := Network{Egress: Egress{BlockPrivate: true}}
	if !pol.Attached() {
		t.Fatal("a policy implies network")
	}
	if p, _ := pol.OutboundPolicy(); p == nil || !p.blockPrivate {
		t.Fatal("policy must compile")
	}
	if p, _ := (Network{Enabled: true}).OutboundPolicy(); p.restricted {
		t.Fatal("net = true with no rules is open")
	}
	if err := (Network{Enabled: true, Ports: []PortMap{{Host: 80, Guest: 80}, {Host: 80, Guest: 80, UDP: true}}}).Validate(); err != nil {
		t.Fatalf("same port on tcp and udp is fine: %v", err)
	}
	many := make([]PortMap, MaxPorts+1)
	for i := range many {
		many[i] = PortMap{Host: uint16(1000 + i), Guest: 80}
	}
	for name, n := range map[string]Network{
		"twice":    {Ports: []PortMap{{Host: 80, Guest: 1}, {Host: 80, Guest: 2}}},
		"zero":     {Ports: []PortMap{{Host: 0, Guest: 80}}},
		"too many": {Ports: many},
		"bad cidr": {Egress: Egress{AllowCIDRs: []string{"x"}}},
	} {
		if err := n.Validate(); err == nil {
			t.Fatalf("%s must be refused", name)
		}
	}
}
