package domain

import (
	"errors"
	"fmt"
	"net/netip"
	"strconv"
	"strings"
)

// MaxPorts é o teto de portas publicadas por máquina (64): cada uma é um
// listener no host, e uma declaração não deve poder abrir centenas.
const MaxPorts = 64

// PortMap publica uma porta do guest no 127.0.0.1 do host. Só o loopback: expor na rede
// do host é decisão de quem roda o host, não de quem escreve o runner.code.
type PortMap struct {
	Host  uint16 `json:"host"`
	Guest uint16 `json:"guest"`
	UDP   bool   `json:"udp,omitempty"`
}

// String devolve a porta no formato de entrada.
func (p PortMap) String() string {
	s := fmt.Sprintf("%d:%d", p.Host, p.Guest)
	if p.UDP {
		s += "/udp"
	}
	return s
}

// ParsePort lê "8080" (mesma porta dos dois lados), "8080:80" e o sufixo "/udp" ou "/tcp".
func ParsePort(s string) (PortMap, error) {
	var p PortMap
	spec := s
	switch {
	case strings.HasSuffix(spec, "/udp"):
		p.UDP, spec = true, strings.TrimSuffix(spec, "/udp")
	case strings.HasSuffix(spec, "/tcp"):
		spec = strings.TrimSuffix(spec, "/tcp")
	}
	host, guest, pair := strings.Cut(spec, ":")
	if !pair {
		guest = host
	}
	h, err1 := parsePortNumber(host)
	g, err2 := parsePortNumber(guest)
	if err1 != nil || err2 != nil {
		return p, fmt.Errorf("port %q must be PORT, HOST:GUEST or either with /udp (1-65535)", s)
	}
	p.Host, p.Guest = h, g
	return p, nil
}

// ParsePorts lê uma porta (como ParsePort) ou uma faixa de mesmo tamanho dos dois lados:
// "5173-5180:5173-5180", "5173-5180" (iguais) ou com /udp. Devolve uma PortMap por porta,
// porque cada uma é um listener no host; faixa maior que MaxPorts é recusada aqui, antes de
// alocar 65 mil entradas para a validação recusar depois.
func ParsePorts(s string) ([]PortMap, error) {
	spec, proto := s, ""
	for _, suf := range []string{"/udp", "/tcp"} {
		if strings.HasSuffix(spec, suf) {
			spec, proto = strings.TrimSuffix(spec, suf), suf
		}
	}
	if !strings.Contains(spec, "-") {
		p, err := ParsePort(s)
		return []PortMap{p}, err
	}
	host, guest, pair := strings.Cut(spec, ":")
	if !pair {
		guest = host
	}
	h1, h2, err1 := parsePortRange(host)
	g1, g2, err2 := parsePortRange(guest)
	switch {
	case err1 != nil || err2 != nil:
		return nil, fmt.Errorf("port %q must be PORT, HOST:GUEST, a range A-B[:C-D] or either with /udp (1-65535)", s)
	case h2-h1 != g2-g1:
		return nil, fmt.Errorf("port range %q: both sides must have the same length", s)
	case int(h2-h1)+1 > MaxPorts:
		return nil, fmt.Errorf("port range %q: %d ports, at most %d", s, int(h2-h1)+1, MaxPorts)
	}
	out := make([]PortMap, 0, int(h2-h1)+1)
	for i := 0; i <= int(h2-h1); i++ {
		out = append(out, PortMap{Host: h1 + uint16(i), Guest: g1 + uint16(i), UDP: proto == "/udp"})
	}
	return out, nil
}

// parsePortRange lê "A-B" com A ≤ B.
func parsePortRange(s string) (uint16, uint16, error) {
	a, b, ok := strings.Cut(s, "-")
	lo, err1 := parsePortNumber(a)
	hi, err2 := parsePortNumber(b)
	if !ok || err1 != nil || err2 != nil || lo > hi {
		return 0, 0, fmt.Errorf("bad port range %q", s)
	}
	return lo, hi, nil
}

// parsePortNumber aceita só 1–65535 em decimal.
func parsePortNumber(s string) (uint16, error) {
	n, err := strconv.ParseUint(s, 10, 16)
	if err != nil || n == 0 {
		return 0, fmt.Errorf("bad port %q", s)
	}
	return uint16(n), nil
}

// Egress é a política de saída de uma máquina com rede. Vazia = internet aberta (com o
// piso, abaixo). Qualquer lista de permissão (hosts, padrões ou CIDRs) inverte o padrão:
// só sai o que estiver permitido.
type Egress struct {
	// AllowHosts casa o nome e os subdomínios ("example.com" libera api.example.com);
	// "=nome" casa só o nome; "*.x" só os subdomínios.
	AllowHosts []string `json:"allow_hosts,omitempty"`
	// AllowHostPatterns casa exato ("registry.npmjs.org") ou só subdomínios ("*.x.com").
	AllowHostPatterns []string `json:"allow_host_patterns,omitempty"`
	AllowCIDRs        []string `json:"allow_cidrs,omitempty"`
	// DenyCIDRs é conferida antes de qualquer permissão.
	DenyCIDRs []string `json:"deny_cidrs,omitempty"`
	// BlockPrivate fecha as faixas privadas (RFC 1918, CGNAT, ULA); AllowPrivate reabre
	// faixas dentro delas e implica BlockPrivate.
	BlockPrivate bool     `json:"block_private,omitempty"`
	AllowPrivate []string `json:"allow_private,omitempty"`
}

// Restricted diz se há lista de permissão: aí o padrão é negar.
func (e Egress) Restricted() bool {
	return len(e.AllowHosts) > 0 || len(e.AllowHostPatterns) > 0 || len(e.AllowCIDRs) > 0
}

// Empty diz se nenhuma regra foi declarada.
func (e Egress) Empty() bool {
	return !e.Restricted() && len(e.DenyCIDRs) == 0 && !e.BlockPrivate && len(e.AllowPrivate) == 0
}

// floorBlocked são destinos que nenhuma regra abre: metadata de nuvem (de onde sai
// credencial da conta inteira) e o link-local onde ela mora. Isso fecha o ataque clássico
// de SSRF a 169.254.169.254 mesmo com a internet aberta.
var floorBlocked = mustPrefixes("169.254.0.0/16", "fe80::/10", "100.100.100.200/32", "fd00:ec2::254/128")

// loopbackPrefixes é o loopback do host: fechado por padrão (um serviço local do dono
// não é internet), reaberto só por allow_cidrs explícito, nunca por IP aprendido em DNS,
// para um domínio permitido não conseguir apontar para 127.0.0.1 (DNS rebinding).
var loopbackPrefixes = mustPrefixes("127.0.0.0/8", "::1/128", "0.0.0.0/8")

// privatePrefixes são as faixas fechadas por block_private.
var privatePrefixes = mustPrefixes("10.0.0.0/8", "172.16.0.0/12", "192.168.0.0/16", "100.64.0.0/10", "fc00::/7")

// mustPrefixes converte literais conhecidos; só falha com erro de digitação no código.
func mustPrefixes(ss ...string) []netip.Prefix {
	out := make([]netip.Prefix, len(ss))
	for i, s := range ss {
		out[i] = netip.MustParsePrefix(s)
	}
	return out
}

// parsePrefixes aceita CIDR ou IP solto (vira /32 ou /128).
func parsePrefixes(ss []string) ([]netip.Prefix, error) {
	out := make([]netip.Prefix, 0, len(ss))
	for _, s := range ss {
		if p, err := netip.ParsePrefix(s); err == nil {
			out = append(out, p.Masked())
			continue
		}
		a, err := netip.ParseAddr(s)
		if err != nil {
			return nil, fmt.Errorf("%q is not a CIDR or an IP address", s)
		}
		out = append(out, netip.PrefixFrom(a, a.BitLen()))
	}
	return out, nil
}

// contains diz se algum prefixo contém o endereço.
func contains(ps []netip.Prefix, a netip.Addr) bool {
	for _, p := range ps {
		if p.Contains(a) {
			return true
		}
	}
	return false
}

// normalizeHost deixa o nome comparável: minúsculo e sem o ponto final do DNS.
func normalizeHost(s string) string {
	return strings.TrimSuffix(strings.ToLower(strings.TrimSpace(s)), ".")
}

// validHostPattern recusa entrada vazia, com esquema, porta ou caminho: "https://x.com"
// ou "x.com:443" nunca casariam, e a pessoa acharia que liberou.
func validHostPattern(s string) error {
	h := strings.TrimPrefix(strings.TrimPrefix(s, "="), "*.")
	if h == "" || strings.ContainsAny(h, ":/ *=") {
		return fmt.Errorf("host %q must be a bare name (example.com, =example.com or *.example.com)", s)
	}
	return nil
}

// Policy é a Egress compilada: CIDRs parseados e nomes normalizados, pronta para decidir
// a cada conexão sem reparsear.
type Policy struct {
	// denyAll fecha toda saída: máquina com porta publicada e sem rede declarada.
	denyAll      bool
	restricted   bool
	allow, deny  []netip.Prefix
	allowPrivate []netip.Prefix
	blockPrivate bool
	// suffix casa nome e subdomínios; exact só o nome; sub só os subdomínios.
	suffix, exact, sub []string
}

// Compile valida a política e devolve a forma de decidir.
func (e Egress) Compile() (*Policy, error) {
	p := &Policy{restricted: e.Restricted(), blockPrivate: e.BlockPrivate || len(e.AllowPrivate) > 0}
	var err error
	if p.allow, err = parsePrefixes(e.AllowCIDRs); err != nil {
		return nil, fmt.Errorf("allow_cidrs: %w", err)
	}
	if p.deny, err = parsePrefixes(e.DenyCIDRs); err != nil {
		return nil, fmt.Errorf("deny_cidrs: %w", err)
	}
	if p.allowPrivate, err = parsePrefixes(e.AllowPrivate); err != nil {
		return nil, fmt.Errorf("allow_private: %w", err)
	}
	for _, h := range e.AllowHosts {
		if err := validHostPattern(h); err != nil {
			return nil, err
		}
		h = normalizeHost(h)
		switch {
		case strings.HasPrefix(h, "="):
			p.exact = append(p.exact, h[1:])
		case strings.HasPrefix(h, "*."):
			p.sub = append(p.sub, h[2:])
		default:
			p.suffix = append(p.suffix, h)
		}
	}
	for _, h := range e.AllowHostPatterns {
		if err := validHostPattern(h); err != nil || strings.HasPrefix(h, "=") {
			return nil, fmt.Errorf("allow_host_patterns: %q must be a name or *.name", h)
		}
		h = normalizeHost(h)
		if strings.HasPrefix(h, "*.") {
			p.sub = append(p.sub, h[2:])
		} else {
			p.exact = append(p.exact, h)
		}
	}
	return p, nil
}

// FiltersNames diz se o DNS filtra por nome (há lista de hosts). Só com CIDRs o nome
// resolve livre e a conexão é que decide.
func (p *Policy) FiltersNames() bool { return len(p.suffix)+len(p.exact)+len(p.sub) > 0 }

// NameAllowed decide se o DNS responde por um nome. Sem lista de hosts, todo nome resolve.
func (p *Policy) NameAllowed(name string) bool {
	if p.denyAll {
		return false
	}
	if !p.FiltersNames() {
		return true
	}
	n := normalizeHost(name)
	for _, s := range p.suffix {
		if n == s || strings.HasSuffix(n, "."+s) {
			return true
		}
	}
	for _, s := range p.exact {
		if n == s {
			return true
		}
	}
	for _, s := range p.sub {
		if strings.HasSuffix(n, "."+s) {
			return true
		}
	}
	return false
}

// StaticHosts são as entradas que dá para resolver no start (nome e "=nome"); "*.x" só se
// aprende pelo DNS, porque não há como enumerar subdomínios.
func (p *Policy) StaticHosts() []string {
	return append(append([]string{}, p.suffix...), p.exact...)
}

// IPAllowed decide uma conexão para o endereço. learned diz se o IP veio de uma resposta
// de DNS para um nome permitido. A ordem é a que fecha os ataques: piso, deny, privado,
// e só então as permissões.
func (p *Policy) IPAllowed(a netip.Addr, learned bool) (bool, string) {
	a = a.Unmap()
	if p.denyAll {
		return false, "outbound network is off (net = false)"
	}
	if contains(floorBlocked, a) {
		return false, "cloud metadata and link-local are always blocked"
	}
	if contains(loopbackPrefixes, a) && !contains(p.allow, a) {
		return false, "host loopback needs an explicit allow_cidrs entry"
	}
	if contains(p.deny, a) {
		return false, "deny_cidrs"
	}
	if p.blockPrivate && contains(privatePrefixes, a) && !contains(p.allowPrivate, a) && !contains(p.allow, a) {
		return false, "block_private"
	}
	if !p.restricted || contains(p.allow, a) {
		return true, ""
	}
	// IP aprendido só autoriza quando o DNS filtra nomes: aí ele veio de um nome da lista.
	// Sem lista de hosts todo nome resolve, todo IP seria "aprendido", e allow_cidrs sozinho
	// deixava o guest sair para qualquer destino com nome (medido: example.com e o S3 da AWS
	// responderam a uma máquina com allow_cidrs = ["127.0.0.1/32"]).
	if learned && p.FiltersNames() && !contains(loopbackPrefixes, a) {
		return true, ""
	}
	return false, "not in the allow list"
}

// Network é a rede declarada de uma máquina: ligada ou não, portas publicadas e a política.
// Porta ou política sem Enabled continuam valendo: porta sem rede de saída serve o
// guest para o host sem deixar o guest discar para fora.
type Network struct {
	Enabled bool      `json:"enabled,omitempty"`
	Ports   []PortMap `json:"ports,omitempty"`
	Egress  Egress    `json:"egress,omitempty"`
	// Name é a rede nomeada: máquinas com o mesmo nome se alcançam por IP (TCP) e por
	// <máquina>.<rede>.internal. Address é o IP desta máquina nela, dado no create.
	Name    string `json:"name,omitempty"`
	Address string `json:"address,omitempty"`
	// Credentials são as credenciais que a máquina usa sem ler (troca no gateway).
	Credentials []Credential `json:"credentials,omitempty"`
}

// FabricPrefix é a faixa dos IPs das redes nomeadas: 100.98.0.0/16, ao lado do enlace
// guest↔gateway (100.96.0.0/30) e fora das faixas que uma rede doméstica ou de nuvem usa.
var FabricPrefix = netip.MustParsePrefix("100.98.0.0/16")

// fabricFirst é o primeiro IP dado a uma máquina: .0.1 a .0.9 ficam livres para uso futuro
// (um resolvedor da rede, por exemplo) sem renumerar quem já existe.
var fabricFirst = netip.MustParseAddr("100.98.0.10")

// NextFabricAddress devolve o primeiro IP livre da faixa, dado o que já está em uso.
func NextFabricAddress(used []string) (netip.Addr, error) {
	taken := map[netip.Addr]bool{}
	for _, u := range used {
		if a, err := netip.ParseAddr(u); err == nil {
			taken[a] = true
		}
	}
	for a := fabricFirst; FabricPrefix.Contains(a); a = a.Next() {
		if !taken[a] && a.As4()[3] != 0 && a.As4()[3] != 255 {
			return a, nil
		}
	}
	return netip.Addr{}, errors.New("named network is full")
}

// Attached diz se a máquina ganha placa de rede (rede de saída, porta ou política).
func (n Network) Attached() bool {
	return n.Enabled || len(n.Ports) > 0 || !n.Egress.Empty() || n.Name != "" || len(n.Credentials) > 0
}

// OutboundPolicy compila a política efetiva: sem rede de saída declarada, nega tudo (só
// as portas publicadas funcionam); com política ou credencial, ela implica rede.
func (n Network) OutboundPolicy() (*Policy, error) {
	if !n.Enabled && n.Egress.Empty() && len(n.Credentials) == 0 {
		return &Policy{denyAll: true, restricted: true}, nil
	}
	return n.Egress.Compile()
}

// Validate confere portas, política e a rede nomeada.
func (n Network) Validate() error {
	if n.Name != "" && !machineName.MatchString(n.Name) {
		return fmt.Errorf("network name %q must be lowercase letters, digits and -, up to 63", n.Name)
	}
	if n.Address != "" {
		a, err := netip.ParseAddr(n.Address)
		if err != nil || !FabricPrefix.Contains(a) {
			return fmt.Errorf("network address %q must be in %s", n.Address, FabricPrefix)
		}
	}
	if len(n.Ports) > MaxPorts {
		return fmt.Errorf("at most %d ports", MaxPorts)
	}
	seen := map[string]bool{}
	for _, p := range n.Ports {
		if p.Host == 0 || p.Guest == 0 {
			return fmt.Errorf("port %s: ports must be 1-65535", p)
		}
		k := fmt.Sprintf("%d/%v", p.Host, p.UDP)
		if seen[k] {
			return fmt.Errorf("port %s: host port published twice", p)
		}
		seen[k] = true
	}
	if _, err := n.Egress.Compile(); err != nil {
		return err
	}
	return validateCredentials(n.Credentials, n.Egress)
}
