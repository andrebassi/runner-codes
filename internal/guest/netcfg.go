package guest

import (
	"errors"
	"fmt"
	"net/netip"
	"os"
	"strings"

	"runner.code/internal/protocol"
)

// NetConfig é a rede que o host mandou o guest montar.
type NetConfig struct {
	Addr    netip.Prefix
	Gateway netip.Addr
	DNS     []netip.Addr
}

// GatewayOnLink diz se o gateway fica fora do prefixo do endereço, e então precisa de uma rota de link
// própria antes da rota padrão. É o caso do Calico: a eth0 do pod ganha um /32 e o gateway 169.254.1.1,
// respondido por proxy ARP no host; sem a rota de link o kernel recusa a padrão com "network
// unreachable" e o pod fica sem saída (medido no microk8s 1.35, 20/10/2026).
func (c NetConfig) GatewayOnLink() bool {
	return c.Gateway.IsValid() && !c.Addr.Contains(c.Gateway)
}

// NetConfigFromEnv lê a configuração das variáveis RUNNER_NET_*. Sem RUNNER_NET_ADDR a
// máquina não tem rede (ok=false), o que é o padrão.
func NetConfigFromEnv(get func(string) string) (cfg NetConfig, ok bool, err error) {
	addr := get(protocol.EnvNetAddr)
	if addr == "" {
		return cfg, false, nil
	}
	if cfg.Addr, err = netip.ParsePrefix(addr); err != nil {
		return cfg, true, fmt.Errorf("%s: %w", protocol.EnvNetAddr, err)
	}
	if cfg.Gateway, err = netip.ParseAddr(get(protocol.EnvNetGateway)); err != nil {
		return cfg, true, fmt.Errorf("%s: %w", protocol.EnvNetGateway, err)
	}
	for _, d := range strings.Split(get(protocol.EnvNetDNS), ",") {
		if d = strings.TrimSpace(d); d == "" {
			continue
		}
		a, err := netip.ParseAddr(d)
		if err != nil {
			return cfg, true, fmt.Errorf("%s: %w", protocol.EnvNetDNS, err)
		}
		cfg.DNS = append(cfg.DNS, a)
	}
	return cfg, true, nil
}

// ResolvConf monta o /etc/resolv.conf. options timeout:2 attempts:2 acompanha o prazo do
// gateway (2 s por consulta): o padrão da glibc (5 s x 2) deixaria um nome negado travando
// o programa por 10 s antes do erro.
func ResolvConf(servers []netip.Addr) []byte {
	var b strings.Builder
	b.WriteString("# gerado pelo runner (rede da máquina)\n")
	for _, s := range servers {
		fmt.Fprintf(&b, "nameserver %s\n", s)
	}
	b.WriteString("options timeout:2 attempts:2\n")
	return []byte(b.String())
}

// writeResolvConf grava o arquivo trocando um link simbólico (o Fedora aponta para o
// systemd-resolved, que não roda aqui) por um arquivo comum.
func writeResolvConf(path string, content []byte) error {
	if fi, err := os.Lstat(path); err == nil && fi.Mode()&os.ModeSymlink != 0 {
		if err := os.Remove(path); err != nil {
			return err
		}
	}
	return os.WriteFile(path, content, 0o644)
}

// resolvConfPath é onde o resolv.conf é gravado; o teste troca.
var resolvConfPath = "/etc/resolv.conf"

// configureLink monta eth0 (endereço, rota, up); o teste troca.
var configureLink = configureLinkOS

// errNoNetSupport é a resposta fora do Linux: só o guest configura rede.
var errNoNetSupport = errors.New("network configuration is only available inside the Linux guest")

// ConfigureNetwork monta a rede pedida pelo host, se houver. Erro aqui não derruba o
// guest: o exec e o cp continuam funcionando, e a falha aparece no console da máquina.
func ConfigureNetwork(get func(string) string) error {
	cfg, ok, err := NetConfigFromEnv(get)
	if !ok || err != nil {
		return err
	}
	if err := configureLink(cfg); err != nil {
		return fmt.Errorf("configure eth0: %w", err)
	}
	if len(cfg.DNS) == 0 {
		return nil
	}
	if err := writeResolvConf(resolvConfPath, ResolvConf(cfg.DNS)); err != nil {
		return fmt.Errorf("write resolv.conf: %w", err)
	}
	return nil
}
