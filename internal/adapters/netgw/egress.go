package netgw

import (
	"context"
	"encoding/binary"
	"fmt"
	"net"
	"net/netip"
	"strings"
	"time"
)

// SinkholeIP é o endereço que o DNS dá a um nome negado quando a política pede a página de bloqueio
// (domain.Egress.BlockPage) e o host tem proxy de saída: a conexão a ele vai ao proxy marcada como negada, e ele
// serve a página "Sem acesso" no lugar do "host não encontrado". É do TEST-NET-1 (192.0.2.0/24, RFC 5737): reservado
// para documentação, nunca roteado, e o navegador o trata como endereço público. No CGNAT (100.96.0.9, a primeira
// escolha) o Firefox via "rede local" e pedia ao visitante permissão de acesso à rede local a cada site negado
// (medido em 25/10/2026, no trial).
var SinkholeIP = netip.MustParseAddr("192.0.2.9")

// sinkholeTTL é o TTL da resposta do sinkhole: curto, para um nome liberado depois (política nova com a máquina
// ligada) voltar a resolver o IP real em segundos.
const sinkholeTTL = 5

// egressDialTimeout é o teto para abrir a conexão ao proxy de saída, que está no mesmo host: passou disso, ele está
// fora do ar, e a conexão do guest é recusada (falha fechada: nada sai sem passar pelo registro).
const egressDialTimeout = 3 * time.Second

// proxySignature são os 12 bytes que abrem um header PROXY v2.
var proxySignature = []byte{0x0D, 0x0A, 0x0D, 0x0A, 0x00, 0x0D, 0x0A, 0x51, 0x55, 0x49, 0x54, 0x0A}

// TLVs próprios do header (faixa 0xE0–0xEF, de uso livre pela especificação); o edgeproxy lê os mesmos
// (edgeproxy/src/egress/proxy_header.rs no superbot).
const (
	tlvMachine = 0xE0
	tlvName    = 0xE1
	tlvDenied  = 0xE2
	tlvAllowed = 0xE3
	// tlvPaths são as regras de caminho ("host/prefixo", separadas por vírgula) que o proxy libera request a request
	// numa conexão negada (domain.Egress.AllowPaths).
	tlvPaths = 0xE4
)

// proxyHeader monta o header PROXY v2 de uma conexão a dst: a máquina, o nome que o guest resolveu para o IP (vazio
// se ele discou um IP), se a política negou e, nas negadas, o que está liberado (os hosts, para a página de bloqueio, e
// os caminhos, que o proxy deixa passar mesmo com o host negado).
func proxyHeader(dst netip.AddrPort, machine, name string, denied bool, allowed, paths []string) []byte {
	var tlvs []byte
	tlv := func(kind byte, value string) {
		tlvs = append(tlvs, kind)
		tlvs = binary.BigEndian.AppendUint16(tlvs, uint16(len(value)))
		tlvs = append(tlvs, value...)
	}
	tlv(tlvMachine, machine)
	if name != "" {
		tlv(tlvName, name)
	}
	if denied {
		tlv(tlvDenied, "1")
		if len(allowed) > 0 {
			tlv(tlvAllowed, strings.Join(allowed, ","))
		}
		if len(paths) > 0 {
			tlv(tlvPaths, strings.Join(paths, ","))
		}
	}
	out := append([]byte{}, proxySignature...)
	// 0x21: versão 2, comando PROXY; 0x11: IPv4 sobre TCP.
	out = append(out, 0x21, 0x11)
	out = binary.BigEndian.AppendUint16(out, uint16(12+len(tlvs)))
	src := GuestIP.As4()
	d := dst.Addr().Unmap().As4()
	out = append(out, src[:]...)
	out = append(out, d[:]...)
	// A porta de origem não importa ao proxy (ele não a registra): vai zero.
	out = binary.BigEndian.AppendUint16(out, 0)
	out = binary.BigEndian.AppendUint16(out, dst.Port())
	return append(out, tlvs...)
}

// dialEgress abre a conexão ao proxy de saída e escreve o header: o que vier depois é o tráfego do guest.
func (g *Gateway) dialEgress(dst netip.AddrPort, name string, denied bool, allowed, paths []string) (net.Conn, error) {
	ctx, cancel := context.WithTimeout(g.ctx, egressDialTimeout)
	defer cancel()
	c, err := g.cfg.Dial(ctx, "tcp", g.cfg.EgressProxy)
	if err != nil {
		return nil, fmt.Errorf("egress proxy %s: %w", g.cfg.EgressProxy, err)
	}
	if _, err := c.Write(proxyHeader(dst, g.cfg.MachineName, name, denied, allowed, paths)); err != nil {
		c.Close()
		return nil, fmt.Errorf("egress proxy header: %w", err)
	}
	return c, nil
}

// uninspectedUDP diz se um datagrama UDP fica de fora com o proxy de saída ligado: 443 e 80 são o HTTP/3 (QUIC), que o
// proxy não abre; sem essa recusa o navegador iria a esses sites por QUIC, direto e sem registro. Recusado, ele volta
// ao TCP sozinho (o que todo navegador faz quando o QUIC não responde).
func (g *Gateway) uninspectedUDP(port uint16) bool {
	return g.cfg.EgressProxy != "" && (port == 443 || port == 80)
}
