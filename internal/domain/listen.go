package domain

import (
	"sort"
	"strconv"
	"strings"
)

// tcpListen é o estado LISTEN no /proc/net/tcp do Linux (TCP_LISTEN = 10, em hexa).
const tcpListen = "0A"

// ParseListeningPorts lê o conteúdo de /proc/net/tcp e /proc/net/tcp6 (podem vir concatenados) e devolve, em ordem e
// sem repetir, as portas TCP em LISTEN que o host consegue alcançar discando 127.0.0.1 no guest: as que escutam em
// endereço curinga (0.0.0.0, ::) ou de loopback (127.0.0.0/8, ::1, ::ffff:127.x). Servidor preso só ao IP da eth0
// fica de fora: o encaminhamento do terminal web chega pelo loopback do guest e não o alcançaria.
func ParseListeningPorts(procNet string) []uint16 {
	seen := map[uint16]bool{}
	for _, line := range strings.Split(procNet, "\n") {
		f := strings.Fields(line)
		// Linha de dados: "N: LOCAL REMOTE ST ..."; o cabeçalho ("sl local_address ...") não tem ':' no 1º campo.
		if len(f) < 4 || !strings.HasSuffix(f[0], ":") || f[3] != tcpListen {
			continue
		}
		addr, portHex, ok := strings.Cut(f[1], ":")
		if !ok || !reachableFromLoopback(addr) {
			continue
		}
		port, err := strconv.ParseUint(portHex, 16, 16)
		if err != nil || port == 0 {
			continue
		}
		seen[uint16(port)] = true
	}
	out := make([]uint16, 0, len(seen))
	for p := range seen {
		out = append(out, p)
	}
	sort.Slice(out, func(i, j int) bool { return out[i] < out[j] })
	return out
}

// reachableFromLoopback diz se o endereço em hexa do /proc/net/tcp[6] aceita conexão vinda de 127.0.0.1.
// O kernel grava cada palavra de 32 bits em little-endian: 127.0.0.1 aparece como 0100007F.
func reachableFromLoopback(hexAddr string) bool {
	switch len(hexAddr) {
	case 8: // IPv4
		return hexAddr == "00000000" || strings.HasSuffix(hexAddr, "7F")
	case 32: // IPv6
		switch hexAddr {
		case strings.Repeat("0", 32), // ::
			"00000000000000000000000001000000": // ::1
			return true
		}
		// ::ffff:a.b.c.d (v4 mapeado): prefixo 0000…0000FFFF0000 e o IPv4 na última palavra.
		return strings.HasPrefix(hexAddr, "0000000000000000FFFF0000") && reachableFromLoopback(hexAddr[24:])
	}
	return false
}
