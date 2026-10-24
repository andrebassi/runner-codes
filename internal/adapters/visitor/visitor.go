// Package visitor é o miolo do rc-visitor, o processo que roda dentro da Fly Machine de cada visitante do playground:
// o firewall que fecha a rede privada do Fly, a lista de portas abertas, a ponte de portas para o front e o hub do
// terminal web que se refaz quando o programa principal sai.
package visitor

import (
	"context"
	"crypto/subtle"
	"fmt"
	"net"
	"net/http"
	"net/http/httputil"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"sync"

	"runner.codes/internal/adapters/webterm"
	"runner.codes/internal/domain"
)

// Rules são as regras do firewall da máquina do visitante, medidas no Fly em 20/10/2026
// (scripts/fly-firewall-probe.sh). O kernel do Fly não tem o módulo owner, então a regra não pode ser "só o uid do
// visitante": vale para a máquina inteira e barra a ABERTURA de conexão (SYN) e o UDP para a rede privada. As
// respostas do rc-visitor ao front (SYN-ACK em diante) seguem saindo; o visitante não alcança outra máquina nem a API
// interna do Fly (_api.internal), e o DNS do Fly (fdaa::3) e a internet continuam abertos.
func Rules() [][]string {
	var out [][]string
	v6 := func(args ...string) { out = append(out, append([]string{"ip6tables", "-w", "-A", "OUTPUT"}, args...)) }
	v4 := func(args ...string) { out = append(out, append([]string{"iptables", "-w", "-A", "OUTPUT"}, args...)) }
	// DNS do Fly antes do bloqueio: sem ele nenhum nome resolve (o resolv.conf da máquina aponta para fdaa::3).
	v6("-d", "fdaa::3", "-p", "udp", "--dport", "53", "-j", "ACCEPT")
	v6("-d", "fdaa::3", "-p", "tcp", "--dport", "53", "-j", "ACCEPT")
	// fc00::/7 cobre a 6PN (fdaa::/16) e qualquer ULA; fe80::/10 é o enlace local.
	for _, net6 := range []string{"fc00::/7", "fe80::/10"} {
		v6("-d", net6, "-p", "tcp", "--syn", "-j", "REJECT", "--reject-with", "tcp-reset")
		v6("-d", net6, "-p", "udp", "-j", "REJECT")
	}
	// IPv4 privado, CGNAT e link-local (metadados de nuvem moram em 169.254.169.254).
	for _, net4 := range []string{"10.0.0.0/8", "172.16.0.0/12", "192.168.0.0/16", "100.64.0.0/10", "169.254.0.0/16"} {
		v4("-d", net4, "-p", "tcp", "--syn", "-j", "REJECT", "--reject-with", "tcp-reset")
		v4("-d", net4, "-p", "udp", "-j", "REJECT")
	}
	return out
}

// ApplyFirewall aplica as regras com run (exec no rc-visitor); a primeira que falha aborta: sem firewall a máquina
// não sobe (o rc-visitor sai e o Fly a deixa parada).
func ApplyFirewall(run func(argv []string) error) error {
	for _, r := range Rules() {
		if err := run(r); err != nil {
			return fmt.Errorf("firewall %s: %w", strings.Join(r, " "), err)
		}
	}
	return nil
}

// LockFlyAPI fecha o /.fly da máquina para quem não é root. O Fly põe ali o socket /.fly/api, um proxy da API de
// Machines com a credencial do app (visto no log do boot em 20/10/2026): aberto, o visitante listaria e apagaria as
// máquinas dos outros visitantes. Diretório ausente (fora do Fly) não é erro.
func LockFlyAPI(root string) error {
	dir := filepath.Join(root, ".fly")
	if _, err := os.Stat(dir); os.IsNotExist(err) {
		return nil
	}
	return filepath.Walk(dir, func(p string, info os.FileInfo, err error) error {
		if err != nil {
			return err
		}
		if info.IsDir() {
			return os.Chmod(p, 0o700)
		}
		return os.Chmod(p, 0o600)
	})
}

// FilterEnv tira do ambiente as variáveis do rc-visitor (RC_*: os tokens), que não descem ao shell do visitante.
func FilterEnv(env []string) []string {
	out := make([]string, 0, len(env))
	for _, kv := range env {
		if !strings.HasPrefix(kv, "RC_") {
			out = append(out, kv)
		}
	}
	return out
}

// Ports lista as portas TCP em LISTEN da máquina pelo /proc (ProcRoot, /proc no Fly), sem as do próprio rc-visitor.
type Ports struct {
	ProcRoot string
	Exclude  []uint16
	// HideOwners esconde as portas cujo socket pertence a um processo com este nome (o argv[0] sem diretório). O
	// Claude Code abre sozinho um servidor interno em 127.0.0.1 numa porta aleatória (visto em 20/10/2026: pid do
	// `claude --dangerously-skip-permissions` em 127.0.0.1:35795) e ele virava uma aba que não leva a nada. Servidor
	// que o visitante ou o Claude sobem roda em outro processo (python, node) e continua aparecendo.
	HideOwners []string
}

// List lê tcp e tcp6 e devolve as portas alcançáveis pelo loopback, em ordem.
func (p Ports) List(context.Context) ([]uint16, error) {
	var all strings.Builder
	for _, f := range []string{"net/tcp", "net/tcp6"} {
		b, err := os.ReadFile(filepath.Join(p.ProcRoot, f))
		if err != nil && f == "net/tcp" {
			return nil, err
		}
		all.Write(b)
		all.WriteByte('\n')
	}
	hidden := p.hiddenPorts(all.String())
	var out []uint16
	for _, port := range domain.ParseListeningPorts(all.String()) {
		if !contains(p.Exclude, port) && !hidden[port] {
			out = append(out, port)
		}
	}
	return out, nil
}

// hiddenPorts acha as portas em LISTEN cujo inode de socket está aberto por um processo de HideOwners: junta
// inode→porta do /proc/net/tcp* com os /proc/<pid>/fd que apontam para socket:[inode].
func (p Ports) hiddenPorts(procNet string) map[uint16]bool {
	hidden := map[uint16]bool{}
	if len(p.HideOwners) == 0 {
		return hidden
	}
	byInode := map[string]uint16{}
	for _, line := range strings.Split(procNet, "\n") {
		f := strings.Fields(line)
		// "N: LOCAL REMOTE ST TX:RX TR:WHEN RETR UID TIMEOUT INODE": 0A é LISTEN, o inode é o 10º campo.
		if len(f) < 10 || f[3] != "0A" {
			continue
		}
		_, portHex, _ := strings.Cut(f[1], ":")
		if port, err := strconv.ParseUint(portHex, 16, 16); err == nil {
			byInode[f[9]] = uint16(port)
		}
	}
	pids, _ := filepath.Glob(filepath.Join(p.ProcRoot, "[0-9]*"))
	for _, dir := range pids {
		cmd, err := os.ReadFile(filepath.Join(dir, "cmdline"))
		if err != nil {
			continue
		}
		argv0, _, _ := strings.Cut(string(cmd), "\x00")
		if !containsString(p.HideOwners, filepath.Base(argv0)) {
			continue
		}
		fds, _ := os.ReadDir(filepath.Join(dir, "fd"))
		for _, fd := range fds {
			target, err := os.Readlink(filepath.Join(dir, "fd", fd.Name()))
			if inode, ok := strings.CutPrefix(target, "socket:["); err == nil && ok {
				if port, ok := byInode[strings.TrimSuffix(inode, "]")]; ok {
					hidden[port] = true
				}
			}
		}
	}
	return hidden
}

// containsString diz se s está na lista.
func containsString(list []string, s string) bool {
	for _, x := range list {
		if x == s {
			return true
		}
	}
	return false
}

// contains diz se port está na lista.
func contains(list []uint16, port uint16) bool {
	for _, x := range list {
		if x == port {
			return true
		}
	}
	return false
}

// Dial disca a porta no loopback: 127.0.0.1 primeiro e, se recusar, [::1] (servidor de dev preso ao IPv6).
func Dial(ctx context.Context, port uint16) (net.Conn, error) {
	var d net.Dialer
	c, err := d.DialContext(ctx, "tcp", net.JoinHostPort("127.0.0.1", strconv.Itoa(int(port))))
	if err == nil {
		return c, nil
	}
	return d.DialContext(ctx, "tcp", net.JoinHostPort("::1", strconv.Itoa(int(port))))
}

// PortBridge atende o front: com o token certo em X-Rc-Token, leva a requisição à porta de X-Rc-Port no loopback,
// com o Host original e sem os dois cabeçalhos. Token errado é 403 (a rede privada já é fechada pelo firewall; o
// token é a segunda tranca).
func PortBridge(token, headerPort, headerToken string, dial func(ctx context.Context, port uint16) (net.Conn, error)) http.Handler {
	return http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		if subtle.ConstantTimeCompare([]byte(r.Header.Get(headerToken)), []byte(token)) != 1 {
			http.Error(w, "forbidden", http.StatusForbidden)
			return
		}
		port, err := strconv.ParseUint(r.Header.Get(headerPort), 10, 16)
		if err != nil || port == 0 {
			http.Error(w, "invalid port", http.StatusBadRequest)
			return
		}
		rp := &httputil.ReverseProxy{
			Rewrite: func(pr *httputil.ProxyRequest) {
				pr.Out.URL.Scheme = "http"
				pr.Out.URL.Host = fmt.Sprintf("127.0.0.1:%d", port)
				pr.Out.Host = pr.In.Host
				pr.Out.Header.Del(headerPort)
				pr.Out.Header.Del(headerToken)
				// O X-Forwarded-For do front já vem pronto (o navegador do visitante); mantém em vez de acrescentar.
				pr.Out.Header["X-Forwarded-For"] = pr.In.Header["X-Forwarded-For"]
			},
			Transport: &http.Transport{DialContext: func(ctx context.Context, _, _ string) (net.Conn, error) { return dial(ctx, uint16(port)) }},
			ErrorHandler: func(w http.ResponseWriter, _ *http.Request, _ error) {
				http.Error(w, "nothing is answering on port "+strconv.Itoa(int(port))+" in this machine", http.StatusBadGateway)
			},
		}
		rp.ServeHTTP(w, r)
	})
}

// Holder guarda o hub do terminal e o refaz quando o programa principal saiu: o visitante que recarrega a página
// depois de sair do Claude ganha um novo, em vez de uma tela parada.
type Holder struct {
	New     func() *webterm.Hub
	mu      sync.Mutex
	hub     *webterm.Hub
	handler http.Handler
}

// current devolve o handler do hub vivo, abrindo um novo se preciso.
func (h *Holder) current() http.Handler {
	h.mu.Lock()
	defer h.mu.Unlock()
	if h.hub == nil || h.hub.MainEnded() {
		h.hub = h.New()
		h.handler = webterm.Handler(h.hub)
	}
	return h.handler
}

// ServeHTTP passa a requisição ao hub vivo.
func (h *Holder) ServeHTTP(w http.ResponseWriter, r *http.Request) { h.current().ServeHTTP(w, r) }
