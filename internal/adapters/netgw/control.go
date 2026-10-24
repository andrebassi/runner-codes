package netgw

import (
	"bufio"
	"encoding/json"
	"fmt"
	"net"
	"os"

	"runner.codes/internal/domain"
)

// maxControlMsg limita a política recebida pelo socket de controle: 1 MiB comporta
// milhares de entradas, e sem teto um cliente local prenderia memória do processo da VM.
const maxControlMsg = 1 << 20

// ServeControl escuta no socket Unix path pedidos de troca de política com a máquina
// ligada: cada conexão manda uma linha com o JSON de domain.Egress e recebe "ok" ou
// "error: ...". O socket nasce 0600: só o dono da máquina troca a política.
func (g *Gateway) ServeControl(path string, enabled bool) error {
	os.Remove(path)
	l, err := net.Listen("unix", path)
	if err != nil {
		return fmt.Errorf("control socket: %w", err)
	}
	if err := os.Chmod(path, 0o600); err != nil {
		l.Close()
		return fmt.Errorf("control socket: %w", err)
	}
	g.listeners = append(g.listeners, l)
	g.wg.Add(1)
	go func() {
		defer g.wg.Done()
		for {
			c, err := l.Accept()
			if err != nil {
				return
			}
			go g.control(c, enabled)
		}
	}()
	return nil
}

// control atende um pedido de troca de política.
func (g *Gateway) control(c net.Conn, enabled bool) {
	defer c.Close()
	sc := bufio.NewScanner(c)
	sc.Buffer(make([]byte, 64<<10), maxControlMsg)
	if !sc.Scan() {
		return
	}
	var e domain.Egress
	if err := json.Unmarshal(sc.Bytes(), &e); err != nil {
		fmt.Fprintf(c, "error: %v\n", err)
		return
	}
	p, err := domain.Network{Enabled: enabled, Egress: e}.OutboundPolicy()
	if err != nil {
		fmt.Fprintf(c, "error: %v\n", err)
		return
	}
	g.SetPolicy(p)
	fmt.Fprintln(c, "ok")
}

// UpdatePolicy é o lado cliente: manda a política nova ao gateway de uma máquina ligada.
func UpdatePolicy(path string, e domain.Egress) error {
	c, err := net.Dial("unix", path)
	if err != nil {
		return fmt.Errorf("machine network is not reachable: %w", err)
	}
	defer c.Close()
	b, _ := json.Marshal(e)
	// Falha na escrita é o mesmo caso de não haver resposta: o gateway fechou antes de ler
	// (medido: o teste do servidor mudo dava "broken pipe" cru em 2 de 20 rodadas).
	if _, err := c.Write(append(b, '\n')); err != nil {
		return fmt.Errorf("no answer from the machine network: %w", err)
	}
	line, err := bufio.NewReader(c).ReadString('\n')
	if err != nil {
		return fmt.Errorf("no answer from the machine network: %w", err)
	}
	if line != "ok\n" {
		return fmt.Errorf("%s", line[:len(line)-1])
	}
	return nil
}
