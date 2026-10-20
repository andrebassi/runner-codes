//go:build e2e

package e2e

import (
	"os"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// TestNamedNetworkMachines trava a rede nomeada em VMs reais:
//   - duas máquinas da mesma rede ganham IPs seguidos e se alcançam pelo IP e pelo nome
//     <máquina>.<rede>.internal, sem --net (é tráfego interno, não saída);
//   - o servidor vê o IP de rede de quem conectou, não o do gateway;
//   - máquina de outra rede não alcança o mesmo IP nem resolve o nome;
//   - máquina parada some da rede: o nome deixa de resolver.
func TestNamedNetworkMachines(t *testing.T) {
	lab, other := uniqueName("lab"), uniqueName("oth")
	srv, cli, out := uniqueName("srv"), uniqueName("cli"), uniqueName("out")
	for _, n := range []string{srv, cli, out} {
		defer rcMachine(t, "delete", "--name", n, "--force")
	}
	dir := t.TempDir()
	rc := filepath.Join(dir, "runner.code")
	os.WriteFile(rc, []byte("stack = \"python\"\ncmd = [\"python3\", \"-m\", \"http.server\", \"8080\"]\n"), 0o600)
	mustMachine(t, "create", "-f", rc, "--name", srv, "--network", lab)
	mustMachine(t, "create", "--name", cli, "--stack", "python", "--network", lab)
	mustMachine(t, "create", "--name", out, "--stack", "python", "--network", other)
	for _, n := range []string{srv, cli, out} {
		mustMachine(t, "start", "--name", n)
	}
	srvIP, cliIP := "100.98.0.10", "100.98.0.11"
	fetch := func(machine, url string) (string, int) {
		o, _, code := rcMachine(t, "exec", "--name", machine, "--", "wget", "-qO-", "-T", "3", url)
		return o, code
	}
	waitFor(t, "the server to answer by IP", 20*time.Second, func() bool {
		_, code := fetch(cli, "http://"+srvIP+":8080/")
		return code == 0
	})
	if o, code := fetch(cli, "http://"+srv+"."+lab+".internal:8080/"); code != 0 || !strings.Contains(o, "Directory listing") {
		t.Fatalf("by name: rc=%d %q", code, o)
	}
	if logs := mustMachine(t, "logs", "--name", srv); !strings.Contains(logs, cliIP) {
		t.Fatalf("the server must see the caller's network IP %s: %q", cliIP, logs)
	}
	start := time.Now()
	if _, code := fetch(out, "http://"+srvIP+":8080/"); code == 0 {
		t.Fatal("another network must not reach this one")
	}
	if time.Since(start) > 10*time.Second {
		t.Fatal("the refusal must be fast")
	}
	if _, code := fetch(out, "http://"+srv+"."+lab+".internal:8080/"); code == 0 {
		t.Fatal("another network must not resolve this one's names")
	}
	mustMachine(t, "stop", "--name", srv)
	if o, _, _ := rcMachine(t, "exec", "--name", cli, "--", "nslookup", srv+"."+lab+".internal"); strings.Contains(o, srvIP) {
		t.Fatalf("a stopped machine leaves the network's DNS: %q", o)
	}
	for _, n := range []string{cli, out} {
		mustMachine(t, "stop", "--name", n)
	}
}
