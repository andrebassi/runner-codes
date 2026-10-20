//go:build e2e

package e2e

import (
	"context"
	"io"
	"net"
	"net/http"
	"os"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"
)

// needInternet pula o teste se o host não alcança a internet: o que se testa aqui é o
// gateway, não a conexão do laptop.
func needInternet(t *testing.T) {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)
	defer cancel()
	if _, err := net.DefaultResolver.LookupHost(ctx, "example.com"); err != nil {
		t.Skipf("host has no internet: %v", err)
	}
}

// freeHostPort devolve uma porta TCP livre no 127.0.0.1.
func freeHostPort(t *testing.T) int {
	l, err := net.Listen("tcp", "127.0.0.1:0")
	if err != nil {
		t.Fatal(err)
	}
	defer l.Close()
	return l.Addr().(*net.TCPAddr).Port
}

// machineDir é o diretório da máquina no cache do host.
func machineDir(t *testing.T, name string) string {
	// O mesmo cálculo do rc: $RUNNER_CACHE_DIR, senão o cache do usuário (Library/Caches no
	// macOS, ~/.cache no Linux).
	base := os.Getenv("RUNNER_CACHE_DIR")
	if base == "" {
		c, _ := os.UserCacheDir()
		base = filepath.Join(c, "runner-codes")
	}
	return filepath.Join(base, "machines", name)
}

// TestMachineNoNetwork: sem --net a máquina não tem eth0 e nada sai, como sempre foi.
func TestMachineNoNetwork(t *testing.T) {
	name := uniqueName("nonet")
	defer rcMachine(t, "delete", "--name", name, "--force")
	mustMachine(t, "create", "--name", name, "--stack", "python")
	mustMachine(t, "start", "--name", name)
	out, _, _ := rcMachine(t, "exec", "--name", name, "--", "sh", "-c", "ls /sys/class/net; timeout 5 wget -qO- -T 3 http://1.1.1.1 >/dev/null 2>&1; echo rc=$?")
	if strings.Contains(out, "eth0") || !strings.Contains(out, "rc=1") {
		t.Fatalf("a machine without --net must have no NIC and no egress: %q", out)
	}
	mustMachine(t, "stop", "--name", name)
}

// TestMachineNetwork: com --net a eth0 sobe com o IP do enlace, o DNS é o gateway, a
// internet responde, e a metadata de nuvem é recusada na hora mesmo com a rede aberta.
func TestMachineNetwork(t *testing.T) {
	needInternet(t)
	name := uniqueName("net")
	defer rcMachine(t, "delete", "--name", name, "--force")
	mustMachine(t, "create", "--name", name, "--stack", "python", "--net")
	mustMachine(t, "start", "--name", name)
	out := mustMachine(t, "exec", "--name", name, "--", "sh", "-c",
		"ip -4 addr show eth0 | grep -o 'inet [0-9./]*'; grep nameserver /etc/resolv.conf; timeout 20 wget -qO- -T 15 http://example.com | grep -o '<title>[^<]*'")
	if !strings.Contains(out, "inet 100.96.0.2/30") || !strings.Contains(out, "nameserver 100.96.0.1") || !strings.Contains(out, "<title>Example Domain") {
		t.Fatalf("network: %q", out)
	}
	start := time.Now()
	out, _, _ = rcMachine(t, "exec", "--name", name, "--", "sh", "-c", "timeout 10 wget -qO- -T 8 http://169.254.169.254/ 2>&1; echo rc=$?")
	if !strings.Contains(out, "rc=1") || time.Since(start) > 5*time.Second {
		t.Fatalf("metadata must be refused fast: %q in %v", out, time.Since(start))
	}
	mustMachine(t, "stop", "--name", name)
}

// TestMachineEgressPolicy: com --allow-host só o nome (e subdomínios) resolve; o resto
// recebe NXDOMAIN na hora e vai para o log de recusas; o update com a máquina ligada libera
// outro nome na consulta seguinte, e o remove o fecha de novo.
func TestMachineEgressPolicy(t *testing.T) {
	needInternet(t)
	name := uniqueName("pol")
	defer rcMachine(t, "delete", "--name", name, "--force")
	mustMachine(t, "create", "--name", name, "--stack", "python", "--allow-host", "example.com")
	mustMachine(t, "start", "--name", name)
	fetch := func(host string) string {
		out, _, _ := rcMachine(t, "exec", "--name", name, "--", "sh", "-c", "timeout 20 wget -qO /dev/null -T 15 http://"+host+" 2>&1; echo rc=$?")
		return out
	}
	if out := fetch("example.com"); !strings.Contains(out, "rc=0") {
		t.Fatalf("allowed host: %q", out)
	}
	start := time.Now()
	if out := fetch("iana.org"); !strings.Contains(out, "bad address") || time.Since(start) > 5*time.Second {
		t.Fatalf("blocked host must fail fast with bad address: %q in %v", out, time.Since(start))
	}
	log, _ := os.ReadFile(filepath.Join(machineDir(t, name), "egress-denials.log"))
	if !strings.Contains(string(log), "deny dns iana.org") {
		t.Fatalf("denials log: %q", log)
	}
	mustMachine(t, "update", "--name", name, "--allow-host", "iana.org")
	if out := fetch("iana.org"); !strings.Contains(out, "rc=0") {
		t.Fatalf("live update must open the host: %q", out)
	}
	mustMachine(t, "update", "--name", name, "--remove-allow-host", "iana.org")
	if out := fetch("iana.org"); !strings.Contains(out, "bad address") {
		t.Fatalf("live removal must close it again: %q", out)
	}
	mustMachine(t, "stop", "--name", name)
}

// TestMachineEgressCIDROnly trava o furo fechado em 19/10/2026: com allow_cidrs e sem lista
// de hosts, o DNS resolve qualquer nome, e o IP "aprendido" não pode abrir a conexão — antes,
// allow_cidrs = ["127.0.0.1/32"] alcançava example.com e o S3 da AWS. O CIDR liberado
// continua alcançável.
func TestMachineEgressCIDROnly(t *testing.T) {
	needInternet(t)
	name := uniqueName("cidr")
	defer rcMachine(t, "delete", "--name", name, "--force")
	mustMachine(t, "create", "--name", name, "--stack", "python", "--net", "--allow-cidr", "1.1.1.1/32")
	mustMachine(t, "start", "--name", name)
	connect := func(host string) string {
		out, _, _ := rcMachine(t, "exec", "--name", name, "--", "python3", "-c",
			"import socket\ntry:\n socket.create_connection(('"+host+"', 80), 5); print('open')\nexcept OSError as e: print('closed', e)")
		return out
	}
	if out := connect("1.1.1.1"); !strings.Contains(out, "open") {
		t.Fatalf("the allowed CIDR must connect: %q", out)
	}
	if out := connect("example.com"); !strings.Contains(out, "closed") {
		t.Fatalf("a name outside the allowed CIDRs must not connect: %q", out)
	}
	mustMachine(t, "stop", "--name", name)
}

// TestMachinePortsAndVolumes: um runner.code com volume (rw e ro) e porta publicada,
// sem rede de saída: o workload serve do volume pela porta, o host lê pela porta, o que o
// guest grava no volume aparece no host, o :ro recusa escrita, e o guest não disca para fora.
func TestMachinePortsAndVolumes(t *testing.T) {
	name := uniqueName("ports")
	dir := t.TempDir()
	www, ro := filepath.Join(dir, "www"), filepath.Join(dir, "ro")
	os.MkdirAll(www, 0o755)
	os.MkdirAll(ro, 0o755)
	os.WriteFile(filepath.Join(www, "index.html"), []byte("served from the host volume\n"), 0o644)
	port := freeHostPort(t)
	os.WriteFile(filepath.Join(dir, "runner.code"), []byte(`
stack = "python"
volumes = ["./www:/var/www", "./ro:/ro:ro"]
ports = ["`+strconv.Itoa(port)+`:8000"]
cmd = ["python3", "-m", "http.server", "8000", "--directory", "/var/www"]
`), 0o600)
	defer rcMachine(t, "delete", "--name", name, "--force")
	mustMachine(t, "create", "-f", filepath.Join(dir, "runner.code"), "--name", name)
	mustMachine(t, "start", "--name", name)
	var body []byte
	waitFor(t, "the workload to serve on the published port", 15*time.Second, func() bool {
		resp, err := http.Get("http://127.0.0.1:" + strconv.Itoa(port) + "/index.html")
		if err != nil {
			return false
		}
		defer resp.Body.Close()
		body, _ = io.ReadAll(resp.Body)
		return resp.StatusCode == 200
	})
	if string(body) != "served from the host volume\n" {
		t.Fatalf("body: %q", body)
	}
	out, _, _ := rcMachine(t, "exec", "--name", name, "--", "sh", "-c",
		"echo from-guest > /var/www/out.txt; echo x > /ro/y 2>&1; echo ro=$?; mount -o remount,rw /ro 2>&1; echo z > /ro/z 2>&1; echo remount=$?; timeout 5 wget -qO- -T 3 http://1.1.1.1 >/dev/null 2>&1; echo net=$?")
	// O remount,rw pelo root do guest é o ataque: o :ro tem de valer no device (libkrun),
	// não só na opção de montagem que o próprio guest controla.
	if !strings.Contains(out, "ro=1") || !strings.Contains(out, "remount=1") || !strings.Contains(out, "net=1") {
		t.Fatalf("ro must refuse writes and ports-only must not dial out: %q", out)
	}
	if b, _ := os.ReadFile(filepath.Join(www, "out.txt")); string(b) != "from-guest\n" {
		t.Fatalf("guest write must reach the host: %q", b)
	}
	if _, err := os.Stat(filepath.Join(ro, "z")); !os.IsNotExist(err) {
		t.Fatal("a guest remount must not open the read-only volume")
	}
	if _, err := os.Stat(filepath.Join(ro, "y")); !os.IsNotExist(err) {
		t.Fatal("nothing may appear in the read-only volume")
	}
	mustMachine(t, "stop", "--name", name)
	mustMachine(t, "start", "--name", name)
	if out := mustMachine(t, "exec", "--name", name, "--", "cat", "/var/www/out.txt"); out != "from-guest\n" {
		t.Fatalf("volumes must be mounted again on every start: %q", out)
	}
	mustMachine(t, "stop", "--name", name)
}

// TestMachinePing: o ping do guest (ICMP) passa pelo gateway sob a mesma política do TCP —
// com a internet aberta ele volta, e com uma allowlist que não inclui o destino ele não volta
// e fica no log de negações. Antes não havia ICMP nenhum.
func TestMachinePing(t *testing.T) {
	needInternet(t)
	open, closed := uniqueName("ping"), uniqueName("pingdeny")
	defer rcMachine(t, "delete", "--name", open, "--force")
	defer rcMachine(t, "delete", "--name", closed, "--force")
	mustMachine(t, "create", "--name", open, "--stack", "python", "--net")
	mustMachine(t, "create", "--name", closed, "--stack", "python", "--allow-cidr", "198.51.100.0/24")
	mustMachine(t, "start", "--name", open)
	mustMachine(t, "start", "--name", closed)
	ping := "ping -c 2 -W 3 1.1.1.1 >/dev/null 2>&1; echo rc=$?"
	if out, _, _ := rcMachine(t, "exec", "--name", open, "--", "sh", "-c", ping); !strings.Contains(out, "rc=0") {
		t.Fatalf("ping with the internet open: %q", out)
	}
	if out, _, _ := rcMachine(t, "exec", "--name", closed, "--", "sh", "-c", ping); strings.Contains(out, "rc=0") {
		t.Fatalf("ping outside the allowlist must not come back: %q", out)
	}
	if b, _ := os.ReadFile(filepath.Join(machineDir(t, closed), "egress-denials.log")); !strings.Contains(string(b), "deny icmp 1.1.1.1") {
		t.Fatalf("the denied ping is audited: %q", string(b))
	}
	mustMachine(t, "stop", "--name", open)
	mustMachine(t, "stop", "--name", closed)
}
