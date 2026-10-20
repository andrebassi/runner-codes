//go:build e2e

package e2e

import (
	"io"
	"net"
	"path/filepath"
	"strings"
	"testing"
	"time"
)

// TestDockerSocketAndDiskVolume trava as duas peças do docs/docker-in-machine sem precisar do
// Docker (que é lento de instalar):
//   - docker_socket: um servidor no /var/run/docker.sock do guest responde a quem conecta no
//     docker.sock do diretório da máquina, no host;
//   - volume de disco (-v /var/lib/x): ext4 de verdade, fora do overlay, e persiste.
func TestDockerSocketAndDiskVolume(t *testing.T) {
	name := uniqueName("sock")
	defer rcMachine(t, "delete", "--name", name, "--force")
	mustMachine(t, "create", "--name", name, "--stack", "python", "--docker-socket", "-v", "/var/lib/dados",
		"--", "python3", "-c", `
import os, socket
os.makedirs("/var/run", exist_ok=True)
s = socket.socket(socket.AF_UNIX); s.bind("/var/run/docker.sock"); s.listen()
while True:
    c, _ = s.accept(); c.sendall(b"do-guest:" + c.recv(64)); c.close()`)
	mustMachine(t, "start", "--name", name)
	dir := strings.TrimSpace(mustMachine(t, "data-dir", "--name", name))
	var got string
	waitFor(t, "the guest socket server through the host docker.sock", 20*time.Second, func() bool {
		c, err := net.DialTimeout("unix", filepath.Join(dir, "docker.sock"), time.Second)
		if err != nil {
			return false
		}
		defer c.Close()
		c.Write([]byte("ping"))
		c.(*net.UnixConn).CloseWrite()
		b, _ := io.ReadAll(c)
		got = string(b)
		return got != ""
	})
	if got != "do-guest:ping" {
		t.Fatalf("bridge: %q", got)
	}
	if out := mustMachine(t, "exec", "--name", name, "--", "sh", "-c", "stat -f -c %T /var/lib/dados; echo fica > /var/lib/dados/marca"); out != "ext2/ext3\n" {
		t.Fatalf("disk volume must be ext4, not the overlay: %q", out)
	}
	mustMachine(t, "stop", "--name", name)
	mustMachine(t, "start", "--name", name)
	if out := mustMachine(t, "exec", "--name", name, "--", "cat", "/var/lib/dados/marca"); out != "fica\n" {
		t.Fatalf("disk volume must persist: %q", out)
	}
	mustMachine(t, "stop", "--name", name)
}

// TestExposeAndMountSockets: as pontes de socket, nas duas direções,
// numa VM de verdade:
//   - --expose-socket GUEST: o workload escuta um socket e o host conecta no do diretório da
//     máquina (e num HOST escolhido);
//   - --mount-socket HOST:GUEST: um servidor do host aparece dentro da máquina, num diretório
//     que a imagem não tinha.
func TestExposeAndMountSockets(t *testing.T) {
	name := uniqueName("bridge")
	defer rcMachine(t, "delete", "--name", name, "--force")
	tmp := t.TempDir()
	hostSock := filepath.Join(tmp, "h.sock")
	l, err := net.Listen("unix", hostSock)
	if err != nil {
		t.Fatal(err)
	}
	defer l.Close()
	go func() {
		for {
			c, err := l.Accept()
			if err != nil {
				return
			}
			b := make([]byte, 64)
			n, _ := c.Read(b)
			c.Write(append([]byte("do-host:"), b[:n]...))
			c.Close()
		}
	}()
	chosen := filepath.Join(tmp, "escolhido.sock")
	mustMachine(t, "create", "--name", name, "--stack", "python",
		"--expose-socket", "/run/app.sock", "--expose-socket", "/run/b.sock:"+chosen,
		"--mount-socket", hostSock+":/run/novo/h.sock",
		"--", "python3", "-c", `
import socket, threading
def serve(path, tag):
    s = socket.socket(socket.AF_UNIX); s.bind(path); s.listen()
    while True:
        c, _ = s.accept(); c.sendall(tag + c.recv(64)); c.close()
threading.Thread(target=serve, args=("/run/b.sock", b"b:"), daemon=True).start()
serve("/run/app.sock", b"do-guest:")`)
	mustMachine(t, "start", "--name", name)
	dir := strings.TrimSpace(mustMachine(t, "data-dir", "--name", name))
	for path, want := range map[string]string{filepath.Join(dir, "app.sock"): "do-guest:ping", chosen: "b:ping"} {
		var got string
		waitFor(t, "the guest server at "+path, 20*time.Second, func() bool {
			c, err := net.DialTimeout("unix", path, time.Second)
			if err != nil {
				return false
			}
			defer c.Close()
			c.Write([]byte("ping"))
			b, _ := io.ReadAll(c)
			got = string(b)
			return got != ""
		})
		if got != want {
			t.Fatalf("expose %s: %q", path, got)
		}
	}
	out := mustMachine(t, "exec", "--name", name, "--", "python3", "-c", `
import socket
s = socket.socket(socket.AF_UNIX); s.connect("/run/novo/h.sock"); s.sendall(b"pong"); print(s.recv(64).decode())`)
	if out != "do-host:pong\n" {
		t.Fatalf("mount socket: %q", out)
	}
	mustMachine(t, "stop", "--name", name)
}
