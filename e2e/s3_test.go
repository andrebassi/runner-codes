//go:build e2e

package e2e

import (
	"context"
	"crypto/md5"
	"encoding/hex"
	"net"
	"os"
	"os/exec"
	"path/filepath"
	"strconv"
	"strings"
	"testing"
	"time"
)

// s3Server sobe um S3 local (rclone serve s3, via nix shell: o MinIO saiu do nixpkgs em
// 11/10/2026) servindo root, com a chave dada, e devolve a porta. Cada pasta de root é um
// bucket. O processo morre no fim do teste.
func s3Server(t *testing.T, root string) int {
	t.Helper()
	if _, err := exec.LookPath("nix"); err != nil {
		t.Skip("nix is needed for the local S3 server")
	}
	port := freeHostPort(t)
	ctx, cancel := context.WithCancel(context.Background())
	cmd := exec.CommandContext(ctx, "nix", "shell", "nixpkgs#rclone", "-c", "rclone", "serve", "s3", root,
		"--addr", "127.0.0.1:"+strconv.Itoa(port), "--auth-key", "e2ekey,e2esecret123")
	cmd.Stdout, cmd.Stderr = os.Stderr, os.Stderr
	if err := cmd.Start(); err != nil {
		t.Fatal(err)
	}
	t.Cleanup(func() { cancel(); cmd.Wait() })
	waitFor(t, "the local S3 server", 120*time.Second, func() bool {
		c, err := net.DialTimeout("tcp", "127.0.0.1:"+strconv.Itoa(port), time.Second)
		if err == nil {
			c.Close()
		}
		return err == nil
	})
	return port
}

// TestMachineS3Volume trava o volume s3:// numa VM real, contra um servidor S3 de verdade:
//   - o guest monta por FUSE e lê o que já está no bucket;
//   - o que o guest grava (shell, Python e 8 MiB binários) chega ao bucket igual, byte a byte;
//   - mkdir + arquivo dentro, mv (CopyObject + Delete) e rm refletem no bucket;
//   - o :ro recusa escrita; o volume volta a montar depois de stop/start;
//   - credencial errada falha o start com o motivo do S3, não um EIO no primeiro ls.
func TestMachineS3Volume(t *testing.T) {
	root := t.TempDir()
	data := filepath.Join(root, "e2ebkt", "data")
	os.MkdirAll(data, 0o755)
	os.WriteFile(filepath.Join(data, "hello.txt"), []byte("ola do bucket\n"), 0o644)
	port := s3Server(t, root)
	name, bad := uniqueName("s3"), uniqueName("s3bad")
	defer rcMachine(t, "delete", "--name", name, "--force")
	defer rcMachine(t, "delete", "--name", bad, "--force")
	endpoint := "AWS_ENDPOINT_URL=http://100.96.0.1:" + strconv.Itoa(port)
	// O guest alcança o S3 do host pelo gateway (100.96.0.1 vira o loopback do host), que só
	// abre com o loopback em allow_cidrs.
	mustMachine(t, "create", "--name", name, "--stack", "python", "--net", "--allow-cidr", "127.0.0.1/32",
		"-e", "AWS_ACCESS_KEY_ID=e2ekey", "-e", "AWS_SECRET_ACCESS_KEY=e2esecret123", "-e", endpoint,
		"-v", "s3://e2ebkt/data:/data", "-v", "s3://e2ebkt:/ro:ro")
	mustMachine(t, "start", "--name", name)
	sh := func(script string) string {
		out, errOut, _ := rcMachine(t, "exec", "--name", name, "--", "sh", "-c", script)
		return out + errOut
	}
	if out := sh("cat /data/hello.txt; mount | grep -c 'fuse.s3'"); out != "ola do bucket\n2\n" {
		t.Fatalf("read and two fuse mounts: %q", out)
	}
	out := sh(`echo pelo-shell > /data/shell.txt
python3 -c "open('/data/py.txt','w').write('pelo python\n')"
dd if=/dev/urandom of=/data/big.bin bs=1M count=8 2>/dev/null; md5sum /data/big.bin | cut -d' ' -f1
mkdir /data/sub && echo dentro > /data/sub/x.txt
echo mover > /data/a.txt && mv /data/a.txt /data/b.txt
echo apagar > /data/gone.txt && rm /data/gone.txt
(echo x > /ro/y) 2>/dev/null; echo ro=$?`)
	lines := strings.Split(strings.TrimSpace(out), "\n")
	if len(lines) != 2 || lines[1] != "ro=1" {
		t.Fatalf("guest script: %q", out)
	}
	read := func(rel string) string { b, _ := os.ReadFile(filepath.Join(data, rel)); return string(b) }
	big, _ := os.ReadFile(filepath.Join(data, "big.bin"))
	sum := md5.Sum(big)
	if hex.EncodeToString(sum[:]) != lines[0] || len(big) != 8<<20 {
		t.Fatalf("8 MiB must reach the bucket byte for byte: %d bytes", len(big))
	}
	if read("shell.txt") != "pelo-shell\n" || read("py.txt") != "pelo python\n" || read("sub/x.txt") != "dentro\n" || read("b.txt") != "mover\n" {
		t.Fatal("writes must reach the bucket")
	}
	for _, gone := range []string{"a.txt", "gone.txt", "ro/y"} {
		if _, err := os.Stat(filepath.Join(data, gone)); !os.IsNotExist(err) {
			t.Fatalf("%s must not exist in the bucket", gone)
		}
	}
	mustMachine(t, "stop", "--name", name)
	mustMachine(t, "start", "--name", name)
	if out := sh("cat /data/sub/x.txt /ro/data/b.txt"); out != "dentro\nmover\n" {
		t.Fatalf("the volume must mount again after a restart: %q", out)
	}
	mustMachine(t, "stop", "--name", name)

	mustMachine(t, "create", "--name", bad, "--stack", "python", "--net", "--allow-cidr", "127.0.0.1/32",
		"-e", "AWS_ACCESS_KEY_ID=e2ekey", "-e", "AWS_SECRET_ACCESS_KEY=wrong-secret", "-e", endpoint, "-v", "s3://e2ebkt:/data")
	if _, errOut, rc := rcMachine(t, "start", "--name", bad); rc == 0 || !strings.Contains(errOut, "mount-s3") || !strings.Contains(errOut, "s3: 403") {
		t.Fatalf("bad credentials must fail the start with the S3 reason: rc=%d %q", rc, errOut)
	}
}
