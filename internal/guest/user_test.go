package guest

import (
	"errors"
	"io"
	"net"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync/atomic"
	"testing"

	"runner.code/internal/protocol"
)

// usersFixture aponta passwd e group para arquivos de teste.
func usersFixture(t *testing.T) {
	t.Helper()
	dir := t.TempDir()
	passwdFile, groupFile = filepath.Join(dir, "passwd"), filepath.Join(dir, "group")
	t.Cleanup(func() { passwdFile, groupFile = "/etc/passwd", "/etc/group" })
	os.WriteFile(passwdFile, []byte("# comentário\nroot:x:0:0:root:/root:/bin/sh\n\napp:x:1000:1000:Linux User,,,:/home/app:/bin/sh\nbad:x:nan:1:b:/b:/bin/sh\n"), 0o644)
	os.WriteFile(groupFile, []byte("root:x:0:root\napp:x:1000:\nwheel:x:10:root,app\ndocker:x:998:app\nshort:x\n"), 0o644)
}

// TestLookupUser: nome do passwd (com os grupos suplementares), uid numérico com e sem gid
// (o nome e o HOME vêm do passwd quando o uid existe), uid que não está no passwd, vazio é
// root sem mudança, e as recusas com o motivo.
func TestLookupUser(t *testing.T) {
	usersFixture(t)
	u, err := lookupUser("app")
	if err != nil || u.uid != 1000 || u.gid != 1000 || u.home != "/home/app" || len(u.groups) != 2 || u.groups[0] != 10 || u.groups[1] != 998 {
		t.Fatalf("by name: %+v %v", u, err)
	}
	if u, _ := lookupUser("1000"); u.name != "app" || u.gid != 1000 || u.home != "/home/app" {
		t.Fatalf("numeric known uid: %+v", u)
	}
	if u, _ := lookupUser("1000:20"); u.gid != 20 {
		t.Fatalf("uid:gid: %+v", u)
	}
	if u, _ := lookupUser("501:20"); u.uid != 501 || u.gid != 20 || u.home != "/" || u.name != "501:20" {
		t.Fatalf("host uid not in passwd (a mounted host directory's owner): %+v", u)
	}
	if u, err := lookupUser(""); u != nil || err != nil {
		t.Fatal("empty is root, unchanged")
	}
	if _, err := lookupUser("ghost"); err == nil || !strings.Contains(err.Error(), "adduser") {
		t.Fatalf("unknown user: %v", err)
	}
	if _, err := lookupUser("bad"); err == nil || !strings.Contains(err.Error(), "malformed") {
		t.Fatalf("malformed entry: %v", err)
	}
	for _, s := range []string{"1:x", "x:1"} {
		if _, _, ok := parseNumeric(s); ok {
			t.Fatalf("%q is not numeric", s)
		}
	}
	passwdFile = filepath.Join(t.TempDir(), "missing")
	if u, err := lookupUser("0"); err != nil || u.uid != 0 {
		t.Fatalf("no passwd file still takes a numeric uid: %+v %v", u, err)
	}
	passwdFile = t.TempDir() // diretório: erro de leitura que não é "não existe"
	if _, err := lookupUser("app"); err == nil {
		t.Fatal("an unreadable passwd must fail")
	}
}

// TestSecretEnv: o set-env guarda os segredos e eles entram em todo comando, antes do env do
// pedido (que vence); um novo set-env troca a lista inteira.
func TestSecretEnv(t *testing.T) {
	defer setSecretEnv(nil)
	host, guest := connPair(t)
	s := NewServer(ServerConfig{})
	go s.handleConnection(guest)
	protocol.WriteFrame(host, Job{TraceID: "secrets", Op: protocol.OpSetEnv, Env: []string{"API=v1"}})
	var res protocol.Result
	if err := protocol.ReadFrame(host, &res); err != nil || res.Status != "success" {
		t.Fatalf("set-env: %+v %v", res, err)
	}
	env := strings.Join(environ(nil, []string{"API=do-pedido"}), " ")
	if !strings.Contains(env, "API=v1 API=do-pedido") {
		t.Fatalf("secrets before the request env: %s", env)
	}
	if r := execArgv(Job{Argv: []string{"sh", "-c", "echo $API"}}, nil); r.Stdout != "v1\n" {
		t.Fatalf("a command sees the secret: %q", r.Stdout)
	}
	setSecretEnv([]string{"OTHER=x"})
	if strings.Contains(strings.Join(environ(nil, nil), " "), "API=v1") {
		t.Fatal("a new set-env replaces the list")
	}
}

// TestEnvironAndApply: o env do pedido vence o do usuário; o credential entra sem perder o
// Setpgid do workload; sem usuário nada muda.
func TestEnvironAndApply(t *testing.T) {
	u := &userInfo{name: "app", uid: 1000, gid: 1000, groups: []uint32{10}, home: "/home/app"}
	env := environ(u, []string{"HOME=/custom"})
	if strings.Join(env[len(env)-4:], " ") != "HOME=/home/app USER=app LOGNAME=app HOME=/custom" {
		t.Fatalf("env order: %v", env[len(env)-4:])
	}
	if len(environ(nil, nil)) != len(os.Environ()) {
		t.Fatal("no user, no extra env")
	}
	cmd := exec.Command("true")
	applyUser(cmd, nil)
	if cmd.SysProcAttr != nil {
		t.Fatal("no user, no credential")
	}
	applyUser(cmd, u)
	if c := cmd.SysProcAttr.Credential; c.Uid != 1000 || c.Gid != 1000 || c.Groups[0] != 10 {
		t.Fatalf("credential: %+v", c)
	}
}

// TestSSHAgentRelay: o guest escuta o socket do agente (0666, para o user da máquina) e
// repassa cada conexão à porta vsock do host; um segundo OpSSHAgent troca o listener; host
// que recusa fecha o cliente; caminho impossível falha com o motivo.
func TestSSHAgentRelay(t *testing.T) {
	orig := dialHost
	defer func() { dialHost = orig }()
	// askedPort é escrito pela goroutine do relay e lido pelo teste: atômico.
	var askedPort atomic.Uint32
	dialHost = func(port uint32) (io.ReadWriteCloser, error) {
		askedPort.Store(port)
		if port == 9 {
			return nil, errors.New("refused")
		}
		a, b := net.Pipe()
		go func() {
			buf := make([]byte, 64)
			n, _ := b.Read(buf)
			b.Write(append([]byte("agente:"), buf[:n]...))
			b.Close()
		}()
		return a, nil
	}
	sock := filepath.Join(t.TempDir(), "agent.sock")
	if r := startSSHAgent(Job{Path: sock, Port: 5001}); r.Status != "success" {
		t.Fatalf("start: %+v", r)
	}
	if fi, _ := os.Stat(sock); fi.Mode().Perm() != 0o666 {
		t.Fatalf("socket mode: %v", fi.Mode().Perm())
	}
	if r := startSSHAgent(Job{Path: sock, Port: 5001}); r.Status != "success" {
		t.Fatal("a second start replaces the listener")
	}
	c, err := net.Dial("unix", sock)
	if err != nil {
		t.Fatal(err)
	}
	c.Write([]byte("list"))
	got, _ := io.ReadAll(c)
	if string(got) != "agente:list" || askedPort.Load() != 5001 {
		t.Fatalf("relay: %q to port %d", got, askedPort.Load())
	}
	startSSHAgent(Job{Path: sock, Port: 9})
	r, _ := net.Dial("unix", sock)
	if b, _ := io.ReadAll(r); len(b) != 0 {
		t.Fatal("a refused host port must close the client")
	}
	// Debaixo de um arquivo comum nem o MkdirAll cria o diretório (vale também como root).
	file := filepath.Join(t.TempDir(), "file")
	os.WriteFile(file, nil, 0o600)
	if res := startSSHAgent(Job{Path: filepath.Join(file, "a.sock"), Port: 5001}); res.Status == "success" {
		t.Fatal("an impossible path must fail")
	}
	// Um segundo caminho (--mount-socket) convive com o do agente, no diretório que ainda não
	// existia, e cada um vai para a sua porta.
	other := filepath.Join(t.TempDir(), "run", "podman", "podman.sock")
	startSSHAgent(Job{Path: sock, Port: 5001})
	if r := startSSHAgent(Job{Path: other, Port: 5100}); r.Status != "success" {
		t.Fatalf("second path: %+v", r)
	}
	for path, port := range map[string]uint32{sock: 5001, other: 5100} {
		c, err := net.Dial("unix", path)
		if err != nil {
			t.Fatalf("%s: %v", path, err)
		}
		c.Write([]byte("x"))
		io.ReadAll(c)
		if askedPort.Load() != port {
			t.Fatalf("%s went to port %d, want %d", path, askedPort.Load(), port)
		}
	}
	if _, err := dialHostVsock(1); err == nil {
		t.Fatal("outside the Linux guest there is no vsock")
	}
}
