package guest

import (
	"net"
	"os"
	"path/filepath"
	"strings"
	"syscall"
	"testing"
	"time"

	"runner.codes/internal/protocol"
)

// procEnv é o PATH mínimo dos testes: o processo do container só vê o Env da spec.
var procEnv = []string{"PATH=/usr/bin:/bin"}

// procClient abre um OpProc pelo fio, como o shim faz, e devolve o lado do host.
func procClient(t *testing.T, job Job) net.Conn {
	t.Helper()
	c, srv := net.Pipe()
	go NewServer(DefaultServerConfig()).handleConnection(srv)
	c.SetDeadline(time.Now().Add(15 * time.Second))
	job.Op = protocol.OpProc
	if job.Env == nil {
		job.Env = procEnv
	}
	if err := protocol.WriteFrame(c, job); err != nil {
		t.Fatal(err)
	}
	return c
}

// procCollect lê até o exit e devolve stdout, stderr, a saída do TTY e o código.
func procCollect(t *testing.T, c net.Conn) (stdout, stderr, tty string, code int) {
	t.Helper()
	var o, e, y strings.Builder
	for {
		var ev protocol.Event
		if err := protocol.ReadFrame(c, &ev); err != nil {
			t.Fatalf("read: %v (stdout %q stderr %q)", err, o.String(), e.String())
		}
		switch ev.Type {
		case protocol.EventStdout:
			o.Write(ev.Bytes)
		case protocol.EventStderr:
			e.Write(ev.Bytes)
		case protocol.EventOutput:
			y.Write(ev.Bytes)
		case protocol.EventExit:
			return o.String(), e.String(), y.String(), ev.Code
		}
	}
}

// TestProcStdio: stdout e stderr chegam separados (o kubectl logs distingue), o stdin chega e o eof
// fecha, e o código de saída volta.
func TestProcStdio(t *testing.T) {
	c := procClient(t, Job{Argv: []string{"sh", "-c", `echo out; echo err >&2; cat; exit 3`}})
	defer c.Close()
	protocol.WriteFrame(c, protocol.Event{Type: protocol.EventInput, Bytes: []byte("from-stdin\n")})
	protocol.WriteFrame(c, protocol.Event{Type: protocol.EventEOF})
	out, errOut, _, code := procCollect(t, c)
	if code != 3 || out != "out\nfrom-stdin\n" || errOut != "err\n" {
		t.Fatalf("code=%d out=%q err=%q", code, out, errOut)
	}
}

// TestProcEnvIsolated: o processo do container não herda o ambiente do guest, só o da spec.
func TestProcEnvIsolated(t *testing.T) {
	t.Setenv("RC_GUEST_ONLY", "leak")
	c := procClient(t, Job{Argv: []string{"sh", "-c", `echo "[$RC_GUEST_ONLY][$FOO]"`}, Env: append([]string{"FOO=bar"}, procEnv...)})
	defer c.Close()
	out, _, _, _ := procCollect(t, c)
	if out != "[][bar]\n" {
		t.Fatalf("ambiente vazou ou faltou: %q", out)
	}
}

// TestProcSignal: o Kill do containerd chega como sinal, e o código é 128+sinal.
func TestProcSignal(t *testing.T) {
	c := procClient(t, Job{Argv: []string{"sleep", "30"}})
	defer c.Close()
	time.Sleep(200 * time.Millisecond)
	protocol.WriteFrame(c, protocol.Event{Type: protocol.EventSignal, Code: int(syscall.SIGTERM)})
	_, _, _, code := procCollect(t, c)
	if code != 128+int(syscall.SIGTERM) {
		t.Fatalf("code=%d, esperava 143", code)
	}
}

// TestProcTTY: com TTY o programa vê um terminal do tamanho pedido, o resize muda o tamanho e toda
// a saída vem como output.
func TestProcTTY(t *testing.T) {
	c := procClient(t, Job{Argv: []string{"sh", "-c", `test -t 1 && echo IS-TTY; stty size; read l; stty size`}, TTY: true, Rows: 30, Cols: 100})
	defer c.Close()
	time.Sleep(300 * time.Millisecond)
	protocol.WriteFrame(c, protocol.Event{Type: protocol.EventResize, Rows: 40, Cols: 120})
	protocol.WriteFrame(c, protocol.Event{Type: protocol.EventEOF}) // ignorado com TTY
	protocol.WriteFrame(c, protocol.Event{Type: protocol.EventInput, Bytes: []byte("x\n")})
	_, _, tty, code := procCollect(t, c)
	if code != 0 || !strings.Contains(tty, "IS-TTY") || !strings.Contains(tty, "30 100") || !strings.Contains(tty, "40 120") {
		t.Fatalf("code=%d tty=%q", code, tty)
	}
}

// TestProcTTYDefaultSize: sem tamanho, o TTY nasce 24x80.
func TestProcTTYDefaultSize(t *testing.T) {
	c := procClient(t, Job{Argv: []string{"stty", "size"}, TTY: true})
	defer c.Close()
	_, _, tty, _ := procCollect(t, c)
	if !strings.Contains(tty, "24 80") {
		t.Fatalf("tty=%q", tty)
	}
}

// TestProcFailures: argv vazio e programa que não existe saem com 127 e o motivo no stderr, nos dois
// modos (o pod fica em erro com mensagem, em vez de pendurado).
func TestProcFailures(t *testing.T) {
	for _, job := range []Job{{}, {Argv: []string{"/no/such/binary"}}, {Argv: []string{"/no/such/binary"}, TTY: true}} {
		c := procClient(t, job)
		_, errOut, _, code := procCollect(t, c)
		c.Close()
		if code != 127 || errOut == "" {
			t.Fatalf("%+v: code=%d err=%q", job.Argv, code, errOut)
		}
	}
}

// TestProcHostGone: se o host fecha a conexão, o processo morre em vez de ficar órfão na VM.
func TestProcHostGone(t *testing.T) {
	marker := filepath.Join(t.TempDir(), "alive")
	c := procClient(t, Job{Argv: []string{"sh", "-c", "sleep 2; touch " + marker}})
	time.Sleep(200 * time.Millisecond)
	c.Close()
	time.Sleep(2500 * time.Millisecond)
	if _, err := os.Stat(marker); err == nil {
		t.Fatal("o processo sobreviveu ao host")
	}
}

// TestResolveInRoot: o argv[0] sem barra é achado pelo PATH do Env dentro do root (link absoluto
// conta, como o /bin/sh -> /bin/busybox); com barra vai como veio; sem achar, volta o nome.
func TestResolveInRoot(t *testing.T) {
	root := t.TempDir()
	os.MkdirAll(filepath.Join(root, "opt/bin"), 0o755)
	os.Symlink("/no/where/busybox", filepath.Join(root, "opt/bin/sh"))
	if got := resolveInRoot(root, "sh", []string{"PATH=::/opt/bin"}); got != "/opt/bin/sh" {
		t.Fatalf("got %q", got)
	}
	if got := resolveInRoot(root, "./x", nil); got != "./x" {
		t.Fatalf("got %q", got)
	}
	if got := resolveInRoot(root, "nada", nil); got != "nada" {
		t.Fatalf("got %q", got)
	}
}

// TestProcCommandUser: usuário da spec vira Credential; root sem grupos não mexe (o padrão do guest).
func TestProcCommandUser(t *testing.T) {
	cmd := procCommand(Job{Argv: []string{"/bin/true"}, Root: "/r", UID: 1000, GID: 1000, Groups: []uint32{10}})
	if cmd.SysProcAttr.Chroot != "/r" || cmd.SysProcAttr.Credential.Uid != 1000 || cmd.Dir != "/" {
		t.Fatalf("%+v", cmd.SysProcAttr)
	}
	if procCommand(Job{Argv: []string{"/bin/true"}, Cwd: "/w"}).SysProcAttr.Credential != nil {
		t.Fatal("root sem grupos não leva Credential")
	}
}

// TestExitCode: erro que não é de saída vira 1.
func TestExitCode(t *testing.T) {
	if exitCode(os.ErrNotExist) != 1 || exitCode(nil) != 0 {
		t.Fatal("exitCode")
	}
}
