package guest

import (
	"errors"
	"io"
	"net"
	"os"
	"os/exec"
	"path/filepath"
	"strings"
	"sync"
	"syscall"
	"time"

	"github.com/creack/pty"

	"runner.codes/internal/protocol"
)

// procDrain é quanto se espera, depois de o processo sair, pela saída que ainda está nos pipes;
// sem esperar, a última linha do `kubectl logs` podia se perder.
const procDrain = time.Second

// resolveInRoot acha argv[0] dentro do chroot pelo PATH do Env do processo (o do guest não vale lá
// dentro). Caminho com barra vai como veio. Lstat e não Stat: o /bin/sh das imagens é link absoluto
// (/bin/busybox), que no lado de fora apontaria para o guest e não para o container.
func resolveInRoot(root, name string, env []string) string {
	if strings.Contains(name, "/") {
		return name
	}
	path := "/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin"
	for _, kv := range env {
		if v, ok := strings.CutPrefix(kv, "PATH="); ok {
			path = v
		}
	}
	for _, dir := range filepath.SplitList(path) {
		if dir == "" {
			continue
		}
		if _, err := os.Lstat(filepath.Join(root, dir, name)); err == nil {
			return filepath.Join(dir, name)
		}
	}
	return name
}

// procCommand monta o processo do container: só o Env da spec (o ambiente do guest não vaza para o
// container), chroot no Root, usuário numérico e o cwd já dentro do chroot.
func procCommand(job Job) *exec.Cmd {
	root := job.Root
	cmd := &exec.Cmd{Path: resolveInRoot(root, job.Argv[0], job.Env), Args: job.Argv, Env: job.Env, Dir: job.Cwd}
	if cmd.Dir == "" {
		cmd.Dir = "/"
	}
	cmd.SysProcAttr = &syscall.SysProcAttr{Chroot: root}
	if job.UID != 0 || job.GID != 0 || len(job.Groups) > 0 {
		cmd.SysProcAttr.Credential = &syscall.Credential{Uid: job.UID, Gid: job.GID, Groups: job.Groups}
	}
	return cmd
}

// exitCode traduz o fim do processo no código que o containerd espera: o do programa, ou 128+sinal
// quando morreu por sinal (o 137 do SIGKILL).
func exitCode(err error) int {
	var ee *exec.ExitError
	if errors.As(err, &ee) {
		if st, ok := ee.Sys().(syscall.WaitStatus); ok && st.Signaled() {
			return 128 + int(st.Signal())
		}
		return ee.ExitCode()
	}
	if err != nil {
		return 1
	}
	return 0
}

// procSession roda um processo de container e liga a conexão a ele até o fim: do host chegam
// input (stdin), eof (fecha o stdin), resize (TTY) e signal; do guest saem stdout/stderr (ou só
// output, com TTY) e exit com o código. Se o host some, o processo leva SIGKILL: o shim que o
// acompanhava morreu, e ninguém mais colheria a saída.
func procSession(conn net.Conn, job Job) {
	conn.SetReadDeadline(time.Time{})
	var mu sync.Mutex
	send := func(ev protocol.Event) error {
		mu.Lock()
		defer mu.Unlock()
		return protocol.WriteFrame(conn, ev)
	}
	fail := func(msg string) {
		send(protocol.Event{Type: protocol.EventStderr, Bytes: []byte(msg + "\n")})
		send(protocol.Event{Type: protocol.EventExit, Code: 127})
	}
	if len(job.Argv) == 0 {
		fail("proc: argv vazio")
		return
	}
	cmd := procCommand(job)
	var stdin io.WriteCloser
	var pumps sync.WaitGroup
	pump := func(r io.Reader, typ string) {
		defer pumps.Done()
		buf := make([]byte, 32<<10)
		for {
			n, err := r.Read(buf)
			if n > 0 && send(protocol.Event{Type: typ, Bytes: append([]byte(nil), buf[:n]...)}) != nil {
				return
			}
			if err != nil {
				return
			}
		}
	}
	var resize func(rows, cols uint16)
	if job.TTY {
		size := &pty.Winsize{Rows: job.Rows, Cols: job.Cols}
		if size.Rows == 0 || size.Cols == 0 {
			size.Rows, size.Cols = 24, 80
		}
		// Setsid e Setctty fazem do TTY o terminal de controle: Ctrl+C vira SIGINT no programa.
		cmd.SysProcAttr.Setsid, cmd.SysProcAttr.Setctty = true, true
		ptmx, err := pty.StartWithAttrs(cmd, size, cmd.SysProcAttr)
		if err != nil {
			fail(err.Error())
			return
		}
		defer ptmx.Close()
		stdin = ptmx
		resize = func(rows, cols uint16) { pty.Setsize(ptmx, &pty.Winsize{Rows: rows, Cols: cols}) }
		pumps.Add(1)
		go pump(ptmx, protocol.EventOutput)
	} else {
		// os.Pipe e não StdoutPipe: o Wait do exec fecha os pipes dele e cortaria a saída que ainda
		// está sendo lida; com *os.File o Wait não toca neles, e o pump lê até o EOF.
		inR, inW, _ := os.Pipe()
		outR, outW, _ := os.Pipe()
		errR, errW, _ := os.Pipe()
		cmd.Stdin, cmd.Stdout, cmd.Stderr = inR, outW, errW
		err := cmd.Start()
		inR.Close()
		outW.Close()
		errW.Close()
		if err != nil {
			inW.Close()
			outR.Close()
			errR.Close()
			fail(err.Error())
			return
		}
		defer outR.Close()
		defer errR.Close()
		defer inW.Close()
		stdin = inW
		pumps.Add(2)
		go pump(outR, protocol.EventStdout)
		go pump(errR, protocol.EventStderr)
	}
	go func() {
		for {
			var ev protocol.Event
			if err := protocol.ReadFrame(conn, &ev); err != nil {
				cmd.Process.Kill()
				return
			}
			switch ev.Type {
			case protocol.EventInput:
				stdin.Write(ev.Bytes)
			case protocol.EventEOF:
				if !job.TTY {
					stdin.Close()
				}
			case protocol.EventResize:
				if resize != nil && ev.Rows > 0 && ev.Cols > 0 {
					resize(ev.Rows, ev.Cols)
				}
			case protocol.EventSignal:
				cmd.Process.Signal(syscall.Signal(ev.Code))
			}
		}
	}()
	err := cmd.Wait()
	drained := make(chan struct{})
	go func() { pumps.Wait(); close(drained) }()
	select {
	case <-drained:
	case <-time.After(procDrain):
	}
	send(protocol.Event{Type: protocol.EventExit, Code: exitCode(err)})
}
