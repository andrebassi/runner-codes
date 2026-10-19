package guest

import (
	"errors"
	"net"
	"os"
	"os/exec"
	"sync"
	"time"

	"github.com/creack/pty"

	"runner.code/internal/protocol"
)

// defaultShell é o comando quando a sessão não diz qual: bash se a imagem tiver (em
// /usr/local/bin nas imagens oficiais), senão o sh. -i e não -l: o login shell do Alpine
// relê /etc/profile e troca o PATH, perdendo o da imagem (JAVA_HOME, /usr/local/go/bin...).
var defaultShell = []string{"/bin/sh", "-c", `if b=$(command -v bash); then exec "$b" -i; fi; exec /bin/sh -i`}

// ptyDrain é quanto se espera, depois de o programa sair, pela saída que ainda está no PTY;
// sem esperar, a última linha (o prompt ou um "exit") podia se perder.
const ptyDrain = time.Second

// ptySession roda o Argv num PTY e liga o terminal à conexão: frames `input` e `resize`
// chegam do host, `output` sai com o que o terminal escreve, e `exit` fecha com o código.
// O PTY dá ao programa um terminal de verdade: Ctrl+C vira SIGINT no grupo em primeiro
// plano, e o programa sabe o tamanho da janela. Se o host some, o programa é morto.
func ptySession(conn net.Conn, job Job) {
	// O handleConnection põe prazo de leitura de 5 min; uma sessão interativa dura o que
	// o usuário quiser.
	conn.SetReadDeadline(time.Time{})
	var mu sync.Mutex
	send := func(ev protocol.Event) error {
		mu.Lock()
		defer mu.Unlock()
		return protocol.WriteFrame(conn, ev)
	}
	argv := job.Argv
	if len(argv) == 0 {
		argv = defaultShell
	}
	cmd := exec.Command(argv[0], argv[1:]...)
	cmd.Env = append(os.Environ(), job.Env...)
	cmd.Dir = job.Cwd
	if cmd.Dir == "" {
		cmd.Dir = "/"
		if home := os.Getenv("HOME"); home != "" {
			cmd.Dir = home
		}
	}
	size := &pty.Winsize{Rows: job.Rows, Cols: job.Cols}
	if size.Rows == 0 || size.Cols == 0 {
		size.Rows, size.Cols = 24, 80
	}
	ptmx, err := pty.StartWithSize(cmd, size)
	if err != nil {
		send(protocol.Event{Type: protocol.EventOutput, Bytes: []byte(err.Error() + "\r\n")})
		send(protocol.Event{Type: protocol.EventExit, Code: 127})
		return
	}
	defer ptmx.Close()

	outDone := make(chan struct{})
	go func() {
		defer close(outDone)
		buf := make([]byte, 32<<10)
		for {
			n, err := ptmx.Read(buf)
			if n > 0 {
				if send(protocol.Event{Type: protocol.EventOutput, Bytes: append([]byte(nil), buf[:n]...)}) != nil {
					return
				}
			}
			if err != nil {
				return
			}
		}
	}()
	go func() {
		for {
			var ev protocol.Event
			if err := protocol.ReadFrame(conn, &ev); err != nil {
				// Host fechou (terminal fechado, rc morto): ninguém mais lê, então o programa
				// morre em vez de ficar órfão na máquina.
				cmd.Process.Kill()
				return
			}
			switch ev.Type {
			case protocol.EventInput:
				ptmx.Write(ev.Bytes)
			case protocol.EventResize:
				if ev.Rows > 0 && ev.Cols > 0 {
					pty.Setsize(ptmx, &pty.Winsize{Rows: ev.Rows, Cols: ev.Cols})
				}
			}
		}
	}()

	code := 0
	var ee *exec.ExitError
	if err := cmd.Wait(); errors.As(err, &ee) {
		code = ee.ExitCode()
		if code < 0 {
			code = 128 + 9 // morto por sinal (o Kill acima): o 137 de costume
		}
	} else if err != nil {
		code = 1
	}
	select {
	case <-outDone:
	case <-time.After(ptyDrain):
	}
	send(protocol.Event{Type: protocol.EventExit, Code: code})
}
