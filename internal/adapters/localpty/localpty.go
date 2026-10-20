// Package localpty roda um programa num PTY desta máquina, no formato webterm.ShellFunc: é o terminal do rc-visitor,
// que vive dentro da Fly Machine do visitante (lá não há VM do rc: a máquina do Fly já é a fronteira de isolamento).
package localpty

import (
	"context"
	"errors"
	"io"
	"os"
	"os/exec"
	"syscall"

	"github.com/creack/pty"

	"runner.code/internal/ports"
)

// Command descreve o processo: argv, ambiente, diretório e, opcionalmente, o usuário sem privilégio que o roda.
type Command struct {
	Argv []string
	Env  []string
	Dir  string
	// Cred, se não nil, troca uid/gid do filho (o rc-visitor roda como root para montar o firewall; o visitante não).
	Cred *syscall.Credential
}

// Run abre o PTY no tamanho dado, copia in para o terminal e o terminal para out, aplica os resize e espera o
// processo sair. O ctx cancelado mata o processo. Devolve o código de saída.
func Run(ctx context.Context, c Command, size ports.Winsize, in io.Reader, out io.Writer, resize <-chan ports.Winsize) (int, error) {
	if len(c.Argv) == 0 {
		return -1, errors.New("localpty: empty argv")
	}
	cmd := exec.CommandContext(ctx, c.Argv[0], c.Argv[1:]...)
	cmd.Env, cmd.Dir = c.Env, c.Dir
	if c.Cred != nil {
		cmd.SysProcAttr = &syscall.SysProcAttr{Credential: c.Cred}
	}
	f, err := pty.StartWithSize(cmd, &pty.Winsize{Rows: size.Rows, Cols: size.Cols})
	if err != nil {
		return -1, err
	}
	defer f.Close()
	go func() {
		for {
			select {
			case <-ctx.Done():
				return
			case s, ok := <-resize:
				if !ok {
					return
				}
				pty.Setsize(f, &pty.Winsize{Rows: s.Rows, Cols: s.Cols})
			}
		}
	}()
	// A entrada vai até o in acabar; ninguém espera por ela (o processo pode sair antes).
	go io.Copy(f, in)
	// A saída acaba com EIO quando o processo fecha o lado escravo: é o fim normal no Linux, não erro.
	io.Copy(out, f)
	err = cmd.Wait()
	var ee *exec.ExitError
	if errors.As(err, &ee) {
		return ee.ExitCode(), nil
	}
	if err != nil {
		return -1, err
	}
	return 0, nil
}

// Environ é o ambiente base do terminal: o do processo, com TERM de cor verdadeira e HOME/USER do visitante.
func Environ(home, user string, extra ...string) []string {
	env := append([]string{}, os.Environ()...)
	env = append(env, "TERM=xterm-256color", "COLORTERM=truecolor", "HOME="+home, "USER="+user, "LOGNAME="+user)
	return append(env, extra...)
}
