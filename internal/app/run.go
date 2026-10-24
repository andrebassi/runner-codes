package app

import (
	"context"
	"crypto/rand"
	"encoding/hex"
	"errors"
	"io"
	"time"

	"runner.codes/internal/domain"
	"runner.codes/internal/ports"
	"runner.codes/internal/protocol"
)

// RunRequest é um `machine run`: a máquina declarada (flags e/ou runner.code), o comando
// depois do `--`, e o modo. Sem Detach a máquina é efêmera: some quando o comando termina.
type RunRequest struct {
	Machine domain.Machine
	Command []string
	// Interactive abre um terminal (PTY) em vez de só repassar a saída (o `-it`).
	Interactive bool
	// Detach deixa a máquina ligada com o workload rodando e volta na hora.
	Detach bool
	// DiskFrom é o disco de partida (o de um pack feito de máquina); vazio usa um novo.
	DiskFrom string
}

// RunIO são os fluxos do terminal de quem chamou.
type RunIO struct {
	Stdin          io.Reader
	Stdout, Stderr io.Writer
	Size           ports.Winsize
	Resize         <-chan ports.Winsize
	// Term é o TERM do host, repassado ao shell interativo.
	Term string
}

// cleanupTimeout é quanto a limpeza da máquina efêmera pode levar depois do comando:
// matar o vmm e apagar o diretório levam milissegundos; o prazo só existe para um disco
// travado não prender o terminal para sempre.
const cleanupTimeout = 30 * time.Second

// errNothingToRun é a recusa de um run sem comando, sem workload e sem terminal.
var errNothingToRun = domain.ValidationError{Err: domain.MachineError{Code: domain.CodeConfig,
	Msg: "nothing to run: pass a command after --, declare cmd/entrypoint in the runner.code, or use -it for a shell"}}

// newRunName dá o nome da máquina efêmera: "run-" + 8 hex aleatórios, válido como nome
// de máquina e sem colisão prática entre runs simultâneos.
func newRunName() string {
	b := make([]byte, 4)
	rand.Read(b)
	return "run-" + hex.EncodeToString(b)
}

// foreground decide o que roda em primeiro plano: o comando do `--`, senão o workload
// declarado, senão (com terminal) o shell padrão do guest (argv vazio). Sem nada disso é
// erro: uma VM que liga e desliga sem rodar nada não serve a ninguém.
func foreground(req RunRequest) ([]string, error) {
	switch {
	case len(req.Command) > 0:
		return req.Command, nil
	case len(req.Machine.Workload) > 0:
		return req.Machine.Workload, nil
	case req.Interactive:
		return nil, nil
	}
	return nil, errNothingToRun
}

// Run cria, liga e usa uma máquina de uma vez. Com Detach, devolve o nome com a máquina
// ligada (o workload em segundo plano). Sem Detach, roda o comando em primeiro plano, devolve
// o código de saída dele e apaga a máquina, mesmo se o contexto for cancelado (Ctrl+C).
func (m *Machines) Run(ctx context.Context, req RunRequest, rio RunIO) (name string, code int, err error) {
	mc := req.Machine
	if req.Detach {
		if mc.Name == "" {
			mc.Name = "default"
		}
		// O comando do -- vira o workload: é ele que a máquina mantém rodando.
		if len(req.Command) > 0 {
			mc.Workload = req.Command
		}
		if _, err := m.CreateFrom(ctx, mc, req.DiskFrom); err != nil {
			return mc.Name, 0, err
		}
		if _, err := m.Start(ctx, mc.Name); err != nil {
			return mc.Name, 0, err
		}
		return mc.Name, 0, nil
	}
	argv, err := foreground(req)
	if err != nil {
		return "", 0, err
	}
	if mc.Name == "" {
		mc.Name = newRunName()
	}
	// O workload vira o comando de primeiro plano: não sobe também em segundo plano, e o
	// stop_on_exit perde o sentido (a máquina some no fim de qualquer jeito).
	mc.Workload, mc.StopOnExit = nil, false
	if _, err := m.CreateFrom(ctx, mc, req.DiskFrom); err != nil {
		return mc.Name, 0, err
	}
	defer func() {
		cctx, cancel := context.WithTimeout(context.Background(), cleanupTimeout)
		defer cancel()
		// O fim do run devolve as cópias staged ao host antes de a máquina sumir.
		if serr := m.Sync(cctx, mc.Name); serr != nil && err == nil {
			err = serr
		}
		if derr := m.Delete(cctx, mc.Name, true); derr != nil && err == nil {
			err = derr
		}
	}()
	// Presa ao processo: se quem chamou morrer (até SIGKILL), a VM não fica rodando sem dono.
	if _, err := m.start(ctx, mc.Name, true); err != nil {
		return mc.Name, 0, err
	}
	if req.Interactive {
		env := []string{}
		if rio.Term != "" {
			env = append(env, "TERM="+rio.Term)
		}
		code, err = m.Shell(ctx, mc.Name, ShellRequest{Argv: argv, Env: env, Size: rio.Size}, rio.Stdin, rio.Stdout, rio.Resize)
		return mc.Name, code, err
	}
	var stdin string
	if rio.Stdin != nil {
		b, err := io.ReadAll(rio.Stdin)
		if err != nil {
			return mc.Name, 0, err
		}
		stdin = string(b)
	}
	res, err := m.Exec(ctx, mc.Name, ExecRequest{Argv: argv, Stdin: stdin}, func(stream, data string) {
		if stream == protocol.EventStderr {
			io.WriteString(rio.Stderr, data)
			return
		}
		io.WriteString(rio.Stdout, data)
	})
	if err != nil {
		if errors.Is(ctx.Err(), context.Canceled) {
			return mc.Name, 130, nil // Ctrl+C: o código que o shell daria (128+SIGINT)
		}
		return mc.Name, 0, err
	}
	if res.TimedOut {
		return mc.Name, 124, nil
	}
	return mc.Name, res.ExitCode, nil
}
