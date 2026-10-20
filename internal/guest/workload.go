package guest

import (
	"bytes"
	"fmt"
	"io"
	"os"
	"os/exec"
	"sync"
	"syscall"
	"time"
)

// workloadPrefix marca cada linha do workload no console da máquina: o `rc machine logs
// --source workload` filtra por ele, e o resto do console (o guest) fica de fora.
const workloadPrefix = "[workload] "

// workload é o processo que a máquina mantém rodando. Um por máquina: o guest recusa um
// segundo start enquanto o primeiro vive.
type workload struct {
	mu   sync.Mutex
	cmd  *exec.Cmd
	done chan struct{}
	code int
}

// console é onde o workload escreve; o stdout do guest é o console.log da máquina.
var console io.Writer = os.Stdout

// startWorkload liga o Argv em segundo plano, com o env e o cwd do job, e a saída linha a
// linha no console. Com StopOnExit, quando o processo sai o guest grava o disco e sai também,
// e a VM desliga sozinha.
func (s *Server) startWorkload(job Job) Result {
	res := Result{TraceID: job.TraceID, Status: "success"}
	if len(job.Argv) == 0 {
		return failed(res, "workload: empty argv")
	}
	s.wl.mu.Lock()
	defer s.wl.mu.Unlock()
	if s.wl.cmd != nil {
		select {
		case <-s.wl.done:
		default:
			return failed(res, "workload: already running")
		}
	}
	u, err := lookupUser(job.User)
	if err != nil {
		return failed(res, "workload: "+err.Error())
	}
	cmd := exec.Command(job.Argv[0], job.Argv[1:]...)
	cmd.Env = environ(u, job.Env)
	cmd.Dir = job.Cwd
	if cmd.Dir == "" {
		cmd.Dir = "/"
	}
	out := &prefixWriter{w: console, prefix: workloadPrefix}
	cmd.Stdout, cmd.Stderr = out, out
	// Grupo próprio: o stop_signal vai ao workload e aos filhos dele (um shell e o servidor).
	cmd.SysProcAttr = &syscall.SysProcAttr{Setpgid: true}
	applyUser(cmd, u)
	if err := cmd.Start(); err != nil {
		return failed(res, "workload: "+err.Error())
	}
	done := make(chan struct{})
	s.wl.cmd, s.wl.done = cmd, done
	stopOnExit := job.StopOnExit
	go func() {
		err := cmd.Wait()
		out.Flush()
		code := 0
		if ee, ok := err.(*exec.ExitError); ok {
			code = ee.ExitCode()
		}
		s.wl.mu.Lock()
		s.wl.code = code
		s.wl.mu.Unlock()
		fmt.Fprintf(console, "%sexited with code %d\n", workloadPrefix, code)
		close(done)
		if stopOnExit {
			syncDisks()
			exitProcess(0)
		}
	}()
	return res
}

// stopWorkload manda o sinal ao grupo do workload e espera até timeout; quem não sai no
// prazo leva SIGKILL. Sem workload vivo não faz nada.
func (s *Server) stopWorkload(signal int, timeout time.Duration) {
	s.wl.mu.Lock()
	cmd, done := s.wl.cmd, s.wl.done
	s.wl.mu.Unlock()
	if cmd == nil {
		return
	}
	select {
	case <-done:
		return
	default:
	}
	if signal == 0 {
		signal = int(syscall.SIGTERM)
	}
	syscall.Kill(-cmd.Process.Pid, syscall.Signal(signal))
	select {
	case <-done:
	case <-time.After(timeout):
		syscall.Kill(-cmd.Process.Pid, syscall.SIGKILL)
		<-done
	}
}

// prefixWriter põe o prefixo no começo de cada linha. Guarda a linha incompleta até a
// quebra chegar (ou o Flush), para duas escritas parciais não virarem duas linhas.
type prefixWriter struct {
	mu      sync.Mutex
	w       io.Writer
	prefix  string
	pending []byte
}

// Write separa as linhas completas e as escreve com o prefixo.
func (p *prefixWriter) Write(b []byte) (int, error) {
	p.mu.Lock()
	defer p.mu.Unlock()
	p.pending = append(p.pending, b...)
	for {
		i := bytes.IndexByte(p.pending, '\n')
		if i < 0 {
			break
		}
		if _, err := fmt.Fprintf(p.w, "%s%s\n", p.prefix, p.pending[:i]); err != nil {
			return 0, err
		}
		p.pending = p.pending[i+1:]
	}
	return len(b), nil
}

// Flush escreve a última linha sem quebra, se houver.
func (p *prefixWriter) Flush() {
	p.mu.Lock()
	defer p.mu.Unlock()
	if len(p.pending) > 0 {
		fmt.Fprintf(p.w, "%s%s\n", p.prefix, p.pending)
		p.pending = nil
	}
}
