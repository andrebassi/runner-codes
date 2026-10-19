package krun

import (
	"crypto/rand"
	"encoding/hex"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"os"
	"os/exec"
	"path/filepath"
	"sync"
	"syscall"

	"runner.code/internal/ports"
)

// Launcher cria processos vmm. Cada Start é um spawn novo do próprio binário,
// nunca fork: fork de processo multi-thread é morto pelo HVF ~2 s depois do boot
//.
type Launcher struct {
	// Exe é o binário que tem o subcomando `vmm` (normalmente os.Executable()).
	Exe string
	// LibDir vai em DYLD_LIBRARY_PATH/LD_LIBRARY_PATH: a libkrun carrega a libkrunfw
	// por nome e, sem isso, falha com "Couldn't find or load libkrunfw".
	LibDir string
	// LibPath é a libkrun repassada ao vmm em --lib.
	LibPath string
	// WorkDir guarda spec e console de cada VM.
	WorkDir string
	// Stderr recebe o stderr do processo vmm (erros da libkrun). Nil descarta.
	Stderr io.Writer
}

// Process é uma VM viva. Kill é idempotente.
type Process struct {
	ID       string
	Spec     Spec
	cmd      *exec.Cmd
	watch    *os.File
	specPath string
	done     chan struct{}
	exitCode int
	killOnce sync.Once
}

// newID gera um id curto para nomear arquivos da VM.
func newID() string {
	b := make([]byte, 6)
	rand.Read(b)
	return hex.EncodeToString(b)
}

// Start grava a spec, cria o pipe de vigia e lança `<Exe> vmm`.
func (l *Launcher) Start(s Spec) (*Process, error) {
	if err := s.Validate(); err != nil {
		return nil, err
	}
	if err := os.MkdirAll(l.WorkDir, 0o700); err != nil {
		return nil, fmt.Errorf("create workdir: %w", err)
	}
	id := newID()
	specPath := filepath.Join(l.WorkDir, id+".json")
	raw, _ := json.Marshal(s)
	if err := os.WriteFile(specPath, raw, 0o600); err != nil {
		return nil, fmt.Errorf("write spec: %w", err)
	}
	r, w, err := os.Pipe()
	if err != nil {
		os.Remove(specPath)
		return nil, fmt.Errorf("create watch pipe: %w", err)
	}
	// O lado de leitura vira o fd 3 do filho (primeiro de ExtraFiles).
	cmd := exec.Command(l.Exe, "vmm", "--spec", specPath, "--watch-fd", "3", "--lib", l.LibPath)
	cmd.ExtraFiles = []*os.File{r}
	cmd.Env = append(os.Environ(), libPathEnv+"="+l.LibDir)
	cmd.SysProcAttr = sysProcAttr()
	cmd.Stderr = l.Stderr
	if err := cmd.Start(); err != nil {
		r.Close()
		w.Close()
		os.Remove(specPath)
		return nil, fmt.Errorf("launch vmm: %w", err)
	}
	r.Close()
	p := &Process{ID: id, Spec: s, cmd: cmd, watch: w, specPath: specPath, done: make(chan struct{})}
	go func() {
		err := cmd.Wait()
		p.exitCode = exitCodeOf(err)
		close(p.done)
	}()
	return p, nil
}

// StartDetached lança o vmm de uma máquina persistente desligado do `rc`: sessão própria
// (Setsid, que também cria o grupo para o SIGKILL do stop), sem pipe de vigia e sem
// Pdeathsig, porque a máquina tem de sobreviver ao comando que a ligou. A spec fica em
// specPath (no diretório da máquina) e stdout/stderr do vmm vão para logPath.
func (l *Launcher) StartDetached(s Spec, specPath, logPath string) (int, error) {
	if err := s.Validate(); err != nil {
		return 0, err
	}
	raw, _ := json.Marshal(s)
	if err := os.WriteFile(specPath, raw, 0o600); err != nil {
		return 0, fmt.Errorf("write spec: %w", err)
	}
	log, err := os.OpenFile(logPath, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0o600)
	if err != nil {
		return 0, fmt.Errorf("open machine log: %w", err)
	}
	defer log.Close()
	cmd := exec.Command(l.Exe, "vmm", "--spec", specPath, "--lib", l.LibPath)
	cmd.Env = append(os.Environ(), libPathEnv+"="+l.LibDir)
	cmd.SysProcAttr = &syscall.SysProcAttr{Setsid: true}
	cmd.Stdout, cmd.Stderr = log, log
	if err := cmd.Start(); err != nil {
		return 0, fmt.Errorf("launch vmm: %w", err)
	}
	pid := cmd.Process.Pid
	// O Wait só recolhe o processo quando ele sair. Sem isso, num processo de vida longa
	// (rc serve) a VM desligada vira zumbi: kill(pid, 0) continua achando o pid, a máquina
	// parece ligada, e o stop espera o prazo inteiro (medido: 10 s a cada stop pela API).
	// Na CLI o rc sai antes, e o init recolhe o órfão.
	go cmd.Wait()
	return pid, nil
}

// StartVM satisfaz ports.VMLauncher: o app só conhece a porta, não o *Process.
func (l *Launcher) StartVM(s ports.VMSpec) (ports.VM, error) {
	p, err := l.Start(s)
	if err != nil {
		return nil, err
	}
	return p, nil
}

// exitCodeOf extrai o código de saída; -1 quando o processo morreu por sinal.
func exitCodeOf(err error) int {
	if err == nil {
		return 0
	}
	var ee *exec.ExitError
	if errors.As(err, &ee) {
		return ee.ExitCode()
	}
	return -1
}

// PID devolve o pid do processo vmm.
func (p *Process) PID() int { return p.cmd.Process.Pid }

// Done fecha quando o processo vmm termina.
func (p *Process) Done() <-chan struct{} { return p.done }

// Wait espera o fim e devolve o código de saída (o do workload, quando a VM desligou sozinha).
func (p *Process) Wait() int {
	<-p.done
	p.cleanup()
	return p.exitCode
}

// Kill manda SIGKILL ao grupo inteiro (o vmm e qualquer filho da libkrun), fecha o
// vigia e espera o fim. É o mecanismo de timeout: medido no spike, nada sobrevive.
func (p *Process) Kill() {
	p.killOnce.Do(func() {
		syscall.Kill(-p.cmd.Process.Pid, syscall.SIGKILL)
		p.watch.Close()
		<-p.done
		p.cleanup()
	})
}

// cleanup apaga a spec e fecha o vigia; seguro de chamar mais de uma vez.
func (p *Process) cleanup() {
	p.watch.Close()
	os.Remove(p.specPath)
}
