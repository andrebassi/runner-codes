package app

import (
	"bytes"
	"context"
	"errors"
	"strings"
	"testing"
	"time"

	"runner.code/internal/domain"
	"runner.code/internal/ports"
	"runner.code/internal/protocol"
)

// lastJob devolve o último job de exec/pty que o guest falso recebeu.
func lastJob(g *fileGuest) protocol.Job {
	g.mu.Lock()
	defer g.mu.Unlock()
	return g.jobs[len(g.jobs)-1]
}

// TestRunEphemeral: sem -d a máquina ganha nome run-<hex>, roda o comando em primeiro
// plano com a saída repassada (stdout e stderr), devolve o código dele e some no fim, com
// o workload declarado sem subir em segundo plano.
func TestRunEphemeral(t *testing.T) {
	m, st, _, _, g := machinesFixture(t)
	ctx := context.Background()
	var out, errOut bytes.Buffer
	name, code, err := m.Run(ctx, RunRequest{Machine: domain.Machine{Stack: "python", Workload: []string{"srv"}, StopOnExit: true},
		Command: []string{"exit7"}}, RunIO{Stdin: strings.NewReader("in"), Stdout: &out, Stderr: &errOut})
	if err != nil || code != 7 || !strings.HasPrefix(name, "run-") || len(name) != 12 {
		t.Fatalf("run: %s %d %v", name, code, err)
	}
	if out.String() != "part1 part2\n" {
		t.Fatalf("stdout: %q", out.String())
	}
	if j := lastJob(g); j.Argv[0] != "exit7" || j.Stdin != "in" {
		t.Fatalf("job: %+v", j)
	}
	if len(jobsOf(g, protocol.OpWorkload)) != 0 {
		t.Fatal("the declared workload must not also start in the background")
	}
	if _, err := st.Load(name); errCode(err) != domain.CodeNotFound {
		t.Fatal("the ephemeral machine must be gone")
	}
	if _, code, _ := m.Run(ctx, RunRequest{Machine: domain.Machine{Stack: "python", Workload: []string{"late"}}},
		RunIO{Stdout: &out, Stderr: &errOut}); code != 124 {
		t.Fatalf("workload as foreground + timeout: %d", code)
	}
}

// TestRunInteractive: com -it o comando (ou o shell padrão, argv vazio) vai para um PTY
// com o TERM e o tamanho do host; o código é o do programa.
func TestRunInteractive(t *testing.T) {
	m, _, _, _, g := machinesFixture(t)
	resize := make(chan ports.Winsize, 1)
	resize <- ports.Winsize{Rows: 10, Cols: 20}
	var out bytes.Buffer
	_, code, err := m.Run(context.Background(), RunRequest{Machine: domain.Machine{Name: "it", Stack: "python"}, Interactive: true},
		RunIO{Stdin: strings.NewReader("ls\n"), Stdout: &out, Term: "xterm-256color", Size: ports.Winsize{Rows: 40, Cols: 120}, Resize: resize})
	if err != nil || code != 6 || out.String() != "size=10x20;ls\n" {
		t.Fatalf("interactive: %d %v %q", code, err, out.String())
	}
	if j := lastJob(g); len(j.Argv) != 0 || j.Env[0] != "TERM=xterm-256color" || j.Rows != 40 || j.Cols != 120 {
		t.Fatalf("pty job: %+v", j)
	}
}

// TestRunDetached: com -d a máquina fica ligada com o workload, sob o nome "default" ou o
// pedido; nome repetido é CONFLICT.
func TestRunDetached(t *testing.T) {
	m, _, _, _, g := machinesFixture(t)
	ctx := context.Background()
	name, _, err := m.Run(ctx, RunRequest{Machine: domain.Machine{Stack: "python", Workload: []string{"srv"}}, Detach: true}, RunIO{})
	if err != nil || name != "default" {
		t.Fatalf("detach: %s %v", name, err)
	}
	if st, _ := m.Status("default"); st.State != StateRunning || len(jobsOf(g, protocol.OpWorkload)) != 1 {
		t.Fatal("a detached run keeps the machine running its workload")
	}
	if _, _, err := m.Run(ctx, RunRequest{Machine: domain.Machine{Name: "cmd", Stack: "python", Workload: []string{"srv"}},
		Command: []string{"sleep", "9"}, Detach: true}, RunIO{}); err != nil {
		t.Fatal(err)
	}
	if wl := jobsOf(g, protocol.OpWorkload); wl[len(wl)-1].Argv[0] != "sleep" {
		t.Fatalf("with -d the command after -- becomes the workload: %+v", wl[len(wl)-1])
	}
	if _, _, err := m.Run(ctx, RunRequest{Machine: domain.Machine{Stack: "python"}, Detach: true}, RunIO{}); errCode(err) != domain.CodeConflict {
		t.Fatalf("name taken: %v", err)
	}
	m.Launcher.(*detached).fail = errors.New("no hypervisor")
	if _, _, err := m.Run(ctx, RunRequest{Machine: domain.Machine{Name: "x", Stack: "python"}, Detach: true}, RunIO{}); err == nil {
		t.Fatal("start failure must surface")
	}
}

// TestRunFailures: nada para rodar é CONFIG_ERROR antes de criar; máquina inválida não
// liga; falha do start ainda apaga a máquina; Ctrl+C (contexto cancelado) vira 130 e
// também apaga; stdin que falha e erro do guest são repassados.
func TestRunFailures(t *testing.T) {
	m, st, _, d, g := machinesFixture(t)
	ctx := context.Background()
	var out bytes.Buffer
	if _, _, err := m.Run(ctx, RunRequest{Machine: domain.Machine{Stack: "python"}}, RunIO{}); errCode(err) != domain.CodeConfig {
		t.Fatalf("nothing to run: %v", err)
	}
	if _, _, err := m.Run(ctx, RunRequest{Machine: domain.Machine{Stack: "nope"}, Command: []string{"x"}}, RunIO{}); err == nil {
		t.Fatal("unknown stack must fail")
	}
	d.fail = errors.New("no hypervisor")
	name, _, err := m.Run(ctx, RunRequest{Machine: domain.Machine{Stack: "python"}, Command: []string{"x"}}, RunIO{})
	if err == nil {
		t.Fatal("start failure must surface")
	}
	if _, err := st.Load(name); errCode(err) != domain.CodeNotFound {
		t.Fatal("a failed start must still remove the ephemeral machine")
	}
	d.fail = nil
	cctx, cancel := context.WithCancel(ctx)
	go func() { time.Sleep(100 * time.Millisecond); cancel() }()
	name, code, err := m.Run(cctx, RunRequest{Machine: domain.Machine{Stack: "python"}, Command: []string{"slow"}}, RunIO{Stdout: &out, Stderr: &out})
	if err != nil || code != 130 || !strings.Contains(out.String(), "waiting") {
		t.Fatalf("ctrl+c: %d %v %q", code, err, out.String())
	}
	if _, err := st.Load(name); errCode(err) != domain.CodeNotFound {
		t.Fatal("ctrl+c must still remove the machine")
	}
	if _, _, err := m.Run(ctx, RunRequest{Machine: domain.Machine{Stack: "python"}, Command: []string{"x"}}, RunIO{Stdin: errReader{}}); err == nil {
		t.Fatal("stdin error must surface")
	}
	g.execErr = errors.New("guest gone")
	if _, _, err := m.Run(ctx, RunRequest{Machine: domain.Machine{Stack: "python"}, Command: []string{"x"}}, RunIO{Stdout: &out, Stderr: &out}); err == nil {
		t.Fatal("guest error must surface")
	}
}
