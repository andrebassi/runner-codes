package krun

import (
	"encoding/json"
	"fmt"
	"io"
	"os"
	"os/signal"

	"runner.codes/internal/ports"
)

// ExitParentGone é o código com que o vmm sai quando o pipe de vigia fecha:
// 128+SIGKILL, o mesmo que um kill -9 deixaria, para o pai tratar os dois igual.
const ExitParentGone = 137

// exitFunc é os.Exit; os testes trocam para observar a saída sem matar o processo de teste.
var exitFunc = os.Exit

// ReadSpec lê e valida a spec gravada pelo launcher.
func ReadSpec(path string) (Spec, error) {
	var s Spec
	raw, err := os.ReadFile(path)
	if err != nil {
		return s, fmt.Errorf("read spec: %w", err)
	}
	if err := json.Unmarshal(raw, &s); err != nil {
		return s, fmt.Errorf("decode spec: %w", err)
	}
	return s, s.Validate()
}

// RunVMM é o corpo do subcomando `vmm`: liga o vigia do pai, configura a VM e
// entrega o processo à libkrun. Só volta em erro; no sucesso a libkrun chama exit()
// com o código do workload quando a VM desliga.
func RunVMM(api API, s Spec, watchdog io.Reader) error {
	if watchdog != nil {
		// O pai segura o lado de escrita do pipe e nunca escreve: EOF só chega quando
		// ele morre (ou fecha de propósito no Kill). No macOS é o único aviso de morte do pai.
		go func() {
			io.Copy(io.Discard, watchdog)
			exitFunc(ExitParentGone)
		}()
	}
	netFD := -1
	if s.Net != nil {
		fd, env, gw, err := startNetwork(s.Net, s.VsockSocket)
		if err != nil {
			return err
		}
		// O gateway roda nas goroutines deste processo enquanto a libkrun ocupa a thread
		// do krun_start_enter; se a VM nem subir, ele é fechado aqui.
		defer gw.Close()
		netFD, s.Env = fd, append(append([]string{}, s.Env...), env...)
	}
	if s.PodNetns != "" {
		fd, env, mac, closer, err := startPodNet(s.PodNetns)
		if err != nil {
			return err
		}
		defer closer.Close()
		netFD, s.NetMAC, s.Env = fd, mac, append(append([]string{}, s.Env...), env...)
	}
	if len(s.Sockets) > 0 {
		// Pontes de socket (docker.sock): escutam no host enquanto este processo viver.
		sb, err := startSockets(s.Sockets, s.VsockSocket)
		if err != nil {
			return err
		}
		defer sb.Close()
	}
	scr, err := startDisplay(s.Display, s.VsockSocket)
	if err != nil {
		return err
	}
	ctx, err := configureFull(api, s, netFD, scr)
	if err != nil {
		return err
	}
	if err := sandbox(s); err != nil {
		return err
	}
	snap, canSnap := api.(SnapshotAPI)
	if s.SnapshotDir != "" && canSnap {
		armSuspend(snap, ctx, s.SnapshotDir)
	}
	if s.RestoreFrom != "" {
		if !canSnap {
			return fmt.Errorf("restore from %s: this libkrun cannot restore snapshots", s.RestoreFrom)
		}
		return callErr("krun_restore", snap.Restore(ctx, s.RestoreFrom, 0))
	}
	return callErr("krun_start_enter", api.StartEnter(ctx))
}

// SuspendSignal é o sinal de suspender (ports.SuspendSignal): o vmm grava o snapshot em SnapshotDir e sai.
const SuspendSignal = ports.SuspendSignal

// notifySuspend é o signal.Notify; os testes trocam para disparar o pedido sem mandar sinal ao processo de teste.
var notifySuspend = func(c chan<- os.Signal) { signal.Notify(c, SuspendSignal) }

// armSuspend espera o sinal de suspender numa goroutine: a thread principal fica no krun_start_enter (que não volta),
// e o pedido de snapshot é feito de outra thread — a libkrun congela as vCPUs, grava e encerra o processo com 0. Se
// o pedido falhar, a VM segue rodando e o erro vai para o stderr (o console log da máquina), sem matar nada.
func armSuspend(snap SnapshotAPI, ctx uint32, dir string) {
	c := make(chan os.Signal, 1)
	notifySuspend(c)
	go func() {
		<-c
		if rc := snap.SnapshotRequest(ctx, dir, 0); rc < 0 {
			fmt.Fprintf(os.Stderr, "suspend: krun_snapshot_request: %d\n", rc)
		}
	}()
}

// preload é preloadKrunfw; os testes trocam porque a máquina de teste pode não ter libkrunfw.
var preload = preloadKrunfw

// applySandbox é ApplySandbox; os testes trocam para observar a política sem confinar
// o processo de teste.
var applySandbox = ApplySandbox

// sandbox confina o processo vmm de uma VM de job antes de entregá-lo à libkrun.
// Tudo o que a libkrun abriria só no krun_start_enter é preparado antes: a libkrunfw
// carregada e o console criado, porque depois do Landlock esses open falhariam.
func sandbox(s Spec) error {
	if !SandboxEnabled(s) {
		switch {
		case !SandboxSupported || s.RootDisk == "" || s.Network || s.Net != nil || s.PodNetns != "":
		case s.GPU:
			fmt.Fprintln(os.Stderr, "warning: GPU VM runs without Landlock/seccomp (the virglrenderer needs /dev/dri and execve)")
		default:
			fmt.Fprintf(os.Stderr, "warning: sandbox disabled by %s=off\n", sandboxDisabledEnv)
		}
		return nil
	}
	if err := preload(); err != nil {
		return err
	}
	if s.Console != "" {
		f, err := os.OpenFile(s.Console, os.O_CREATE|os.O_WRONLY|os.O_APPEND, 0o600)
		if err != nil {
			return fmt.Errorf("create console: %w", err)
		}
		f.Close()
	}
	return applySandbox(PolicyFor(s, loadedLibDirs()))
}
