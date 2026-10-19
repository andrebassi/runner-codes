package krun

import (
	"encoding/json"
	"fmt"
	"io"
	"os"
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
	ctx, err := Configure(api, s)
	if err != nil {
		return err
	}
	if err := sandbox(s); err != nil {
		return err
	}
	return callErr("krun_start_enter", api.StartEnter(ctx))
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
		case !SandboxSupported || s.RootDisk == "" || s.Network:
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
