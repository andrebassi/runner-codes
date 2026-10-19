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
		return s, fmt.Errorf("ler spec: %w", err)
	}
	if err := json.Unmarshal(raw, &s); err != nil {
		return s, fmt.Errorf("decodificar spec: %w", err)
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
	return callErr("krun_start_enter", api.StartEnter(ctx))
}
