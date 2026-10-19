//go:build !linux

package guest

import "errors"

// mountProc fora do Linux só existe para o pacote compilar no host (testes no macOS);
// o guest nunca roda aqui, então devolve erro em vez de fingir que montou.
func mountProc() error {
	return errors.New("mounting /proc is only supported on Linux")
}
