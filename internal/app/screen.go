package app

import "runner.codes/internal/domain"

// DisplayPort devolve a porta do visualizador da tela de uma máquina ligada: é por ela que a CLI, a API e o MCP
// capturam a tela e mandam mouse e teclado. Máquina sem tela é CONFIG (não há o que controlar); pausada ou desligada
// é INVALID_STATE, como nas outras operações que precisam do guest.
func (m *Machines) DisplayPort(name string) (uint16, error) {
	if _, err := m.running(name); err != nil {
		return 0, err
	}
	st, err := m.Status(name)
	if err != nil {
		return 0, err
	}
	if st.Display == nil {
		return 0, domain.MachineError{Code: domain.CodeConfig, Msg: "machine " + name + " has no display (create it with --gpu --display WxH)"}
	}
	return st.Display.Port, nil
}
