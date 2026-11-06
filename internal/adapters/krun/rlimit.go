package krun

import "syscall"

// raiseNofile sobe o limite de arquivos abertos do processo vmm até o teto rígido. O virtio-fs em passthrough segura
// um descritor no HOST por arquivo que o convidado tem aberto; um rootfs inteiro por virtio-fs (o Android, milhares
// de arquivos no boot) estoura o 1024 herdado de um shell, e o convidado recebe EMFILE no exec — o apexd do Android
// caía com "Too many open files" e o init dele reiniciava a máquina (medido em 05/11/2026, scripts/android/ do HQ).
// Devolve o limite de antes e o novo, para o log.
func raiseNofile() (before, after uint64, err error) {
	var lim syscall.Rlimit
	if err = getrlimit(syscall.RLIMIT_NOFILE, &lim); err != nil {
		return 0, 0, err
	}
	before = lim.Cur
	if lim.Cur >= lim.Max {
		return before, lim.Cur, nil
	}
	lim.Cur = lim.Max
	if err = setrlimit(syscall.RLIMIT_NOFILE, &lim); err != nil {
		return before, before, err
	}
	return before, lim.Cur, nil
}

// getrlimit e setrlimit são as chamadas do sistema; os testes trocam para simular limites e recusas.
var (
	getrlimit = syscall.Getrlimit
	setrlimit = syscall.Setrlimit
)
