package krun

import ll "github.com/landlock-lsm/go-landlock/landlock/syscall"

// TSyncLandlockABI é a primeira ABI do Landlock que aplica o ruleset em todas as threads de uma vez
// (LANDLOCK_RESTRICT_SELF_TSYNC). Abaixo dela o go-landlock usa o psx, que sem cgo cai no
// syscall.AllThreadsSyscall, e esse devolve ENOTSUP porque o purego faz o runtime se ver "com cgo":
// "prctl(PR_SET_NO_NEW_PRIVS): operation not supported" (medido no Ubuntu 24.04, ABI 4, 20/10/2026).
const TSyncLandlockABI = 8

// threadRule é uma regra path-beneath do Landlock: o caminho e os acessos liberados sob ele.
type threadRule struct {
	Path   string
	Access uint64
}

// fileAccess são os únicos direitos que o kernel aceita numa regra de arquivo (não diretório);
// pedir outro em arquivo devolve EINVAL no landlock_add_rule.
const fileAccess = ll.AccessFSExecute | ll.AccessFSWriteFile | ll.AccessFSReadFile | ll.AccessFSTruncate | ll.AccessFSIoctlDev

// handledFS devolve os direitos de arquivo que o kernel conhece em cada ABI: pedir um direito que o
// kernel não conhece no ruleset é EINVAL, e não pedir um que ele conhece deixa a ação liberada.
func handledFS(abi int) uint64 {
	if abi < 1 {
		return 0
	}
	all := uint64(ll.AccessFSExecute | ll.AccessFSWriteFile | ll.AccessFSReadFile | ll.AccessFSReadDir |
		ll.AccessFSRemoveDir | ll.AccessFSRemoveFile | ll.AccessFSMakeChar | ll.AccessFSMakeDir | ll.AccessFSMakeReg |
		ll.AccessFSMakeSock | ll.AccessFSMakeFifo | ll.AccessFSMakeBlock | ll.AccessFSMakeSym)
	if abi >= 2 {
		all |= ll.AccessFSRefer
	}
	if abi >= 3 {
		all |= ll.AccessFSTruncate
	}
	if abi >= 5 {
		all |= ll.AccessFSIoctlDev
	}
	return all
}

// legacyRuleset monta, para um kernel com ABI abaixo de TSyncLandlockABI, o mesmo confinamento do
// ApplySandbox: arquivos só nos caminhos da política, nenhuma conexão ou bind TCP (ABI ≥ 4) e escopo
// de sinais e sockets abstratos (ABI ≥ 6). Função pura: a aplicação fica em restrictThread (Linux).
func legacyRuleset(p SandboxPolicy, abi int) (ll.RulesetAttr, []threadRule) {
	fs := handledFS(abi)
	attr := ll.RulesetAttr{HandledAccessFS: fs}
	if abi >= 4 {
		// Nenhuma regra de porta: todo bind e connect TCP fica negado, como no caminho TSYNC.
		attr.HandledAccessNet = ll.AccessNetBindTCP | ll.AccessNetConnectTCP
	}
	if abi >= 6 {
		attr.Scoped = ll.ScopeAbstractUnixSocket | ll.ScopeSignal
	}
	readFile := uint64(ll.AccessFSExecute | ll.AccessFSReadFile)
	var rules []threadRule
	add := func(paths []string, access uint64) {
		for _, path := range paths {
			rules = append(rules, threadRule{Path: path, Access: access & fs})
		}
	}
	add(p.ReadOnlyFiles, readFile)
	add(p.ReadOnlyDirs, readFile|ll.AccessFSReadDir)
	add(p.ReadWriteFiles, ll.AccessFSReadFile|ll.AccessFSWriteFile|ll.AccessFSTruncate)
	// /dev/kvm é controlado por ioctl: sem IoctlDev (ABI ≥ 5) a libkrun não cria a VM.
	add(p.DeviceFiles, ll.AccessFSReadFile|ll.AccessFSWriteFile|ll.AccessFSIoctlDev)
	if p.SocketDir != "" {
		// O diretório desta VM com todos os direitos: criar e apagar o socket.
		add([]string{p.SocketDir}, fs)
	}
	// Abaixo da ABI 9 o kernel não controla connect() em socket Unix: ler o caminho basta.
	add(p.UnixConnect, ll.AccessFSReadFile)
	add(p.ListenDirs, fs)
	return attr, rules
}

// ruleAccess restringe os direitos de uma regra ao que o tipo do caminho aceita: em arquivo, só os
// direitos de arquivo (o kernel recusa o resto com EINVAL).
func ruleAccess(r threadRule, isDir bool) uint64 {
	if isDir {
		return r.Access
	}
	return r.Access & fileAccess
}
