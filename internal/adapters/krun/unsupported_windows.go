//go:build windows

package krun

// O build para Windows falha de propósito, com o motivo no nome do símbolo que falta:
// a libkrun upstream não tem backend WHP (só Linux KVM e macOS HVF).
var _ = runnerCodesDoesNotSupportWindows_libkrunUpstreamHasNoWHPBackend
