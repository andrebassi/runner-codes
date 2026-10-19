//go:build darwin

package krun

// SandboxSupported é false no macOS: não existe Landlock nem seccomp. O processo vmm
// fica com o entitlement do hypervisor e o grupo de processo; está declarado na doc.
const SandboxSupported = false

// preloadKrunfw não é necessário no macOS: sem confinamento, a libkrun abre a libkrunfw
// normalmente dentro de krun_start_enter.
func preloadKrunfw() error { return nil }

// loadedLibDirs não é usado no macOS (sem Landlock).
func loadedLibDirs() []string { return nil }

// LandlockABI é 0 no macOS: não há Landlock.
func LandlockABI() int { return 0 }

// ApplySandbox não faz nada no macOS; ver SandboxSupported.
func ApplySandbox(SandboxPolicy) error { return nil }
