package main

import (
	"strings"
	"testing"

	"github.com/spf13/pflag"
)

// buildFlags monta as flags de máquina, lê args e devolve o resultado do build.
func buildFlags(t *testing.T, args ...string) (string, error) {
	t.Helper()
	f := pflag.NewFlagSet("t", pflag.ContinueOnError)
	mf := addMachineFlags(f, "m")
	if err := f.Parse(args); err != nil {
		t.Fatal(err)
	}
	mc, _, err := mf.build(f, nil)
	if err != nil {
		return "", err
	}
	if mc.Display == nil {
		return "", nil
	}
	return mc.Display.Title, nil
}

// TestDisplayTitleFlag: --display-title chega à tela; sem --display é recusado, porque não haveria página
// para levar o nome e o valor sumiria calado.
func TestDisplayTitleFlag(t *testing.T) {
	if title, err := buildFlags(t, "--stack", "python", "--display", "800x600", "--display-title", "superbot.computer"); err != nil || title != "superbot.computer" {
		t.Fatalf("%q %v", title, err)
	}
	if _, err := buildFlags(t, "--stack", "python", "--display-title", "x"); err == nil || !strings.Contains(err.Error(), "needs --display") {
		t.Fatalf("título sem tela: %v", err)
	}
}
