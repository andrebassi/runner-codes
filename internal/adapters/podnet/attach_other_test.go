//go:build !linux

package podnet

import "testing"

// TestAttachNeedsLinux: fora do Linux a rede de pod recusa com o motivo (não há netns nem tc).
func TestAttachNeedsLinux(t *testing.T) {
	if _, _, err := Attach("/run/netns/x"); err == nil {
		t.Fatal("Attach fora do Linux não recusou")
	}
}
