//go:build !linux

package podnet

import (
	"errors"
	"os"
)

// Attach só existe no Linux: netns, TAP e tc são do kernel Linux, e o rc como runtime de Kubernetes
// só roda em nó Linux com KVM.
func Attach(string) (*os.File, Info, error) {
	return nil, Info{}, errors.New("pod network needs Linux (netns, tap, tc)")
}
