//go:build !linux

package guest

import (
	"errors"
	"io"
)

// dialHostVsock fora do Linux só recusa: o guest real é Linux, e os testes trocam dialHost.
func dialHostVsock(uint32) (io.ReadWriteCloser, error) {
	return nil, errors.New("vsock is only available in the Linux guest")
}
