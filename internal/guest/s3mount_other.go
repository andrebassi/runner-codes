//go:build !linux

package guest

import (
	"errors"

	"runner.code/internal/s3fs"
)

// mountFUSEOS fora do Linux só recusa: o guest real é sempre Linux, e os testes no host
// trocam mountFUSE por um falso.
func mountFUSEOS(string, *s3fs.FS) (unmounter, error) {
	return nil, errors.New("FUSE is only available in the Linux guest")
}
