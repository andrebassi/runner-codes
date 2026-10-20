//go:build linux

package guest

import "runner.code/internal/s3fs"

// mountFUSEOS monta pelo go-fuse (mount(2) direto em /dev/fuse).
func mountFUSEOS(dir string, core *s3fs.FS) (unmounter, error) {
	return s3fs.Mount(dir, core)
}
