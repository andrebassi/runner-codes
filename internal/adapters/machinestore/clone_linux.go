//go:build linux

package machinestore

import (
	"os"

	"golang.org/x/sys/unix"
)

// cloneFile usa o FICLONE (reflink, em btrfs e XFS) e cai na cópia esparsa nos sistemas
// de arquivos que não têm (ext4).
func cloneFile(src, dst string) error {
	in, err := os.Open(src)
	if err != nil {
		return err
	}
	defer in.Close()
	out, err := os.OpenFile(dst, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0o600)
	if err != nil {
		return err
	}
	if err := unix.IoctlFileClone(int(out.Fd()), int(in.Fd())); err == nil {
		return out.Close()
	}
	out.Close()
	return copySparse(src, dst)
}
