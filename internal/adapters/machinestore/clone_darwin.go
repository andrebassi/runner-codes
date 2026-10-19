//go:build darwin

package machinestore

import "golang.org/x/sys/unix"

// cloneFile usa o clonefile do APFS: o disco de 10 GiB vira cópia instantânea que só
// ocupa espaço no que a máquina escrever. Fora do APFS (ou entre volumes) cai na cópia.
func cloneFile(src, dst string) error {
	if err := unix.Clonefile(src, dst, 0); err == nil {
		return nil
	}
	return copySparse(src, dst)
}
