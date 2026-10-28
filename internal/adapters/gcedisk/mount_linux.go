//go:build linux

package gcedisk

import (
	"fmt"
	"os/exec"
	"path/filepath"
	"syscall"
)

// mkfsExt4 formata o disco novo em ext4. -m 0: sem os 5% reservados ao root (o disco é de uma máquina só, e 5% de
// 13 GB é espaço pago que ninguém usa). -F: o disco recém-criado não tem tabela de partição, e o mkfs pergunta sem.
func mkfsExt4(dev string) error {
	out, err := exec.Command("mkfs.ext4", "-q", "-F", "-m", "0", dev).CombinedOutput()
	if err != nil {
		return fmt.Errorf("mkfs.ext4: %w: %s", err, out)
	}
	return nil
}

// mountExt4 monta o disco no diretório da máquina. noatime: ler um arquivo não vira escrita no disco de rede.
func mountExt4(dev, dir string) error {
	return syscall.Mount(dev, dir, "ext4", syscall.MS_NOATIME, "")
}

// unmountDir desmonta o diretório.
func unmountDir(dir string) error {
	return syscall.Unmount(dir, 0)
}

// isMountpoint diz se dir é a raiz de uma montagem: o dispositivo dele difere do do diretório pai.
func isMountpoint(dir string) bool {
	var a, b syscall.Stat_t
	if syscall.Stat(dir, &a) != nil || syscall.Stat(filepath.Dir(dir), &b) != nil {
		return false
	}
	return a.Dev != b.Dev
}
