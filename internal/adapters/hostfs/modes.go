// Package hostfs mexe no lado do host de um volume compartilhado com o guest.
package hostfs

import (
	"io/fs"
	"os"
	"path/filepath"
	"strconv"
	"strings"

	"golang.org/x/sys/unix"
)

// overrideStat é o xattr em que o virtio-fs da libkrun no macOS guarda o dono e o modo que o
// guest deu ao arquivo ("uid:gid:modo octal"), enquanto o arquivo de verdade fica 0600 (0700
// diretório) e do usuário do host. Medido em 20/10/2026: "user.containers.override_stat:
// 0:0:0100644" num arquivo que o guest criou com 644.
const overrideStat = "user.containers.override_stat"

// ApplyGuestModes percorre dir e dá a cada arquivo e diretório com o xattr as permissões que
// o guest pediu: sem isso, o que volta de um sync staged chega 0600 ao host (um script perde
// o +x). Só as 9 permissões: setuid, setgid e sticky de dentro da VM não passam para o host,
// nem o dono, que continua o usuário do host. Link simbólico é pulado (o xattr seria do alvo).
func ApplyGuestModes(dir string) error {
	buf := make([]byte, 128)
	return filepath.WalkDir(dir, func(p string, d fs.DirEntry, err error) error {
		if err != nil || d.Type()&fs.ModeSymlink != 0 {
			return err
		}
		n, gerr := unix.Getxattr(p, overrideStat, buf)
		if gerr != nil {
			return nil // arquivo que o host criou, ou sistema sem o xattr: fica como está
		}
		perm, ok := guestPerm(string(buf[:n]))
		if !ok {
			return nil
		}
		return os.Chmod(p, perm)
	})
}

// guestPerm lê "uid:gid:modo" (modo em octal, com o tipo do arquivo) e devolve as 9 permissões.
func guestPerm(v string) (fs.FileMode, bool) {
	parts := strings.Split(strings.TrimRight(v, "\x00"), ":")
	if len(parts) != 3 {
		return 0, false
	}
	mode, err := strconv.ParseUint(parts[2], 8, 32)
	if err != nil {
		return 0, false
	}
	return fs.FileMode(mode) & fs.ModePerm, true
}
