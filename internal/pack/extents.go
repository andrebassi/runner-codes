package pack

import (
	"errors"
	"os"

	"golang.org/x/sys/unix"
)

// extents devolve os trechos com dado do arquivo, saltando os buracos (SEEK_DATA e
// SEEK_HOLE, no APFS e no ext4/xfs/btrfs). Sistema de arquivos sem suporte (EINVAL) devolve o
// arquivo inteiro como um trecho: fica maior, mas correto.
func extents(path string, size int64) ([]Extent, error) {
	f, err := os.Open(path)
	if err != nil {
		return nil, err
	}
	defer f.Close()
	fd := int(f.Fd())
	var out []Extent
	for off := int64(0); off < size; {
		data, err := unix.Seek(fd, off, unix.SEEK_DATA)
		if errors.Is(err, unix.ENXIO) {
			break // só buraco daqui até o fim
		}
		if errors.Is(err, unix.EINVAL) {
			return []Extent{{Off: 0, Len: size}}, nil
		}
		if err != nil {
			return nil, err
		}
		hole, err := unix.Seek(fd, data, unix.SEEK_HOLE)
		if err != nil {
			return nil, err
		}
		if hole > size {
			hole = size
		}
		out = append(out, Extent{Off: data, Len: hole - data})
		off = hole
	}
	return out, nil
}
