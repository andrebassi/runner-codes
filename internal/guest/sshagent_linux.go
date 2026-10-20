//go:build linux

package guest

import (
	"io"
	"os"

	"golang.org/x/sys/unix"
)

// dialHostVsock conecta na porta vsock do host (CID 2). Devolve o descritor como *os.File, e
// não net.FileConn: o net do Go só conhece socket Unix e IP, e com AF_VSOCK o FileConn falha
// (medido: o relay fechava todo cliente com reset, enquanto um connect AF_VSOCK em Python, no
// mesmo guest, respondia sempre).
func dialHostVsock(port uint32) (io.ReadWriteCloser, error) {
	fd, err := unix.Socket(unix.AF_VSOCK, unix.SOCK_STREAM|unix.SOCK_CLOEXEC, 0)
	if err != nil {
		return nil, err
	}
	if err := unix.Connect(fd, &unix.SockaddrVM{CID: unix.VMADDR_CID_HOST, Port: port}); err != nil {
		unix.Close(fd)
		return nil, err
	}
	return os.NewFile(uintptr(fd), "vsock"), nil
}
