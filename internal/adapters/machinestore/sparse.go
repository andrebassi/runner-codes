package machinestore

import (
	"bytes"
	"io"
	"os"
)

// sparseBlock é o passo da cópia esparsa: blocos de 1 MiB só de zeros viram buraco no
// destino (Seek em vez de Write), e o disco de 10 GiB recém-formatado ocupa só o que o
// mkfs escreveu. Vale no ext4/XFS; o APFS aloca o trecho pulado mesmo assim (medido), e lá
// o caminho normal é o clonefile.
const sparseBlock = 1 << 20

// copySparse copia src para dst preservando os buracos: o fallback quando não há clone.
func copySparse(src, dst string) error {
	in, err := os.Open(src)
	if err != nil {
		return err
	}
	defer in.Close()
	st, err := in.Stat()
	if err != nil {
		return err
	}
	out, err := os.OpenFile(dst, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0o600)
	if err != nil {
		return err
	}
	buf, zero := make([]byte, sparseBlock), make([]byte, sparseBlock)
	for {
		n, rerr := io.ReadFull(in, buf)
		if n > 0 {
			if bytes.Equal(buf[:n], zero[:n]) {
				_, err = out.Seek(int64(n), io.SeekCurrent)
			} else {
				_, err = out.Write(buf[:n])
			}
			if err != nil {
				out.Close()
				return err
			}
		}
		if rerr == io.EOF || rerr == io.ErrUnexpectedEOF {
			break
		}
		if rerr != nil {
			out.Close()
			return rerr
		}
	}
	// Truncate no tamanho final: se o arquivo termina em buraco, o Seek sozinho não o estica.
	if err := out.Truncate(st.Size()); err != nil {
		out.Close()
		return err
	}
	return out.Close()
}
