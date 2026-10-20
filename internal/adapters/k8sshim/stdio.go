package k8sshim

import (
	"context"
	"io"
	"net/url"
	"os"
	"path/filepath"
	"strings"
	"syscall"

	"github.com/containerd/fifo"
)

// closers fecha vários descritores de uma vez.
type closers []io.Closer

// Close fecha todos, ignorando os erros (é limpeza de fim de processo).
func (c closers) Close() error {
	for _, x := range c {
		x.Close()
	}
	return nil
}

// openOut abre um destino de saída: FIFO do CRI (caminho), ou file:// do ctr. Vazio = descarta.
func openOut(ctx context.Context, p string) (io.WriteCloser, error) {
	if p == "" {
		return nil, nil
	}
	if u, err := url.Parse(p); err == nil && u.Scheme == "file" {
		os.MkdirAll(filepath.Dir(u.Path), 0o755)
		return os.OpenFile(u.Path, os.O_WRONLY|os.O_CREATE|os.O_APPEND, 0o644)
	}
	// O_WRONLY sem O_NONBLOCK espera o leitor (o containerd já abriu o dele antes do Start); o fifo
	// abre em segundo plano e respeita o ctx.
	return fifo.OpenFifo(ctx, p, syscall.O_WRONLY, 0)
}

// OpenFifos é o IOOpener de verdade: as FIFOs que o containerd criou para o processo. stdin abre
// O_RDONLY|O_NONBLOCK, como no shim do runc: sem leitor do outro lado não bloqueia, e o EOF só chega
// quando o containerd fecha a escrita. Com terminal, o stderr não é aberto (vai tudo pelo TTY).
func OpenFifos(ctx context.Context, stdin, stdout, stderr string, terminal bool) (Stdio, io.Closer, error) {
	var st Stdio
	var all closers
	fail := func(err error) (Stdio, io.Closer, error) {
		all.Close()
		return Stdio{}, nil, err
	}
	if stdin != "" && !strings.HasPrefix(stdin, "file://") {
		f, err := fifo.OpenFifo(ctx, stdin, syscall.O_RDONLY|syscall.O_NONBLOCK, 0)
		if err != nil {
			return fail(err)
		}
		st.Stdin, all = f, append(all, f)
	}
	out, err := openOut(ctx, stdout)
	if err != nil {
		return fail(err)
	}
	if out != nil {
		st.Stdout, all = out, append(all, out)
	}
	if !terminal {
		errw, err := openOut(ctx, stderr)
		if err != nil {
			return fail(err)
		}
		if errw != nil {
			st.Stderr, all = errw, append(all, errw)
		}
	}
	return st, all, nil
}
