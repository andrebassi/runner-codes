package guest

import (
	"io"
	"net"
	"os"
	"path/filepath"
	"sync"
)

// dialHost abre a conexão até a porta vsock do host (CID 2); trocado nos testes, que rodam
// fora de uma VM.
var dialHost = dialHostVsock

// relays guarda os listeners por caminho: o agente SSH e cada --mount-socket. O start manda
// de novo a cada boot, e o mesmo caminho troca o listener antigo pelo novo.
var (
	relaysMu sync.Mutex
	relays   = map[string]net.Listener{}
)

// startSSHAgent atende o OpSSHAgent: escuta job.Path e repassa cada conexão à porta vsock
// job.Port do host, onde a libkrun liga o socket do host (o SSH_AUTH_SOCK de quem deu o
// start, ou o HOST de um --mount-socket). 0666 porque o `user` da máquina (não root) também
// usa; quem está na VM já alcançaria o socket de qualquer jeito.
func startSSHAgent(job Job) Result {
	res := Result{TraceID: job.TraceID, Status: "success"}
	relaysMu.Lock()
	defer relaysMu.Unlock()
	if old := relays[job.Path]; old != nil {
		old.Close()
	}
	os.Remove(job.Path)
	// O diretório do socket pode não existir na imagem (/run/podman, por exemplo).
	if err := os.MkdirAll(filepath.Dir(job.Path), 0o755); err != nil {
		return failed(res, "ssh-agent: "+err.Error())
	}
	l, err := net.Listen("unix", job.Path)
	if err != nil {
		return failed(res, "ssh-agent: "+err.Error())
	}
	if err := os.Chmod(job.Path, 0o666); err != nil {
		l.Close()
		return failed(res, "ssh-agent: "+err.Error())
	}
	relays[job.Path] = l
	// O discador é lido aqui, uma vez: as goroutines das conexões não tocam na variável de
	// pacote, que um teste troca e restaura enquanto o listener ainda existe.
	port, dial := uint32(job.Port), dialHost
	go func() {
		for {
			c, err := l.Accept()
			if err != nil {
				return
			}
			go func() {
				h, err := dial(port)
				if err != nil {
					c.Close()
					return
				}
				relay(c, h)
			}()
		}
	}()
	return res
}

// relay copia nos dois sentidos e fecha os dois no fim.
func relay(a, b io.ReadWriteCloser) {
	var wg sync.WaitGroup
	cp := func(dst, src io.ReadWriteCloser) {
		defer wg.Done()
		buf := make([]byte, 32<<10)
		for {
			n, err := src.Read(buf)
			if n > 0 {
				if _, werr := dst.Write(buf[:n]); werr != nil {
					break
				}
			}
			if err != nil {
				break
			}
		}
		dst.Close()
	}
	wg.Add(2)
	go cp(a, b)
	go cp(b, a)
	wg.Wait()
}
