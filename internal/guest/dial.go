package guest

import (
	"io"
	"net"
	"strconv"
	"sync"
	"time"
)

// dialTimeout é a espera pelo servidor local do guest: ele está na mesma máquina, então
// conectar ou é imediato ou não vai acontecer (ninguém escutando dá recusa na hora).
const dialTimeout = 5 * time.Second

// dialSession atende o OpDial: conecta em 127.0.0.1:Port (ou no socket Unix Path, a ponte
// do docker.sock), responde o resultado e, no sucesso, copia bytes nos dois sentidos até um
// lado fechar. 127.0.0.1 cobre o servidor que escuta só em localhost e o que escuta em todas
// as interfaces (0.0.0.0).
func dialSession(conn net.Conn, job Job) {
	conn.SetReadDeadline(time.Time{}) // a conexão vive o quanto o cliente do host quiser
	res := Result{TraceID: job.TraceID, Status: "success"}
	network, addr := "tcp", net.JoinHostPort("127.0.0.1", strconv.Itoa(int(job.Port)))
	if job.Path != "" {
		network, addr = "unix", job.Path
	}
	target, err := net.DialTimeout(network, addr, dialTimeout)
	if err != nil {
		writeResult(conn, failed(res, "dial: "+err.Error()))
		return
	}
	defer target.Close()
	writeResult(conn, res)
	var wg sync.WaitGroup
	cp := func(dst, src net.Conn) {
		defer wg.Done()
		io.Copy(dst, src)
		// Half-close: o outro lado recebe EOF e ainda pode terminar a resposta.
		if cw, ok := dst.(interface{ CloseWrite() error }); ok {
			cw.CloseWrite()
		}
	}
	wg.Add(2)
	go cp(target, conn)
	go cp(conn, target)
	wg.Wait()
}
