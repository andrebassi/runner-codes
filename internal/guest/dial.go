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

// dialSession atende o OpDial: conecta em 127.0.0.1:Port, ou [::1]:Port se o IPv4 recusar (ou no socket Unix
// Path, a ponte do docker.sock), responde o resultado e, no sucesso, copia bytes nos dois sentidos até um lado fechar.
// 127.0.0.1 cobre o servidor em localhost IPv4 e o de todas as interfaces (0.0.0.0, ::); o [::1] cobre o que escuta
// só em IPv6, como o Node 17+ faz quando o programa pede "localhost" (o Vite, por exemplo).
func dialSession(conn net.Conn, job Job) {
	conn.SetReadDeadline(time.Time{}) // a conexão vive o quanto o cliente do host quiser
	res := Result{TraceID: job.TraceID, Status: "success"}
	var target net.Conn
	var err error
	if job.Path != "" {
		target, err = net.DialTimeout("unix", job.Path, dialTimeout)
	} else {
		target, err = dialLoopback(job.Port)
	}
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

// dialLoopback conecta na porta pelo loopback do guest: IPv4 primeiro (o caso comum); se recusar, IPv6. O erro
// devolvido, quando os dois falham, é o do IPv4, que é o que quem chama espera ler.
func dialLoopback(port uint16) (net.Conn, error) {
	p := strconv.Itoa(int(port))
	c, err := net.DialTimeout("tcp", net.JoinHostPort("127.0.0.1", p), dialTimeout)
	if err == nil {
		return c, nil
	}
	if c6, err6 := net.DialTimeout("tcp", net.JoinHostPort("::1", p), dialTimeout); err6 == nil {
		return c6, nil
	}
	return nil, err
}
