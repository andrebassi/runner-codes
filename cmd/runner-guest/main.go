// Command runner-guest roda dentro da microVM e atende jobs na porta vsock.
//
// É um binário separado do host de propósito: o host importa o purego (dlopen da libkrun),
// que no Linux força link dinâmico contra a glibc; dentro de uma imagem Alpine (musl) esse
// binário não acha o interpretador e o exec falha com "not found". Este aqui não importa
// nada do host e sai estático.
package main

import (
	"flag"
	"log"
	"os"

	"runner.code/internal/guest"
)

// main lê a porta e entrega o processo ao servidor do guest.
func main() {
	port := flag.Uint("port", guest.DefaultPort, "porta vsock")
	flag.Parse()
	// Rede antes de atender: o init e o workload da máquina já podem precisar dela.
	if err := guest.ConfigureNetwork(os.Getenv); err != nil {
		log.Printf("network: %v", err)
	}
	cfg := guest.DefaultServerConfig()
	cfg.Port = uint32(*port)
	log.Fatal(guest.NewServer(cfg).Run())
}
