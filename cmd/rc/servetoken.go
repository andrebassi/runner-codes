package main

import (
	"errors"
	"net"
	"os"
	"strings"
)

// serveToken decide o token da API do `rc serve`: o do arquivo (--token-file, para o segredo não
// aparecer no ps) ou o da variável RUNNER_API_TOKEN. Escutar fora do loopback sem token é recusado:
// a API liga, apaga e exporta o disco de qualquer máquina, e uma porta aberta na rede é tudo isso para
// quem a alcançar.
func serveToken(tokenFile, listen string) (string, error) {
	token := strings.TrimSpace(os.Getenv("RUNNER_API_TOKEN"))
	if tokenFile != "" {
		raw, err := os.ReadFile(tokenFile)
		if err != nil {
			return "", err
		}
		token = strings.TrimSpace(string(raw))
		if token == "" {
			return "", errors.New("token file is empty")
		}
	}
	host, _, err := net.SplitHostPort(listen)
	if err != nil {
		return "", err
	}
	ip := net.ParseIP(host)
	loopback := host == "localhost" || (ip != nil && ip.IsLoopback())
	if !loopback && token == "" {
		return "", errors.New("refusing to listen on " + listen + " without --token-file or RUNNER_API_TOKEN")
	}
	return token, nil
}
