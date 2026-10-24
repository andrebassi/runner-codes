// rc-playground é o front público do playground no Fly (app claude-in-chrome): serve a página de entrada, dá a cada
// visitante uma Fly Machine própria (app das máquinas de visitante, teto de 3) e leva o terminal e as portas até o
// rc-visitor dela pela rede privada. Escuta só no loopback: quem publica é o cloudflared da mesma máquina.
//
// Ambiente: FLY_API_TOKEN (token com escopo no app das máquinas), RC_SECRET (hex, assina os tokens; fixo entre
// deploys, senão as máquinas já criadas ficam com o token antigo), RC_VISITOR_APP, RC_VISITOR_IMAGE.
package main

import (
	"context"
	"encoding/hex"
	"errors"
	"flag"
	"log"
	"net"
	"net/http"
	"os"
	"os/signal"
	"strings"
	"syscall"
	"time"

	"runner.codes/internal/adapters/flymachines"
	"runner.codes/internal/adapters/playground"
)

// reapEvery é de quanto em quanto tempo o front desliga e apaga as máquinas ociosas: 1 min basta para 30 min e 24 h.
const reapEvery = time.Minute

// main só liga as peças; a lógica está em internal/adapters/playground (FlyBackend) e é coberta por teste.
func main() {
	listen := flag.String("listen", "127.0.0.1:8080", "address to listen on (the cloudflared on this machine publishes it)")
	host := flag.String("host", "claude-in-chrome.runner.codes", "public host name of the page")
	portDomain := flag.String("port-domain", "runner.codes", "domain of the port subdomains")
	maxMachines := flag.Int("max", 3, "visitor machines in the app (running or stopped)")
	idle := flag.Duration("idle", 30*time.Minute, "stop a visitor machine after this long without use")
	maxAge := flag.Duration("max-age", 24*time.Hour, "delete a visitor machine after this long without use")
	regions := flag.String("regions", "gru,iad", "Fly regions of the visitor machines, in order (the next is tried when one has no capacity)")
	flag.Parse()

	secret, err := hex.DecodeString(os.Getenv("RC_SECRET"))
	if err != nil || len(secret) < 32 {
		log.Fatal("rc-playground: RC_SECRET must be at least 32 bytes in hex")
	}
	api := &flymachines.Client{
		Base: flymachines.DefaultBase, App: mustEnv("RC_VISITOR_APP"), Token: mustEnv("FLY_API_TOKEN"),
		// 2 vCPUs compartilhadas e 2 GiB cabem um Claude Code e um servidor de dev; o volume de 1 GB guarda o login e o
		// /workspace (o disco raiz da Fly Machine volta zerado a cada partida).
		Spec: flymachines.Spec{Image: mustEnv("RC_VISITOR_IMAGE"), Regions: strings.Split(*regions, ","), CPUKind: "shared", CPUs: 2, MemoryMB: 2048, VolumeGB: 1, Mount: "/data"},
	}
	var d net.Dialer
	fly := &playground.FlyBackend{
		API: api, Secret: secret, Max: *maxMachines, IdleStop: *idle, MaxAge: *maxAge,
		Host: *host, PortDomain: *portDomain, Now: time.Now,
		Dial:         func(ctx context.Context, addr string) (net.Conn, error) { return d.DialContext(ctx, "tcp", addr) },
		ReadyTimeout: 20 * time.Second,
	}
	srv := playground.New(playground.Config{
		Host: *host, PortDomain: *portDomain, Secret: secret, Backend: fly,
		IsFull: func(err error) bool { return errors.Is(err, playground.ErrFull) },
	})
	ctx, stop := signal.NotifyContext(context.Background(), syscall.SIGTERM, syscall.SIGINT)
	defer stop()
	go func() {
		t := time.NewTicker(reapEvery)
		defer t.Stop()
		for {
			select {
			case <-ctx.Done():
				return
			case <-t.C:
				for _, line := range fly.Reap(ctx) {
					log.Println("playground:", line)
				}
			}
		}
	}()
	// O WriteTimeout fica de fora: o terminal é um stream longo (SSE) e cortaria a sessão.
	hs := &http.Server{Addr: *listen, Handler: srv, ReadHeaderTimeout: 10 * time.Second}
	go func() { <-ctx.Done(); hs.Close() }()
	log.Printf("rc-playground on http://%s (public https://%s, max %d machines)", *listen, *host, *maxMachines)
	if err := hs.ListenAndServe(); err != nil && !errors.Is(err, http.ErrServerClosed) {
		log.Fatal(err)
	}
}

// mustEnv lê a variável obrigatória.
func mustEnv(k string) string {
	v := os.Getenv(k)
	if v == "" {
		log.Fatalf("rc-playground: %s is required", k)
	}
	return v
}
