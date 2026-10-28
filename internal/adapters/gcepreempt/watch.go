// Package gcepreempt ouve o aviso de preempção do GCE dentro da própria VM spot: o metadata server muda
// instance/preempted de FALSE para TRUE no começo dos ~30 s que o GCE dá antes do desligamento. Esse é o sinal mais
// cedo que existe — antes do ACPI, antes de o status da instância mudar na API —, e é o que dá tempo de desligar as
// máquinas com o disco gravado e soltar os discos delas para outro host adotar.
package gcepreempt

import (
	"context"
	"fmt"
	"io"
	"net/http"
	"strings"
	"time"
)

// Watcher espera o aviso. Base é o metadata server (http://metadata.google.internal/computeMetadata/v1).
type Watcher struct {
	HTTP *http.Client
	Base string
	// Retry é a pausa depois de um erro (metadata server reiniciando, rede piscando).
	Retry time.Duration
}

// New devolve o Watcher de produção. O cliente não tem timeout próprio: o long-poll do metadata server segura a
// conexão até a mudança ou o timeout_sec pedido.
func New() *Watcher {
	return &Watcher{HTTP: &http.Client{}, Base: "http://metadata.google.internal/computeMetadata/v1", Retry: time.Second}
}

// Wait bloqueia até o aviso de preempção (devolve nil) ou o ctx acabar (devolve o erro do ctx). Erro de rede não
// para a espera: perder o aviso custa um failover lento, então ele tenta de novo até o fim.
//
// É o long-poll do metadata server: wait_for_change com o last_etag da leitura anterior volta na hora em que o valor
// muda, sem gastar requisição por segundo.
func (w *Watcher) Wait(ctx context.Context) error {
	etag := ""
	for {
		value, next, err := w.read(ctx, etag)
		if ctx.Err() != nil {
			return ctx.Err()
		}
		if err != nil {
			if serr := w.pause(ctx); serr != nil {
				return serr
			}
			continue
		}
		if value == "TRUE" {
			return nil
		}
		etag = next
	}
}

// Preempted lê instance/preempted uma vez, sem esperar mudança: diz se a VM está em preempção AGORA. É a pergunta do
// SIGTERM do rc serve — na preempção, o desligamento por ACPI chega junto com o aviso, e o serve precisa saber se o
// SIGTERM é de um restart comum (segue matando as VMs) ou do host sumindo (suspende e solta os discos antes de sair).
func (w *Watcher) Preempted(ctx context.Context) (bool, error) {
	value, _, err := w.read(ctx, "")
	return value == "TRUE", err
}

// read lê instance/preempted; com etag, espera a mudança (até 5 min por chamada).
func (w *Watcher) read(ctx context.Context, etag string) (string, string, error) {
	u := w.Base + "/instance/preempted"
	if etag != "" {
		u += "?wait_for_change=true&timeout_sec=300&last_etag=" + etag
	}
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, u, nil)
	if err != nil {
		return "", "", err
	}
	req.Header.Set("Metadata-Flavor", "Google")
	resp, err := w.HTTP.Do(req)
	if err != nil {
		return "", "", err
	}
	defer resp.Body.Close()
	raw, _ := io.ReadAll(io.LimitReader(resp.Body, 64))
	if resp.StatusCode != http.StatusOK {
		return "", "", fmt.Errorf("metadata preempted: status %d", resp.StatusCode)
	}
	return strings.TrimSpace(string(raw)), resp.Header.Get("ETag"), nil
}

// pause espera Retry, ou o ctx acabar.
func (w *Watcher) pause(ctx context.Context) error {
	t := time.NewTimer(w.Retry)
	defer t.Stop()
	select {
	case <-ctx.Done():
		return ctx.Err()
	case <-t.C:
		return nil
	}
}
