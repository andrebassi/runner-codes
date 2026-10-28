// Package gcsimages é o cache de imagens de máquina num bucket do Cloud Storage, compartilhado pelos hosts de uma
// frota: o primeiro host que constrói um ext4 o sobe, e os outros só baixam. Isso faz duas coisas que um cache por
// host não faz:
//
//  1. o host novo de um failover fica pronto em segundos, não nos ~2 min de construir a imagem (o blue-green da
//     preempção do spot espera exatamente isso: o servidor verde ficar pronto);
//  2. TODOS os hosts rodam o mesmo arquivo, byte a byte. O mkfs.ext4 não é determinístico (UUID, hash seed), e o
//     restore de um snapshot só vale sobre a imagem base em que ele foi feito — com uma imagem por host, a máquina
//     suspensa num host era recusada (ou, antes da trava, voltava com o cache de diretórios de outro disco).
//
// O objeto é <prefixo><chave>.ext4.zst, a chave é a do cache local (digest OCI + warmup + guest): conteúdo, não nome,
// e por isso nunca é sobrescrito — o upload só cria (ifGenerationMatch=0). Fala com a API JSON do Storage com o token
// da conta de serviço da própria VM (metadata server), sem SDK, como o gcedisk.
package gcsimages

import (
	"context"
	"encoding/json"
	"errors"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"os"
	"strings"
	"sync"
	"time"

	"github.com/klauspost/compress/zstd"
)

// ErrNotFound é a imagem que ninguém subiu ainda: quem pediu constrói.
var ErrNotFound = errors.New("image not in the shared cache")

// Store é o cache num bucket. API e Metadata existem para os testes trocarem os servidores do Google por fakes.
type Store struct {
	Bucket   string
	Prefix   string
	HTTP     *http.Client
	API      string // https://storage.googleapis.com
	Metadata string // http://metadata.google.internal/computeMetadata/v1

	mu      sync.Mutex
	token   string
	expires time.Time
}

// Parse lê o destino da flag (gs://bucket/prefixo/) num Store de produção.
func Parse(spec string) (*Store, error) {
	rest, ok := strings.CutPrefix(spec, "gs://")
	bucket, prefix, _ := strings.Cut(rest, "/")
	if !ok || bucket == "" {
		return nil, fmt.Errorf("image cache %q: want gs://bucket/prefix/", spec)
	}
	if prefix != "" && !strings.HasSuffix(prefix, "/") {
		prefix += "/"
	}
	return &Store{Bucket: bucket, Prefix: prefix, HTTP: &http.Client{}, API: "https://storage.googleapis.com",
		Metadata: "http://metadata.google.internal/computeMetadata/v1"}, nil
}

// object é o nome do objeto da chave.
func (s *Store) object(key string) string { return s.Prefix + key + ".ext4.zst" }

// Get baixa a imagem da chave para dst. Grava ao lado e renomeia no fim: um download interrompido nunca deixa um ext4
// pela metade no lugar de onde o cache local lê.
func (s *Store) Get(ctx context.Context, key, dst string) error {
	u := fmt.Sprintf("%s/storage/v1/b/%s/o/%s?alt=media", s.API, url.PathEscape(s.Bucket), url.PathEscape(s.object(key)))
	res, err := s.do(ctx, http.MethodGet, u, nil)
	if err != nil {
		return err
	}
	defer res.Body.Close()
	if res.StatusCode == http.StatusNotFound {
		return ErrNotFound
	}
	if res.StatusCode != http.StatusOK {
		return fmt.Errorf("image cache get %s: status %d", key, res.StatusCode)
	}
	// NewReader só falha com opção inválida, e aqui não há opção nenhuma.
	dec, _ := zstd.NewReader(res.Body)
	defer dec.Close()
	tmp := dst + ".download"
	f, err := os.OpenFile(tmp, os.O_CREATE|os.O_WRONLY|os.O_TRUNC, 0o644)
	if err != nil {
		return err
	}
	if _, err := io.Copy(f, dec); err != nil {
		f.Close()
		os.Remove(tmp)
		return fmt.Errorf("image cache get %s: %w", key, err)
	}
	if err := f.Close(); err != nil {
		os.Remove(tmp)
		return err
	}
	return os.Rename(tmp, dst)
}

// Put sobe a imagem da chave, comprimida no caminho (o ext4 é quase todo zero: um desktop de ~3 GB vira poucas
// centenas de MB). Objeto que já existe (outro host subiu primeiro) não é erro: o conteúdo da chave é o mesmo.
func (s *Store) Put(ctx context.Context, key, src string) error {
	f, err := os.Open(src)
	if err != nil {
		return err
	}
	defer f.Close()
	pr, pw := io.Pipe()
	go func() {
		// NewWriter só falha com opção inválida; o nível fixo aqui é válido.
		enc, _ := zstd.NewWriter(pw, zstd.WithEncoderLevel(zstd.SpeedFastest))
		if _, err := io.Copy(enc, f); err != nil {
			enc.Close()
			pw.CloseWithError(err)
			return
		}
		pw.CloseWithError(enc.Close())
	}()
	u := fmt.Sprintf("%s/upload/storage/v1/b/%s/o?uploadType=media&ifGenerationMatch=0&name=%s",
		s.API, url.PathEscape(s.Bucket), url.QueryEscape(s.object(key)))
	res, err := s.do(ctx, http.MethodPost, u, pr)
	if err != nil {
		pr.CloseWithError(err)
		return err
	}
	defer res.Body.Close()
	switch res.StatusCode {
	case http.StatusOK, http.StatusPreconditionFailed:
		return nil
	default:
		msg, _ := io.ReadAll(io.LimitReader(res.Body, 512))
		return fmt.Errorf("image cache put %s: status %d: %s", key, res.StatusCode, strings.TrimSpace(string(msg)))
	}
}

// do manda a requisição com o token da VM.
func (s *Store) do(ctx context.Context, method, u string, body io.Reader) (*http.Response, error) {
	tok, err := s.accessToken(ctx)
	if err != nil {
		return nil, err
	}
	req, err := http.NewRequestWithContext(ctx, method, u, body)
	if err != nil {
		return nil, err
	}
	req.Header.Set("Authorization", "Bearer "+tok)
	if body != nil {
		req.Header.Set("Content-Type", "application/zstd")
	}
	return s.HTTP.Do(req)
}

// accessToken devolve o token da conta de serviço da VM, guardado até 1 min antes de vencer.
func (s *Store) accessToken(ctx context.Context) (string, error) {
	s.mu.Lock()
	defer s.mu.Unlock()
	if s.token != "" && time.Now().Before(s.expires) {
		return s.token, nil
	}
	req, err := http.NewRequestWithContext(ctx, http.MethodGet, s.Metadata+"/instance/service-accounts/default/token", nil)
	if err != nil {
		return "", err
	}
	req.Header.Set("Metadata-Flavor", "Google")
	res, err := s.HTTP.Do(req)
	if err != nil {
		return "", fmt.Errorf("token: %w", err)
	}
	defer res.Body.Close()
	var t struct {
		AccessToken string `json:"access_token"`
		ExpiresIn   int    `json:"expires_in"`
	}
	if res.StatusCode != http.StatusOK || json.NewDecoder(res.Body).Decode(&t) != nil || t.AccessToken == "" {
		return "", fmt.Errorf("token: status %d", res.StatusCode)
	}
	s.token, s.expires = t.AccessToken, time.Now().Add(time.Duration(t.ExpiresIn)*time.Second-time.Minute)
	return s.token, nil
}
