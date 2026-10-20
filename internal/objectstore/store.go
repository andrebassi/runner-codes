package objectstore

import (
	"bytes"
	"context"
	"crypto/sha256"
	"encoding/hex"
	"encoding/xml"
	"errors"
	"fmt"
	"io"
	"net/http"
	"net/url"
	"os"
	"strconv"
	"strings"
	"time"
)

// ErrNotFound é objeto (ou bucket) que não existe.
var ErrNotFound = errors.New("not found")

// Store fala com um bucket. Endpoint é a raiz do serviço (https://s3.us-east-1.amazonaws.com,
// a de um R2 ou de um MinIO); o bucket vai no caminho (path-style), que todos aceitam.
type Store struct {
	Endpoint *url.URL
	Bucket   string
	Region   string
	Creds    Credentials
	HTTP     *http.Client
	// now é o relógio da assinatura; trocado nos testes pelo vetor de referência.
	now func() time.Time
}

// FromEnv monta o Store a partir do ambiente da máquina (as variáveis padrão da AWS):
// AWS_ACCESS_KEY_ID, AWS_SECRET_ACCESS_KEY, AWS_SESSION_TOKEN, AWS_REGION (padrão
// us-east-1) e AWS_ENDPOINT_URL (R2, MinIO...). Sem par de chaves, acesso anônimo.
func FromEnv(env []string, bucket string) (*Store, error) {
	vars := map[string]string{}
	for _, kv := range env {
		if k, v, ok := strings.Cut(kv, "="); ok {
			vars[k] = v
		}
	}
	region := vars["AWS_REGION"]
	if region == "" {
		region = "us-east-1"
	}
	endpoint := vars["AWS_ENDPOINT_URL"]
	if endpoint == "" {
		endpoint = "https://s3." + region + ".amazonaws.com"
	}
	u, err := url.Parse(strings.TrimRight(endpoint, "/"))
	if err != nil || (u.Scheme != "http" && u.Scheme != "https") || u.Host == "" {
		return nil, fmt.Errorf("AWS_ENDPOINT_URL %q must be an http(s) URL", endpoint)
	}
	if (vars["AWS_ACCESS_KEY_ID"] == "") != (vars["AWS_SECRET_ACCESS_KEY"] == "") {
		return nil, errors.New("AWS_ACCESS_KEY_ID and AWS_SECRET_ACCESS_KEY go together")
	}
	return &Store{Endpoint: u, Bucket: bucket, Region: region, HTTP: &http.Client{Timeout: 5 * time.Minute},
		Creds: Credentials{AccessKey: vars["AWS_ACCESS_KEY_ID"], SecretKey: vars["AWS_SECRET_ACCESS_KEY"], SessionToken: vars["AWS_SESSION_TOKEN"]}}, nil
}

// Entry é um item de uma listagem: objeto (Key, Size, ModTime) ou "diretório" (prefixo
// comum, IsDir, Key terminando em /).
type Entry struct {
	Key     string
	Size    int64
	ModTime time.Time
	IsDir   bool
}

// Error é a recusa do S3, com o código do XML (NoSuchBucket, AccessDenied...).
type Error struct {
	Status  int
	Code    string
	Message string
}

// Error junta status, código e mensagem.
func (e *Error) Error() string {
	return fmt.Sprintf("s3: %d %s: %s", e.Status, e.Code, e.Message)
}

// Is faz 404 casar com ErrNotFound (errors.Is).
func (e *Error) Is(target error) bool {
	return target == ErrNotFound && e.Status == http.StatusNotFound
}

// request monta, assina e manda; body nil é corpo vazio. Resposta fora de 2xx vira *Error.
func (s *Store) request(ctx context.Context, method, key string, query url.Values, hdr http.Header, body io.ReadSeeker, hash string) (*http.Response, error) {
	u := *s.Endpoint
	u.Path = strings.TrimRight(u.Path, "/") + "/" + s.Bucket
	if key != "" {
		u.Path += "/" + key
	}
	u.RawPath = encodePath(u.Path)
	u.RawQuery = query.Encode()
	var rd io.Reader
	if body != nil {
		rd = body
	}
	req, err := http.NewRequestWithContext(ctx, method, u.String(), rd)
	if err != nil {
		return nil, err
	}
	for k, v := range hdr {
		req.Header[k] = v
	}
	// O net/http só deduz o tamanho de bytes.Reader e afins; com *os.File mandaria chunked,
	// que o S3 recusa no PUT. O tamanho vem do Put, pelo cabeçalho, e vira o campo da requisição.
	if cl := req.Header.Get("Content-Length"); cl != "" {
		req.ContentLength, _ = strconv.ParseInt(cl, 10, 64)
		req.Header.Del("Content-Length")
	}
	if hash == "" {
		hash = emptyHash
	}
	now := time.Now
	if s.now != nil {
		now = s.now
	}
	sign(req, hash, s.Creds, s.Region, now())
	resp, err := s.HTTP.Do(req)
	if err != nil {
		return nil, err
	}
	if resp.StatusCode/100 != 2 {
		defer resp.Body.Close()
		return nil, readError(resp.StatusCode, resp.Body)
	}
	return resp, nil
}

// readError lê o <Error> do S3; corpo vazio (HEAD) fica só com o status.
func readError(status int, r io.Reader) error {
	var x struct {
		Code    string `xml:"Code"`
		Message string `xml:"Message"`
	}
	b, _ := io.ReadAll(io.LimitReader(r, 64<<10))
	xml.Unmarshal(b, &x)
	if x.Code == "" {
		x.Code = http.StatusText(status)
	}
	return &Error{Status: status, Code: x.Code, Message: x.Message}
}

// List devolve o que está logo abaixo de prefix (delimitador "/"): objetos e subprefixos,
// seguindo a paginação até o fim.
func (s *Store) List(ctx context.Context, prefix string) ([]Entry, error) {
	var out []Entry
	token := ""
	for {
		q := url.Values{"list-type": {"2"}, "delimiter": {"/"}, "prefix": {prefix}}
		if token != "" {
			q.Set("continuation-token", token)
		}
		resp, err := s.request(ctx, http.MethodGet, "", q, nil, nil, "")
		if err != nil {
			return nil, err
		}
		var page struct {
			Contents []struct {
				Key          string    `xml:"Key"`
				Size         int64     `xml:"Size"`
				LastModified time.Time `xml:"LastModified"`
			} `xml:"Contents"`
			CommonPrefixes []struct {
				Prefix string `xml:"Prefix"`
			} `xml:"CommonPrefixes"`
			IsTruncated           bool   `xml:"IsTruncated"`
			NextContinuationToken string `xml:"NextContinuationToken"`
		}
		err = xml.NewDecoder(resp.Body).Decode(&page)
		resp.Body.Close()
		if err != nil {
			return nil, fmt.Errorf("s3 list: %w", err)
		}
		for _, c := range page.Contents {
			out = append(out, Entry{Key: c.Key, Size: c.Size, ModTime: c.LastModified})
		}
		for _, p := range page.CommonPrefixes {
			out = append(out, Entry{Key: p.Prefix, IsDir: true})
		}
		if !page.IsTruncated || page.NextContinuationToken == "" {
			return out, nil
		}
		token = page.NextContinuationToken
	}
}

// Head devolve tamanho e data do objeto; ErrNotFound se não existe.
func (s *Store) Head(ctx context.Context, key string) (Entry, error) {
	resp, err := s.request(ctx, http.MethodHead, key, nil, nil, nil, "")
	if err != nil {
		return Entry{}, err
	}
	resp.Body.Close()
	t, _ := http.ParseTime(resp.Header.Get("Last-Modified"))
	return Entry{Key: key, Size: resp.ContentLength, ModTime: t}, nil
}

// Get lê n bytes a partir de off (n <= 0 lê até o fim).
func (s *Store) Get(ctx context.Context, key string, off, n int64) (io.ReadCloser, error) {
	h := http.Header{}
	switch {
	case n > 0:
		h.Set("Range", "bytes="+strconv.FormatInt(off, 10)+"-"+strconv.FormatInt(off+n-1, 10))
	case off > 0:
		h.Set("Range", "bytes="+strconv.FormatInt(off, 10)+"-")
	}
	resp, err := s.request(ctx, http.MethodGet, key, nil, h, nil, "")
	if err != nil {
		return nil, err
	}
	return resp.Body, nil
}

// Put grava o objeto com o conteúdo de body. O hash do corpo é calculado antes (uma leitura
// a mais) porque o S3 exige o SHA-256 real em http, onde UNSIGNED-PAYLOAD é recusado.
func (s *Store) Put(ctx context.Context, key string, body io.ReadSeeker) error {
	h := sha256.New()
	size, err := io.Copy(h, body)
	if err != nil {
		return err
	}
	if _, err := body.Seek(0, io.SeekStart); err != nil {
		return err
	}
	hdr := http.Header{"Content-Length": {strconv.FormatInt(size, 10)}}
	if size == 0 {
		// Corpo não nulo com tamanho 0 é "tamanho desconhecido" para o net/http (chunked).
		body = nil
	}
	resp, err := s.request(ctx, http.MethodPut, key, nil, hdr, body, hex.EncodeToString(h.Sum(nil)))
	if err != nil {
		return err
	}
	resp.Body.Close()
	return nil
}

// PutFile grava o objeto com o conteúdo de um arquivo local.
func (s *Store) PutFile(ctx context.Context, key, path string) error {
	f, err := os.Open(path)
	if err != nil {
		return err
	}
	defer f.Close()
	return s.Put(ctx, key, f)
}

// Delete apaga o objeto; apagar o que não existe não é erro (como no S3).
func (s *Store) Delete(ctx context.Context, key string) error {
	resp, err := s.request(ctx, http.MethodDelete, key, nil, nil, nil, "")
	if err != nil {
		return err
	}
	resp.Body.Close()
	return nil
}

// Copy copia src para dst no próprio servidor (CopyObject). O S3 pode responder 200 com um
// <Error> no corpo quando a cópia falha no meio: isso também é erro, e quem renomeia não
// apaga a origem.
func (s *Store) Copy(ctx context.Context, src, dst string) error {
	hdr := http.Header{"X-Amz-Copy-Source": {"/" + s.Bucket + "/" + uriEncode(src, false)}}
	resp, err := s.request(ctx, http.MethodPut, dst, nil, hdr, nil, "")
	if err != nil {
		return err
	}
	defer resp.Body.Close()
	b, _ := io.ReadAll(io.LimitReader(resp.Body, 64<<10))
	if i := bytes.Index(b, []byte("<Error>")); i >= 0 {
		// O <Error> pode vir aninhado no <CopyObjectResult>: lê a partir dele.
		return readError(http.StatusInternalServerError, bytes.NewReader(b[i:]))
	}
	return nil
}
