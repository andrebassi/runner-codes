// Package objectstore é um cliente S3 mínimo (AWS, R2, MinIO): o que o volume s3:// do guest
// precisa — listar por prefixo, ler por faixa, gravar, apagar e copiar. Assinatura SigV4 e
// requisição path-style escritas aqui com a biblioteca padrão: o guest é um binário estático,
// e o aws-sdk-go-v2 somaria megabytes para seis chamadas.
package objectstore

import (
	"crypto/hmac"
	"crypto/sha256"
	"encoding/hex"
	"net/http"
	"sort"
	"strings"
	"time"
)

// Credentials são as chaves da máquina (AWS_ACCESS_KEY_ID e afins). Sem AccessKey, a
// requisição vai anônima, sem assinatura: bucket público.
type Credentials struct {
	AccessKey    string
	SecretKey    string
	SessionToken string
}

// emptyHash é o SHA-256 de corpo vazio, o x-amz-content-sha256 de GET, HEAD e DELETE.
const emptyHash = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"

// amzDate é o formato de data do SigV4 (ISO 8601 básico, UTC).
const amzDate = "20060102T150405Z"

// sign assina req com SigV4 (serviço s3) para payloadHash, o SHA-256 hexa do corpo. Assina
// host, range e todo x-amz-*: o mínimo que o S3 exige, e nenhum cabeçalho que um proxy no
// meio possa reescrever.
func sign(req *http.Request, payloadHash string, c Credentials, region string, now time.Time) {
	t := now.UTC()
	req.Header.Set("X-Amz-Date", t.Format(amzDate))
	req.Header.Set("X-Amz-Content-Sha256", payloadHash)
	if c.SessionToken != "" {
		req.Header.Set("X-Amz-Security-Token", c.SessionToken)
	}
	if c.AccessKey == "" {
		return
	}
	headers := map[string]string{"host": req.Host}
	if headers["host"] == "" {
		headers["host"] = req.URL.Host
	}
	for k, v := range req.Header {
		lk := strings.ToLower(k)
		if strings.HasPrefix(lk, "x-amz-") || lk == "range" {
			headers[lk] = strings.TrimSpace(strings.Join(v, ","))
		}
	}
	names := make([]string, 0, len(headers))
	for k := range headers {
		names = append(names, k)
	}
	sort.Strings(names)
	var canonHeaders strings.Builder
	for _, k := range names {
		canonHeaders.WriteString(k + ":" + headers[k] + "\n")
	}
	signed := strings.Join(names, ";")
	canonical := strings.Join([]string{req.Method, encodePath(req.URL.Path), canonicalQuery(req), canonHeaders.String(), signed, payloadHash}, "\n")
	day := t.Format("20060102")
	scope := day + "/" + region + "/s3/aws4_request"
	toSign := "AWS4-HMAC-SHA256\n" + t.Format(amzDate) + "\n" + scope + "\n" + hexSHA256([]byte(canonical))
	key := hmacSHA256([]byte("AWS4"+c.SecretKey), day)
	for _, part := range []string{region, "s3", "aws4_request"} {
		key = hmacSHA256(key, part)
	}
	req.Header.Set("Authorization", "AWS4-HMAC-SHA256 Credential="+c.AccessKey+"/"+scope+
		", SignedHeaders="+signed+", Signature="+hex.EncodeToString(hmacSHA256(key, toSign)))
}

// canonicalQuery ordena a query por chave e valor, com o escape do SigV4.
func canonicalQuery(req *http.Request) string {
	q := req.URL.Query()
	var pairs []string
	for k, vs := range q {
		for _, v := range vs {
			pairs = append(pairs, uriEncode(k, true)+"="+uriEncode(v, true))
		}
	}
	sort.Strings(pairs)
	return strings.Join(pairs, "&")
}

// encodePath escapa cada segmento do caminho, mantendo as barras.
func encodePath(p string) string {
	if p == "" {
		return "/"
	}
	return uriEncode(p, false)
}

// uriEncode é o escape do SigV4: só A-Z a-z 0-9 - _ . ~ ficam; o resto vira %XX maiúsculo.
// encodeSlash decide a barra (escapada na query, mantida no caminho). O url.PathEscape do Go
// deixa passar caracteres como "!" e "$", e a assinatura deixaria de bater com a do servidor.
func uriEncode(s string, encodeSlash bool) string {
	const hexDigits = "0123456789ABCDEF"
	var b strings.Builder
	for i := 0; i < len(s); i++ {
		c := s[i]
		switch {
		case 'A' <= c && c <= 'Z', 'a' <= c && c <= 'z', '0' <= c && c <= '9', c == '-', c == '_', c == '.', c == '~':
			b.WriteByte(c)
		case c == '/' && !encodeSlash:
			b.WriteByte(c)
		default:
			b.WriteByte('%')
			b.WriteByte(hexDigits[c>>4])
			b.WriteByte(hexDigits[c&15])
		}
	}
	return b.String()
}

// hexSHA256 é o SHA-256 em hexa minúsculo.
func hexSHA256(b []byte) string {
	h := sha256.Sum256(b)
	return hex.EncodeToString(h[:])
}

// hmacSHA256 é o HMAC-SHA256 de data com key.
func hmacSHA256(key []byte, data string) []byte {
	m := hmac.New(sha256.New, key)
	m.Write([]byte(data))
	return m.Sum(nil)
}
