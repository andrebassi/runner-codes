package main

import (
	"os"
	"path/filepath"
	"testing"
)

// TestServeToken: loopback sem token pode; rede sem token não; o arquivo vence a variável; arquivo
// vazio ou inexistente é erro; endereço sem porta é erro.
func TestServeToken(t *testing.T) {
	dir := t.TempDir()
	good := filepath.Join(dir, "tok")
	empty := filepath.Join(dir, "vazio")
	os.WriteFile(good, []byte(" abc \n"), 0o600)
	os.WriteFile(empty, []byte("\n"), 0o600)
	t.Setenv("RUNNER_API_TOKEN", "")
	for name, c := range map[string]struct {
		file, listen, want string
		fail               bool
	}{
		"loopback aberto":    {"", "127.0.0.1:18093", "", false},
		"localhost aberto":   {"", "localhost:18093", "", false},
		"ipv6 loopback":      {"", "[::1]:18093", "", false},
		"rede sem token":     {"", "10.128.0.9:18093", "", true},
		"todas sem token":    {"", "0.0.0.0:18093", "", true},
		"rede com arquivo":   {good, "10.128.0.9:18093", "abc", false},
		"arquivo vazio":      {empty, "127.0.0.1:1", "", true},
		"arquivo que falta":  {filepath.Join(dir, "nada"), "127.0.0.1:1", "", true},
		"endereço sem porta": {"", "10.0.0.1", "", true},
	} {
		got, err := serveToken(c.file, c.listen)
		if (err != nil) != c.fail || got != c.want {
			t.Errorf("%s: %q %v", name, got, err)
		}
	}
	t.Setenv("RUNNER_API_TOKEN", "da-env")
	if got, err := serveToken("", "10.0.0.1:1"); err != nil || got != "da-env" {
		t.Fatalf("variável: %q %v", got, err)
	}
	if got, _ := serveToken(good, "10.0.0.1:1"); got != "abc" {
		t.Fatalf("arquivo vence a variável: %q", got)
	}
}
