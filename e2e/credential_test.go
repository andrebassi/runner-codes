//go:build e2e

package e2e

import (
	"strings"
	"testing"
)

// credentialProbe roda no guest: imprime a variável (o que o guest tem) e o que o servidor de
// eco recebeu no Authorization, para a URL dada. Python lê o SSL_CERT_FILE que a máquina
// exporta, sem configuração nenhuma. O User-Agent é próprio porque o Cloudflare na frente do
// postman-echo recusa o "Python-urllib" com 403 (error code 1010, medido do host também).
const credentialProbe = `
import json, os, sys, urllib.request, urllib.error
tok = os.environ["E2E_TOKEN"]
print("guest:", tok)
req = urllib.request.Request(sys.argv[1].replace("TOK", tok), headers={"Authorization": "Bearer " + tok, "User-Agent": "runner-code-e2e/1"})
try:
    body = json.load(urllib.request.urlopen(req, timeout=20))
    headers = {k.lower(): v for k, v in body["headers"].items()}
    print("server:", headers.get("authorization", ""))
except urllib.error.HTTPError as e:
    print("status:", e.code, e.read().decode().strip())
`

// TestMachineCredential trava a substituição de credencial numa VM real, contra um serviço
// de eco HTTPS de verdade (postman-echo.com devolve os cabeçalhos recebidos):
//   - o guest só vê o placeholder; o servidor recebe o valor real, que vem do ambiente de
//     quem deu o start (nunca do guest, do registro ou do disco);
//   - placeholder na URL é recusado com 403 pelo gateway, sem sair;
//   - host fora da binding recebe o placeholder literal (o TLS passa intacto).
func TestMachineCredential(t *testing.T) {
	needInternet(t)
	name := uniqueName("cred")
	defer rcMachine(t, "delete", "--name", name, "--force")
	mustMachine(t, "create", "--name", name, "--stack", "python", "--credential", "tok=E2E_TOKEN@postman-echo.com")
	t.Setenv("E2E_TOKEN", "e2e-value-not-a-secret")
	mustMachine(t, "start", "--name", name)
	probe := func(url string) string {
		out, errOut, _ := rcMachine(t, "exec", "--name", name, "--", "python3", "-c", credentialProbe, url)
		return out + errOut
	}
	out := probe("https://postman-echo.com/headers")
	if !strings.Contains(out, "guest: RC_PLACEHOLDER_TOK_") || strings.Contains(out, "guest: e2e-value") {
		t.Fatalf("the guest must hold only the placeholder: %q", out)
	}
	if !strings.Contains(out, "server: Bearer e2e-value-not-a-secret") {
		t.Fatalf("the server must receive the real value: %q", out)
	}
	if out := probe("https://postman-echo.com/headers?leak=TOK"); !strings.Contains(out, "status: 403") || strings.Contains(out, "e2e-value") {
		t.Fatalf("a placeholder in the URL must be refused: %q", out)
	}
	if out := probe("https://httpbin.org/headers"); strings.Contains(out, "e2e-value") || !strings.Contains(out, "server: Bearer RC_PLACEHOLDER_TOK_") {
		t.Fatalf("another host must get the literal placeholder: %q", out)
	}
	mustMachine(t, "stop", "--name", name)
}
