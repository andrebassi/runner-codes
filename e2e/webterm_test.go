//go:build e2e

package e2e

import (
	"bufio"
	"bytes"
	"context"
	"encoding/base64"
	"net/http"
	"os/exec"
	"regexp"
	"strings"
	"sync"
	"testing"
	"time"
)

// lockedBuffer guarda o stderr do rc enquanto o teste procura a URL nele.
type lockedBuffer struct {
	mu sync.Mutex
	b  bytes.Buffer
}

// Write grava sob o lock.
func (l *lockedBuffer) Write(p []byte) (int, error) {
	l.mu.Lock()
	defer l.mu.Unlock()
	return l.b.Write(p)
}

// String lê sob o lock.
func (l *lockedBuffer) String() string { l.mu.Lock(); defer l.mu.Unlock(); return l.b.String() }

// TestWebTerminal trava o terminal web numa VM real: URL só em 127.0.0.1 com token, o PTY do guest com o tamanho
// inicial e o do resize, o teclado chegando ao programa, o fim com o código dele, e o token exigido de verdade.
func TestWebTerminal(t *testing.T) {
	name := uniqueName("wt")
	defer rcMachine(t, "delete", "--name", name, "--force")
	mustMachine(t, "create", "--name", name, "--stack", "python")
	mustMachine(t, "start", "--name", name)

	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
	defer cancel()
	// O programa mostra o tamanho do PTY, espera o resize chegar, lê uma linha e sai com 5.
	script := `stty size; echo pronto; sleep 1; stty size; read x; echo recebido:$x; exit 5`
	cmd := exec.CommandContext(ctx, bin(t), "machine", "web-terminal", "--name", name, "--no-open", "--", "sh", "-c", script)
	var stderr lockedBuffer
	cmd.Stderr = &stderr
	if err := cmd.Start(); err != nil {
		t.Fatal(err)
	}
	waitErr := make(chan error, 1)
	go func() { waitErr <- cmd.Wait() }()

	urlRe := regexp.MustCompile(`http://127\.0\.0\.1:\d+/\?t=[0-9a-f]{32}`)
	var url string
	for i := 0; i < 200 && url == ""; i++ {
		url = urlRe.FindString(stderr.String())
		time.Sleep(50 * time.Millisecond)
	}
	if url == "" {
		t.Fatalf("URL não impressa: %q", stderr.String())
	}
	base, token := url[:strings.Index(url, "/?")], url[strings.Index(url, "t=")+2:]

	// Sem token, nada: é um shell na máquina.
	if resp, err := http.Get(base + "/stream"); err != nil || resp.StatusCode != http.StatusForbidden {
		t.Fatalf("stream sem token: %v %v", resp, err)
	}

	resp, err := http.Get(base + "/stream?t=" + token)
	if err != nil {
		t.Fatal(err)
	}
	defer resp.Body.Close()
	var screen strings.Builder
	lines := make(chan string, 64)
	go func() {
		sc := bufio.NewScanner(resp.Body)
		event := ""
		for sc.Scan() {
			l := sc.Text()
			switch {
			case strings.HasPrefix(l, "event: "):
				event = strings.TrimPrefix(l, "event: ")
			case strings.HasPrefix(l, "data: "):
				d := strings.TrimPrefix(l, "data: ")
				if event == "exit" {
					lines <- "[exit " + d + "]"
				} else if b, err := base64.StdEncoding.DecodeString(d); err == nil {
					lines <- string(b)
				}
			case l == "":
				event = ""
			}
		}
		close(lines)
	}()
	// waitFor acumula a tela até aparecer want.
	waitFor := func(want string) {
		t.Helper()
		deadline := time.After(30 * time.Second)
		for !strings.Contains(screen.String(), want) {
			select {
			case l, ok := <-lines:
				if !ok {
					t.Fatalf("SSE fechou sem %q; tela: %q", want, screen.String())
				}
				screen.WriteString(l)
			case <-deadline:
				t.Fatalf("sem %q; tela: %q", want, screen.String())
			}
		}
	}
	waitFor("32 120")
	waitFor("pronto")
	post := func(path, ctype, body string) {
		r, err := http.Post(base+path+"?t="+token, ctype, strings.NewReader(body))
		if err != nil || r.StatusCode != http.StatusNoContent {
			t.Fatalf("%s: %v %v", path, r, err)
		}
		r.Body.Close()
	}
	post("/resize", "application/json", `{"rows":40,"cols":100}`)
	waitFor("40 100")
	post("/input", "text/plain", "abc\r")
	waitFor("recebido:abc")
	waitFor(`[exit {"code":5}]`)

	select {
	case err := <-waitErr:
		ee, ok := err.(*exec.ExitError)
		if !ok || ee.ExitCode() != 5 {
			t.Fatalf("rc do web-terminal: %v (queria 5)", err)
		}
	case <-time.After(30 * time.Second):
		t.Fatal("rc machine web-terminal não saiu depois do programa")
	}
	// A máquina segue ligada: o terminal é uma sessão, não a VM.
	if st := stateOf(t, name); st != "running" {
		t.Fatalf("estado depois do terminal: %s", st)
	}
}

// TestWebTerminalPortForward: um servidor que o programa sobe dentro da VM aparece em /ports e responde no 127.0.0.1
// do host; quando ele cai, a porta some. Sem --net: o caminho é o vsock, não a rede da máquina.
func TestWebTerminalPortForward(t *testing.T) {
	name := uniqueName("wtp")
	defer rcMachine(t, "delete", "--name", name, "--force")
	mustMachine(t, "create", "--name", name, "--stack", "python")
	mustMachine(t, "start", "--name", name)
	ctx, cancel := context.WithTimeout(context.Background(), 2*time.Minute)
	defer cancel()
	// Dois servidores por 8 s: um em todas as interfaces (8123) e um só em [::1] (8124), o caso do "localhost" do Node
	// 17+. Depois eles caem; o sleep final deixa ver as portas sumirem.
	script := `mkdir -p /tmp/www && echo do-guest > /tmp/www/index.html && cd /tmp/www && ` +
		`(timeout 8 python3 -m http.server 8124 --bind ::1 &) && timeout 8 python3 -m http.server 8123; sleep 6`
	cmd := exec.CommandContext(ctx, bin(t), "machine", "web-terminal", "--name", name, "--no-open", "--", "sh", "-c", script)
	var stderr lockedBuffer
	cmd.Stderr = &stderr
	if err := cmd.Start(); err != nil {
		t.Fatal(err)
	}
	defer cmd.Wait()
	urlRe := regexp.MustCompile(`http://127\.0\.0\.1:\d+/\?t=[0-9a-f]{32}`)
	var url string
	for i := 0; i < 200 && url == ""; i++ {
		url = urlRe.FindString(stderr.String())
		time.Sleep(50 * time.Millisecond)
	}
	if url == "" {
		t.Fatalf("URL não impressa: %q", stderr.String())
	}
	base, token := url[:strings.Index(url, "/?")], url[strings.Index(url, "t=")+2:]
	// forwards lê /ports.
	forwards := func() string {
		r, err := http.Get(base + "/ports?t=" + token)
		if err != nil {
			return ""
		}
		defer r.Body.Close()
		var b bytes.Buffer
		b.ReadFrom(r.Body)
		return b.String()
	}
	for _, guest := range []string{"8123", "8124"} {
		hostRe := regexp.MustCompile(`"guest":` + guest + `,"host":(\d+)`)
		var host string
		for i := 0; i < 60 && host == ""; i++ {
			if m := hostRe.FindStringSubmatch(forwards()); m != nil {
				host = m[1]
			}
			time.Sleep(200 * time.Millisecond)
		}
		if host == "" {
			t.Fatalf("a porta %s não apareceu em /ports: %s", guest, forwards())
		}
		r, err := http.Get("http://127.0.0.1:" + host + "/")
		if err != nil {
			t.Fatalf("porta %s: %v", guest, err)
		}
		var body bytes.Buffer
		body.ReadFrom(r.Body)
		r.Body.Close()
		if r.StatusCode != 200 || !strings.Contains(body.String(), "do-guest") {
			t.Fatalf("GET pelo host na %s: %d %q", guest, r.StatusCode, body.String())
		}
	}
	gone := false
	for i := 0; i < 60 && !gone; i++ {
		gone = !strings.Contains(forwards(), `"guest":8123`)
		time.Sleep(250 * time.Millisecond)
	}
	if !gone {
		t.Fatalf("a porta não sumiu depois de o servidor cair: %s", forwards())
	}
}

// TestWebTerminalStoppedMachine: máquina parada é recusada antes de abrir servidor e navegador.
func TestWebTerminalStoppedMachine(t *testing.T) {
	name := uniqueName("wts")
	defer rcMachine(t, "delete", "--name", name, "--force")
	mustMachine(t, "create", "--name", name, "--stack", "python")
	_, errOut, rc := rcMachine(t, "web-terminal", "--name", name, "--no-open")
	if rc == 0 || !strings.Contains(errOut, "start it first") {
		t.Fatalf("rc=%d stderr=%q", rc, errOut)
	}
}
