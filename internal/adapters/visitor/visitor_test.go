package visitor

import (
	"context"
	"errors"
	"fmt"
	"io"
	"net"
	"net/http"
	"net/http/httptest"
	"os"
	"path/filepath"
	"strings"
	"testing"

	"runner.codes/internal/adapters/webterm"
	"runner.codes/internal/ports"
)

// TestRules: o DNS do Fly é liberado ANTES do bloqueio da rede privada, e todo bloqueio é só de abertura (SYN) ou UDP
// — nunca de saída TCP inteira, que cortaria as respostas do rc-visitor ao front.
func TestRules(t *testing.T) {
	rules := Rules()
	text := make([]string, len(rules))
	for i, r := range rules {
		text[i] = strings.Join(r, " ")
	}
	all := strings.Join(text, "\n")
	dns := strings.Index(all, "-d fdaa::3 -p udp --dport 53 -j ACCEPT")
	block := strings.Index(all, "-d fc00::/7")
	if dns < 0 || block < 0 || dns > block {
		t.Fatalf("ordem DNS/bloqueio:\n%s", all)
	}
	for _, want := range []string{"fe80::/10", "10.0.0.0/8", "172.16.0.0/12", "192.168.0.0/16", "100.64.0.0/10", "169.254.0.0/16"} {
		if !strings.Contains(all, "-d "+want+" -p tcp --syn") || !strings.Contains(all, "-d "+want+" -p udp -j REJECT") {
			t.Errorf("faltou bloquear %s", want)
		}
	}
	for _, l := range text {
		if strings.Contains(l, "REJECT") && !strings.Contains(l, "--syn") && !strings.Contains(l, "-p udp") {
			t.Errorf("regra corta resposta TCP: %s", l)
		}
		if strings.Contains(l, "owner") {
			t.Errorf("o kernel do Fly não tem o módulo owner: %s", l)
		}
	}
}

// TestApplyFirewall: aplica todas em ordem; a primeira que falha para tudo e aparece no erro.
func TestApplyFirewall(t *testing.T) {
	var ran int
	if err := ApplyFirewall(func([]string) error { ran++; return nil }); err != nil || ran != len(Rules()) {
		t.Fatalf("ran=%d err=%v", ran, err)
	}
	ran = 0
	err := ApplyFirewall(func(argv []string) error {
		ran++
		if ran == 3 {
			return errors.New("sem módulo")
		}
		return nil
	})
	if err == nil || ran != 3 || !strings.Contains(err.Error(), "sem módulo") || !strings.Contains(err.Error(), "ip6tables") {
		t.Fatalf("ran=%d err=%v", ran, err)
	}
}

// TestFilterEnv: as variáveis RC_* (tokens) não descem ao shell; o resto desce.
func TestFilterEnv(t *testing.T) {
	got := FilterEnv([]string{"RC_TOKEN=x", "PATH=/bin", "RC_PORTS_TOKEN=y", "HOME=/data"})
	if strings.Join(got, ",") != "PATH=/bin,HOME=/data" {
		t.Fatalf("%v", got)
	}
}

// TestPortsList: lê tcp e tcp6, junta, ordena e tira as portas do próprio rc-visitor; sem tcp é erro; sem tcp6 vale.
func TestPortsList(t *testing.T) {
	dir := t.TempDir()
	os.MkdirAll(filepath.Join(dir, "net"), 0o755)
	// 0BB8 = 3000 em 0.0.0.0; 1E01 = 7681 (o terminal, excluído); 1F90 = 8080 em ::1 no tcp6.
	tcp := "  sl  local_address rem_address   st\n   0: 00000000:0BB8 00000000:0000 0A\n   1: 00000000:1E01 00000000:0000 0A\n"
	tcp6 := "  sl  local_address rem_address   st\n   0: 00000000000000000000000001000000:1F90 00000000000000000000000000000000:0000 0A\n"
	os.WriteFile(filepath.Join(dir, "net/tcp"), []byte(tcp), 0o644)
	p := Ports{ProcRoot: dir, Exclude: []uint16{7681, 7682}}
	if got, err := p.List(context.Background()); err != nil || fmt.Sprint(got) != "[3000]" {
		t.Fatalf("sem tcp6: %v %v", got, err)
	}
	os.WriteFile(filepath.Join(dir, "net/tcp6"), []byte(tcp6), 0o644)
	if got, _ := p.List(context.Background()); fmt.Sprint(got) != "[3000 8080]" {
		t.Fatalf("com tcp6: %v", got)
	}
	if _, err := (Ports{ProcRoot: t.TempDir()}).List(context.Background()); err == nil {
		t.Fatal("sem /proc/net/tcp passou")
	}
}

// TestDial: acha o servidor em 127.0.0.1 e o preso ao ::1; porta fechada nos dois é erro.
func TestDial(t *testing.T) {
	for _, addr := range []string{"127.0.0.1:0", "[::1]:0"} {
		ln, err := net.Listen("tcp", addr)
		if err != nil {
			t.Skip("sem " + addr)
		}
		port := uint16(ln.Addr().(*net.TCPAddr).Port)
		c, err := Dial(context.Background(), port)
		if err != nil {
			t.Fatalf("%s: %v", addr, err)
		}
		c.Close()
		ln.Close()
		if _, err := Dial(context.Background(), port); err == nil {
			t.Fatalf("%s fechada atendeu", addr)
		}
	}
}

// TestPortBridge: token certo chega à app com o Host original e sem os cabeçalhos da ponte; token errado é 403;
// porta inválida é 400; nada escutando é 502.
func TestPortBridge(t *testing.T) {
	app := httptest.NewServer(http.HandlerFunc(func(w http.ResponseWriter, r *http.Request) {
		fmt.Fprintf(w, "%s %s port=%q tok=%q xff=%q", r.Host, r.URL.Path, r.Header.Get("X-Rc-Port"), r.Header.Get("X-Rc-Token"), r.Header.Get("X-Forwarded-For"))
	}))
	defer app.Close()
	dial := func(ctx context.Context, port uint16) (net.Conn, error) {
		if port == 9 {
			return nil, errors.New("refused")
		}
		return (&net.Dialer{}).DialContext(ctx, "tcp", app.Listener.Addr().String())
	}
	h := PortBridge("tok", "X-Rc-Port", "X-Rc-Token", dial)
	call := func(tok, port string) (int, string) {
		req := httptest.NewRequest("GET", "http://3000-abc.example.com/docs", nil)
		req.Header.Set("X-Rc-Token", tok)
		req.Header.Set("X-Rc-Port", port)
		req.Header.Set("X-Forwarded-For", "203.0.113.7")
		rec := httptest.NewRecorder()
		h.ServeHTTP(rec, req)
		b, _ := io.ReadAll(rec.Body)
		return rec.Code, string(b)
	}
	if code, body := call("tok", "3000"); code != 200 || body != `3000-abc.example.com /docs port="" tok="" xff="203.0.113.7"` {
		t.Fatalf("ponte: %d %s", code, body)
	}
	for _, c := range []struct {
		tok, port string
		want      int
	}{{"errado", "3000", 403}, {"", "3000", 403}, {"tok", "x", 400}, {"tok", "0", 400}, {"tok", "70000", 400}, {"tok", "9", 502}} {
		if code, _ := call(c.tok, c.port); code != c.want {
			t.Errorf("%+v: %d", c, code)
		}
	}
}

// TestHolder: o mesmo hub enquanto o programa roda; um novo quando ele sai.
func TestHolder(t *testing.T) {
	ctx, cancel := context.WithCancel(context.Background())
	defer cancel()
	exit := make(chan struct{})
	made := 0
	h := &Holder{New: func() *webterm.Hub {
		made++
		sh := func(ctx context.Context, _ ports.Winsize, _ io.Reader, _ io.Writer, _ <-chan ports.Winsize) (int, error) {
			select {
			case <-exit:
			case <-ctx.Done():
			}
			return 0, nil
		}
		return webterm.NewHub(ctx, "t", "m", webterm.Start(ctx, sh, ports.Winsize{}), "claude", nil, nil)
	}}
	state := func() int {
		rec := httptest.NewRecorder()
		h.ServeHTTP(rec, httptest.NewRequest("GET", "http://127.0.0.1/state?t=t", nil))
		return rec.Code
	}
	if state() != 200 || state() != 200 || made != 1 {
		t.Fatalf("made=%d", made)
	}
	old := h.hub
	close(exit)
	for !old.MainEnded() {
	}
	if state() != 200 || made != 2 {
		t.Fatalf("depois de sair: made=%d", made)
	}
}

// TestLockFlyAPI: o /.fly e o que há dentro ficam só para o root; sem /.fly não é erro; caminho ilegível é erro.
func TestLockFlyAPI(t *testing.T) {
	root := t.TempDir()
	if err := LockFlyAPI(root); err != nil {
		t.Fatalf("sem /.fly: %v", err)
	}
	os.MkdirAll(filepath.Join(root, ".fly"), 0o755)
	os.WriteFile(filepath.Join(root, ".fly/api"), nil, 0o666)
	if err := LockFlyAPI(root); err != nil {
		t.Fatal(err)
	}
	d, _ := os.Stat(filepath.Join(root, ".fly"))
	f, _ := os.Stat(filepath.Join(root, ".fly/api"))
	if d.Mode().Perm() != 0o700 || f.Mode().Perm() != 0o600 {
		t.Fatalf("modos %v %v", d.Mode().Perm(), f.Mode().Perm())
	}
	os.Chmod(filepath.Join(root, ".fly"), 0o000)
	defer os.Chmod(filepath.Join(root, ".fly"), 0o700)
	if os.Geteuid() != 0 {
		if err := LockFlyAPI(root); err == nil {
			t.Fatal("diretório ilegível passou")
		}
	}
}

// TestPortsHideOwners: a porta cujo socket é do processo claude some; a do python (outro processo) fica; processo sem
// cmdline legível e fd que não é socket são ignorados; sem HideOwners nada some.
func TestPortsHideOwners(t *testing.T) {
	dir := t.TempDir()
	os.MkdirAll(filepath.Join(dir, "net"), 0o755)
	// 8BD3 = 35795 (do claude, inode 2530) e 0BB8 = 3000 (do python, inode 777), as duas em LISTEN.
	tcp := "  sl  local_address rem_address   st tx rx tr retr uid timeout inode\n" +
		"   0: 0100007F:8BD3 00000000:0000 0A 0:0 0:0 0 1000 0 2530\n" +
		"   1: 00000000:0BB8 00000000:0000 0A 0:0 0:0 0 1000 0 777\n"
	os.WriteFile(filepath.Join(dir, "net/tcp"), []byte(tcp), 0o644)
	proc := func(pid, argv0 string, links map[string]string) {
		os.MkdirAll(filepath.Join(dir, pid, "fd"), 0o755)
		if argv0 != "" {
			os.WriteFile(filepath.Join(dir, pid, "cmdline"), []byte(argv0+"\x00--dangerously-skip-permissions\x00"), 0o644)
		}
		for fd, target := range links {
			os.Symlink(target, filepath.Join(dir, pid, "fd", fd))
		}
	}
	proc("690", "/bin/claude", map[string]string{"0": "/dev/pts/0", "21": "socket:[2530]"})
	proc("700", "python3", map[string]string{"3": "socket:[777]"})
	proc("800", "", nil)
	p := Ports{ProcRoot: dir, HideOwners: []string{"claude"}}
	if got, err := p.List(context.Background()); err != nil || fmt.Sprint(got) != "[3000]" {
		t.Fatalf("com HideOwners: %v %v", got, err)
	}
	if got, _ := (Ports{ProcRoot: dir}).List(context.Background()); fmt.Sprint(got) != "[3000 35795]" {
		t.Fatalf("sem HideOwners: %v", got)
	}
}
