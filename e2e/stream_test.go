//go:build e2e

package e2e

import (
	"bufio"
	"bytes"
	"context"
	"encoding/json"
	"net/http"
	"os/exec"
	"strings"
	"syscall"
	"testing"
	"time"
)

// streamProgram escreve, dorme 1,5 s e escreve de novo. Sem streaming as duas linhas
// chegam juntas no fim; com streaming a primeira chega pelo menos 1 s antes da segunda.
const streamProgram = "import time\nprint('early')\ntime.sleep(1.5)\nprint('late')\n"

// minStreamGap é a distância mínima entre a chegada das duas linhas que prova a entrega
// antecipada: 1,5 s de sleep menos 0,5 s de folga para host carregado.
const minStreamGap = time.Second

// TestRunStreams trava o streaming do `rc run`: a linha escrita antes do sleep aparece no
// stdout do host enquanto o programa ainda roda, e o rc continua o do programa.
func TestRunStreams(t *testing.T) {
	ctx, cancel := context.WithTimeout(context.Background(), 60*time.Second)
	defer cancel()
	cmd := exec.CommandContext(ctx, bin(t), "run", "--stack", "python", "--code", streamProgram)
	out, err := cmd.StdoutPipe()
	if err != nil {
		t.Fatal(err)
	}
	var errb bytes.Buffer
	cmd.Stderr = &errb
	if err := cmd.Start(); err != nil {
		t.Fatal(err)
	}
	seen := map[string]time.Time{}
	sc := bufio.NewScanner(out)
	for sc.Scan() {
		seen[sc.Text()] = time.Now()
	}
	if err := cmd.Wait(); err != nil {
		t.Fatalf("rc run failed: %v\n%s", err, errb.String())
	}
	early, ok1 := seen["early"]
	late, ok2 := seen["late"]
	if !ok1 || !ok2 {
		t.Fatalf("missing lines: %v stderr=%s", seen, errb.String())
	}
	if gap := late.Sub(early); gap < minStreamGap {
		t.Fatalf("'early' arrived only %v before 'late': the CLI is not streaming", gap)
	}
	if !strings.Contains(errb.String(), "[status=success exit=0") {
		t.Fatalf("status line missing: %s", errb.String())
	}
}

// TestServeStream trava a rota SSE com a VM real: o evento da primeira linha chega antes
// do fim, o `result` fecha com exit 0, e job recusado responde 400 antes de abrir o stream.
func TestServeStream(t *testing.T) {
	addr := "127.0.0.1:18182"
	cmd := exec.Command(bin(t), "serve", "--listen", addr, "--pool-size", "1")
	var logs bytes.Buffer
	cmd.Stderr = &logs
	if err := cmd.Start(); err != nil {
		t.Fatal(err)
	}
	defer cmd.Process.Kill()
	url := "http://" + addr
	ready := false
	for end := time.Now().Add(60 * time.Second); time.Now().Before(end); time.Sleep(100 * time.Millisecond) {
		r, err := http.Get(url + "/health")
		if err != nil {
			continue
		}
		var h struct{ Pool map[string]int }
		json.NewDecoder(r.Body).Decode(&h)
		r.Body.Close()
		if h.Pool["python"] >= 1 {
			ready = true
			break
		}
	}
	if !ready {
		t.Fatalf("python pool did not fill: %s", logs.String())
	}

	r, err := http.Post(url+"/api/v1/execute/stream", "application/json", strings.NewReader(`{"language":"cobol","code":"x"}`))
	if err != nil {
		t.Fatal(err)
	}
	r.Body.Close()
	if r.StatusCode != 400 {
		t.Fatalf("rejected job must be 400 before streaming: %d", r.StatusCode)
	}

	body, _ := json.Marshal(map[string]interface{}{"language": "python", "code": streamProgram, "timeout": 10})
	r, err = http.Post(url+"/api/v1/execute/stream", "application/json", bytes.NewReader(body))
	if err != nil {
		t.Fatal(err)
	}
	defer r.Body.Close()
	if r.StatusCode != 200 || r.Header.Get("Content-Type") != "text/event-stream" {
		t.Fatalf("stream response: %d %v", r.StatusCode, r.Header)
	}
	type event struct {
		name, data string
		at         time.Time
	}
	var evs []event
	var cur event
	sc := bufio.NewScanner(r.Body)
	for sc.Scan() {
		line := sc.Text()
		switch {
		case strings.HasPrefix(line, "event: "):
			cur = event{name: strings.TrimPrefix(line, "event: ")}
		case strings.HasPrefix(line, "data: "):
			cur.data, cur.at = strings.TrimPrefix(line, "data: "), time.Now()
		case line == "":
			evs = append(evs, cur)
		}
	}
	var early, late time.Time
	var stdout strings.Builder
	for _, e := range evs {
		if e.name != "stdout" {
			continue
		}
		var s string
		json.Unmarshal([]byte(e.data), &s)
		stdout.WriteString(s)
		if strings.Contains(s, "early") && early.IsZero() {
			early = e.at
		}
		if strings.Contains(s, "late") {
			late = e.at
		}
	}
	if stdout.String() != "early\nlate\n" || early.IsZero() || late.IsZero() {
		t.Fatalf("streamed stdout %q events %+v", stdout.String(), evs)
	}
	if gap := late.Sub(early); gap < minStreamGap {
		t.Fatalf("'early' event arrived only %v before 'late': the API is not streaming", gap)
	}
	last := evs[len(evs)-1]
	if last.name != "result" || !strings.Contains(last.data, `"status":"success"`) || !strings.Contains(last.data, `"exitCode":0`) {
		t.Fatalf("final event: %+v", last)
	}
	cmd.Process.Signal(syscall.SIGTERM)
	cmd.Wait()
	for end := time.Now().Add(5 * time.Second); time.Now().Before(end) && vmmAlive(); time.Sleep(100 * time.Millisecond) {
	}
	if vmmAlive() {
		t.Fatal("VM survived the serve SIGTERM")
	}
}
