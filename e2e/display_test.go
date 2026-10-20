//go:build e2e

package e2e

import (
	"image/jpeg"
	"net/http"
	"strconv"
	"strings"
	"testing"
	"time"
)

// TestMachineDisplay trava a tela e a entrada de uma máquina:
//   - o modetest do guest acende a tela e o quadro chega ao visualizador do host em JPEG, com as
//     barras SMPTE nas cores certas (o formato de pixel foi convertido);
//   - teclado e tablet aparecem no guest com os nomes do rc;
//   - uma tecla e um clique mandados por POST /input chegam ao evdev do guest, cada um no seu
//     pacote (a tecla não some).
func TestMachineDisplay(t *testing.T) {
	needInternet(t) // apk add libdrm-tests evtest
	name := uniqueName("disp")
	port := freeHostPort(t)
	base := "http://127.0.0.1:" + strconv.Itoa(port)
	defer rcMachine(t, "delete", "--name", name, "--force")
	mustMachine(t, "create", "--name", name, "--image", "alpine:latest", "--gpu", "--net",
		"--display", "1024x768", "--display-port", strconv.Itoa(port))
	mustMachine(t, "start", "--name", name)
	out := mustMachine(t, "exec", "--name", name, "--", "sh", "-c",
		"apk add -q --no-cache libdrm-tests evtest >/dev/null 2>&1; for d in /sys/class/input/event*; do cat $d/device/name; done")
	if !strings.Contains(out, "runner.code keyboard") || !strings.Contains(out, "runner.code tablet") {
		t.Fatalf("input devices: %q", out)
	}
	mustMachine(t, "exec", "--name", name, "--", "sh", "-c",
		"c=$(modetest -M virtio_gpu -c | awk '/connected/ {print $1; exit}'); (sleep 60 | modetest -M virtio_gpu -s $c:1024x768 >/dev/null 2>&1 &); sleep 2")
	var resp *http.Response
	waitFor(t, "a frame on the viewer", 15*time.Second, func() bool {
		r, err := http.Get(base + "/frame?after=0")
		if err != nil || r.StatusCode != 200 {
			return false
		}
		resp = r
		return true
	})
	img, err := jpeg.Decode(resp.Body)
	resp.Body.Close()
	if err != nil || img.Bounds().Dx() != 1024 || img.Bounds().Dy() != 768 {
		t.Fatalf("frame: %v %v", img.Bounds(), err)
	}
	// Barra amarela (2ª de 7) e azul (7ª) do padrão SMPTE no alto da tela.
	near := func(x, y int, wr, wg, wb uint32) bool {
		r, g, b, _ := img.At(x, y).RGBA()
		d := func(a, b uint32) uint32 {
			if a > b {
				return a - b
			}
			return b - a
		}
		return d(r>>8, wr) < 40 && d(g>>8, wg) < 40 && d(b>>8, wb) < 40
	}
	if !near(220, 100, 192, 192, 0) || !near(950, 100, 0, 0, 192) {
		t.Fatal("the SMPTE bars must reach the viewer with the right colors")
	}

	done := make(chan string, 1)
	go func() {
		out, _, _ := rcMachine(t, "exec", "--name", name, "--", "sh", "-c",
			"(timeout 6 evtest /dev/input/event0 > /tmp/k.log 2>&1 &); (timeout 6 evtest /dev/input/event1 > /tmp/m.log 2>&1 &); sleep 7; grep -h 'Event: time' /tmp/k.log /tmp/m.log")
		done <- out
	}()
	time.Sleep(2 * time.Second)
	for _, body := range []string{
		`[{"type":1,"code":30,"value":1},{"type":0,"code":0,"value":0},{"type":1,"code":30,"value":0},{"type":0,"code":0,"value":0}]`,
		`[{"type":3,"code":0,"value":16000},{"type":3,"code":1,"value":8000},{"type":1,"code":272,"value":1},{"type":0,"code":0,"value":0}]`,
	} {
		r, err := http.Post(base+"/input", "application/json", strings.NewReader(body))
		if err != nil || r.StatusCode != 204 {
			t.Fatalf("input: %v %v", r, err)
		}
		r.Body.Close()
	}
	got := <-done
	for _, want := range []string{"(KEY_A), value 1", "(KEY_A), value 0", "(ABS_X), value 16000", "(BTN_LEFT), value 1"} {
		if !strings.Contains(got, want) {
			t.Fatalf("guest evdev is missing %q:\n%s", want, got)
		}
	}
	// A tecla que desce e a que sobe vêm em pacotes separados (um SYN_REPORT entre elas).
	down := strings.Index(got, "(KEY_A), value 1")
	up := strings.Index(got, "(KEY_A), value 0")
	if down < 0 || up < down || !strings.Contains(got[down:up], "SYN_REPORT") {
		t.Fatal("key down and up must be separate packets")
	}
	mustMachine(t, "stop", "--name", name)
}
