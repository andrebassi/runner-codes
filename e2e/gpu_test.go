//go:build e2e

package e2e

import (
	"bytes"
	"context"
	"fmt"
	"os/exec"
	"regexp"
	"strconv"
	"strings"
	"testing"
	"time"
)

// runStack executa `rc run` numa stack com flags extras e devolve a saída combinada e o rc.
func runStack(t *testing.T, stack, code string, extra ...string) (string, int) {
	t.Helper()
	ctx, cancel := context.WithTimeout(context.Background(), 5*time.Minute)
	defer cancel()
	args := append([]string{"run", "--stack", stack, "--timeout", "240s", "--code", code}, extra...)
	cmd := exec.CommandContext(ctx, bin(t), args...)
	var out bytes.Buffer
	cmd.Stdout, cmd.Stderr = &out, &out
	err := cmd.Run()
	rc := 0
	if ee, ok := err.(*exec.ExitError); ok {
		rc = ee.ExitCode()
	} else if err != nil {
		t.Fatal(err)
	}
	return out.String(), rc
}

// needLlama pula o teste quando a imagem da stack llama não foi construída neste host.
func needLlama(t *testing.T) {
	t.Helper()
	out, _ := exec.Command(bin(t), "doctor").CombinedOutput()
	if !strings.Contains(string(out), "ok    image llama") {
		t.Skip("llama image not built (rc image build --stack llama)")
	}
}

// TestGPUVulkan trava o caminho da GPU: com --gpu o guest enxerga um device Vulkan servido
// pelo Venus (virtio-gpu → virglrenderer → MoltenVK → Metal); sem --gpu não há /dev/dri.
// O defeito que este teste pega é o do Mesa sem o alinhamento de 16 KiB, que falhava no
// vkCreateInstance com "Error adding memory map" (medido).
func TestGPUVulkan(t *testing.T) {
	needLlama(t)
	out, rc := runStack(t, "llama", "vulkaninfo --summary 2>&1 | grep -E 'deviceName|driverName'", "--gpu")
	if rc != 0 || !strings.Contains(out, "Venus") {
		t.Fatalf("no Venus device with --gpu (rc=%d):\n%s", rc, out)
	}
	out, _ = runStack(t, "llama", "ls /dev/dri 2>&1 || echo NO-DRI")
	if !strings.Contains(out, "NO-DRI") {
		t.Fatalf("a VM without --gpu must not see a GPU:\n%s", out)
	}
	if out, rc := runStack(t, "python", "print(1)", "--gpu"); rc == 0 || !strings.Contains(out, "E007") {
		t.Fatalf("--gpu on a stack without gpu must be refused with E007 (rc=%d):\n%s", rc, out)
	}
}

// benchRate extrai o t/s de uma linha da tabela do llama-bench (pp = prompt, tg = geração).
func benchRate(t *testing.T, out, test string) float64 {
	t.Helper()
	m := regexp.MustCompile(`\|\s*` + test + `\d+\s*\|\s*([0-9.]+)`).FindStringSubmatch(out)
	if m == nil {
		t.Fatalf("no %s line in llama-bench output:\n%s", test, out)
	}
	v, _ := strconv.ParseFloat(m[1], 64)
	return v
}

// TestGPULlama roda o mesmo modelo no llama.cpp com e sem GPU. Com GPU, o device usado tem
// de ser o Venus (a coluna "Vulkan" aparece até no run de CPU, então não basta) e o prompt
// tem de sair mais rápido que na CPU: medido 1361 contra 215 t/s no M1 Pro, folga larga
// para host carregado. A geração (tg) não entra no assert: num modelo de 0,5B a CPU gera
// mais rápido (117 contra 57 t/s), porque cada token paga a ida e volta pelo virtio.
func TestGPULlama(t *testing.T) {
	needLlama(t)
	bench := "llama-bench -m /opt/models/qwen2.5-0.5b-q4_k_m.gguf -p 128 -n 32 -r 1 -ngl %d %s 2>&1"
	gpuOut, rc := runStack(t, "llama", fmt.Sprintf(bench, 99, "-dev Vulkan0"), "--gpu")
	if rc != 0 || !strings.Contains(gpuOut, "Virtio-GPU Venus") || !strings.Contains(gpuOut, "Vulkan0") {
		t.Fatalf("GPU bench did not run on the Venus device (rc=%d):\n%s", rc, gpuOut)
	}
	cpuOut, rc := runStack(t, "llama", fmt.Sprintf(bench, 0, ""))
	if rc != 0 {
		t.Fatalf("CPU bench failed (rc=%d):\n%s", rc, cpuOut)
	}
	gpp, cpp := benchRate(t, gpuOut, "pp"), benchRate(t, cpuOut, "pp")
	gtg, ctg := benchRate(t, gpuOut, "tg"), benchRate(t, cpuOut, "tg")
	fmt.Printf("llama pp128: gpu %.0f t/s, cpu %.0f t/s; tg32: gpu %.1f t/s, cpu %.1f t/s\n", gpp, cpp, gtg, ctg)
	if gpp < 2*cpp {
		t.Fatalf("prompt on GPU (%.0f t/s) is not clearly faster than on CPU (%.0f t/s): the GPU is not doing the work", gpp, cpp)
	}
}
