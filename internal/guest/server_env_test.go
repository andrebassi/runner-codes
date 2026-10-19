package guest

import (
	"os"
	"testing"
)

// TestSetDefaultEnvKeepsExisting trava a correção do HOME: o valor vindo do start.sh
// (tmpfs, porque o root é read-only) vence o padrão /root.
func TestSetDefaultEnvKeepsExisting(t *testing.T) {
	t.Setenv("RUNNER_TEST_ENV", "/tmp/home")
	setDefaultEnv("RUNNER_TEST_ENV", "/root")
	if got := os.Getenv("RUNNER_TEST_ENV"); got != "/tmp/home" {
		t.Fatalf("valor existente sobrescrito: %s", got)
	}
	t.Setenv("RUNNER_TEST_ENV", "")
	setDefaultEnv("RUNNER_TEST_ENV", "/root")
	if got := os.Getenv("RUNNER_TEST_ENV"); got != "/root" {
		t.Fatalf("padrão não aplicado: %s", got)
	}
}
