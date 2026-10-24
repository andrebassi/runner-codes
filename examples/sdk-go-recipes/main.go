// Receitas do SDK Go contra um `rc serve`: cada função é um caso de uso que a página de SDKs
// mostra, e este programa roda todas, imprimindo uma linha conferível por receita (runbook 10d).
//
// Uso: go run ./examples/sdk-go-recipes [http://127.0.0.1:8080]
package main

import (
	"context"
	"errors"
	"fmt"
	"os"
	"strings"
	"time"

	"runner.codes/pkg/client"
)

// main liga o cliente e roda as receitas em ordem; a primeira que falhar encerra com rc 1.
func main() {
	base := "http://127.0.0.1:8080"
	if len(os.Args) > 1 {
		base = os.Args[1]
	}
	c := client.New(base)
	ctx := context.Background()

	recipes := []struct {
		name string
		run  func(context.Context, *client.Client) error
	}{
		{"untrusted", runUntrusted},
		{"files", filesAndStdin},
		{"timeout", timeout},
		{"compile", compileError},
		{"machine", machineWorkflow},
		{"branch", branch},
		{"errors", typedError},
	}
	for _, r := range recipes {
		if err := r.run(ctx, c); err != nil {
			fmt.Fprintf(os.Stderr, "%s: %v\n", r.name, err)
			os.Exit(1)
		}
	}
}

// runUntrusted roda código vindo de fora (de um LLM, por exemplo) numa VM descartável. O
// `status` diz só que a API executou; quem diz se o programa deu certo é `exception` e `stderr`.
func runUntrusted(ctx context.Context, c *client.Client) error {
	code := "import sys\nprint(sum(range(10)))\nprint('aviso', file=sys.stderr)"
	r, err := c.Execute(ctx, client.Job{Language: "python", Code: code, Timeout: 10})
	if err != nil {
		return err // transporte ou 4xx/5xx: a API não executou
	}
	if r.Exception != "" {
		return fmt.Errorf("o programa falhou: %s", r.Exception)
	}
	fmt.Printf("untrusted: stdout=%s stderr=%s tempo=%dms\n",
		strings.TrimSpace(r.Stdout), strings.TrimSpace(r.Stderr), r.ExecutionTime)
	return nil
}

// filesAndStdin manda um projeto de vários arquivos e uma entrada padrão. O primeiro arquivo é o
// ponto de entrada; os outros ficam ao lado dele, no mesmo diretório.
func filesAndStdin(ctx context.Context, c *client.Client) error {
	r, err := c.Execute(ctx, client.Job{
		Language: "python",
		Files: []client.File{
			{Name: "main.py", Content: "import util, sys\nprint(util.dobro(int(sys.stdin.read())))"},
			{Name: "util.py", Content: "def dobro(n):\n    return 2 * n"},
		},
		Stdin: "21",
	})
	if err != nil {
		return err
	}
	fmt.Printf("files: %s\n", strings.TrimSpace(r.Stdout))
	return nil
}

// timeout mostra o prazo: o job é morto (a VM inteira) e o resultado vem com exception "Timeout",
// sem erro de Go, porque a API executou o que foi pedido.
func timeout(ctx context.Context, c *client.Client) error {
	r, err := c.Execute(ctx, client.Job{Language: "python", Code: "while True: pass", Timeout: 2})
	if err != nil {
		return err
	}
	fmt.Printf("timeout: exception=%s\n", r.Exception)
	return nil
}

// compileError mostra a falha de compilação numa linguagem compilada (C, C++, Rust, Java...):
// exception "Compilation failed" e a mensagem do compilador no stderr. Go é exceção: roda por
// `go run`, e o erro de compilação chega só no stderr, com exception vazio.
func compileError(ctx context.Context, c *client.Client) error {
	r, err := c.Execute(ctx, client.Job{Language: "c", Code: "int main(void) { return x; }"})
	if err != nil {
		return err
	}
	fmt.Printf("compile: exception=%s stderr_tem_erro=%t\n", r.Exception, r.Stderr != "")
	return nil
}

// machineWorkflow é a máquina persistente: cria com saída liberada só para pypi.org, liga, grava
// um arquivo, roda com ambiente e diretório próprios, lê o resultado de volta e apaga no fim.
func machineWorkflow(ctx context.Context, c *client.Client) error {
	name := fmt.Sprintf("recipe-go-%d", os.Getpid())
	_, err := c.CreateMachine(ctx, client.MachineSpec{
		Name:   name,
		Stack:  "python",
		Net:    true,
		Egress: &client.Egress{AllowHosts: []string{"pypi.org"}},
	})
	if err != nil {
		return err
	}
	// force=true apaga mesmo ligada: a máquina não sobra se uma etapa falhar no meio.
	defer c.DeleteMachine(ctx, name, true)

	if _, err := c.Start(ctx, name); err != nil {
		return err
	}
	// Programa que roda DENTRO da máquina: tenta dois sites e grava o resultado num arquivo.
	script := `import os, urllib.request
# status HTTP do endereço, ou "bloqueado" quando a política de saída recusa
def get(u):
    try:
        return urllib.request.urlopen(u, timeout=5).status
    except Exception:
        return "bloqueado"
open("saida.txt", "w").write(f"{os.environ['QUEM']} pypi={get('https://pypi.org/simple/')} example={get('https://example.com')}")
`
	if err := c.WriteFile(ctx, name, "/work/app.py", []byte(script), 0o644); err != nil {
		return err
	}
	out, err := c.Exec(ctx, name, []string{"python3", "app.py"},
		client.ExecOptions{Env: []string{"QUEM=sdk-go"}, Workdir: "/work", Timeout: 30})
	if err != nil {
		return err
	}
	if out.ExitCode != 0 {
		return fmt.Errorf("exit %d: %s", out.ExitCode, out.Stderr)
	}
	data, err := c.ReadFile(ctx, name, "/work/saida.txt")
	if err != nil {
		return err
	}
	fmt.Printf("machine: %s\n", data)
	return nil
}

// branch copia uma máquina: a mãe é pausada para o disco ficar consistente, a filha nasce com o
// que a mãe tinha gravado e segue a vida dela.
func branch(ctx context.Context, c *client.Client) error {
	mom := fmt.Sprintf("recipe-go-mom-%d", os.Getpid())
	kid := mom + "-kid"
	if _, err := c.CreateMachine(ctx, client.MachineSpec{Name: mom, Stack: "python"}); err != nil {
		return err
	}
	defer c.DeleteMachine(ctx, mom, true)
	if _, err := c.Start(ctx, mom); err != nil {
		return err
	}
	if err := c.WriteFile(ctx, mom, "/root/estado.txt", []byte("feito antes do branch"), 0o644); err != nil {
		return err
	}
	if err := c.Pause(ctx, mom); err != nil {
		return err
	}
	if _, err := c.Branch(ctx, mom, kid); err != nil {
		return err
	}
	defer c.DeleteMachine(ctx, kid, true)
	if _, err := c.Start(ctx, kid); err != nil {
		return err
	}
	data, err := c.ReadFile(ctx, kid, "/root/estado.txt")
	if err != nil {
		return err
	}
	fmt.Printf("branch: %s\n", data)
	return c.Stop(ctx, kid, 10*time.Second)
}

// typedError trata a recusa da API pelo código estável, sem comparar texto de mensagem.
func typedError(ctx context.Context, c *client.Client) error {
	_, err := c.Machine(ctx, "nao-existe")
	var apiErr *client.Error
	if !errors.As(err, &apiErr) {
		return fmt.Errorf("esperava *client.Error, veio %v", err)
	}
	switch apiErr.Code {
	case "NOT_FOUND":
		fmt.Printf("errors: %d %s\n", apiErr.Status, apiErr.Code)
		return nil
	default:
		return apiErr
	}
}
