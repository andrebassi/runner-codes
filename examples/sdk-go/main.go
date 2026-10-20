// Exemplo do SDK Go contra um `rc serve`: um job efêmero e uma máquina do começo ao fim.
// Uso: go run ./examples/sdk-go [http://127.0.0.1:8080]
package main

import (
	"context"
	"errors"
	"fmt"
	"os"

	"runner.code/pkg/client"
)

// main imprime uma linha por etapa e sai com 1 na primeira que falhar.
func main() {
	base := "http://127.0.0.1:8080"
	if len(os.Args) > 1 {
		base = os.Args[1]
	}
	c := client.New(base)
	ctx := context.Background()
	name := fmt.Sprintf("sdk-go-%d", os.Getpid())
	// must apaga a máquina antes de sair: o os.Exit pula o defer, e máquina de exemplo não
	// pode sobrar ligada.
	must := func(what string, err error) {
		if err != nil {
			fmt.Fprintf(os.Stderr, "%s: %v\n", what, err)
			c.DeleteMachine(ctx, name, true)
			os.Exit(1)
		}
	}

	r, err := c.Execute(ctx, client.Job{Language: "python", Code: "print(6*7)"})
	must("execute", err)
	fmt.Printf("job: %s", r.Stdout)

	_, err = c.CreateMachine(ctx, client.MachineSpec{Name: name, Stack: "python"})
	must("create", err)
	defer c.DeleteMachine(ctx, name, true)
	m, err := c.Start(ctx, name)
	must("start", err)
	fmt.Printf("machine: %s %s\n", m.Name, m.State)
	must("write", c.WriteFile(ctx, name, "/root/hello.py", []byte("print('ola do sdk go')\n"), 0o644))
	out, err := c.Exec(ctx, name, []string{"python3", "/root/hello.py"}, client.ExecOptions{})
	must("exec", err)
	fmt.Printf("exec: %s", out.Stdout)
	_, err = c.Machine(ctx, "nao-existe")
	var apiErr *client.Error
	if errors.As(err, &apiErr) {
		fmt.Printf("error: %s\n", apiErr.Code)
	}
}
