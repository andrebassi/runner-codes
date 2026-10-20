---
sidebar_position: 2
title: SDKs Go e Python
---

# SDKs Go e Python

Dois clientes da [API HTTP](/api-reference/overview) (`rc serve`), com os mesmos métodos e só biblioteca padrão.
Eles cobrem o job efêmero e o ciclo de vida inteiro das máquinas.

| SDK | Onde | Dependências | Cobertura (linha / branch) |
|-----|------|--------------|----------------------------|
| Go | `runner.code/pkg/client` | nenhuma | 100% (statement; Go não mede branch) |
| Python 3.9+ | `sdk/python/runner_code` | nenhuma | 100% / 100% |

**Node fica de fora.** Pela regra do projeto, código novo não nasce em JavaScript/TypeScript fora do navegador. Quem
usa Node chama a API HTTP direto com `fetch`: a tabela da [referência](/api-reference/overview#máquinas) tem cada rota
e cada corpo.

## Go

```go
c := client.New("http://127.0.0.1:8080")
ctx := context.Background()

r, _ := c.Execute(ctx, client.Job{Language: "python", Code: "print(6*7)"})
fmt.Print(r.Stdout) // 42

c.CreateMachine(ctx, client.MachineSpec{Name: "dev", Stack: "python", Network: "lab"})
c.Start(ctx, "dev")
c.WriteFile(ctx, "dev", "/root/app.py", []byte("print('ola')\n"), 0o644)
out, _ := c.Exec(ctx, "dev", []string{"python3", "/root/app.py"}, client.ExecOptions{Timeout: 30})
fmt.Print(out.Stdout, out.ExitCode)

_, err := c.Machine(ctx, "nao-existe")
var e *client.Error
if errors.As(err, &e) {
	fmt.Println(e.Status, e.Code) // 404 NOT_FOUND
}
c.DeleteMachine(ctx, "dev", true)
```

Exemplo completo: `examples/sdk-go/main.go`.

## Python

```python
from runner_code import Client, Job, RunnerCodeError

c = Client("http://127.0.0.1:8080")
print(c.execute(Job(language="python", code="print(6*7)")).stdout)  # 42

c.create_machine("dev", stack="python", network="lab", egress={"allow_hosts": ["pypi.org"]})
c.start("dev")
c.write_file("dev", "/root/app.py", b"print('ola')\n")
r = c.exec("dev", ["python3", "/root/app.py"], timeout=30)
print(r.stdout, r.exit_code)

try:
    c.machine("nao-existe")
except RunnerCodeError as e:
    print(e.status, e.code)  # 404 NOT_FOUND
c.delete_machine("dev", force=True)
```

Para usar sem instalar: `PYTHONPATH=sdk/python`. Exemplo completo: `examples/sdk-python/main.py`.

## Métodos

| Go | Python | Rota |
|----|--------|------|
| `Execute` | `execute` | `POST /api/v1/execute` |
| `CreateMachine` | `create_machine` | `POST /api/v1/machines` |
| `Machines` / `Machine` | `machines` / `machine` | `GET /api/v1/machines[/:name]` |
| `Start` / `Stop` | `start` / `stop` | `POST .../start`, `.../stop?timeout=N` |
| `Exec` | `exec` | `POST .../exec` |
| `WriteFile` / `ReadFile` | `write_file` / `read_file` | `PUT`/`GET .../files?path=` |
| `Logs` | `logs` | `GET .../logs` |
| `UpdateEgress` | `update_egress` | `PUT .../egress` |
| `Pause` / `Resume` / `Branch` | `pause` / `resume` / `branch` | `POST .../pause`, `.../resume`, `.../branch` |
| `DeleteMachine` | `delete_machine` | `DELETE /api/v1/machines/:name[?force=true]` |

## O que vale saber

- **Erro da API é tipado** (`*client.Error`, `RunnerCodeError`), com o status HTTP, o código estável das rotas de máquina
  (`NOT_FOUND`, `INVALID_STATE`, `CONFIG`...) e a mensagem. As rotas de job não têm código: ele fica vazio. Falha de
  transporte (servidor fora do ar) chega como erro comum, não como erro da API.
- **Campo vazio não vai no corpo.** No create, o servidor trata campo presente como override do `runner_code`; mandar
  `cpus: 0` apagaria o valor do arquivo.
- **`Stop`**: timeout negativo (Go) ou `None` (Python) usa o `stop_timeout` da máquina, e `0` mata na hora.
- **`status: "success"` no job** quer dizer que a API executou, não que o programa passou. O resultado do programa está
  em `exception` (`Timeout`, `Compilation failed`) e em `stderr`.
- O nome da máquina vai escapado no caminho (`a/b` vira `a%2Fb`).

## Testes

```bash
task test:cov      # inclui pkg/client (piso de 90%) e o SDK Python
task sdk:py:cov    # só o Python: linha e branch, cada um >= 90%, coverage.py via nix shell
```

Os dois SDKs passaram por 12 defeitos plantados de propósito (escape do nome, query do stop, leitura de erro, campo
vazio no corpo, modo octal...). Os testes pegaram os 12. O passo 10b do runbook roda os dois exemplos contra um
`rc serve` real, com VMs de verdade.
