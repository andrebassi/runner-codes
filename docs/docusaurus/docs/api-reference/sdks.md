---
sidebar_position: 2
title: SDKs Go e Python
---

# SDKs Go e Python

Dois clientes da [API HTTP](/api-reference/overview) (`rc serve`), com os mesmos métodos e só biblioteca padrão.
Eles cobrem o job efêmero e o ciclo de vida inteiro das máquinas.

| SDK | Pacote | Dependências | Cobertura (linha / branch) |
|-----|--------|--------------|----------------------------|
| Go | `runner.codes/pkg/client` | nenhuma | 100% (statement; Go não mede branch) |
| Python 3.9+ | `runner_code` (distribuição `runner-code`) | nenhuma | 100% / 100% |

**Node fica de fora.** Pela regra do projeto, código novo não nasce em JavaScript/TypeScript fora do navegador. Quem
usa Node chama a API HTTP direto com `fetch`: a tabela da [referência](/api-reference/overview#máquinas) tem cada rota
e cada corpo.

Os SDKs falam com um `rc serve` que já está no ar, na sua máquina ou num servidor:

```bash
rc serve --listen 127.0.0.1:8080
```

## Instalar

### Go

```bash
go get runner.codes/pkg/client@latest
```

O `runner.codes` responde ao `go get` apontando para o repositório no GitHub, então não há clone nem `replace`. O
pacote importa só a biblioteca padrão. O módulo pede Go 1.27.2: o `go get` sobe a linha `go` do seu `go.mod` para essa
versão, e um toolchain mais antigo baixa a nova sozinho.

### Python

```bash
pip install "git+https://github.com/andrebassi/runner-codes@v1.1.1#subdirectory=sdk/python"
```

Instala o pacote `runner_code`, sem dependência nenhuma. Sem instalar, de um clone do repositório, basta
`PYTHONPATH=sdk/python`.

## Primeiro programa

Um job efêmero: o código roda numa microVM nova, sem rede, que é destruída no fim.

```go title="Go"
package main

import (
	"context"
	"fmt"
	"log"

	"runner.codes/pkg/client"
)

func main() {
	c := client.New("http://127.0.0.1:8080")
	r, err := c.Execute(context.Background(), client.Job{Language: "python", Code: "print(6*7)"})
	if err != nil {
		log.Fatal(err) // a API não executou: servidor fora do ar, 400, 500
	}
	fmt.Print(r.Stdout) // 42
}
```

```python title="Python"
from runner_code import Client, Job

c = Client("http://127.0.0.1:8080")
r = c.execute(Job(language="python", code="print(6*7)"))
print(r.stdout, end="")  # 42
```

## Receitas

Cada receita abaixo é uma função de `examples/sdk-go-recipes/main.go` e de `examples/sdk-python-recipes/main.py`. Os
dois programas rodam as sete contra um `rc serve` real no passo 10d do runbook. A saída mostrada em cada uma é a medida
nesse passo.

```bash
go run ./examples/sdk-go-recipes http://127.0.0.1:8080
PYTHONPATH=sdk/python python3 examples/sdk-python-recipes/main.py http://127.0.0.1:8080
```

### 1. Rodar código de fora (gerado por um LLM, por exemplo)

`status: "success"` quer dizer só que a API executou o pedido. Quem diz se o programa deu certo é `exception` (vazio
quando deu) e o `stderr`.

```go title="Go"
code := "import sys\nprint(sum(range(10)))\nprint('aviso', file=sys.stderr)"
r, err := c.Execute(ctx, client.Job{Language: "python", Code: code, Timeout: 10})
if err != nil {
	return err // transporte ou 4xx/5xx: a API não executou
}
if r.Exception != "" {
	return fmt.Errorf("o programa falhou: %s", r.Exception)
}
fmt.Printf("stdout=%s stderr=%s tempo=%dms\n",
	strings.TrimSpace(r.Stdout), strings.TrimSpace(r.Stderr), r.ExecutionTime)
```

```python title="Python"
code = "import sys\nprint(sum(range(10)))\nprint('aviso', file=sys.stderr)"
r = c.execute(Job(language="python", code=code, timeout=10))
if r.exception:
    raise RuntimeError(f"o programa falhou: {r.exception}")
print(f"stdout={r.stdout.strip()} stderr={r.stderr.strip()} tempo={r.execution_time}ms")
```

Saída: `stdout=45 stderr=aviso tempo=58ms` (o tempo varia; com o pool cheio fica entre 26 e 69 ms).

### 2. Vários arquivos e entrada padrão

O primeiro arquivo é o ponto de entrada; os outros ficam no mesmo diretório e podem ser importados.

```go title="Go"
r, err := c.Execute(ctx, client.Job{
	Language: "python",
	Files: []client.File{
		{Name: "main.py", Content: "import util, sys\nprint(util.dobro(int(sys.stdin.read())))"},
		{Name: "util.py", Content: "def dobro(n):\n    return 2 * n"},
	},
	Stdin: "21",
})
```

```python title="Python"
r = c.execute(Job(
    language="python",
    files=[
        {"name": "main.py", "content": "import util, sys\nprint(util.dobro(int(sys.stdin.read())))"},
        {"name": "util.py", "content": "def dobro(n):\n    return 2 * n"},
    ],
    stdin="21",
))
```

Saída: `42`.

### 3. Prazo

Passou do `Timeout` (segundos), a VM inteira morre. O resultado chega normal, com `exception: "Timeout"`, e não como
erro do SDK.

```go title="Go"
r, _ := c.Execute(ctx, client.Job{Language: "python", Code: "while True: pass", Timeout: 2})
fmt.Println(r.Exception) // Timeout
```

```python title="Python"
r = c.execute(Job(language="python", code="while True: pass", timeout=2))
print(r.exception)  # Timeout
```

### 4. Erro de compilação

Em linguagem compilada (C, C++, Rust, Java, Kotlin...), a falha de compilação vem com `exception: "Compilation failed"`
e a mensagem do compilador no `stderr`.

```go title="Go"
r, _ := c.Execute(ctx, client.Job{Language: "c", Code: "int main(void) { return x; }"})
fmt.Println(r.Exception, r.Stderr != "") // Compilation failed true
```

```python title="Python"
r = c.execute(Job(language="c", code="int main(void) { return x; }"))
print(r.exception, bool(r.stderr))  # Compilation failed True
```

**Go é exceção:** ele roda por `go run`, que compila e executa num passo só. O erro de compilação chega só no `stderr`,
com `exception` vazio.

### 5. Máquina persistente com rede filtrada

Uma máquina fica de pé entre um comando e outro. Aqui a saída para a internet está liberada só para `pypi.org`: o mesmo
programa consegue falar com ele e é recusado no `example.com`.

```go title="Go"
name := "minha-maquina"
_, err := c.CreateMachine(ctx, client.MachineSpec{
	Name:   name,
	Stack:  "python",
	Net:    true,
	Egress: &client.Egress{AllowHosts: []string{"pypi.org"}},
})
if err != nil {
	return err
}
defer c.DeleteMachine(ctx, name, true) // force: apaga mesmo ligada, se algo falhar no meio

c.Start(ctx, name)
c.WriteFile(ctx, name, "/work/app.py", []byte(script), 0o644)
out, err := c.Exec(ctx, name, []string{"python3", "app.py"},
	client.ExecOptions{Env: []string{"QUEM=sdk-go"}, Workdir: "/work", Timeout: 30})
if err == nil && out.ExitCode != 0 {
	err = fmt.Errorf("exit %d: %s", out.ExitCode, out.Stderr)
}
data, _ := c.ReadFile(ctx, name, "/work/saida.txt")
```

```python title="Python"
name = "minha-maquina"
c.create_machine(name, stack="python", net=True, egress={"allow_hosts": ["pypi.org"]})
try:
    c.start(name)
    c.write_file(name, "/work/app.py", script.encode())
    out = c.exec(name, ["python3", "app.py"], env=["QUEM=sdk-python"], workdir="/work", timeout=30)
    if out.exit_code != 0:
        raise RuntimeError(f"exit {out.exit_code}: {out.stderr}")
    print(c.read_file(name, "/work/saida.txt").decode())
finally:
    c.delete_machine(name, force=True)
```

O `script` tenta abrir `https://pypi.org/simple/` e `https://example.com` e grava o status de cada um em `saida.txt`.
Saída: `sdk-go pypi=200 example=bloqueado`.

`Exec` devolve o código de saída do processo em `ExitCode`/`exit_code`: diferente do job, comando que falha dentro da
máquina **não** vira erro do SDK.

### 6. Branch

Copia uma máquina com o disco dela. A mãe é pausada antes, para o disco estar consistente; a filha nasce com o que a mãe
tinha gravado e segue independente.

```go title="Go"
c.WriteFile(ctx, "mae", "/root/estado.txt", []byte("feito antes do branch"), 0o644)
c.Pause(ctx, "mae")
c.Branch(ctx, "mae", "filha")
c.Start(ctx, "filha")
data, _ := c.ReadFile(ctx, "filha", "/root/estado.txt")
```

```python title="Python"
c.write_file("mae", "/root/estado.txt", b"feito antes do branch")
c.pause("mae")
c.branch("mae", "filha")
c.start("filha")
print(c.read_file("filha", "/root/estado.txt").decode())
```

Saída: `feito antes do branch`.

### 7. Erros da API

A recusa da API chega tipada, com o status HTTP e um código estável. Compare pelo código, nunca pelo texto da mensagem.

```go title="Go"
_, err := c.Machine(ctx, "nao-existe")
var apiErr *client.Error
if errors.As(err, &apiErr) && apiErr.Code == "NOT_FOUND" {
	fmt.Println(apiErr.Status, apiErr.Code) // 404 NOT_FOUND
}
```

```python title="Python"
try:
    c.machine("nao-existe")
except RunnerCodeError as e:
    if e.code == "NOT_FOUND":
        print(e.status, e.code)  # 404 NOT_FOUND
```

Os códigos das rotas de máquina são `NOT_FOUND`, `INVALID_STATE`, `CONFLICT`, `CONFIG` e os demais da
[referência](/api-reference/overview#máquinas). Falha de transporte (servidor fora do ar) chega como erro comum, não
como erro da API.

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
| `Screenshot` | `screenshot` | `GET .../screen` (JPEG e tamanho) |
| `ScreenActions` / `Click` / `Type` / `Key` | `screen_actions` / `click` / `type` / `key` | `POST .../screen/actions` ([Controle de tela](/cli/screen)) |
| `DeleteMachine` | `delete_machine` | `DELETE /api/v1/machines/:name[?force=true]` |

## O que vale saber

- **Campo vazio não vai no corpo.** No create, o servidor trata campo presente como override do `runner_code`; mandar
  `cpus: 0` apagaria o valor do arquivo.
- **`Stop`**: timeout negativo (Go) ou `None` (Python) usa o `stop_timeout` da máquina, e `0` mata na hora.
- **Prazo do cliente**: os dois esperam até 10 min por resposta (Go: `http.Client` com `Timeout` de 10 min; Python:
  600 s), acima dos 300 s que um job pode durar, para o servidor responder o `Timeout` do job antes. Em Go, um `context`
  com prazo menor vence; o campo `HTTP` do `Client` aceita outro `http.Client`.
- O nome da máquina vai escapado no caminho (`a/b` vira `a%2Fb`).

## Testes

```bash
task test:cov      # inclui pkg/client (piso de 90%) e o SDK Python
task sdk:py:cov    # só o Python: linha e branch, cada um >= 90%, coverage.py via nix shell
```

Os dois SDKs passaram por 12 defeitos plantados de propósito (escape do nome, query do stop, leitura de erro, campo
vazio no corpo, modo octal...). Os testes pegaram os 12. No runbook, o passo 10b roda os exemplos básicos, o 10c baixa
o SDK Go pelo `go get` público num projeto de fora e o 10d roda as sete receitas, todos contra um `rc serve` real, com
VMs de verdade.
