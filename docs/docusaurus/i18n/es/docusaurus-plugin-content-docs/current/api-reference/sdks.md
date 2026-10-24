---
sidebar_position: 2
title: SDKs de Go y Python
---

# SDKs de Go y Python

Dos clientes de la [API HTTP](/api-reference/overview) (`rc serve`), con los mismos métodos y solo la biblioteca estándar.
Cubren el job efímero y el ciclo de vida completo de las máquinas.

| SDK | Paquete | Dependencias | Cobertura (línea / branch) |
|-----|---------|--------------|----------------------------|
| Go | `runner.codes/pkg/client` | ninguna | 100% (statement; Go no mide branch) |
| Python 3.9+ | `runner_code` (distribución `runner-code`) | ninguna | 100% / 100% |

**Node queda fuera.** Por la regla del proyecto, el código nuevo no nace en JavaScript/TypeScript fuera del navegador. Quien
usa Node llama a la API HTTP directamente con `fetch`: la tabla de la [referencia](/api-reference/overview#máquinas) tiene cada ruta
y cada cuerpo.

Los SDKs hablan con un `rc serve` que ya está en marcha, en tu máquina o en un servidor:

```bash
rc serve --listen 127.0.0.1:8080
```

## Instalar

### Go

```bash
go get runner.codes/pkg/client@latest
```

`runner.codes` responde al `go get` apuntando al repositorio en GitHub, así que no hay clon ni `replace`. El paquete
importa solo la biblioteca estándar. El módulo pide Go 1.27.2: el `go get` sube la línea `go` de tu `go.mod` a esa
versión, y un toolchain más antiguo descarga la nueva solo.

### Python

```bash
pip install "git+https://github.com/andrebassi/runner-codes@v1.2.0#subdirectory=sdk/python"
```

Instala el paquete `runner_code`, sin ninguna dependencia. Sin instalar, desde un clon del repositorio, basta con
`PYTHONPATH=sdk/python`.

## Primer programa

Un job efímero: el código corre en una microVM nueva, sin red, que se destruye al final.

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
		log.Fatal(err) // la API no lo ejecutó: servidor caído, 400, 500
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

## Recetas

Cada receta de abajo es una función de `examples/sdk-go-recipes/main.go` y de `examples/sdk-python-recipes/main.py`. Los
dos programas ejecutan las siete contra un `rc serve` real en el paso 10d del runbook. La salida mostrada en cada una es
la medida en ese paso.

```bash
go run ./examples/sdk-go-recipes http://127.0.0.1:8080
PYTHONPATH=sdk/python python3 examples/sdk-python-recipes/main.py http://127.0.0.1:8080
```

### 1. Ejecutar código de fuera (generado por un LLM, por ejemplo)

`status: "success"` solo significa que la API ejecutó la petición. Quien dice si el programa salió bien es `exception`
(vacío cuando salió bien) y el `stderr`.

```go title="Go"
code := "import sys\nprint(sum(range(10)))\nprint('aviso', file=sys.stderr)"
r, err := c.Execute(ctx, client.Job{Language: "python", Code: code, Timeout: 10})
if err != nil {
	return err // transporte o 4xx/5xx: la API no lo ejecutó
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

Salida: `stdout=45 stderr=aviso tempo=58ms` (el tiempo varía; con el pool lleno queda entre 26 y 69 ms).

### 2. Varios archivos y entrada estándar

El primer archivo es el punto de entrada; los otros quedan en el mismo directorio y se pueden importar.

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

Salida: `42`.

### 3. Plazo

Si se pasa del `Timeout` (segundos), la VM entera muere. El resultado llega normal, con `exception: "Timeout"`, y no
como error del SDK.

```go title="Go"
r, _ := c.Execute(ctx, client.Job{Language: "python", Code: "while True: pass", Timeout: 2})
fmt.Println(r.Exception) // Timeout
```

```python title="Python"
r = c.execute(Job(language="python", code="while True: pass", timeout=2))
print(r.exception)  # Timeout
```

### 4. Error de compilación

En un lenguaje compilado (C, C++, Rust, Java, Kotlin...), el fallo de compilación llega con
`exception: "Compilation failed"` y el mensaje del compilador en el `stderr`.

```go title="Go"
r, _ := c.Execute(ctx, client.Job{Language: "c", Code: "int main(void) { return x; }"})
fmt.Println(r.Exception, r.Stderr != "") // Compilation failed true
```

```python title="Python"
r = c.execute(Job(language="c", code="int main(void) { return x; }"))
print(r.exception, bool(r.stderr))  # Compilation failed True
```

**Go es la excepción:** corre con `go run`, que compila y ejecuta en un solo paso. El error de compilación llega solo en
el `stderr`, con `exception` vacío.

### 5. Máquina persistente con red filtrada

Una máquina queda en pie entre un comando y otro. Aquí la salida a internet está permitida solo para `pypi.org`: el
mismo programa consigue hablar con él y es rechazado en `example.com`.

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
defer c.DeleteMachine(ctx, name, true) // force: la borra aunque esté encendida, si algo falla a mitad de camino

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

El `script` intenta abrir `https://pypi.org/simple/` y `https://example.com` y graba el status de cada uno en
`saida.txt`. Salida: `sdk-go pypi=200 example=bloqueado`.

`Exec` devuelve el código de salida del proceso en `ExitCode`/`exit_code`: a diferencia del job, un comando que falla
dentro de la máquina **no** se convierte en error del SDK.

### 6. Branch

Copia una máquina con su disco. La madre se pausa antes, para que el disco esté consistente; la hija nace con lo que la
madre tenía grabado y sigue independiente.

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

Salida: `feito antes do branch`.

### 7. Errores de la API

El rechazo de la API llega tipado, con el status HTTP y un código estable. Compara por el código, nunca por el texto del
mensaje.

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

Los códigos de las rutas de máquina son `NOT_FOUND`, `INVALID_STATE`, `CONFLICT`, `CONFIG` y los demás de la
[referencia](/api-reference/overview#máquinas). Un fallo de transporte (servidor caído) llega como error común, no
como error de la API.

## Métodos

| Go | Python | Ruta |
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
| `Screenshot` | `screenshot` | `GET .../screen` (JPEG y tamaño) |
| `ScreenActions` / `Click` / `Type` / `Key` | `screen_actions` / `click` / `type` / `key` | `POST .../screen/actions` ([Control de pantalla](/cli/screen)) |
| `DeleteMachine` | `delete_machine` | `DELETE /api/v1/machines/:name[?force=true]` |

## Lo que conviene saber

- **Un campo vacío no va en el cuerpo.** En el create, el servidor trata un campo presente como override del `runner_code`; enviar
  `cpus: 0` borraría el valor del archivo.
- **`Stop`**: un timeout negativo (Go) o `None` (Python) usa el `stop_timeout` de la máquina, y `0` la mata al instante.
- **Plazo del cliente**: los dos esperan hasta 10 min por la respuesta (Go: `http.Client` con `Timeout` de 10 min;
  Python: 600 s), por encima de los 300 s que puede durar un job, para que el servidor responda antes el `Timeout` del
  job. En Go, un `context` con un plazo menor gana; el campo `HTTP` del `Client` acepta otro `http.Client`.
- El nombre de la máquina va escapado en la ruta (`a/b` se convierte en `a%2Fb`).

## Pruebas

```bash
task test:cov      # incluye pkg/client (mínimo de 90%) y el SDK de Python
task sdk:py:cov    # solo Python: línea y branch, cada uno >= 90%, coverage.py vía nix shell
```

En los dos SDKs se plantaron 12 defectos a propósito (escape del nombre, query del stop, lectura de errores, campo
vacío en el cuerpo, modo octal...). Las pruebas detectaron los 12. En el runbook, el paso 10b ejecuta los ejemplos
básicos, el 10c descarga el SDK de Go con el `go get` público en un proyecto de fuera y el 10d ejecuta las siete
recetas, todos contra un `rc serve` real, con VMs de verdad.
