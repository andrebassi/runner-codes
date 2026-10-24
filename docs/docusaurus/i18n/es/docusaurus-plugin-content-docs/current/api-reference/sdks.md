---
sidebar_position: 2
title: SDKs de Go y Python
---

# SDKs de Go y Python

Dos clientes de la [API HTTP](/api-reference/overview) (`rc serve`), con los mismos métodos y solo la biblioteca estándar.
Cubren el job efímero y el ciclo de vida completo de las máquinas.

| SDK | Dónde | Dependencias | Cobertura (línea / branch) |
|-----|------|--------------|----------------------------|
| Go | `runner.codes/pkg/client` | ninguna | 100% (statement; Go no mide branch) |
| Python 3.9+ | `sdk/python/runner_code` | ninguna | 100% / 100% |

**Node queda fuera.** Por la regla del proyecto, el código nuevo no nace en JavaScript/TypeScript fuera del navegador. Quien
usa Node llama a la API HTTP directamente con `fetch`: la tabla de la [referencia](/api-reference/overview#máquinas) tiene cada ruta
y cada cuerpo.

## Go

En un proyecto tuyo, fuera de este repositorio:

```bash
go get runner.codes/pkg/client@latest
```

`runner.codes` responde al `go get` apuntando al repositorio en GitHub, así que no hay clon ni `replace`. El paquete
importa solo la biblioteca estándar; el `go.mod` del módulo pide Go 1.27.2, y un toolchain más antiguo lo descarga solo.

```go
import "runner.codes/pkg/client"

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

Ejemplo completo: `examples/sdk-go/main.go`.

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

Para usarlo sin instalar: `PYTHONPATH=sdk/python`. Ejemplo completo: `examples/sdk-python/main.py`.

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
| `DeleteMachine` | `delete_machine` | `DELETE /api/v1/machines/:name[?force=true]` |

## Lo que conviene saber

- **El error de la API es tipado** (`*client.Error`, `RunnerCodeError`), con el status HTTP, el código estable de las rutas de máquina
  (`NOT_FOUND`, `INVALID_STATE`, `CONFIG`...) y el mensaje. Las rutas de job no tienen código: queda vacío. Un fallo de
  transporte (servidor caído) llega como error común, no como error de la API.
- **Un campo vacío no va en el cuerpo.** En el create, el servidor trata un campo presente como override del `runner_code`; enviar
  `cpus: 0` borraría el valor del archivo.
- **`Stop`**: un timeout negativo (Go) o `None` (Python) usa el `stop_timeout` de la máquina, y `0` la mata al instante.
- **`status: "success"` en el job** significa que la API lo ejecutó, no que el programa pasó. El resultado del programa está
  en `exception` (`Timeout`, `Compilation failed`) y en `stderr`.
- El nombre de la máquina va escapado en la ruta (`a/b` se convierte en `a%2Fb`).

## Pruebas

```bash
task test:cov      # incluye pkg/client (mínimo de 90%) y el SDK de Python
task sdk:py:cov    # solo Python: línea y branch, cada uno >= 90%, coverage.py vía nix shell
```

En los dos SDKs se plantaron 12 defectos a propósito (escape del nombre, query del stop, lectura de errores, campo
vacío en el cuerpo, modo octal...). Las pruebas detectaron los 12. El paso 10b del runbook ejecuta los dos ejemplos contra un
`rc serve` real, con VMs de verdad.
