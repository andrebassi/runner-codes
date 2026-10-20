---
title: 'Visión general de la API'
description: 'Endpoints HTTP, formatos de solicitud y respuesta, errores y límites'
---

## Base

La API se inicia con `rc serve` y escucha por defecto en `http://127.0.0.1:8080`. **No hay autenticación.** El valor por defecto es solo loopback; no la exponga fuera de él sin un proxy autenticado delante.

| Método | Ruta | Qué hace |
|--------|---------|-----------|
| GET | `/health` | estado y versión |
| GET | `/api/v1/languages` | catálogo y si la imagen de cada lenguaje está lista |
| POST | `/api/v1/run` | ejecuta archivos, con stdin único o en lote |
| POST | `/api/v1/execute` | ejecuta `code` suelto o archivos, con timeout por solicitud |

## GET /health

```json
{"status": "ok", "version": "dev"}
```

`version` es la versión grabada en el binario en el release; en un build local, `dev`.

## GET /api/v1/languages

```json
[
  {"name": "python", "image": "python:3.12-alpine", "ready": true},
  {"name": "nodejs", "image": "node:22-alpine", "ready": false}
]
```

`ready` indica si el ext4 del lenguaje ya se construyó en este host. Un lenguaje con `ready: false` se rechaza con E006, como si no estuviera en el catálogo; construya la imagen con [`image build`](/cli/image).

## POST /api/v1/run

```json
{
  "language": "python",
  "files": [{"name": "main.py", "content": "print(input())"}],
  "stdin": "olá"
}
```

| Campo | Tipo | Obligatorio | Descripción |
|-------|------|-------------|-----------|
| `language` | string | sí | nombre en el catálogo |
| `files` | lista de `{name, content}` | sí | el primer archivo es el principal |
| `stdin` | string o lista | no | una lista se ejecuta una vez por elemento, cada una en una VM nueva |

Respuesta con stdin único:

```json
{"status": "success", "stdout": "olá\n", "stderr": null, "exception": null, "executionTime": 33}
```

Con `"stdin": ["a", "b"]`, la respuesta es una lista, con el stdin repetido en cada elemento:

```json
[
  {"status": "success", "stdout": "a\n", "stderr": null, "exception": null, "executionTime": 32, "stdin": "a"},
  {"status": "success", "stdout": "b\n", "stderr": null, "exception": null, "executionTime": 31, "stdin": "b"}
]
```

Los valores de `executionTime` anteriores son ilustrativos.

## POST /api/v1/execute

```json
{
  "language": "python",
  "code": "import time; time.sleep(2); print('ok')",
  "timeout": 5
}
```

| Campo | Tipo | Obligatorio | Descripción |
|-------|------|-------------|-----------|
| `language` | string | sí | nombre en el catálogo |
| `code` | string | no | código del archivo principal, cuando no hay `files` |
| `files` | lista de `{name, content}` | no | archivos; el primero es el principal |
| `stdin` | string | no | entrada estándar |
| `timeout` | entero (segundos) | no | por defecto 10, máximo 300 |

La respuesta tiene el mismo formato que `/api/v1/run`.

## POST /api/v1/execute/stream

Mismo cuerpo que `/api/v1/execute`. La respuesta es `text/event-stream` (Server-Sent Events): la salida llega **mientras
el programa se ejecuta**, y un evento `result` cierra la respuesta.

```bash
curl -N -X POST http://127.0.0.1:8080/api/v1/execute/stream -H 'content-type: application/json' \
  -d '{"language":"python","code":"import time\nprint(\"early\")\ntime.sleep(1.5)\nprint(\"late\")"}'
```

```
event: stdout
data: "early\n"

event: stdout
data: "late\n"

event: result
data: {"status":"success","exitCode":0,"exception":null,"executionTime":1534,"timedOut":false}
```

| Evento | `data` |
|--------|--------|
| `stdout`, `stderr` | el fragmento de salida como **string JSON** (el salto de línea viene escapado: SSE no acepta saltos crudos en `data`) |
| `result` | `status`, `exitCode`, `exception`, `executionTime` (ms), `timedOut`, y `error` cuando lo hay. No repite stdout/stderr |
| `error` | `{"error": "..."}`: fallo de infraestructura después de que el stream haya empezado (p. ej.: se cayó la conexión con el guest) |

- Un job rechazado en la validación responde **400 JSON**, igual que `/execute`, antes de abrir el stream. Un fallo de
  infraestructura antes del primer evento responde 500 JSON.
- `stdin` en lista (lote) no se acepta en esta ruta: 400.
- La cabecera `X-Accel-Buffering: no` desactiva el buffer de proxy de nginx.
- Los lenguajes con stdout en bloque cuando la salida es un pipe (C, Ruby, Perl) solo entregan en el `flush` del programa; Python
  se ejecuta con `PYTHONUNBUFFERED=1`. Detalles en [`rc run`](/cli/run).

## Campos de la respuesta

| Campo | Descripción |
|-------|-----------|
| `status` | `"success"` cuando la API ejecutó el job. **No** significa que el programa haya funcionado: consulte `stderr` y `exception` |
| `stdout` | salida estándar |
| `stderr` | salida de error, o `null` cuando está vacía |
| `exception` | `null`, `"Timeout"`, `"Compilation failed"` o la descripción de un fallo de infraestructura |
| `executionTime` | tiempo de ejecución dentro de la VM, en ms |
| `error` | presente en un rechazo (E005, E006 o el motivo) y cuando el guest no responde a tiempo (E001) |
| `stdin` | solo en modo lote: el stdin de ese elemento |

## Errores y status HTTP

| Situación | HTTP | Cuerpo |
|----------|------|-------|
| ejecución concluida (incluso un programa que falló o excedió el tiempo) | 200 | respuesta normal |
| JSON inválido o campo obligatorio ausente | 400 | `{"status": "failed", "error": "requisição inválida: ..."}` |
| stdin mayor de 1 MB | 400 | `"error": "E005: stdin too long"` |
| lenguaje fuera del catálogo o sin imagen construida en este host | 400 | `"error": "E006: unsupported language <nome>"` |
| se violó otro límite del dominio (timeout fuera de (0, 300 s], job sin código y sin archivos, archivo repetido, nombre inválido, demasiados archivos, código demasiado grande) | 400 | `"error"` con el motivo |
| el guest no respondió dentro del timeout más el margen del host | 200 | `{"status": "failed", "error": "E001: operation timed out"}` |
| fallo de infraestructura (la VM no arrancó, error en el canal con el guest) | 500 | `{"status": "failed", "exception": "..."}` |

## Límites

| Límite | Valor |
|--------|-------|
| stdin | ≤ 1.000.000 bytes |
| archivos por job | hasta 32 |
| código total (suma de los archivos) | ≤ 4 MiB |
| nombre de archivo | sin `/`, sin `..`, sin empezar con `.`, ≤ 255 bytes |
| timeout | por defecto 10 s, máximo 300 s |

## Máquinas

Ciclo de vida de las [máquinas persistentes](/cli/machine).

| Método | Ruta | Cuerpo / query | Respuesta |
|--------|---------|---------------|----------|
| POST | `/api/v1/machines` | `{"name","stack"\|"image","cpus","memory_mib","disk_gib","gpu","gpu_vram_mib","env","workdir","init","workload","stop_signal","stop_timeout","stop_on_exit","net","ports":["8080:80"],"egress":{"allow_hosts",...},"network":"lab"}` o `{"name","runner_code":"<TOML>"}` (los campos no vacíos prevalecen sobre el archivo) | 201 + máquina (`state: stopped`) |
| GET | `/api/v1/machines` | — | lista con `state` y `pid` |
| GET | `/api/v1/machines/:name` | — | la máquina |
| DELETE | `/api/v1/machines/:name` | `?force=true` la mata si está encendida | 204 |
| POST | `/api/v1/machines/:name/start` | — | 200 + máquina (`state: running`) |
| POST | `/api/v1/machines/:name/stop` | `?timeout=N` en segundos (0–300; 0 la mata al instante); sin él, el `stop_timeout` de la máquina (o 10) | 204 |
| POST | `/api/v1/machines/:name/exec` | `{"command":["argv",...],"env":["K=V"],"workdir","stdin","timeout"}` | `{"exitCode","stdout","stderr","timedOut","executionTime"}` |
| POST | `/api/v1/machines/:name/exec/stream` | mismo cuerpo | SSE, **un evento por línea, texto crudo**: `event: stdout` / `data: line1`; una línea que llega partida espera el resto, y el resto sin `\n` sale antes del `exit` (`{"exitCode","timedOut","executionTime"}`) |
| PUT | `/api/v1/machines/:name/files?path=/abs[&mode=644]` | cuerpo crudo = contenido | `{"path","bytes"}` |
| GET | `/api/v1/machines/:name/files?path=/abs` | — | contenido crudo (`application/octet-stream`) |
| PUT/GET | `/api/v1/machines/:name/files/*path` | ídem, con la ruta en la URL (`files/%2Ftmp%2Fa.txt` o `files/tmp/a.txt`) | ídem |
| GET | `/api/v1/machines/:name/logs?tail=N&source=all\|workload` | — | consola en texto; `workload` filtra la salida del workload, sin la marca |
| PUT | `/api/v1/machines/:name/egress` | `{"allow_hosts","allow_host_patterns","allow_cidrs","deny_cidrs","block_private","allow_private"}` (la política completa) | 200 + máquina; con ella encendida rige en la siguiente conexión. `INVALID_STATE` si está encendida sin tarjeta de red |
| POST | `/api/v1/machines/:name/pause` | — | 204; congela la máquina encendida (`state: paused`), y exec se rechaza hasta el resume |
| POST | `/api/v1/machines/:name/resume` | — | 204; una máquina que ya está en ejecución no es un error |
| POST | `/api/v1/machines/:name/sync` | — | 204; devuelve al host las copias de los volúmenes staged (`INVALID_STATE` si está detenida) |
| POST | `/api/v1/machines/:name/branch` | `{"name":"filha"}` | 201 + hija (`state: stopped`), copia copy-on-write del disco; la madre encendida se congela solo durante el clon |

```bash
curl -s -X POST localhost:8080/api/v1/machines -d '{"name":"dev","stack":"python"}'
curl -s -X POST localhost:8080/api/v1/machines/dev/start
curl -s -X POST localhost:8080/api/v1/machines/dev/exec -d '{"command":["python3","-c","print(42)"]}'
# {"exitCode":0,"stdout":"42\n","stderr":"","timedOut":false,"executionTime":31}
curl -s -X PUT --data-binary @app.py 'localhost:8080/api/v1/machines/dev/files?path=/workspace/app.py'
```

El `command` es un argv **sin shell implícito**. Para usar shell, envíe `["sh","-c","..."]`.

Formas alternativas aceptadas en el create:

| Campo alternativo | Equivale a |
|---|---|
| `"network": true` / `false` | `net` (un string sigue siendo el nombre de la red) |
| `"memoryMb"` | `memory_mib` |
| `"cmd"` | `workload` |
| `"allowedHosts"`, `"allowedCidrs"` | `egress.allow_hosts`, `egress.allow_cidrs`, y activan la red |
| `"nestedVirt"` (o `"nested"`) | `--nested`; se rechaza con 400 a menos que el servidor se inicie con `rc serve --allow-nested-virt` |

Un campo desconocido se **rechaza** con 400 (`CONFIG_ERROR`, el mensaje nombra el campo), en las rutas de create, exec,
egress y branch. Ignorarlo en silencio permitía que un `allowedHost` con un error de tipeo creara la máquina sin la restricción de salida.

El stream de **job** (`/api/v1/execute/stream`) no cambió: allí el `data` sigue siendo string JSON, porque el fragmento
puede contener bytes sin salto de línea.

Errores de las rutas de máquina:

```json
{"error":{"code":"INVALID_STATE","message":"INVALID_STATE: machine dev is not running; start it first"},"code":"INVALID_STATE"}
```

El `code` aparece también en el nivel superior del cuerpo, para el cliente que lo busca allí.

| Código | HTTP |
|--------|------|
| `NOT_FOUND` | 404 |
| `CONFLICT`, `INVALID_STATE` | 409 |
| `CONFIG_ERROR` | 400 |
| `RUNNER_ERROR` (infraestructura) | 500 |

## Protocolo interno

Entre el host y el guest, el job viaja por vsock en frames de tamaño + JSON. Consulte [Job request](/api-reference/job-request), [Job response](/api-reference/job-response) y [Protocolo vsock](/api-reference/vsock-protocol).
