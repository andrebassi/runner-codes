---
title: 'Referencia para agentes'
description: 'Lo que rc hace hoy, cómo usar cada comando, lo que aún no existe y el plan de evolución'
---

Referencia breve y completa para quien opera `rc`, sea una persona o un agente de IA: lo que existe, cómo invocarlo y
qué garantiza cada cosa. Cada línea describe lo que **existe hoy**. Lo que aún no existe queda en la sección
[Lo que aún no existe](#lo-que-aún-no-existe) y en el [plan](#plan-de-evolución).

`rc` ejecuta código no confiable (generado por un LLM) en una **microVM nueva por job**, con kernel propio, sin red,
con root de solo lectura, y destruye la VM al final. El VMM es libkrun y el kernel viene de libkrunfw. El arranque
tarda unos 260 ms.

## Plataformas

| Host | Guest | Hipervisor | Estado |
|------|-------|-----------|----------|
| macOS Apple Silicon | Linux arm64 | Hypervisor.framework | probado de punta a punta |
| Linux x86_64 / aarch64 | Linux de la misma arquitectura | KVM (`/dev/kvm`) | compila; confinamiento probado; arranque completo no medido (sin host con KVM) |
| Windows | — | — | fuera: libkrun upstream no tiene WHP. El build para Windows falla a propósito |

## Instalación

```bash
task krun:install                    # libkrun + libkrunfw (+ GPU en macOS) vía Nix: nix profile add .#krun
task build                           # bin/rc (firmado en macOS) + bin/rc-guest-linux-<arch>
./bin/rc image build --stack python  # o --all; una imagen ext4 por stack
./bin/rc doctor                      # revisa host, libkrun, GPU, guest, sandbox e imágenes
```

Sin Homebrew ni Docker: todo vía Nix. `nix develop` ofrece Go, task, coreutils y Node para desarrollar.

## Referencia rápida

```bash
# ejecutar código en una VM nueva (arranque en frío, la VM desaparece al final)
./bin/rc run --stack python --code 'print(2**10)'
./bin/rc run --stack go --file main.go --stdin 'entrada' --timeout 30s

# la salida aparece mientras el programa se ejecuta (streaming)
./bin/rc run --stack python --code $'import time\nprint("a")\ntime.sleep(2)\nprint("b")'

# shell interactivo en una VM nueva (TTY real: Ctrl+C, tamaño de la ventana)
./bin/rc shell --stack bash

# GPU del host vía Vulkan (solo en stacks con gpu)
./bin/rc run --stack llama --gpu --code 'vulkaninfo --summary | grep deviceName'

# máquina persistente: lo que grabes se mantiene entre comandos y entre stop/start
./bin/rc machine create --name dev --stack python
./bin/rc machine start  --name dev
./bin/rc machine exec   --name dev -- sh -c 'mkdir -p /workspace/out && echo pronto > /etc/motd'   # sin red: créala con --net o --allow-host
./bin/rc machine cp ./app.py dev:/workspace/app.py
./bin/rc machine exec   --name dev -- python3 /workspace/app.py
./bin/rc machine stop   --name dev

# API HTTP con pool de VMs listas (y las rutas de máquina)
./bin/rc serve --listen 127.0.0.1:8080 --pool-size 2
```

## Cuándo usar qué

| Objetivo | Comando |
|----------|---------|
| ejecutar un fragmento de código aislado y obtener la salida | `rc run --stack S --code '...'` |
| ver la salida mientras se ejecuta (build, prueba larga) | `rc run` (ya es streaming) o `POST /api/v1/execute/stream` |
| explorar a mano la imagen de una stack | `rc shell --stack S` |
| entorno de varias etapas que guarda estado | `rc machine create` → `start` → `exec`/`cp` → `stop` ([machine](/cli/machine)) |
| atender muchos jobs con baja latencia | `rc serve`: el pool saca el arranque del camino (26–78 ms por job) |
| usar la GPU (IA, Vulkan) | `--gpu` en la stack `llama` ([GPU](/advanced/gpu)) |
| saber si el host está listo | `rc doctor` |
| listar stacks y cuáles imágenes están construidas | `rc languages` |

## Estructura de la CLI

```
rc run    --stack S (--code C | --file F) [--stdin T] [--timeout 10s] [--gpu]
rc shell  --stack S [--gpu]
rc machine create --name N --stack S [--cpus 2] [--memory 2048] [--disk-gib 10] [--gpu]
rc machine start|stop|status|delete [--name N]          # stop --timeout 10s; delete -f
rc machine ls [--json]
rc machine exec [--name N] [-e K=V] [-w DIR] [-i] [--timeout D] -- CMD ARGS...
rc machine shell [--name N] [-e K=V] [-w DIR] [-- CMD]  # terminal interactiva (PTY)
rc machine web-terminal [--name N] [--port P] [--no-open] [-- CMD]  # la misma terminal en el navegador (127.0.0.1, token)
rc machine cp SRC DST                                    # un lado es nombre:/ruta
rc machine logs [--name N] [--tail N]
rc serve  [--listen 127.0.0.1:8080] [--pool-size 2]
rc image build (--stack S ... | --all)
rc languages
rc doctor
rc vmm --spec <json> [--watch-fd 3] [--lib <libkrun>]   # interno: un proceso por VM
```

`rc run` termina con **0** solo cuando el programa terminó con 0. Una salida distinta de 0, un timeout, una
compilación fallida o un job rechazado dan rc 1.

## Flags

| Flag | Comando | Qué hace |
|------|---------|-----------|
| `--stack` | run, shell, image build | stack del catálogo (`python`, `go`, `rust`, `java`, `nodejs`, `c`, `cpp`, `ruby`, `php`, `perl`, `bash`, `sh`, `awk`, `jq`, `sqlite`, `llama`) |
| `--code` / `--file` | run | código del archivo principal, directo o desde un archivo local |
| `--stdin` | run | entrada estándar del programa (hasta 1 MB) |
| `--timeout` | run | plazo del job, 10 s por defecto, máximo 300 s; al vencer, la VM se mata con `SIGKILL` |
| `--gpu` | run, shell | GPU del host vía Vulkan (virtio-gpu/Venus); solo en stacks con `gpu: true`, si no E007 |
| `--listen` | serve | dirección; loopback por defecto, porque la API no tiene autenticación |
| `--pool-size` | serve | VMs listas por stack (2 por defecto) |
| `--all` | image build | construye todas las stacks del catálogo |

## Variables de entorno

| Variable | Valor por defecto | Uso |
|----------|--------|-----|
| `RUNNER_CACHE_DIR` | `~/Library/Caches/runner-codes` (macOS) | imágenes, VM de build, directorios de las VMs |
| `RUNNER_KRUN_LIB` | `~/.nix-profile/lib/libkrun.dylib` (macOS); en Linux la del perfil de Nix o `libkrun.so.1` | qué libkrun cargar |
| `RUNNER_GUEST_BIN` | `rc-guest-linux-<arch>` junto a `rc` | el guest-runner inyectado en las imágenes |
| `RUNNER_SANDBOX` | activado | `off` desactiva Landlock + seccomp en Linux (solo para diagnóstico; el `vmm` avisa) |
| `RC_MACHINE_NAME` | `default` | nombre de máquina cuando falta `--name` (fíjelo por proyecto con direnv) |

## Catálogo de stacks

Está en `internal/adapters/catalog/languages.yaml`, embebido en el binario. Cada stack tiene:

```yaml
- name: python               # nombre que reciben la CLI y la API; debe existir en el ejecutor del guest
  image: python:3.12-alpine  # imagen OCI oficial de origen
  cpus: 1                    # límite de vCPU por job (contra el abuso)
  memory_mib: 512            # límite de RAM; la memoria de libkrun es elástica
  warmup: python3 -m compileall -q -j0 /usr/local/lib/python3.12   # se ejecuta una vez, en el build, con red
  gpu: false                 # true habilita --gpu
  gpu_vram_mib: 0            # VRAM del guest con GPU (0 = 4096)
```

El build descarga la imagen OCI sin Docker (go-containerregistry), la extrae en una VM de build Alpine, inyecta el
guest, ejecuta el warmup y genera un **ext4 de solo lectura**. La clave de caché incluye el digest de la imagen, el
warmup, los scripts y el hash del guest, así que cambiar cualquiera de ellos provoca una reconstrucción. Un campo
desconocido en el YAML es un error de build.

## API HTTP

`rc serve` mantiene un **pool** de VMs ya arrancadas por stack. Cada VM atiende **un** job y muere, y la reposición
es asíncrona, con un máximo de 4 arranques simultáneos en total.

| Método | Ruta | Qué hace |
|--------|---------|-----------|
| GET | `/health` | `{"status","version","pool":{"python":2,...}}` |
| GET | `/api/v1/languages` | stacks, imagen de origen y si está lista |
| POST | `/api/v1/run` | `{language, files[], stdin}`; `stdin` como lista ejecuta un job por elemento (lote) |
| POST | `/api/v1/execute` | `{language, code \| files[], stdin, timeout, gpu}` |
| POST | `/api/v1/execute/stream` | mismo cuerpo, respuesta en Server-Sent Events |
| — | `/api/v1/machines/...` | ciclo de vida de las máquinas: create, start, stop, exec, exec/stream, files, logs ([detalles](/api-reference/overview#máquinas)) |

Respuesta de `/run` y `/execute`:

```json
{"status":"success","stdout":"1024\n","stderr":null,"exception":null,"executionTime":12}
```

- `status: "success"` indica que la API ejecutó, **no** que el programa haya pasado: revise `stderr` y `exception`.
- `exception` es `null`, `"Timeout"`, `"Compilation failed"` o la descripción de una falla de infraestructura.

Streaming (`/execute/stream`):

```
event: stdout
data: "a\n"

event: result
data: {"status":"success","exitCode":0,"exception":null,"executionTime":2034,"timedOut":false}
```

- El `data` de `stdout`/`stderr` es un string JSON: el salto de línea viene escapado.
- El `result` no repite la salida.
- Una falla después del inicio se convierte en `event: error`.

### Códigos de error

| Código | HTTP | Cuándo |
|--------|------|--------|
| `E001: operation timed out` | 200 | el guest no respondió dentro del timeout más el margen del host (2 s) |
| `E005: stdin too long` | 400 | stdin por encima de 1.000.000 bytes |
| `E006: unsupported language X` | 400 | stack fuera del catálogo o sin imagen construida en este host |
| `E007: stack X has no GPU support` | 400 | `gpu` solicitado en una stack sin `gpu: true` |
| otros límites (timeout, archivos, tamaño, nombre) | 400 | `error` con el motivo |
| falla de infraestructura | 500 | `exception` con el motivo (la VM no arrancó, el canal con el guest se cayó) |

## Patrones importantes

- **Una VM nueva por job**: nada pasa de un job a otro; el pool solo saca el arranque del camino.
- **Sin red**: el vsock se crea sin TSI. Si no se desactivara el vsock implícito, libkrun activaría la red por su cuenta.
- **Root de solo lectura** (ext4 por virtio-blk, no virtiofs, que permitiría al guest alcanzar el resto del host);
  `/tmp` es un tmpfs de 256 MiB.
- **Límites por job**: stdin ≤ 1 MB; hasta 32 archivos; código ≤ 4 MiB en total; timeout de 10 s (máximo 300 s).
- **CPU y RAM por stack** son un límite contra el abuso; la memoria es elástica, y una VM detenida ocupa unos 44 MB.
- **API sin autenticación**: escucha en `127.0.0.1` por defecto.

## Comportamientos importantes

- **Timeout = `SIGKILL` al grupo del proceso de la VM.** No queda ningún proceso; el e2e lo verifica con `pgrep`.
- **Si `rc` muere, las VMs mueren con él**: `Pdeathsig` en Linux, pipe de vigilancia en el fd 3 en macOS.
- **El streaming entrega lo que el programa escribe en el pipe.** Python se ejecuta con `PYTHONUNBUFFERED=1`. C
  (`printf`), Ruby y Perl acumulan el stdout cuando la salida no es una terminal, y solo lo entregan en el `flush`.
- **`rc shell` con terminal** obtiene una consola virtio TTY y pone la terminal del host en modo raw; con pipe, queda la
  consola normal.
- **Imagen desactualizada**: si el guest de la imagen es anterior al streaming, el error indica ejecutar
  `rc image build`.
- **Linux**: el proceso `vmm` de cada VM de job se ejecuta bajo Landlock + seccomp. Con Landlock ABI < 9 no hay
  aislamiento de sockets Unix entre VMs, y `rc doctor` lo avisa. Una VM con GPU queda fuera del confinamiento.
- **GPU**: una VM con GPU arranca en frío, fuera del pool, con un máximo de 2 a la vez, y comparte la GPU del host
  (aislamiento por proceso, no por VM).

## Lo que aún no existe

`rc` usa libkrun + libkrunfw, imágenes OCI y ningún daemon. Ejecuta **jobs desechables** de código no confiable y
también gestiona **máquinas** persistentes; lo siguiente aún no existe, o existe en parte:

| Recurso | En `rc` hoy | Observación |
|---------|--------------|------------|
| `machine stats`, `machine monitor` | **no** | Fase D |
| exec interactivo por WebSocket en la API | **no** | `rc serve` no expone terminal; `rc machine web-terminal` sirve una en el navegador, en 127.0.0.1 |
| `--error-format json` | parcial | la API ya devuelve códigos E00x estables |
| checkpoint de RAM y CPU (Mac → Linux) | **no** | libkrun upstream no tiene snapshot; `branch` copia el disco |
| `--cuda` | **no** | requiere un host Linux con NVIDIA |
| Windows (WHP) | **no** | libkrun upstream no tiene WHP |

El evento final del stream de job es `result`; en el stream de máquina es `exit`. La API HTTP trae un pool
pre-arrancado, y en Linux cada VM de job se ejecuta bajo Landlock + seccomp.

## Plan de evolución

Cada fase se cierra como las anteriores: story AIOX, e2e dos veces seguidas, un defecto plantado a propósito para
probar que la prueba lo detecta, y gate del 90%. **Hechas:** la Fase A (stories 7.1 y 7.2), `runner.code` con imagen
arbitraria (story 7.3, de la Fase D) y la Fase C, red con filtro y volúmenes (story 7.4). Faltan la Fase B y el resto
de la D.

| Fase | Qué incluye | Decisión de diseño | Riesgo conocido |
|------|-------------|--------------------|-----------------|
| **A — máquinas** ✅ | `rc machine create/start/stop/delete/ls/status/exec/shell/logs/cp`; rutas `/api/v1/machines/...` | estado en `<cache>/machines/<nome>/`; disco ext4 escribible por máquina; overlayfs en el guest (imagen abajo, disco arriba) para que lo instalado persista; `/workspace` en el disco; el `vmm` se ejecuta desacoplado, con registro de PID y prueba de vida | hoy el `vmm` muere con el padre a propósito, y los jobs efímeros deben seguir así |
| **B — secretos y SSH** | `--secret-env`, `--secret-file`, `--ssh-agent` | secreto resuelto en el momento del start, nunca grabado en el registro; rechazado por la API (sería una lectura arbitraria del host); SSH por un puerto vsock conectado a `SSH_AUTH_SOCK`, sin que la clave entre en la VM | root en el guest lee el env del proceso; es defensa en capas, no secreto absoluto |
| **C — red con filtro** ✅ (7.4) | `--net`, `--allow-host`, `--allow-cidr`, `--block-private`, `-p`, `machine update`, volúmenes | virtio-net con pila de red en userspace en el host (p. ej., gvproxy) y el filtro hecho por nosotros; el TSI queda solo para la VM de build | es la parte más cara y más sensible; medir antes de prometer |
| **D — conveniencias** | ✅ `runner.code` (7.3), `--image ./img.tar` y `-`, `machine stats`, `--error-format json` | go-containerregistry ya lee `docker save`/OCI layout | — |

Orden sugerido: A → B → D → C, con C al final porque abrir la red es lo que más afecta la garantía central del
proyecto.

## Dónde profundizar

- [Arquitectura](/architecture), [Seguridad](/advanced/security), [GPU](/advanced/gpu)
- [Informe de pruebas](/advanced/test-report): lo que se midió en cada plataforma
- `docs/prd.md`, `docs/architecture.md`, `docs/stories/`: requisitos, decisiones y cada story con el registro de los
  defectos
