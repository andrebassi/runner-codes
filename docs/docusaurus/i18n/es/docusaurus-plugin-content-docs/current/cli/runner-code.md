---
title: 'runner.code'
description: 'El archivo runner.code: la máquina declarada en TOML (stack o imagen, recursos, env, init y workload)'
---

`rc machine create` acepta flags, pero una máquina con entorno, comandos de preparación y un proceso que queda en
ejecución se convierte en una línea demasiado larga para recordarla e imposible de revisar. El **`runner.code`** es
esa declaración en un archivo TOML que va al repositorio junto con el código.

## En una pantalla

```toml
# runner.code
stack = "python"                 # o: image = "redis:7-alpine"
cpus = 2
memory = 2048                    # MiB
storage = 10                     # GiB del disco escribible

env = ["APP_ENV=dev"]
workdir = "/app"

init = [                         # se ejecuta UNA vez, en el primer start del disco
  "pip install --user flask",
]

entrypoint = ["python3", "-m"]   # workload: entrypoint + cmd,
cmd = ["http.server", "8000"]    # arranca en cada start
stop_signal = "SIGINT"
stop_timeout = 30

[dev]                            # perfil de desarrollo, aplicado encima
env = ["DEBUG=1"]
```

```bash
rc machine create --name web               # lee ./runner.code
rc machine create --name web -f infra/runner.code
rc machine start  --name web               # init (1.ª vez) + workload
rc machine logs   --name web --source workload
rc machine stop   --name web               # SIGINT al workload, hasta 30 s, sync, apaga
```

## Claves

| Clave | Tipo | Valor por defecto | Qué hace |
|-------|------|--------|-----------|
| `stack` | texto | — | stack del catálogo como base (`python`, `go`, ...). Usa la imagen con warmup |
| `image` | texto | — | **cualquier** imagen OCI como base (`redis:7-alpine`). Se construye en el primer uso, sin warmup |
| `cpus` | entero | 4 | vCPUs (una ociosa duerme en el hipervisor) |
| `memory` | entero, MiB | 8192 | límite de memoria; libkrun solo asigna lo que el guest toca |
| `storage` | entero, GiB | 10 | disco escribible de la máquina (disperso: ocupa en el host lo que se grabó) |
| `gpu` | booleano | `false` | GPU del host vía Vulkan (Venus). Con `stack`, la stack debe declarar GPU; con `image`, la imagen debe tener Mesa con Venus |
| `gpu_vram` | entero, MiB | el de la stack | ventana de memoria compartida de la GPU; sin `gpu = true` se ignora |
| `env` | lista `K=V` | — | entorno del init, del workload, del `exec` y del `shell` |
| `workdir` | ruta absoluta | — | directorio del init, del workload y del `exec`/`shell` sin `-w`. **Se crea** en el start si no existe |
| `init` | lista de comandos | — | cada elemento se ejecuta con `sh -c`, en orden, **una vez por disco** |
| `entrypoint` | lista | — | inicio del argv del workload |
| `cmd` | lista | — | resto del argv del workload |
| `stop_signal` | texto | `SIGTERM` | señal enviada al workload en el stop: cualquier nombre de Linux (`SIGINT`, `SIGWINCH`, ...), con o sin `SIG`, `SIGRTMIN+N` (numeración de glibc: `SIGRTMIN+3` = 37, el de systemd) o el número (1–64). La tabla es la de Linux, no la del host macOS, donde varios números cambian |
| `stop_timeout` | entero, s | 10 | cuánto tiempo tiene el workload para terminar; después, `SIGKILL`. Máximo 300 |
| `stop_on_exit` | booleano | `false` | cuando el workload termina, la máquina graba el disco y se apaga sola. Requiere un workload |
| `volumes` | lista `HOST:GUEST[:ro\|:staged]` o `/GUEST` | — | directorios del host vía virtiofs, montados en cada start; `:staged` copia al disco y vuelve en el sync; solo `/GUEST` es un directorio en el disco de la máquina. Relativo a la carpeta del archivo; `~` se convierte en el HOME. Consulte [Red y volúmenes](/cli/network#volúmenes) |
| `net` | booleano | `false` | red de salida por el gateway del host, con el piso de bloqueo |
| `ports` | lista `PORTA`, `HOST:GUEST`, rango `A-B:C-D`, `/udp` | — | puertos publicados en el `127.0.0.1` del host; el rango se convierte en un puerto por número (hasta 64 en total) |
| `[network]` | tabla | — | `allow_hosts`, `allow_host_patterns`, `allow_cidrs`, `deny_cidrs`, `block_private`, `allow_private`; cualquiera implica `net`. `name` une la máquina a una [red con nombre](/cli/network#red-con-nombre) (no implica `net`) |
| `[dev]` | tabla | — | `env` **se suma** al de arriba; `workdir`, `init`, `volumes` y `ports` **sustituyen** (una lista vacía no sustituye) |

## Orden de precedencia

**flag > archivo > valor por defecto.** Una flag solo cuenta si se escribió: `--cpus 0` no es lo mismo que no decir
nada.

| Flag | Clave | Cómo se combina |
|------|-------|--------------|
| `-f`, `--file` | — | sin ella, usa `./runner.code` si existe |
| `--stack`, `--image` | `stack`, `image` | una anula a la otra |
| `--cpus`, `--memory`, `--disk-gib`, `--gpu`, `--gpu-vram` | `cpus`, `memory`, `storage`, `gpu`, `gpu_vram` | sustituye |
| `-e`, `--env` (repetible) | `env` | **se suma** al del archivo; dentro de la VM, la última definición de una clave gana |
| `-w`, `--workdir` | `workdir` | sustituye |
| `--init` (repetible) | `init` | sustituye la lista completa |
| `--stop-signal`, `--stop-timeout`, `--stop-on-exit` | las mismas | sustituye |
| `-v`, `--volume` (repetible) | `volumes` | **se suma** (relativo al directorio actual) |
| `--net`, `--block-private` | `net`, `block_private` | sustituye |
| `--network` | `[network] name` | sustituye |
| `-p`, `--port`, `--allow-host`, `--allow-host-pattern`, `--allow-cidr`, `--deny-cidr`, `--allow-private` (repetibles) | `ports`, `[network]` | **se suma** |
| `-- CMD ...` | `entrypoint` + `cmd` | sustituye el workload completo |

`--name` no existe en el archivo a propósito: el mismo `runner.code` sirve para varias máquinas.

## Ciclo de vida

```
rc machine start
  │
  ├─ VM enciende, guest responde                   (igual que una máquina sin runner.code)
  ├─ mkdir -p <workdir>                            (solo si hay init o workload)
  ├─ ¿init ya corrió?            ── sí ───┐        (marcador /var/lib/runner/init-done)
  │        └─ no: cada ítem con sh -c,    │
  │           env + workdir; marca al fin │
  ├─ workload sube en segundo plano  ◀────┘        (grupo de procesos propio)
  │     salida → consola, cada línea con "[workload] "
  └─ start retorna

rc machine stop
  ├─ stop_signal al grupo del workload
  ├─ espera hasta stop_timeout  ── no terminó ──▶ SIGKILL
  ├─ sync del disco
  └─ el guest termina, la VM se apaga
```

Puntos que muerden:

- **Un init que falla deja la máquina encendida** y **no** marca el disco. El `start` devuelve un error con el comando
  y el stderr; `rc machine shell` permite ver el estado, y el siguiente `start` (después de un `stop`) vuelve a
  ejecutar el init desde el principio. Escriba comandos de init que aguanten ejecutarse dos veces (`mkdir -p`,
  `pip install`).
- **El init se ejecuta una vez por disco, no por archivo.** Cambiar el `init` del `runner.code` no ejecuta nada en una
  máquina ya creada; el archivo se lee en el `create`. Para rehacerlo: `rc machine delete` y `create` de nuevo.
- **El workload arranca en cada `start`**, incluso después de un `stop_on_exit`.
- El stop le da al guest `stop_timeout` + 5 s antes de que el host mate la VM: el workload tiene el plazo completo, y
  el sync del disco todavía cabe después.
- `--timeout` en `rc machine stop` vale solo para ese stop y tiene prioridad sobre `stop_timeout`.

## Logs

La consola de la máquina contiene lo que escribe el guest y la salida del workload, cada línea de esta última con la
marca `[workload] `.

```bash
rc machine logs --name web                        # todo
rc machine logs --name web --source workload      # solo el workload, sin la marca
rc machine logs --name web --source workload --tail 20
```

Cuando el workload termina, la última línea es `exited with code N`.

## Cualquier imagen

```toml
image = "redis:7-alpine"
cmd = ["redis-server", "--save", "60", "1"]
```

La imagen se descarga en el host, se aplana y se convierte en un ext4 con la misma VM de build de las stacks, con el
guest y `start.sh` inyectados y el `Env` de la imagen preservado. El nombre interno es `img-` + 12 hex del sha256 de
la referencia: la misma referencia reutiliza el ext4 de la caché.

Diferencia con `stack`: no hay warmup ni entrada en el catálogo, y la imagen debe ser Linux de la arquitectura del
host (arm64 en Mac, amd64 o arm64 en Linux).

## Lo que se rechaza

Una clave que el `runner.code` reconoce pero aún no implementa es un **error con el motivo**, nunca se ignora en
silencio: un `[network] allow_hosts` ignorado haría creer a alguien que la máquina tiene filtro de red.

| Clave | Situación |
|-------|----------|
| `net_backend = "tsi"` | toda la red pasa por el gateway con filtro; `"virtio-net"` se acepta |
| `allow_system_mounts` | montar directorios de sistema del host (`/etc`, `/var/log`) |
| `nested`, `nested_virt` | anidar es decisión de quien enciende la máquina: `--nested` en la CLI |
| `cuda` | rc no tiene CUDA (`--gpu` es Vulkan) |
| `overlay`, `health`, `restart`, `[artifact]`, `[pack]`, `rosetta`, ... | no implementados |

`user`, `docker_socket`, `[secrets]` y `[auth] ssh_agent` se aceptan y funcionan como las flags
(`--user`, `--docker-socket`, `--secret-env`/`--secret-file`, `--ssh-agent`).
| cualquier otra clave | `unknown key(s): ...` (un error de tipeo no pasa) |

## API

`POST /api/v1/machines` acepta los mismos campos en el JSON (`image`, `env`, `workdir`, `init`, `workload`,
`stop_signal`, `stop_timeout`, `stop_on_exit`, `gpu_vram_mib`) o el archivo completo en `runner_code`. Con ambos, el
archivo es la base y todo campo no vacío del JSON tiene prioridad, como las flags en la CLI.

```bash
curl -s localhost:8080/api/v1/machines -d "$(jq -n --rawfile rc runner.code '{name:"web", runner_code:$rc}')"
curl -s 'localhost:8080/api/v1/machines/web/logs?source=workload&tail=20'
curl -s -X POST localhost:8080/api/v1/machines/web/stop          # usa el stop_timeout de la máquina
```
