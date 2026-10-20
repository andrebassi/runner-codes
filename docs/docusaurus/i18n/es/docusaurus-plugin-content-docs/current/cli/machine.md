---
title: 'machine'
description: 'Máquinas persistentes: una VM con nombre y disco propio, que guarda lo que grabes entre comandos y entre stop y start'
---

`rc run` crea una VM nueva por job y la destruye al final. Para trabajo en varias etapas (instalar, compilar, probar,
corregir), use una **máquina**: una VM con nombre, con disco propio, que sigue existiendo entre un comando y otro y
entre `stop` y `start`.

## En una pantalla

```bash
rc machine create --name dev --stack python      # registra y crea el disco (no la enciende)
rc machine start  --name dev                     # la enciende; la VM sigue después de que el comando termina
rc machine exec   --name dev -- python3 -V       # ejecuta un comando (salida en streaming)
rc machine shell  --name dev                     # terminal interactiva (Ctrl+C, vi, resize)
rc machine cp ./app.py dev:/workspace/app.py     # sube un archivo
rc machine exec   --name dev -- python3 /workspace/app.py
rc machine cp dev:/workspace/result.json .       # descarga un archivo
rc machine logs   --name dev --tail 20           # consola de la máquina (funciona detenida)
rc machine stop   --name dev                     # graba el disco y la apaga
rc machine ls                                    # todas, con su estado
rc machine delete --name dev                     # borra máquina y disco (-f si está encendida)
```

`--name` es opcional en todos: el valor por defecto es `$RC_MACHINE_NAME` o, sin ella, `default`. Con direnv se puede
fijar una máquina por proyecto.

Con un [`runner.code`](/cli/runner-code) en el directorio, el `create` lee de él la base (stack o imagen), los
recursos, el entorno, el init y el workload; las flags pasadas tienen prioridad sobre el archivo.

## Qué persiste

| Ruta | ¿Persiste? | Dónde vive |
|---------|-----------|-----------|
| `/` (paquetes, `/etc`, `/opt`, `/root`...) | **sí** | capa escribible (overlay) en el disco de la máquina, encima de la imagen de la stack |
| `/workspace` | **sí** | directorio en el disco de la máquina; el lugar indicado para código, datos y resultados |
| `/tmp` | **no** | tmpfs de 256 MiB, vacío en cada start |

El `HOME` es `/root`, que también persiste.

## Cómo funciona

```
host (macOS)                                      │ guest
                                                  │
~/Library/Caches/runner-codes/machines/dev/       │
  machine.json   registro (stack, cpus, ...)      │
  disk.ext4      disco de máquina ── /dev/vdb ────┼─▶ /persist ─┬─ upper/      (lo grabado)
  spec.json      spec de la VM                    │             ├─ work/       (de overlayfs)
  console.log    consola (rc machine logs)        │             └─ workspace/  ─▶ /workspace
  v.sock         canal con el guest (vsock 5000)  │
  vmm.pid        proceso de la VM                 │  imagen de la stack ── /dev/vda (solo lectura)
                                                  │  overlay: imagen abajo + upper arriba ─▶ /
```

- **Disco**: la primera vez, la VM de build formatea un ext4 vacío disperso (`disks/data-10g.ext4`). Cada máquina
  recibe un **clon** de él: clonefile en APFS, FICLONE en btrfs/XFS, copia dispersa en el resto. Medido en macOS: un
  disco de 10 GiB ocupa 6,4 MB en el host justo después de crearse.
- **Arranque**: el primer proceso de la máquina es `/opt/runner/machine.sh`, en lugar del `start.sh` de los jobs.
  Monta el disco en `/persist` y un overlayfs con la imagen abajo y `upper/` arriba, enlaza `/workspace` y ejecuta el
  guest-runner en un `chroot` ahí.
- **VM desacoplada**: el proceso `rc vmm` de la máquina se ejecuta en una sesión propia (`setsid`), sin el pipe de
  vigilancia y sin `Pdeathsig`. Por eso sobrevive al `rc` que la encendió. El pid queda en `vmm.pid`, y "encendida"
  significa pid vivo. Los jobs efímeros no cambian: su VM sigue muriendo junto con el padre.
- **Stop**: el host le pide `shutdown` al guest, que hace `sync` (graba en el ext4 lo que está en caché), responde y
  termina, y la VM se apaga. Si no termina dentro de `--timeout` (10 s por defecto), recibe `SIGKILL`. `--timeout 0`
  la mata al instante.
- **Terminal (`shell`)**: el guest abre un PTY (devpts montado por `machine.sh`) y ejecuta el shell en él. La conexión
  se convierte en un canal de frames en ambos sentidos: `input` (teclas, en base64 para no perder bytes de control) y
  `resize` del host; `output` y `exit` del guest. La terminal del host queda en modo raw, y el `SIGWINCH` se convierte
  en `resize`. Cerrar la terminal mata el programa en el guest; la máquina sigue encendida.
- **Imagen actualizada**: el start usa la imagen actual de la stack. Si la imagen se reconstruye, la máquina pasa a
  ver la nueva debajo de lo que ella misma grabó, como una actualización del sistema.

## Máquina efímera: `machine run`

Crea, enciende y usa de una sola vez:

```bash
rc machine run --net --image alpine -- uname -a          # ejecuta y borra
rc machine run --net -it --image alpine -- /bin/sh       # terminal; se borra al salir
echo dados | rc machine run -i --stack python -- sh -c 'read x; echo $x'
rc machine run -d -s runner.code                         # queda encendida con el workload
docker save meuapp | rc machine run --image - -- ./app   # imagen local, sin registry
rc machine run --image ./meuapp.tar -- ./app             # tar de docker save o directorio OCI layout
```

| Punto | Cómo es |
|-------|--------|
| qué se ejecuta | el comando después de `--`; si no, el `cmd`/`entrypoint` del `runner.code`; si no, con `-t`, un shell |
| sin `-d` | nombre `run-<hex>`; la máquina se borra al final, también con Ctrl+C (rc 130) y con SIGTERM |
| `-d` | nombre `default` (o `--name`); el comando se convierte en el workload y la máquina queda encendida |
| `-t` / `-it` | terminal real (PTY en el guest); `-i` sin `-t` solo reenvía el stdin; sin ninguno, el stdin no se lee |
| imagen local | ruta que empieza con `/`, `./` o `../`: tar de `docker save` (o tar OCI) o directorio OCI layout; una arquitectura distinta de la del host se rechaza |
| `--image -` | lee el tar del stdin y lo guarda en `<cache>/imports/sha256-<hash>.tar`: la misma imagen reutiliza la caché |
| init | se ejecuta (el disco siempre es nuevo) |

Las flags de declaración son las del `create`, incluidas `-s`/`-f`, `-v`, `-p`, `--net`, la política de red y
`--display LxA` (consulte [Pantalla en el navegador](/cli/display)).

## Branch y pausa

```bash
rc machine pause  --name dev                    # congela: procesos, memoria y conexiones quedan como están
rc machine resume --name dev
rc machine branch --from dev --name try-1       # copia copy-on-write del disco (encendida o detenida)
```

| Punto | Cómo es |
|-------|--------|
| pause | SIGSTOP al proceso de la VM: las vCPUs, el gateway de red y el visor se detienen juntos; `exec`, `cp`, `update` y `start` rechazan con `INVALID_STATE`; `stop` reanuda antes de apagar |
| branch | el disco de la hija es un clon copy-on-write del de la madre (clonefile en APFS, FICLONE en Linux): instantáneo y sin ocupar espacio hasta que diverge. La instalación, `/root` y `/workspace` vienen incluidos, y el init no se vuelve a ejecutar |
| madre encendida o pausada | el guest hace `sync` y la VM se congela solo durante el clon; el disco queda consistente como después de un corte de energía |
| qué no viene | la **ejecución**: procesos y memoria. Un checkpoint de RAM y CPU requiere snapshot, que libkrun upstream no tiene |
| puertos y pantalla | la hija hereda los mismos puertos y el puerto de pantalla; las dos no arrancan juntas sin cambiar uno de ellos |

## Comandos

| Comando | Flags | Observación |
|---------|-------|------------|
| `create` | `--name`, `-f FILE`, `--stack` o `--image`, `--cpus` (4), `--memory` MiB (8192, elástica), `--disk-gib` (10), `--gpu`, `--gpu-vram`, `-e K=V`, `-w DIR`, `-u USER`, `--init CMD`, `--stop-signal`, `--stop-timeout`, `--stop-on-exit`, `-v`, `-p`, `--docker-socket`, `--expose-socket`, `--mount-socket`, `--secret-env`, `--secret-file`, `--ssh-agent`, `--nested`, `-- WORKLOAD` | no enciende la máquina. Sin `-f`, lee `./runner.code` si existe. Consulte [runner.code](/cli/runner-code) |
| `start` | `--name` | espera a que el guest responda (hasta 30 s); si no responde, la mata y muestra el final de la consola. Si el proceso de la VM muere antes (configuración rechazada, como `--nested` en un host que no anida), el error llega al instante con la última línea de su log. Después ejecuta el init (1.ª vez del disco) y levanta el workload |
| `stop` | `--name`, `--timeout` (por defecto: el `stop_timeout` de la máquina, o 10s) | envía el `stop_signal` al workload antes del sync; detener una máquina ya detenida no es un error |
| `status` | `--name`, `--json` | `running` o `stopped`, con el pid |
| `ls` | `--json` | todas las máquinas |
| `delete` | `--name`, `-f` | encendida, solo con `-f` |
| `exec` | `--name`, `-e K=V` (repetible), `-w DIR`, `-i` (reenvía el stdin), `--timeout` (0 = sin límite) | argv **sin shell implícito**: para un shell, `-- sh -c '...'`. El rc de `rc` es el del comando (124 en el timeout) |
| `shell` | `--name`, `-e K=V`, `-w DIR`, `-- CMD` opcional | terminal real (PTY en el guest): Ctrl+C interrumpe solo el comando, el tamaño de la ventana sigue al host. Sin comando, abre `bash` si la imagen lo tiene, si no `sh`. El rc de `rc` es el del `exit` |
| `cp` | `SRC DST`, con `nome:/caminho` en uno de los lados | fragmentos de 1 MiB; atómico en ambos lados; hasta 4 GiB |
| `logs` | `--name`, `--tail N`, `--source all\|workload` | consola de la VM: lo que escriben el guest, `machine.sh` y el workload; `workload` filtra solo este último |
| `sync` | `--name` | devuelve al host las copias de los volúmenes staged (ver [Red y volúmenes](/cli/network#volumen-staged--v-hostgueststaged)) |
| `data-dir` | `--name` | el directorio de la máquina en el host (registro, disco, sockets publicados, logs) |
| `prune` | `--all` | lo mismo que `rc image prune`: libera builds antiguos de imágenes; con `--all`, también las imágenes de referencia que ninguna máquina usa |

## Errores

Todo rechazo trae un código estable. Decida por el código, no por el mensaje:

| Código | Cuándo |
|--------|--------|
| `NOT_FOUND` | la máquina (o el archivo, en el `cp`) no existe |
| `CONFLICT` | `create` con un nombre ya usado |
| `INVALID_STATE` | `start` en una encendida; `start` en una pausada (`is paused; resume it ... or stop it first`); `exec`/`cp` en una detenida; `delete` en una encendida sin `-f`; `sync` en una detenida con volumen staged |
| `CONFIG_ERROR` | nombre inválido (minúsculas, dígitos y `-`, hasta 63), stack sin imagen, `--gpu` en una stack sin GPU, ruta relativa en el `cp` |

## Números medidos

macOS 27, Apple M1 Pro, stack python:

| Medida | Valor |
|--------|-------|
| `start` hasta que el guest responde | 204–244 ms |
| disco de 10 GiB recién creado, espacio en el host | 6,4 MB |
| `cp` de 50 MB hacia la máquina | 620 ms (~84 MB/s) |
| `cp` de 50 MB desde la máquina | 594 ms (~88 MB/s) |

## Usuario, secretos, agente SSH y sockets

| Flag | Qué hace |
|------|-----------|
| `-u`, `--user NOME\|uid[:gid]` | el workload, exec y shell se ejecutan como ese usuario; el init se ejecuta como root. `exec --user` y `shell --user` cambian solo ese comando |
| `--secret-env VAR=HOST_VAR` | el valor de la variable del host, leído en el start, en el entorno de todo comando; queda solo en la memoria del guest, nunca en el registro |
| `--secret-file VAR=/abs/arquivo` | lo mismo, con el contenido de un archivo del host. Un secreto con el nombre de una credencial alimenta el binding y no entra en la VM |
| `--ssh-agent` | el agente de quien ejecuta el start (`SSH_AUTH_SOCK`) aparece en el guest; la clave privada se queda en el host |
| `--docker-socket` | el `/var/run/docker.sock` del guest se convierte en `docker.sock` en el `data-dir` de la máquina |
| `--expose-socket GUEST[:HOST]` | un socket Unix en el que escucha el workload, alcanzable desde el host (por defecto: su nombre en el `data-dir`) |
| `--mount-socket HOST:GUEST` | un socket del host dentro de la máquina; el directorio del guest se crea si falta. Hasta 64 puentes por máquina |
| `--nested` | expone las extensiones de virtualización del host (`/dev/kvm` en el guest). Requiere KVM anidado en Linux o un Mac M3+ con macOS 15+; `rc doctor` indica si este host anida. El `runner.code` no lo activa |

Los puentes de socket solo existen en la CLI: por la API serían rutas arbitrarias en el disco del servidor.

## API

Las mismas operaciones en `rc serve`. Consulte [Visión general de la API](/api-reference/overview#máquinas).

## Fuera de esta versión

El WebSocket interactivo de la API (hoy la terminal es solo por la CLI), `machine stats` y el checkpoint de RAM
(libkrun upstream no tiene snapshot).
Red, puertos y volúmenes están en [Red y volúmenes](/cli/network). El plan está en la
[Referencia para agentes](/agents#plan-de-evolución).
