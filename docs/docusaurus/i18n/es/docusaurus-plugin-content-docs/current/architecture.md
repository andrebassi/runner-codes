---
title: 'Arquitectura'
description: 'Las piezas de Runner Codes y el recorrido de un job y de una máquina, desde HTTP hasta la microVM'
sidebar_position: 3
---

## Visión general

```
 cliente HTTP / CLI / SDK
        │
        ▼
┌───────────────────────────────┐
│ rc  (host: macOS o Linux)     │
│  ├─ dominio: valida job/spec  │
│  ├─ pool: VMs listas por      │
│  │   lenguaje (solo jobs)     │
│  ├─ máquinas: registro, disco │
│  └─ launcher: spawn del vmm   │
└──────────────┬────────────────┘
               │ spawn (nunca fork)
               ▼
┌───────────────────────────────┐
│ rc vmm --spec   (1 por VM)    │
│  libkrun (purego) +           │
│  libkrunfw (kernel Linux)     │
│  HVF en macOS / KVM en Linux  │
│  gateway de red (máquinas     │
│   con --net), visor pantalla  │
└──────────────┬────────────────┘
               │ socket Unix  ⇄  vsock puerto 5000
               ▼
┌───────────────────────────────┐
│ microVM                       │
│  job: root ext4 solo lectura, │
│       /tmp tmpfs, sin red     │
│  máquina: overlay de imagen + │
│       disco propio (/dev/vdb) │
│  /opt/runner/guest (estático) │
└───────────────────────────────┘
```

## Binarios

| Binario | Dónde se ejecuta | Qué hace |
|---------|-----------|-----------|
| `rc` | host | `run`, `shell`, `machine ...`, `pack ...`, `serve` (API + pool), `image build/prune`, `languages`, `doctor` y el subcomando interno `vmm` |
| `runner-guest` | dentro de la VM | escucha en el puerto vsock 5000 y atiende cada operación: ejecutar código, exec, copia de archivos, PTY, workload, puerto publicado, secretos, agente SSH, montaje S3. Se guarda como `bin/rc-guest-linux-<arch>` |

Son dos binarios porque purego fuerza el enlace dinámico contra glibc en Linux, y las imágenes de los lenguajes son
Alpine (musl). Un guest dinámico falla dentro de la VM con `not found`; el guest se compila de forma estática, y
`scripts/build.sh` falla si sale dinámico.

## Capas del host

El código del host sigue una arquitectura hexagonal; `cmd/rc` solo conecta las piezas.

| Capa | Paquete | Responsabilidad |
|--------|--------|------------------|
| dominio | `internal/domain` | job, resultado, límites (stdin, archivos, tamaño, timeout) y errores E00x |
| aplicación | `internal/app` | ejecutor y pool de jobs; ciclo de vida de las máquinas, branch, volúmenes staged, credenciales, secretos |
| ports | `internal/ports` | interfaces (`VMSpec` y su validación, launcher, catálogo, imágenes) |
| adapters | `internal/adapters/krun` | carga de libkrun, configuración de la VM, launcher, proceso `vmm` y confinamiento en Linux |
| | `internal/adapters/oci` | pull de la imagen OCI y build del ext4 en la VM de build |
| | `internal/adapters/catalog` | `languages.yaml` embebido |
| | `internal/adapters/guestproto` | cliente del protocolo vsock |
| | `internal/adapters/httpapi` | API HTTP (jobs y máquinas) |
| | `internal/adapters/machinestore` | registro y disco de cada máquina (clon del disco base) |
| | `internal/adapters/runfile` | lectura del archivo `runner.code` (TOML) |
| | `internal/adapters/netgw` | gateway de red en userspace: política de salida, DNS, puertos, ICMP, red con nombre, intercepción de credenciales |
| | `internal/adapters/screen`, `viewer` | pantalla de la VM en el navegador |
| | `internal/adapters/hostfs` | lado del host de los volúmenes (modo de los archivos de `:staged`) |
| otros | `internal/credca`, `internal/objectstore`, `internal/s3fs`, `internal/pack` | CA por máquina, cliente S3, núcleo del volumen `s3://`, formato del pack |
| guest | `internal/guest` | servidor vsock y ejecutor por lenguaje |

SDKs: `pkg/client` (Go) y `sdk/python` (Python), solo con la biblioteca estándar.

## Recorrido de un job por la API

1. `POST /api/v1/run` llega a `httpapi`, que arma el job.
2. El dominio valida: lenguaje en el catálogo (si no, E006), stdin ≤ 1.000.000 bytes (si no, E005), hasta 32
   archivos, código total ≤ 4 MiB, nombres de archivo válidos, timeout entre 0 y 300 s. Un rechazo se convierte en
   HTTP 400.
3. El pool entrega una VM ya arrancada de ese lenguaje. Si el pool está vacío, arranca una VM en el momento (arranque
   en frío): nunca rechaza.
4. El host se conecta al socket Unix vinculado al puerto vsock 5000 de la VM y envía el job como frame (4 bytes de
   tamaño + JSON).
5. El guest graba los archivos en un directorio en `/tmp`, compila si el lenguaje lo requiere, ejecuta con el stdin
   del job y el timeout, y responde con otro frame.
6. La VM se destruye (SIGKILL al grupo del proceso) y el pool repone una VM nueva en segundo plano.
7. La API devuelve `status`, `stdout`, `stderr`, `exception`, `executionTime`.

## Recorrido de una máquina

1. `rc machine create` graba el registro en `<cache>/machines/<nome>/machine.json` y un disco ext4 clonado del disco
   base (clonefile en APFS, FICLONE en btrfs/XFS, copia dispersa en el resto).
2. `rc machine start` arranca `rc vmm` **desacoplado** (sesión propia, sin vínculo con el padre): la máquina sigue
   encendida después de que el comando termina. El pid queda en `vmm.pid`.
3. El primer proceso de la VM es `/opt/runner/machine.sh`: monta el disco en `/persist`, un overlayfs con la imagen
   abajo y `upper/` arriba, enlaza `/workspace` y ejecuta el guest en un `chroot`.
4. `exec`, `shell`, `cp`, `logs` hablan con el guest por el mismo socket del puerto 5000. Con `--net`, la VM obtiene
   `eth0` y el tráfico pasa por el gateway del propio proceso `vmm`, que aplica la política de salida.
5. `rc machine stop` le pide `shutdown` al guest, que hace `sync` y termina; si no termina dentro del `--timeout`,
   SIGKILL.

Detalles en [machine](/cli/machine) y [Red y volúmenes](/cli/network).

## Ciclo de vida de una VM

- El launcher crea el proceso `rc vmm --spec <arquivo>` mediante **spawn**. Hypervisor.framework mata el fork de un
  proceso multihilo, por eso nunca se usa fork.
- El proceso `vmm` carga libkrun, configura la VM a partir de la spec y llama a `krun_start_enter`, que toma el
  control del proceso hasta que la VM termina.
- Timeout de job: SIGKILL al grupo del proceso.
- La VM de job muere con el padre: `Pdeathsig` en Linux; en macOS, un pipe de vigilancia en el fd 3. La VM de máquina
  está desacoplada.
- Si el `vmm` muere en el arranque, el start termina de inmediato citando la última línea de `vmm.log` escrita en
  este start.

Detalles en [libkrun](/concepts/libkrun).

## Imágenes

Cada stack tiene un ext4 de solo lectura generado a partir de la imagen OCI oficial, con el guest, `start.sh` y el
warmup grabados. El build se ejecuta en una VM Alpine propia. Consulte [Imágenes](/concepts/images).

## Configuración por entorno

| Variable | Valor por defecto | Uso |
|----------|--------|-----|
| `RUNNER_LISTEN` | `127.0.0.1:8080` | dirección de la API (`serve`) |
| `RUNNER_CACHE_DIR` | `~/Library/Caches/runner-codes` (macOS), `~/.cache/runner-codes` (Linux) | imágenes, VM de build, máquinas, packs y sockets |
| `RUNNER_KRUN_LIB` | `~/.nix-profile/lib/libkrun.dylib` (macOS); en Linux `~/.nix-profile/lib/libkrun.so.1` o el soname `libkrun.so.1` | ruta de libkrun |
| `RUNNER_GUEST_BIN` | `rc-guest-linux-<arch>` junto al host | guest inyectado en las imágenes |
| `RUNNER_SANDBOX` | activado | `off` desactiva Landlock + seccomp en Linux, solo para diagnóstico |
