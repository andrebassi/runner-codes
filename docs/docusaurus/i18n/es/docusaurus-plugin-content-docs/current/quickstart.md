---
title: 'Inicio rápido'
description: 'Instalar libkrun, compilar, construir la imagen de Python, ejecutar el primer job y la primera máquina'
sidebar_position: 2
---

## Requisitos previos

- macOS en Apple Silicon, **o** Linux x86_64/aarch64 con acceso de lectura y escritura a `/dev/kvm`.
- [Nix](https://nixos.org) para instalar libkrun; Go y [Task](https://taskfile.dev) para compilar.
- Acceso a internet para descargar las imágenes OCI de los lenguajes (solo durante el build de la imagen; la VM del
  job no tiene red).

Windows no está soportado.

## 1. Instalar libkrun y libkrunfw

Vía Nix, en ambos sistemas (sin Homebrew ni Docker):

```bash
task krun:install    # nix profile add .#krun
```

- **macOS:** libkrun 1.19.6 y libkrunfw 5.6.2, de derivaciones propias del repo (`nix/`). La 1.ª vez compila
  libkrun (~10 min).
- **Linux:** libkrun y libkrunfw de nixpkgs, con virtio-blk activado.

Las dos bibliotecas van a `~/.nix-profile/lib`, donde `rc` las busca por defecto.

## 2. Compilar

```bash
task build
```

Genera en `bin/`:

- `rc`: el host (CLI, API, build de imágenes, máquinas y el proceso de cada VM);
- `rc-guest-linux-<arch>`: el guest estático que se ejecuta dentro de la VM.

En macOS, el build firma `rc` con el entitlement `com.apple.security.hypervisor`. Sin él, `krun_start_enter` falla.

## 3. Verificar el host

```bash
./bin/rc doctor
```

Una línea por elemento: `ok` en `platform`, `libkrun` y `guest`, líneas `info` para lo que este host no ofrece (CUDA,
virtualización anidada, confinamiento en macOS) y una línea `ok image <stack>` por cada imagen construida. Una falla
aparece como `FAIL <item> <motivo>`.

## 4. Construir la imagen del lenguaje

```bash
task image:build -- --stack python
```

Descarga `python:3.12-alpine`, genera el ext4 de solo lectura con el guest y el warmup grabados, y lo guarda en la
caché. En la máquina medida, la imagen de Python tardó ~8–10 s. Un segundo build con las mismas entradas sale de la
caché. Para todos los lenguajes del catálogo: `task image:build -- --all`.

## 5. Ejecutar un job

```bash
./bin/rc run --stack python --code 'print(1)'
./bin/rc shell --stack python      # shell interactivo en una VM nueva; desaparece con exit
```

Cada `run` arranca una VM nueva (arranque en frío), muestra la salida mientras el programa se ejecuta y destruye la
VM al final. El comando termina con rc 1 si el programa falla o excede el tiempo. Consulte [run y shell](/cli/run).

## 6. Encender una máquina persistente

```bash
./bin/rc machine create --name demo --stack python
./bin/rc machine start --name demo
./bin/rc machine exec --name demo -- sh -c 'echo persistiu > /etc/minha-config'
./bin/rc machine stop --name demo
./bin/rc machine start --name demo
./bin/rc machine exec --name demo -- cat /etc/minha-config     # persistiu
./bin/rc machine delete --name demo --force   # sin --force, una máquina encendida se rechaza (INVALID_STATE)
```

Lo que se graba en `/` y en `/workspace` sobrevive al stop; `/tmp` vuelve vacío. Sin `--net`, la máquina no tiene
red. Consulte [machine](/cli/machine), [runner.code](/cli/runner-code) y [Red y volúmenes](/cli/network).

## 7. Levantar la API

```bash
task serve
```

Escucha en `127.0.0.1:8080` y mantiene un [pool](/concepts/pool) de 2 VMs listas por lenguaje con imagen construida.
En otra terminal:

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{"language":"python","files":[{"name":"main.py","content":"print(input()[::-1])"}],"stdin":"abc"}'
```

```json
{"status":"success","stdout":"cba\n","stderr":null,"exception":null,"executionTime":35}
```

El valor de `executionTime` varía en cada ejecución. La API no tiene autenticación: manténgala en loopback. Consulte
[API HTTP](/cli/http-api) y los [SDKs](/api-reference/sdks).

## 8. Verificar todo de una vez

```bash
task runbook
```

Ejecuta `docs/runbook.md` de punta a punta y falla si algún paso sale distinto de lo esperado. En la última ronda
(21/10/2026, macOS 27), hubo 126 verificaciones y 0 fallas.
