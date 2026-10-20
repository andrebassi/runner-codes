---
title: 'Lenguajes'
description: 'El catálogo de lenguajes, los recursos por lenguaje y cómo agregar uno nuevo'
---

## Catálogo

El catálogo viene de `languages.yaml`, embebido en el binario (`internal/adapters/catalog/languages.yaml`). Son 15 lenguajes (más la stack `llama`, más abajo):

| Lenguaje | Nombre en la API | Imagen OCI de origen | vCPU | RAM | Warmup |
|-----------|-------------|----------------------|------|-----|--------|
| [Python](/languages/python) | `python` | `python:3.12-alpine` | 1 | 512 MiB | `compileall` de la stdlib |
| [Node.js](/languages/nodejs) | `nodejs` | `node:22-alpine` | 1 | 512 MiB | — |
| [Go](/languages/go) | `go` | `golang:1.25-alpine` | 2 | 1024 MiB | `GOCACHE` poblado |
| [Rust](/languages/rust) | `rust` | `rust:1-alpine` | 2 | 1024 MiB | — |
| [Java](/languages/java) | `java` | `eclipse-temurin:21-jdk-alpine` | 2 | 1024 MiB | — |
| [C](/languages/c) | `c` | `alpine:3.22` | 1 | 512 MiB | `apk add build-base` |
| [C++](/languages/cpp) | `cpp` | `alpine:3.22` | 1 | 512 MiB | `apk add build-base` |
| [Ruby](/languages/ruby) | `ruby` | `ruby:3.3-alpine` | 1 | 512 MiB | — |
| [PHP](/languages/php) | `php` | `php:8.3-cli-alpine` | 1 | 512 MiB | — |
| [Perl](/languages/perl) | `perl` | `alpine:3.22` | 1 | 512 MiB | `apk add perl` |
| [Bash](/languages/bash) | `bash` | `bash:5.2` | 1 | 256 MiB | — |
| [sh](/languages/sh) | `sh` | `alpine:3.22` | 1 | 256 MiB | — |
| [AWK](/languages/awk) | `awk` | `alpine:3.22` | 1 | 256 MiB | — |
| [jq](/languages/jq) | `jq` | `alpine:3.22` | 1 | 256 MiB | `apk add jq` |
| [SQLite](/languages/sqlite) | `sqlite` | `alpine:3.22` | 1 | 256 MiB | `apk add sqlite` |

`rc languages` (o `GET /api/v1/languages`) muestra la misma lista con `ready`, que indica si la imagen ya se construyó en este host. Un lenguaje sin imagen construida se rechaza con E006.

## Recursos

vCPU y RAM son un **límite contra el abuso**, no una reserva. La memoria de libkrun es elástica: el host solo paga las páginas que el guest toca, y una VM detenida midió 44 MB de footprint independientemente del límite. Una vCPU ociosa duerme en el hipervisor.

## Cómo se ejecuta el código

| Familia | Lenguajes | Ejecución |
|---------|------------|----------|
| interpretado | Python, Node.js, Ruby, PHP, Perl, Bash, sh, AWK, jq | `<interpretador> <arquivo principal>` |
| compilado | C, C++, Rust | compila a un binario y ejecuta el binario |
| JVM | Java | `javac` y `java Main` |
| Go | Go | `go run` |
| SQL | SQLite | el código va por el stdin de `sqlite3 :memory:` |

En todos: el primer elemento de `files` es el archivo principal, el directorio de trabajo está en el tmpfs de `/tmp`, la VM es nueva y se destruye al final, y no hay red.

## Agregar un lenguaje

Son dos condiciones, y ambas deben cumplirse:

1. **El lenguaje existe en el ejecutor del guest** (`internal/guest/executor.go`), con extensión, comando y, si es compilado, el comando de build. El ejecutor tiene entradas para más lenguajes de los que publica el catálogo; si el que quiere no está ahí, debe agregarse al ejecutor (y recompilar el guest).
2. **Una entrada nueva en `languages.yaml`**:

```yaml
  - name: lua
    image: <imagem OCI oficial>
    cpus: 1
    memory_mib: 256
    warmup: ""
```

| Campo | Regla |
|-------|-------|
| `name` | exactamente el nombre de la entrada en el ejecutor del guest |
| `image` | imagen OCI pública, de la arquitectura del host; el build la descarga sin Docker |
| `cpus`, `memory_mib` | límite contra el abuso |
| `warmup` | una línea de `sh`, ejecutada en chroot en la VM de build, con red; el resultado queda grabado en la imagen. Nada del warmup se ejecuta en la VM del job |

Después:

```bash
task build
task image:build -- --stack <nome>
rc run --stack <nome> --code '<programa mínimo>'
```

El binario del ejecutor debe estar en el `PATH` de la imagen o en la ruta absoluta que usa el ejecutor. Una imagen basada en Alpine (musl) es el caso probado; el guest es estático y se ejecuta en cualquiera.

## La stack `llama` (GPU)

Además de los 15 lenguajes, el catálogo trae la stack `llama` (`fedora:43`, 4 vCPU, 4096 MiB, `gpu: true`): llama.cpp
con backend Vulkan y un modelo Qwen2.5 0.5B grabado en la imagen. Existe para `--gpu`; consulte [GPU](/advanced/gpu).

## Fuera del catálogo

- **Máquinas con cualquier imagen:** `rc machine create --image <ref>` (o `image = "..."` en el
  [runner.code](/cli/runner-code)) construye y enciende una imagen OCI que no está en el catálogo. Sirve para
  máquinas, no para los jobs de la API.
- **Parsers de bases de datos:** el ejecutor del guest todavía tiene entradas para validadores de sintaxis SQL y
  NoSQL, pero no tienen imagen en el catálogo. Consulte [Parsers de bases de datos](/guides/database-parsers-setup).
