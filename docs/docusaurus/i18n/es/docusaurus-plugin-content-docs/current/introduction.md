---
title: 'Introducción'
description: 'Ejecución de código no confiable en microVMs libkrun: jobs desechables y máquinas persistentes'
sidebar_position: 1
---

## Qué es Runner Codes

Runner Codes ejecuta código no confiable (normalmente generado por un LLM) dentro de microVMs. El binario es `rc`, y
funciona de dos maneras:

- **Job desechable:** una VM nueva por job, sin red, con root de solo lectura, destruida al final. Es lo que hacen
  `rc run`, `rc shell` y las rutas `/api/v1/run` y `/api/v1/execute`.
- **Máquina persistente:** una VM con disco propio, que guarda lo que se instaló y grabó entre un start y otro.
  Es `rc machine ...`, declarable en un archivo `runner.code` y controlable por la API.

**Código fuente:** https://github.com/andrebassi/runner-codes

## Las piezas

- **VMM:** [libkrun](/concepts/libkrun) 1.19.6 con el kernel de libkrunfw 5.6.2 (Linux 6.12.109), cargada
  directamente como biblioteca mediante purego (sin cgo). Cada VM es un proceso `rc vmm`.
- **Hipervisor:** Hypervisor.framework en macOS Apple Silicon; KVM (`/dev/kvm`) en Linux x86_64 y aarch64.
- **Imágenes:** cada stack se convierte en un ext4 de solo lectura generado a partir de la
  [imagen OCI oficial](/concepts/images), sin Docker. Las máquinas también aceptan cualquier imagen OCI (`--image`).
- **Canal con el guest:** 4 bytes big-endian de tamaño + JSON, en el puerto vsock 5000
  ([protocolo](/api-reference/vsock-protocol)).
- **Latencia:** el [pool](/concepts/pool) de `rc serve` mantiene VMs ya arrancadas por lenguaje, cada una usada una
  sola vez.

## Qué hace `rc`

| Recurso | Dónde está |
|---------|-----------|
| job efímero por la CLI, con salida en streaming; shell interactivo en una VM nueva | [run y shell](/cli/run) |
| máquinas: `create`, `start`, `exec`, `shell`, `cp`, `logs`, `stop`, `delete`, `branch`, `pause`, `resume`, `sync`, `update`, `data-dir`, `prune` | [machine](/cli/machine) |
| máquina declarada en un archivo | [runner.code](/cli/runner-code) |
| red por política (allow-host, CIDR, `--block-private`), redes con nombre, ICMP, puertos y rangos de puertos | [Red y volúmenes](/cli/network) |
| volúmenes: directorio del host, `:ro`, `:staged`, disco de la máquina, S3 | [Red y volúmenes](/cli/network#volúmenes) |
| credenciales que la máquina usa sin leerlas, secretos, agente SSH, puentes de socket | [Credenciales](/cli/credentials), [machine](/cli/machine) |
| GPU vía Vulkan (Venus) y la stack `llama` | [GPU](/advanced/gpu) |
| pantalla de la VM en el navegador, con teclado y mouse (`--display`) | [Pantalla en el navegador](/cli/display) |
| imagen o máquina en un artefacto portátil | [pack](/cli/pack) |
| API HTTP con pool, SSE y rutas de máquina | [API HTTP](/cli/http-api), [Visión general de la API](/api-reference/overview) |
| SDKs Go (`pkg/client`) y Python (`sdk/python`) | [SDKs](/api-reference/sdks) |
| verificación del host | `rc doctor`, en [Visión general de la CLI](/cli/overview#doctor) |

## Plataformas

| Plataforma | Estado |
|------------|--------|
| macOS, Apple Silicon | soportado, probado de punta a punta y medido |
| Linux x86_64 / aarch64 con `/dev/kvm` | soportado; el confinamiento Landlock + seccomp se probó, pero una microVM completa en Linux **aún no se ha medido** |
| Windows | **no soportado**: libkrun upstream no tiene backend WHP, y el build para Windows falla a propósito |

## Números medidos

macOS 27, Apple Silicon. En Linux todavía no hubo medición.

| Qué | Valor |
|-------|-------|
| arranque hasta la 1.ª respuesta de Python (30 arranques, root ext4 ro) | p50 260 ms, p95 266 ms |
| job Python por la CLI, arranque en frío | ~240 ms en total |
| job por la API con el pool lleno | 26–43 ms de solicitud HTTP completa (una stack) |
| `machine start` hasta que el guest responde | 204–244 ms |
| footprint de una VM detenida | 44 MB |
| `go run` con la caché de build caliente en la imagen | 247 ms (sin la caché, 3,4 s) |

Detalles y método en [Rendimiento](/advanced/performance) y en el [Informe de pruebas](/advanced/test-report).

## Lenguajes

El catálogo tiene 15 lenguajes: Python, Node.js, Go, Rust, Java, C, C++, Ruby, PHP, Perl, Bash, sh, AWK, jq y SQLite,
además de la stack `llama` (llama.cpp con Vulkan). Consulte [Lenguajes](/languages/overview).

## Próximos pasos

- [Inicio rápido](/quickstart): instalar libkrun, compilar, construir la imagen, ejecutar el primer job y la primera
  máquina.
- [Referencia para agentes](/agents): cada comando, flag, ruta y error en una sola página.
- [Arquitectura](/architecture): las piezas y el recorrido de un job y de una máquina.
- [Ejemplos y guías](/guides/examples): ejemplos de `runner.code` y guías que prueban cada flujo en una VM real.
- [Seguridad](/advanced/security): qué aísla el código y qué falta todavía.
