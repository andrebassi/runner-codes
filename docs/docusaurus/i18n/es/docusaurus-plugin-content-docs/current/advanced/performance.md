---
title: 'Rendimiento'
description: 'Números medidos de arranque, ejecución, máquinas, memoria y build de imágenes'
---

## Dónde se midió

Todos los números de esta página se midieron en **macOS 27, Apple Silicon (M1 Pro)**, el 19 y 20/10/2026. **Linux
aún no se ha medido**; no asuma que los valores se repiten con KVM.

## Arranque

| Medida | Valor |
|--------|-------|
| arranque hasta la 1.ª respuesta del guest Python, root ext4 de solo lectura, 30 arranques | p50 260 ms, p95 266 ms |
| efecto de la RAM configurada (256 a 2048 MiB) en el arranque | ninguno |

"Respuesta" es la primera respuesta del guest al job `sh true` que la espera de disponibilidad envía cada 1 ms. Es el momento
en que la VM puede recibir un job real.

## Ejecución de un job

| Camino | Valor |
|---------|-------|
| CLI, job Python, arranque en frío | ~240 ms en total, de los cuales 31–38 ms son de ejecución |
| API, pool lleno, un stack con imagen | 26–43 ms de solicitud HTTP completa |
| API, 15 stacks, todos los pools llenos | 49–175 ms |

La diferencia entre CLI y API es el arranque, que el [pool](/concepts/pool) saca del camino de la solicitud. Con el pool vacío, la
API cae en el mismo arranque en frío que la CLI.

Por stack, mediante la CLI con arranque en frío:

| Stack | Ejecución | Total (con arranque) |
|-------|----------|------------------|
| python | 31–44 ms | ~240 ms |
| nodejs | 128–148 ms | 353–371 ms |
| go | 247 ms con la caché de build precalentada (3,4 s sin ella) | 510 ms |
| java | 522 ms | 739 ms |
| c | 71–72 ms | 282 ms |
| sh | 2 ms | 199 ms |

## Máquinas

| Medida | Valor |
|--------|-------|
| `machine start` hasta que el guest responde (python) | 204–244 ms |
| `start` de máquina con red | 190 ms |
| disco de 10 GiB recién creado, espacio en el host | 6,4 MB |
| `cp` de 50 MB hacia la máquina / desde la máquina | 620 ms (~84 MB/s) / 594 ms (~88 MB/s) |
| nombre bloqueado por la política hasta el error en el guest | < 1 s |

## Memoria

| Medida | Valor |
|--------|-------|
| footprint de una VM detenida | 44 MB |
| RSS de la misma VM | 172 MB, incluidas páginas limpias compartidas |

La memoria de la libkrun es elástica: el host solo paga las páginas que el guest toca. El límite de RAM por lenguaje en el catálogo
limita los abusos, pero no se reserva.

## Build de imágenes y pack

| Medida | Valor |
|--------|-------|
| build de la imagen Python | ~8–10 s |
| pull + aplanado de las capas (python) | 2,7 s |
| warmup de Python (`compileall`) | ~1 s |
| build de la imagen Go | 35 s, de los cuales 28 s son de `mkfs.ext4` |
| pack de la imagen python | 134 MiB → payload de 31 MiB |
| pack de una máquina python con un paquete instalado, archivo único | 46 MiB |

El build es un costo único por clave de caché; un segundo build con las mismas entradas sale de la caché. El tiempo de cada stack
está en el [Informe de pruebas](/advanced/test-report).

## GPU

`llama-bench` con Qwen2.5 0.5B: procesar el prompt alcanzó 893–1.365 tokens/s en la GPU (Venus) frente a 193–264 en la CPU.
Detalles y la interpretación de los números en [GPU](/advanced/gpu).

## Lo que el rendimiento debe al diseño

- **Root en disco ext4 de solo lectura**, no virtiofs: virtiofs no fue más rápido y no confina al guest a los directorios
  compartidos.
- **Warmup grabado en la imagen:** el `.pyc` de la stdlib de Python y la caché de build de Go ya vienen listos; para Go, la
  copia al tmpfs ocurre en el arranque, antes de que la VM entre al pool.
- **Pool con VMs de uso único:** el arranque ocurre en segundo plano, con un máximo de 2 simultáneos por lenguaje.
- **Disco de máquina clonado:** clonefile en APFS; el disco nuevo no copia los 10 GiB.

## Mediciones pendientes

- rendimiento en Linux (x86_64 y aarch64, KVM);
- máquinas con GPU en un modelo mayor que 0,5B.
