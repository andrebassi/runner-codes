---
title: 'Ejemplos y guías'
description: 'Ejemplos de los SDKs y guías que demuestran cada flujo de rc en una VM real'
---

## Ejemplos

En [`examples/`](https://github.com/andrebassi/runner-codes/tree/main/examples) están los ejemplos de los SDKs, cada
uno probado contra un `rc serve` local en macOS 27 (Apple M1 Pro). Para declarar una máquina en un archivo versionado,
mira el formato del [`runner.code`](/cli/runner-code).

| Ejemplo | Qué muestra | Estado |
|---------|--------------|--------|
| [`sdk-go`](https://github.com/andrebassi/runner-codes/tree/main/examples/sdk-go) | el [SDK de Go](/api-reference/sdks): job y ciclo de vida de máquina contra un `rc serve` | funciona |
| [`sdk-python`](https://github.com/andrebassi/runner-codes/tree/main/examples/sdk-python) | lo mismo con el [SDK de Python](/api-reference/sdks) | funciona |

## Guías

Cada carpeta de [`examples/guides/`](https://github.com/andrebassi/runner-codes/tree/main/examples/guides) es una guía de
uso de `rc`, con un `README.md` que explica el flujo y un `run.sh` que demuestra lo que dice la guía en una VM de verdad y sale
con el número de fallos.

```bash
task build
task guides                        # todas las carpetas
task guides DIRS="pack teardown"   # algunas
```

En la ejecución del 21/10/2026 (macOS 27, Apple M1 Pro): 9 carpetas, 175 comprobaciones, 0 fallos.

| Guía | Qué demuestra |
|------|-------------|
| [install](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/install) | arranque de verdad: marcador del guest y kernel distinto del host |
| [throwaway-machine](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/throwaway-machine) | repo `:ro`, salida escribible, sin red; red de un host a la vez; `--block-private`, loopback del host |
| [pack](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/pack) | imagen y máquina en un artefacto; el marcador viaja con el artefacto aunque se borre el origen; caché reutilizada |
| [teardown](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/teardown) | detener, borrar, demostrar que queda limpio; Ctrl-C y `kill -9` en la CLI no dejan VMs vivas |
| [credentials](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/credentials) | el valor nunca entra en la VM (búsqueda con contraprueba); interceptación solo en el host del binding; 403 en la URL |
| [branch-and-checkpoint](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/branch-and-checkpoint) | checkpoint de disco con retención y bloqueo, restauración de cualquier generación, pause/resume, hijas de una fuente precalentada, checkpoint exportado como pack |
| [gpu-cuda](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/gpu-cuda) | no hay CUDA: `doctor`, la CLI, `runner.code` y la API dicen que no, con el motivo, sin crear nada |
| [local-api](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/local-api) | `rc serve` con cuerpos JSON de ciclo de vida; `exitCode` en el cuerpo; SSE una línea por evento; campo desconocido rechazado |
| [reference](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/reference) | una etapa por tema con prueba (`runner.code`, redes con nombre, nested, limitaciones, sockets, display, propiedad de archivos, S3) y los temas fuera de alcance, con el motivo |

El `lib.sh` de la carpeta reúne lo que usa todo `run.sh`: el formato de `rc machine ls`, el proceso de una VM
(`rc vmm --spec <dir>/spec.json`), el directorio de la máquina (`rc machine data-dir`) y la lista vacía (`No machines found`).

## Runbook

Además de las guías, el [`docs/runbook.md`](https://github.com/andrebassi/runner-codes/blob/main/docs/runbook.md) es la
secuencia de prueba manual, paso a paso, con el resultado esperado de cada uno; `task runbook` ejecuta los mismos pasos
y falla si alguno sale distinto (126 comprobaciones, 0 fallos el 21/10/2026).
