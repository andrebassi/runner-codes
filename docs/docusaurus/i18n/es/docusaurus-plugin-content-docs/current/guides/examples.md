---
title: 'Ejemplos y guías'
description: 'Ejemplos de runner.code listos para ejecutar y guías que demuestran cada flujo de rc en una VM real'
---

## Ejemplos

Un ejemplo por carpeta en [`examples/`](https://github.com/andrebassi/runner-codes/tree/main/examples), cada uno probado
en una VM real en macOS 27 (Apple M1 Pro). Los de máquina son un archivo [`runner.code`](/cli/runner-code); `rc` acepta
`-s` como sinónimo de `-f` para indicarlo.

| Ejemplo | Qué muestra | Estado |
|---------|--------------|--------|
| [`python-app`](https://github.com/andrebassi/runner-codes/tree/main/examples/python-app) | imagen pública, `init` con `pip` por la red | funciona |
| [`node-app`](https://github.com/andrebassi/runner-codes/tree/main/examples/node-app) | `init` con `npm -g`, app en un puerto publicado | funciona |
| [`openclaw-app`](https://github.com/andrebassi/runner-codes/tree/main/examples/openclaw-app) | gateway de LLM como workload, `/health` por el puerto | funciona |
| [`doom-web`](https://github.com/andrebassi/runner-codes/tree/main/examples/doom-web) | servidor estático desde un volumen, juego en el navegador | funciona |
| [`docker-in-vm`](https://github.com/andrebassi/runner-codes/tree/main/examples/docker-in-vm) | Docker completo dentro de la VM | funciona |
| [`headless-browser`](https://github.com/andrebassi/runner-codes/tree/main/examples/headless-browser) | Chromium headless y captura de pantalla | funciona, sin GPU |
| [`gpu-chrome`](https://github.com/andrebassi/runner-codes/tree/main/examples/gpu-chrome) | Fedora con Vulkan/Venus y Chromium | Vulkan funciona; Chromium sin GPU |
| [`local-llm`](https://github.com/andrebassi/runner-codes/tree/main/examples/local-llm) | llama.cpp con Vulkan en la GPU del host | funciona: 56,7 tokens/s en la GPU frente a 10,3 en la CPU (Qwen2.5 0.5B Q4_K_M) |
| [`desktop`](https://github.com/andrebassi/runner-codes/tree/main/examples/desktop) | escritorio Wayland (Weston) en la [pantalla del navegador](/cli/display), con teclado y ratón | funciona |
| [`sdk-go`](https://github.com/andrebassi/runner-codes/tree/main/examples/sdk-go) | el [SDK de Go](/api-reference/sdks): job y ciclo de vida de máquina contra un `rc serve` | funciona |
| [`sdk-python`](https://github.com/andrebassi/runner-codes/tree/main/examples/sdk-python) | lo mismo con el [SDK de Python](/api-reference/sdks) | funciona |

```bash
rc machine run -s examples/python-app/runner.code -- python3 --version
rc machine run -d --name doom -s examples/doom-web/runner.code && open http://localhost:8080
rc machine run -d --name openclaw -s examples/openclaw-app/runner.code && curl localhost:18789/health
```

### Tiempo del primer start (medido)

El primer start ejecuta el `init` (instalación de paquetes); los siguientes reutilizan el disco de la máquina.

| Ejemplo | 1.er start | Siguientes |
|---------|----------|-----------|
| python-app | 6 s | < 1 s |
| node-app | 2 s | < 1 s |
| openclaw-app | ~73 s (incluye convertir la imagen) | < 1 s |
| docker-in-vm | 4 s | 4 s (el dockerd arranca en cada start) |
| headless-browser | 15 s | < 1 s |
| gpu-chrome | 581 s (metadatos y paquetes de Fedora) | < 1 s |
| local-llm | 488 s (Fedora + build de llama.cpp) | < 1 s |
| desktop | 12 s | 5 s (udev, seatd y Weston arrancan en cada start) |

Las notas de cada ejemplo (por qué Chromium renderiza sin GPU, las trampas de Docker dentro de la VM, la pantalla 2D del
escritorio) están en [`examples/README.md`](https://github.com/andrebassi/runner-codes/tree/main/examples).

## Guías

Cada carpeta de [`examples/guides/`](https://github.com/andrebassi/runner-codes/tree/main/examples/guides) es una guía de
uso de `rc`, con un `README.md` que explica el flujo y un `run.sh` que demuestra lo que dice la guía en una VM de verdad y sale
con el número de fallos.

```bash
task build
task guides                        # todas las carpetas
task guides DIRS="pack teardown"   # algunas
```

En la ejecución del 20/10/2026 (macOS 27, Apple M1 Pro): 12 carpetas, 215 comprobaciones, 0 fallos.

| Guía | Qué demuestra |
|------|-------------|
| [install](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/install) | arranque de verdad: marcador del guest y kernel distinto del host |
| [throwaway-machine](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/throwaway-machine) | repo `:ro`, salida escribible, sin red; red de un host a la vez; `--block-private`, loopback del host |
| [pack](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/pack) | imagen y máquina en un artefacto; el marcador viaja con el artefacto aunque se borre el origen; caché reutilizada |
| [teardown](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/teardown) | detener, borrar, demostrar que queda limpio; Ctrl-C y `kill -9` en la CLI no dejan VMs vivas |
| [credentials](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/credentials) | el valor nunca entra en la VM (búsqueda con contraprueba); interceptación solo en el host del binding; 403 en la URL |
| [dev-env](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/dev-env) | persistencia de paquetes, `$HOME` y disco; `/tmp` se vacía; init una sola vez, como root; `user` y `--user` |
| [docker-in-machine](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/docker-in-machine) | Docker en un ext4 de verdad, contenedor anidado, `docker.sock` en el host, datos que sobreviven |
| [coding-agent](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/coding-agent) | agentes de código instalados por el init; git en el repo montado; agente SSH del host sin la clave en la VM; clave de la API como secreto |
| [branch-and-checkpoint](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/branch-and-checkpoint) | checkpoint de disco con retención y bloqueo, restauración de cualquier generación, pause/resume, hijas de una fuente precalentada, checkpoint exportado como pack |
| [gpu-cuda](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/gpu-cuda) | no hay CUDA: `doctor`, la CLI, `runner.code` y la API dicen que no, con el motivo, sin crear nada |
| [local-api](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/local-api) | `rc serve` con cuerpos JSON de ciclo de vida; `exitCode` en el cuerpo; SSE una línea por evento; campo desconocido rechazado |
| [reference](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/reference) | una etapa por tema con prueba (`runner.code`, redes con nombre, nested, limitaciones, sockets, display, propiedad de archivos, S3) y los temas fuera de alcance, con el motivo |

El `lib.sh` de la carpeta reúne lo que usa todo `run.sh`: el formato de `rc machine ls`, el proceso de una VM
(`rc vmm --spec <dir>/spec.json`), el directorio de la máquina (`rc machine data-dir`) y la lista vacía (`No machines found`).

## Runbook

Además de las guías, el [`docs/runbook.md`](https://github.com/andrebassi/runner-codes/blob/main/docs/runbook.md) es la
secuencia de prueba manual, paso a paso, con el resultado esperado de cada uno; `task runbook` ejecuta los mismos pasos
y falla si alguno sale distinto (116 comprobaciones, 0 fallos el 20/10/2026).
