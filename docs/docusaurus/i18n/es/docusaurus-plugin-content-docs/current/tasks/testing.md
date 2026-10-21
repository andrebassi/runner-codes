---
title: 'Pruebas'
description: 'Pruebas unitarias, gate de cobertura, gate multiplataforma y pruebas de extremo a extremo'
---

## task test

```bash
task test
```

Pruebas unitarias de `./internal/...` con race detector. No necesita la libkrun. En la ejecución del 20/10/2026: 20 paquetes,
todos en verde.

## task test:cov

```bash
task test:cov
```

Gate de cobertura que **falla** (rc ≠ 0), no solo informa:

- cobertura total de `./internal/...` y `./pkg/...` ≥ 90% (mínimo ajustable con `COVERAGE_FLOOR`);
- dominio (`internal/domain`) al 100%;
- guest (`internal/guest`) por encima de la catraca (`GUEST_FLOOR`, hoy 75%);
- SDK de Python (`sdk/python`) con línea y branch ≥ 90% (`task sdk:py:cov`).

En la ejecución del 20/10/2026: 95,2% total, dominio 100%, guest 75%, SDK de Python 100%.

Fuera del cálculo, con el motivo escrito en el script:

| Excluido | Motivo |
|----------|--------|
| `cmd/rc` | wiring: solo conecta las piezas; verificado por el e2e |
| `internal/guest` (del total) | executor heredado, que solo se ejecuta con las toolchains dentro de la VM; medido aparte como deuda declarada, con una catraca que falla si baja |

Prueba de que el gate muerde: `COVERAGE_FLOOR=101 task test:cov` debe salir con rc ≠ 0.

## task test:targets

```bash
task test:targets
```

| Destino | Esperado |
|------|----------|
| darwin/arm64 | compila |
| linux/amd64 | compila |
| linux/arm64 | compila |
| windows/amd64 | **falla**, con el motivo declarado |

También falla ante un archivo con build tag de SO que no apunte a un destino con decisión.

## task test:e2e

```bash
task image:build -- --all
task test:e2e
```

Se ejecuta contra la libkrun real, con VMs de verdad. Necesita las imágenes construidas (la `llama` para las pruebas de GPU). Son
42 pruebas; en la ejecución del 20/10/2026, las 42 pasaron en dos ejecuciones seguidas. Algunas de las cosas que demuestran:

| Área | Prueba |
|------|-------|
| job | stdout y exit code; timeout que mata la VM sin dejar procesos; sin red (con prueba de fallo); root de solo lectura y nada pasa de un job a otro; límite de memoria; familias de lenguajes |
| `shell` | TTY de verdad, Ctrl+C interrumpe solo el comando |
| `serve` | pool, SSE con la 1.ª línea antes del final, rutas de máquina |
| máquinas | ciclo de vida y persistencia, terminal, `runner.code`, `machine run`, branch y pausa, copia de archivos |
| red | sin `--net` no hay `eth0`; política por host y por CIDR; metadata rechazada; ICMP; red con nombre |
| volúmenes y puentes | `:ro` resiste un remount, `:staged`, disco, S3, `docker.sock`, `--expose-socket`/`--mount-socket` |
| credenciales | placeholder en el guest, valor real en el servicio; secretos y agente SSH |
| GPU, pantalla, pack | Venus visible con `--gpu`; fotograma y entrada por la pantalla; pack que preserva el disco |

La lista completa, prueba por prueba, está en el [Informe de pruebas](/advanced/test-report).

## task runbook

```bash
task runbook
```

Ejecuta el `docs/runbook.md` paso a paso y verifica cada resultado esperado. Todo recurso nuevo gana, en la misma entrega,
una sección en el runbook y el paso en `scripts/runbook.sh`. En la ejecución del 21/10/2026: 126 comprobaciones, 0 fallos.

## task guides

```bash
task guides
task guides DIRS="pack teardown"
```

Cada carpeta de `examples/guides` tiene un `run.sh` que demuestra lo que dice la guía en una VM de verdad y sale con el número de
fallos. En la ejecución del 21/10/2026: 9 carpetas, 175 comprobaciones, 0 fallos. Consulte [Ejemplos y guías](/guides/examples).

## task gates

```bash
task gates
```

Ejecución completa: `test:targets`, `test:cov`, `test:e2e` y `runbook`, en ese orden.

## Plataformas probadas

Las pruebas de extremo a extremo, el runbook, las guías y las mediciones de rendimiento se hicieron en macOS 27, Apple Silicon. En
Linux, el gate multiplataforma garantiza que compila, y el confinamiento Landlock + seccomp se probó en Ubuntu 26.04 sin KVM;
una microVM completa en Linux aún no se ha medido. Detalles en el [Informe de pruebas](/advanced/test-report).
