---
title: 'Pool de VMs'
description: 'VMs pre-arrancadas por lenguaje, usadas una sola vez y repuestas en segundo plano'
---

## Para qué sirve

Arrancar una VM y esperar a que el guest responda cuesta del orden de 260 ms (p50 medido). Con el pool, ese costo sale del camino de la solicitud: la API toma una VM que ya está lista.

```bash
rc serve --listen 127.0.0.1:8080 --pool-size 2
```

## Reglas

- **N VMs por lenguaje.** `--pool-size` (por defecto 2) define cuántas VMs quedan listas para cada lenguaje que tiene imagen construida. Un lenguaje sin imagen no entra al pool.
- **Uso único.** Cada VM atiende un job y se destruye. Ningún estado (archivo en `/tmp`, proceso, variable) pasa de un job al siguiente.
- **Reposición en segundo plano.** Cuando una VM sale del pool, se arranca otra a continuación, con un máximo de 2 arranques simultáneos por lenguaje.
- **Nunca rechaza.** Si el pool de ese lenguaje está vacío, el job levanta una VM en el momento (arranque en frío) y la espera.
- **Cierre.** SIGINT o SIGTERM cierran el servidor y matan todas las VMs del pool.

## Qué significa "lista"

Conectarse al socket Unix no basta: la libkrun crea el socket antes de que el guest empiece a escuchar. La espera de disponibilidad envía un job `sh true` cada 1 ms hasta recibir respuesta. Solo entonces la VM entra al pool.

Para Go, la copia del `GOCACHE` precalentado al tmpfs también ocurre en el arranque, antes de que la VM entre al pool, y no pesa en el job.

## Costo en memoria

Una VM detenida midió 44 MB de footprint. El RSS reportado (172 MB) incluye páginas limpias compartidas. La memoria de la libkrun es elástica (el host solo paga lo que el guest toca), así que el límite de RAM del lenguaje en el catálogo no se reserva por cada VM del pool.

## Números

| Camino | Medido |
|---------|--------|
| API con el pool lleno | 26–43 ms de solicitud HTTP completa |
| CLI, arranque en frío | ~240 ms en total |

macOS 27.0.1, Apple Silicon, 19/10/2026. Linux aún no medido.

## La CLI no usa pool

`rc run` siempre levanta una VM nueva (pool de tamaño cero) y la destruye al final.
