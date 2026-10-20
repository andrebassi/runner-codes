---
title: 'Job request'
description: 'El job que el host envía al guest por vsock'
---

## Formato

El host convierte la solicitud HTTP (o la llamada de la CLI) en un job y lo envía al guest como un frame vsock (4 bytes big-endian de tamaño + JSON).

```json
{
  "trace_id": "tr-123",
  "lang": "python",
  "code": "",
  "files": [
    {"name": "main.py", "content": "import util\nprint(util.dobro(int(input())))"},
    {"name": "util.py", "content": "def dobro(x):\n    return 2 * x\n"}
  ],
  "stdin": "21",
  "timeout": 10
}
```

## Campos

| Campo | Tipo | Descripción |
|-------|------|-----------|
| `trace_id` | string | identificador del job, devuelto en el resultado |
| `lang` | string | lenguaje; debe existir en el executor del guest |
| `code` | string | código del archivo principal cuando no hay `files` |
| `files` | lista de `{name, content}` | archivos del job; el **primero** es el principal |
| `stdin` | string | entrada estándar; se omite cuando está vacía |
| `timeout` | entero | segundos |
| `stream` | booleano | pide la salida en fragmentos antes del resultado; consulte [Streaming](/api-reference/vsock-protocol#streaming) |
| `op` | string | operación; vacío es el job de código. Las operaciones de máquina (`exec`, `put`, `pty`...) están en [Protocolo vsock](/api-reference/vsock-protocol#operaciones) |

## Archivo principal

- Con `files`, cada archivo se escribe por su nombre en un directorio temporal en `/tmp`, y el primero es el que se ejecuta (o compila).
- Solo con `code`, el guest escribe `script.<ext>`, excepto en Java, que se convierte en `Main.java`, porque la clase ejecutada es `Main`.

## Reglas aplicadas antes de que el job llegue al guest

El dominio, en el host, rechaza el job (HTTP 400 en la API) cuando:

| Regla | Límite |
|-------|--------|
| lenguaje | en el catálogo y con imagen construida en este host (si no, E006) |
| stdin | ≤ 1.000.000 bytes (si no, E005) |
| timeout | > 0 y ≤ 300 s; si falta, se usa 10 s |
| contenido | `code` o al menos un archivo |
| archivos | hasta 32, sin nombres repetidos |
| tamaño total | suma de `code` y contenidos ≤ 4 MiB |
| nombre de archivo | sin `/`, sin `..`, sin empezar con `.`, ≤ 255 bytes |

Las reglas de nombre impiden escribir fuera del directorio del job dentro del guest, aunque la VM sea desechable.

## Disponibilidad

Antes del primer job real, el host envía a la VM un job `sh true` repetidamente (cada 1 ms) hasta recibir respuesta. Así sabe que el guest ya está escuchando.
