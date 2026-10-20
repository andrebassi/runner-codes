---
title: 'Comunicación vsock'
description: 'Cómo habla el host con el guest: socket Unix, puerto vsock 5000 y frames de tamaño + JSON'
---

## El canal

El canal entre host y guest es un vsock (la VM de job ni siquiera tiene red):

- el guest escucha en el puerto vsock **5000**;
- en el host, `krun_add_vsock_port2` con `listen=true` conecta ese puerto a un **socket Unix** en un directorio de trabajo de la caché;
- el host se conecta a ese socket Unix como a cualquier socket local.

No existe handshake textual: la conexión al socket Unix ya llega al guest.

## Formato del mensaje

![Message framing](/img/message-framing.svg)

Todo mensaje, en ambos sentidos, es:

```
+------------------------+----------------------+
| tamanho (4 bytes, BE)  | payload JSON (UTF-8) |
+------------------------+----------------------+
```

- El tamaño es un entero sin signo de 4 bytes, big-endian, y cuenta solo el payload.
- El frame máximo es de **64 MiB**; el host se niega a leer un frame mayor.

El mismo frame sirve para todas las operaciones: el job efímero y las operaciones de las máquinas (`exec`, copia de archivos,
terminal, workload, puerto publicado, secretos, agente SSH, S3), elegidas por el campo `op` del job.

## Secuencia

```
host                                guest
 │ connect(socket Unix) ─────────────▶ accept (vsock 5000)
 │ [tamanho][Job JSON] ──────────────▶ executa
 │ ◀────────────── [tamanho][Result JSON]
 │ fecha; a VM de job é destruída
```

## Disponibilidad

La libkrun crea el socket Unix antes de que el guest empiece a escuchar, así que una conexión exitosa no prueba que el guest esté listo. La espera de disponibilidad envía un job `sh true` cada 1 ms hasta recibir una respuesta. Solo después la VM recibe jobs reales.

## Payloads

- Job: `{trace_id, lang, code, files[{name, content}], stdin, timeout}`; consulte [Job request](/api-reference/job-request).
- Resultado: `{trace_id, status, stdout, stderr, exception, executionTime, exit_code, error}`; consulte [Job response](/api-reference/job-response).

Detalles byte a byte en [Protocolo vsock](/api-reference/vsock-protocol).
