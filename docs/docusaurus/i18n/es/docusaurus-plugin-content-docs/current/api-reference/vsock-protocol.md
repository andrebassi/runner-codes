---
title: 'Protocolo vsock'
description: 'Frame de tamaño + JSON entre host y guest, byte a byte'
---

## Transporte

| Elemento | Valor |
|------|-------|
| lado del guest | escucha en el puerto vsock 5000 |
| lado del host | socket Unix, conectado al puerto 5000 por `krun_add_vsock_port2` con `listen=true` |
| handshake | ninguno: la conexión al socket Unix llega directamente al guest |
| mensajes por conexión | un job y un resultado |

## Frame

![Message framing](/img/message-framing.svg)

```
offset 0      4                          4 + N
       ┌──────┬──────────────────────────┐
       │  N   │  JSON (N bytes, UTF-8)   │
       └──────┴──────────────────────────┘
       uint32 big-endian
```

- `N` cuenta solo el payload.
- El frame máximo es de 64 MiB. El lector del host rechaza un tamaño mayor antes de asignar memoria: sin el límite, un prefijo corrupto (hasta 4 GiB) haría que el lector asignara todo antes de leer el cuerpo.
- El host envía prefijo y cuerpo en una única escritura, para que el guest nunca lea un prefijo sin el cuerpo correspondiente.

## Ejemplo

El job `{"trace_id":"t1","lang":"python","code":"print(1)","timeout":10}` tiene 64 bytes de JSON:

```
00 00 00 40  7b 22 74 72 61 63 65 5f 69 64 22 3a ...
└─ 64 ─────┘ └─ {"trace_id": ...
```

## Payloads

- Host → guest: [Job request](/api-reference/job-request).
- Guest → host: [Job response](/api-reference/job-response).

## Streaming

Con `"stream": true` en el job, el guest responde con varios frames en lugar de uno, todos con el mismo formato
(tamaño + JSON), en `internal/protocol`:

```json
{"type":"stdout","data":"early\n"}
{"type":"stderr","data":"warn\n"}
{"type":"result","result":{"status":"success","stdout":"early\n","stderr":"warn\n","exception":null,"executionTime":12}}
```

- Un frame por cada escritura del proceso en el pipe (hasta 32 KiB en Linux), en el orden en que el guest la leyó.
- El `result` es siempre el último y trae el resultado completo, igual que en el modo sin streaming.
- Sin `stream`, el protocolo en el cable es idéntico al anterior: un frame de resultado. La prueba de disponibilidad (`sh true`) usa ese modo.
- Si el host recibe un frame sin `type`, el guest de la imagen es anterior al streaming: `rc image build` rehace la imagen.

## Operaciones

El campo `op` del job elige la operación. Vacío es el job de código de siempre, para que el protocolo de los jobs efímeros no cambie; las
demás sirven a las máquinas persistentes (`internal/protocol`).

| `op` | Campos usados | Qué hace |
|------|---------------|-----------|
| (vacío) | `lang`, `code`, `files`, `stdin`, `timeout`, `stream` | ejecuta código de un lenguaje |
| `exec` | `argv`, `env`, `cwd`, `user`, `timeout`, `stream` | ejecuta un programa sin shell implícito |
| `put` / `get` | `path`, `offset`, `data`, `final`, `mode` | copia de archivos en fragmentos de 1 MiB (hasta 4 GiB por archivo) |
| `pty` | `argv`, `rows`, `cols`, `user` | terminal interactiva: la conexión se convierte en un canal de eventos `input`/`resize` (host) y `output`/`exit` (guest) |
| `workload` | `argv`, `env`, `cwd`, `user`, `stop_on_exit` | inicia el proceso que la máquina mantiene en ejecución |
| `shutdown` | `signal`, `timeout` | envía la señal al workload, espera, hace `sync` y apaga |
| `dial` | `port` | tras el resultado exitoso, la conexión se convierte en un tubo de bytes hasta `127.0.0.1:port` dentro del guest (puerto publicado) |
| `set-env` | `env` | guarda los secretos en la memoria del guest y los añade a cada comando siguiente; nunca van al disco |
| `ssh-agent` | `path`, `port` | abre en el guest un socket Unix que reenvía cada conexión al puerto vsock del host (agente SSH) |
| `mount-s3` | `path`, `s3`, `env` | monta un bucket por FUSE en el guest, después de verificar que responde |

## Otros puertos vsock

| Puerto | Sentido | Uso |
|-------|---------|-----|
| 5000 | host → guest | todas las operaciones anteriores |
| 5001 | guest → host | agente SSH (`--ssh-agent`) |
| 5100 + i | guest → host | el i-ésimo `--mount-socket` |
