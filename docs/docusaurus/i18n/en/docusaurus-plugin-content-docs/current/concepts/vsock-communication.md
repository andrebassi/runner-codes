---
title: 'vsock communication'
description: 'How the host talks to the guest: Unix socket, vsock port 5000 and length + JSON frames'
---

## The channel

The channel between host and guest is a vsock (the job VM does not even have a network):

- the guest listens on vsock port **5000**;
- on the host, `krun_add_vsock_port2` with `listen=true` binds that port to a **Unix socket** in a working directory inside the cache;
- the host connects to that Unix socket like any local socket.

There is no text handshake: the connection on the Unix socket already reaches the guest.

## Message format

![Message framing](/img/message-framing.svg)

Every message, in both directions, is:

```
+------------------------+----------------------+
| length (4 bytes, BE)   | JSON payload (UTF-8) |
+------------------------+----------------------+
```

- The length is a 4-byte unsigned integer, big-endian, and counts only the payload.
- The maximum frame is **64 MiB**; the host refuses to read a frame above that.

The same frame serves every operation: the ephemeral job and the machine operations (`exec`, file copy, terminal,
workload, published port, secrets, SSH agent, S3), selected by the job's `op` field.

## Sequence

```
host                                guest
 │ connect(Unix socket) ─────────────▶ accept (vsock 5000)
 │ [length][Job JSON] ───────────────▶ runs
 │ ◀─────────────── [length][Result JSON]
 │ closes; the job VM is destroyed
```

## Readiness

libkrun creates the Unix socket before the guest starts listening, so a successful connection does not prove the guest is ready. The readiness wait sends an `sh true` job every 1 ms until it gets a response. Only after that does the VM receive real jobs.

## Payloads

- Job: `{trace_id, lang, code, files[{name, content}], stdin, timeout}` — see [Job request](/api-reference/job-request).
- Result: `{trace_id, status, stdout, stderr, exception, executionTime, exit_code, error}` — see [Job response](/api-reference/job-response).

Byte-by-byte details in [vsock protocol](/api-reference/vsock-protocol).
