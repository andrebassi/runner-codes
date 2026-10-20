---
title: 'vsock protocol'
description: 'Length + JSON frame between host and guest, byte by byte'
---

## Transport

| Item | Value |
|------|-------|
| guest side | listens on vsock port 5000 |
| host side | Unix socket, bound to port 5000 by `krun_add_vsock_port2` with `listen=true` |
| handshake | none: the connection on the Unix socket goes straight to the guest |
| messages per connection | one job and one result |

## Frame

![Message framing](/img/message-framing.svg)

```
offset 0      4                          4 + N
       ┌──────┬──────────────────────────┐
       │  N   │  JSON (N bytes, UTF-8)   │
       └──────┴──────────────────────────┘
       uint32 big-endian
```

- `N` counts only the payload.
- The maximum frame is 64 MiB. The host reader rejects a length above that before allocating memory: without the limit, a corrupted prefix (up to 4 GiB) would make the reader allocate all of it before reading the body.
- The host sends the prefix and the body in a single write, so the guest never reads a prefix without its matching body.

## Example

The job `{"trace_id":"t1","lang":"python","code":"print(1)","timeout":10}` has 64 bytes of JSON:

```
00 00 00 40  7b 22 74 72 61 63 65 5f 69 64 22 3a ...
└─ 64 ─────┘ └─ {"trace_id": ...
```

## Payloads

- Host → guest: [Job request](/api-reference/job-request).
- Guest → host: [Job response](/api-reference/job-response).

## Streaming

With `"stream": true` in the job, the guest responds with several frames instead of one, all in the same format
(length + JSON), in `internal/protocol`:

```json
{"type":"stdout","data":"early\n"}
{"type":"stderr","data":"warn\n"}
{"type":"result","result":{"status":"success","stdout":"early\n","stderr":"warn\n","exception":null,"executionTime":12}}
```

- One frame per process write to the pipe (up to 32 KiB on Linux), in the order the guest read them.
- The `result` is always last and carries the full result, the same as in non-streaming mode.
- Without `stream`, the wire is identical to before: a single result frame. The readiness probe (`sh true`) uses this mode.
- If the host receives a frame without `type`, the image's guest predates streaming: `rc image build` rebuilds the image.

## Operations

The job's `op` field selects the operation. Empty is the usual code job, so the wire for ephemeral jobs does not
change; the others serve persistent machines (`internal/protocol`).

| `op` | Fields used | What it does |
|------|-------------|--------------|
| (empty) | `lang`, `code`, `files`, `stdin`, `timeout`, `stream` | runs code in a language |
| `exec` | `argv`, `env`, `cwd`, `user`, `timeout`, `stream` | runs a program with no implicit shell |
| `put` / `get` | `path`, `offset`, `data`, `final`, `mode` | file copy in 1 MiB chunks (up to 4 GiB per file) |
| `pty` | `argv`, `rows`, `cols`, `user` | interactive terminal: the connection becomes a channel of `input`/`resize` events (host) and `output`/`exit` events (guest) |
| `workload` | `argv`, `env`, `cwd`, `user`, `stop_on_exit` | starts the process the machine keeps running |
| `shutdown` | `signal`, `timeout` | sends the signal to the workload, waits, runs `sync` and shuts down |
| `dial` | `port` | after a successful result, the connection becomes a byte pipe to `127.0.0.1:port` inside the guest (published port) |
| `set-env` | `env` | keeps the secrets in guest memory and adds them to every subsequent command; they never go to disk |
| `ssh-agent` | `path`, `port` | opens a Unix socket in the guest that forwards each connection to the host's vsock port (SSH agent) |
| `mount-s3` | `path`, `s3`, `env` | mounts a bucket via FUSE in the guest, after checking that it responds |

## Other vsock ports

| Port | Direction | Use |
|------|-----------|-----|
| 5000 | host → guest | all the operations above |
| 5001 | guest → host | SSH agent (`--ssh-agent`) |
| 5100 + i | guest → host | the i-th `--mount-socket` |
