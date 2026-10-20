---
title: 'VM pool'
description: 'Pre-booted VMs per language, used only once and refilled in the background'
---

## What it is for

Booting a VM and waiting for the guest to respond costs on the order of 260 ms (measured p50). With the pool, that cost moves off the request path: the API takes a VM that is already ready.

```bash
rc serve --listen 127.0.0.1:8080 --pool-size 2
```

## Rules

- **N VMs per language.** `--pool-size` (default 2) sets how many VMs stay ready for each language that has a built image. A language without an image does not enter the pool.
- **Single use.** Each VM serves one job and is destroyed. No state — a file in `/tmp`, a process, a variable — carries over from one job to the next.
- **Background refill.** When a VM leaves the pool, another is booted right after, with at most 2 simultaneous boots per language.
- **Never refuses.** If that language's pool is empty, the job boots a VM on the spot (cold boot) and waits for it.
- **Shutdown.** SIGINT or SIGTERM close the server and kill every VM in the pool.

## What "ready" means

Connecting to the Unix socket is not enough: libkrun creates the socket before the guest starts listening. The readiness wait sends an `sh true` job every 1 ms until it gets a response. Only then does the VM enter the pool.

For Go, copying the warm `GOCACHE` to tmpfs also happens at boot, before the VM enters the pool, and does not weigh on the job.

## Memory cost

An idle VM measured 44 MB of footprint. The reported RSS (172 MB) includes shared clean pages. libkrun memory is elastic — the host only pays for what the guest touches — so the language's RAM ceiling in the catalog is not reserved per pool VM.

## Numbers

| Path | Measured |
|------|----------|
| API with a full pool | 26–43 ms for the full HTTP request |
| CLI, cold boot | ~240 ms in total |

macOS 27.0.1, Apple Silicon, 10/19/2026. Linux not measured yet.

## The CLI does not use the pool

`rc run` always boots a fresh VM (pool size zero) and destroys it at the end.
