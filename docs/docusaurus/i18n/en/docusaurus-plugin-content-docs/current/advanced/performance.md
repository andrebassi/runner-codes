---
title: 'Performance'
description: 'Measured numbers for boot, execution, machines, memory and image builds'
---

## Where it was measured

Every number on this page was measured on **macOS 27, Apple Silicon (M1 Pro)**, on 10/19 and 10/20/2026. **Linux has
not been measured yet**; do not assume the values carry over to KVM.

## Boot

| Measurement | Value |
|-------------|-------|
| boot to the first response from the Python guest, read-only ext4 root, 30 boots | p50 260 ms, p95 266 ms |
| effect of configured RAM (256 to 2048 MiB) on boot | none |

"Response" is the guest's first reply to the `sh true` job that the readiness wait sends every 1 ms. It is the moment
the VM can accept a real job.

## Running a job

| Path | Value |
|------|-------|
| CLI, Python job, cold boot | ~240 ms in total, of which 31–38 ms is execution |
| API, full pool, one stack with an image | 26–43 ms for the full HTTP request |
| API, 15 stacks, all pools full | 49–175 ms |

The difference between CLI and API is the boot, which the [pool](/concepts/pool) takes off the request path. With an
empty pool, the API falls back to the same cold boot as the CLI.

Per stack, through the CLI with a cold boot:

| Stack | Execution | Total (with boot) |
|-------|-----------|-------------------|
| python | 31–44 ms | ~240 ms |
| nodejs | 128–148 ms | 353–371 ms |
| go | 247 ms with a warm build cache (3.4 s without it) | 510 ms |
| java | 522 ms | 739 ms |
| c | 71–72 ms | 282 ms |
| sh | 2 ms | 199 ms |

## Machines

| Measurement | Value |
|-------------|-------|
| `machine start` until the guest responds (python) | 204–244 ms |
| `start` of a machine with network | 190 ms |
| freshly created 10 GiB disk, space used on the host | 6.4 MB |
| `cp` of 50 MB to the machine / from the machine | 620 ms (~84 MB/s) / 594 ms (~88 MB/s) |
| name blocked by policy until the error in the guest | < 1 s |

## Memory

| Measurement | Value |
|-------------|-------|
| footprint of an idle VM | 44 MB |
| RSS of the same VM | 172 MB, including shared clean pages |

libkrun memory is elastic: the host only pays for the pages the guest touches. The per-language RAM ceiling in the
catalog limits abuse, but it is not reserved.

## Image build and pack

| Measurement | Value |
|-------------|-------|
| Python image build | ~8–10 s |
| pull + layer flattening (python) | 2.7 s |
| Python warmup (`compileall`) | ~1 s |
| Go image build | 35 s, of which 28 s is `mkfs.ext4` |
| pack of the python image | 134 MiB → 31 MiB payload |
| pack of a python machine with an installed package, single file | 46 MiB |

The build is a one-time cost per cache key; a second build with the same inputs comes from the cache. The time for
each stack is in the [Test report](/advanced/test-report).

## GPU

`llama-bench` with Qwen2.5 0.5B: prompt processing ran at 893–1,365 tokens/s on the GPU (Venus) versus 193–264 on the
CPU. Details and how to read the numbers in [GPU](/advanced/gpu).

## What performance owes to the design

- **Root on a read-only ext4 disk**, not virtiofs: virtiofs was not faster and does not confine the guest to the
  shared directories.
- **Warmup baked into the image:** the `.pyc` files of the Python stdlib and the Go build cache come ready-made; for Go,
  the copy to tmpfs happens at boot, before the VM enters the pool.
- **Pool with single-use VMs:** boot happens in the background, at most 2 at a time per language.
- **Cloned machine disk:** clonefile on APFS; the new disk does not copy the 10 GiB.

## Pending measurements

- performance on Linux (x86_64 and aarch64, KVM);
- GPU machines with a model larger than 0.5B.
