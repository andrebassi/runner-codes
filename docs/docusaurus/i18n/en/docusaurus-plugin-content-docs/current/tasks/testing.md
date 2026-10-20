---
title: 'Testing'
description: 'Unit tests, coverage gate, multi-target gate and end-to-end tests'
---

## task test

```bash
task test
```

Unit tests for `./internal/...` with the race detector. Does not need libkrun. In the 10/20/2026 run: 20 packages,
all green.

## task test:cov

```bash
task test:cov
```

A coverage gate that **fails** (rc ≠ 0), not just reports:

- total coverage of `./internal/...` and `./pkg/...` ≥ 90% (floor adjustable through `COVERAGE_FLOOR`);
- domain (`internal/domain`) at 100%;
- guest (`internal/guest`) above the ratchet (`GUEST_FLOOR`, currently 75%);
- Python SDK (`sdk/python`) with line and branch ≥ 90% (`task sdk:py:cov`).

In the 10/20/2026 run: 95.2% total, domain 100%, guest 75%, Python SDK 100%.

Excluded from the count, with the reason written in the script:

| Excluded | Reason |
|----------|--------|
| `cmd/rc` | wiring: it only connects the pieces; verified by the e2e |
| `internal/guest` (from the total) | inherited executor, which only runs with the toolchains inside the VM; measured separately as declared debt, with a ratchet that fails if it drops |

Proof that the gate bites: `COVERAGE_FLOOR=101 task test:cov` must exit with rc ≠ 0.

## task test:targets

```bash
task test:targets
```

| Target | Expected |
|--------|----------|
| darwin/arm64 | compiles |
| linux/amd64 | compiles |
| linux/arm64 | compiles |
| windows/amd64 | **fails**, with the declared reason |

It also fails on any file with an OS build tag that does not target a platform with a decision.

## task test:e2e

```bash
task image:build -- --all
task test:e2e
```

Runs against the real libkrun, with real VMs. Needs the images built (`llama` for the GPU tests). There are 42 tests;
in the 10/20/2026 run, all 42 passed in two consecutive runs. Some of what they prove:

| Area | Proof |
|------|-------|
| job | stdout and exit code; a timeout that kills the VM without leaving a process; no network (with proof of failure); read-only root and nothing carries over between jobs; memory ceiling; language families |
| `shell` | a real TTY, Ctrl+C interrupts only the command |
| `serve` | pool, SSE with the 1st line before the end, machine routes |
| machines | lifecycle and persistence, terminal, `runner.code`, `machine run`, branch and pause, file copy |
| network | without `--net` there is no `eth0`; per-host and per-CIDR policy; metadata rejected; ICMP; named network |
| volumes and bridges | `:ro` resists remount, `:staged`, disk, S3, `docker.sock`, `--expose-socket`/`--mount-socket` |
| credentials | placeholder in the guest, real value in the service; secrets and SSH agent |
| GPU, display, pack | Venus visible with `--gpu`; frame and input through the display; a pack that preserves the disk |

The full list, test by test, is in the [Test report](/advanced/test-report).

## task runbook

```bash
task runbook
```

Runs `docs/runbook.md` step by step and checks every expected result. Every new feature gets, in the same delivery, a
section in the runbook and the step in `scripts/runbook.sh`. In the 10/20/2026 run: 116 checks, 0 failures.

## task guides

```bash
task guides
task guides DIRS="pack teardown"
```

Each folder in `examples/guides` has a `run.sh` that proves what the guide says in a real VM and exits with the number
of failures. In the 10/20/2026 run: 12 folders, 215 checks, 0 failures. See [Examples and guides](/guides/examples).

## task gates

```bash
task gates
```

Full run: `test:targets`, `test:cov`, `test:e2e` and `runbook`, in that order.

## Tested platforms

The end-to-end tests, the runbook, the guides and the performance measurements were done on macOS 27, Apple Silicon.
On Linux, the multi-target gate guarantees that it compiles, and the Landlock + seccomp confinement was tested on
Ubuntu 26.04 without KVM; a full microVM on Linux has not been measured yet. Details in the
[Test report](/advanced/test-report).
