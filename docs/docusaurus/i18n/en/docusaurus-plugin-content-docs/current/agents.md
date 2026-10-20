---
title: 'Agent reference'
description: 'What rc does today, how to use each command, what does not exist yet and the evolution plan'
---

A short, complete reference for whoever operates `rc`, human or AI agent: what exists, how to call it and what each
thing guarantees. Every line describes what **exists today**. What does not exist yet is in the section
[What does not exist yet](#what-does-not-exist-yet) and in the [plan](#evolution-plan).

`rc` runs untrusted (LLM-generated) code in a **fresh microVM per job**, with its own kernel, no network and a
read-only root, and destroys the VM at the end. The VMM is libkrun and the kernel comes from libkrunfw. Boot takes about 260 ms.

## Platforms

| Host | Guest | Hypervisor | Status |
|------|-------|-----------|----------|
| macOS Apple Silicon | Linux arm64 | Hypervisor.framework | tested end to end |
| Linux x86_64 / aarch64 | Linux of the same architecture | KVM (`/dev/kvm`) | compiles; confinement tested; full boot not measured (no host with KVM) |
| Windows | — | — | out: upstream libkrun has no WHP. The Windows build fails on purpose |

## Installation

```bash
task krun:install                    # libkrun + libkrunfw (+ GPU on macOS) via Nix: nix profile add .#krun
task build                           # bin/rc (signed on macOS) + bin/rc-guest-linux-<arch>
./bin/rc image build --stack python  # or --all; one ext4 image per stack
./bin/rc doctor                      # checks host, libkrun, GPU, guest, sandbox and images
```

No Homebrew and no Docker: everything goes through Nix. `nix develop` gives you Go, task, coreutils and Node for development.

## Quick reference

```bash
# run code in a fresh VM (cold boot, the VM is gone at the end)
./bin/rc run --stack python --code 'print(2**10)'
./bin/rc run --stack go --file main.go --stdin 'entrada' --timeout 30s

# output appears while the program runs (streaming)
./bin/rc run --stack python --code $'import time\nprint("a")\ntime.sleep(2)\nprint("b")'

# interactive shell in a fresh VM (a real TTY: Ctrl+C, window size)
./bin/rc shell --stack bash

# host GPU over Vulkan (only on a stack with gpu)
./bin/rc run --stack llama --gpu --code 'vulkaninfo --summary | grep deviceName'

# persistent machine: what you write survives between commands and across stop/start
./bin/rc machine create --name dev --stack python
./bin/rc machine start  --name dev
./bin/rc machine exec   --name dev -- sh -c 'mkdir -p /workspace/out && echo pronto > /etc/motd'   # no network: create with --net or --allow-host
./bin/rc machine cp ./app.py dev:/workspace/app.py
./bin/rc machine exec   --name dev -- python3 /workspace/app.py
./bin/rc machine stop   --name dev

# HTTP API with a pool of ready VMs (and the machine routes)
./bin/rc serve --listen 127.0.0.1:8080 --pool-size 2
```

## When to use what

| Goal | Command |
|----------|---------|
| run an isolated snippet of code and get the output | `rc run --stack S --code '...'` |
| see the output while it runs (build, long test) | `rc run` (already streams) or `POST /api/v1/execute/stream` |
| explore a stack's image by hand | `rc shell --stack S` |
| multi-step environment that keeps state | `rc machine create` → `start` → `exec`/`cp` → `stop` ([machine](/cli/machine)) |
| serve many jobs with low latency | `rc serve`: the pool takes boot off the critical path (26–78 ms per job) |
| use the GPU (AI, Vulkan) | `--gpu` on the `llama` stack ([GPU](/advanced/gpu)) |
| find out whether the host is ready | `rc doctor` |
| list stacks and which images are built | `rc languages` |

## CLI structure

```
rc run    --stack S (--code C | --file F) [--stdin T] [--timeout 10s] [--gpu]
rc shell  --stack S [--gpu]
rc machine create --name N --stack S [--cpus 2] [--memory 2048] [--disk-gib 10] [--gpu]
rc machine start|stop|status|delete [--name N]          # stop --timeout 10s; delete -f
rc machine ls [--json]
rc machine exec [--name N] [-e K=V] [-w DIR] [-i] [--timeout D] -- CMD ARGS...
rc machine shell [--name N] [-e K=V] [-w DIR] [-- CMD]  # interactive terminal (PTY)
rc machine cp SRC DST                                    # one side is name:/path
rc machine logs [--name N] [--tail N]
rc serve  [--listen 127.0.0.1:8080] [--pool-size 2]
rc image build (--stack S ... | --all)
rc languages
rc doctor
rc vmm --spec <json> [--watch-fd 3] [--lib <libkrun>]   # internal: one process per VM
```

`rc run` exits with **0** only when the program exited with 0. A non-zero exit, a timeout, a failed compilation and a rejected job
all give rc 1.

## Flags

| Flag | Command | What it does |
|------|---------|-----------|
| `--stack` | run, shell, image build | catalog stack (`python`, `go`, `rust`, `java`, `nodejs`, `c`, `cpp`, `ruby`, `php`, `perl`, `bash`, `sh`, `awk`, `jq`, `sqlite`, `llama`) |
| `--code` / `--file` | run | code of the main file, inline or from a local file |
| `--stdin` | run | the program's standard input (up to 1 MB) |
| `--timeout` | run | job deadline, default 10 s, maximum 300 s; when it expires, the VM is killed with `SIGKILL` |
| `--gpu` | run, shell | host GPU over Vulkan (virtio-gpu/Venus); only on a stack with `gpu: true`, otherwise E007 |
| `--listen` | serve | address; loopback by default, because the API has no authentication |
| `--pool-size` | serve | ready VMs per stack (default 2) |
| `--all` | image build | builds every stack in the catalog |

## Environment variables

| Variable | Default | Purpose |
|----------|--------|-----|
| `RUNNER_CACHE_DIR` | `~/Library/Caches/runner-codes` (macOS) | images, build VM, VM directories |
| `RUNNER_KRUN_LIB` | `~/.nix-profile/lib/libkrun.dylib` (macOS); on Linux the one from the Nix profile or `libkrun.so.1` | which libkrun to load |
| `RUNNER_GUEST_BIN` | `rc-guest-linux-<arch>` next to `rc` | the guest-runner injected into the images |
| `RUNNER_SANDBOX` | on | `off` disables Landlock + seccomp on Linux (diagnostics only; the `vmm` warns) |
| `RC_MACHINE_NAME` | `default` | machine name when `--name` is missing (pin one per project with direnv) |

## Stack catalog

It lives in `internal/adapters/catalog/languages.yaml`, embedded in the binary. Each stack has:

```yaml
- name: python               # name the CLI and API accept; must exist in the guest executor
  image: python:3.12-alpine  # official source OCI image
  cpus: 1                    # vCPU ceiling per job (against abuse)
  memory_mib: 512            # RAM ceiling; libkrun memory is elastic
  warmup: python3 -m compileall -q -j0 /usr/local/lib/python3.12   # runs once, at build time, with network
  gpu: false                 # true enables --gpu
  gpu_vram_mib: 0            # guest VRAM with GPU (0 = 4096)
```

The build pulls the OCI image without Docker (go-containerregistry), extracts it in an Alpine build VM, injects the guest, runs the
warmup and generates a **read-only ext4**. The cache key includes the image digest, the warmup, the scripts and the
guest hash, so changing any of them triggers a rebuild. An unknown field in the YAML is a build error.

## HTTP API

`rc serve` keeps a **pool** of already-booted VMs per stack. Each VM serves **one** job and dies, and replenishment is
asynchronous, with at most 4 concurrent boots in total.

| Method | Path | What it does |
|--------|---------|-----------|
| GET | `/health` | `{"status","version","pool":{"python":2,...}}` |
| GET | `/api/v1/languages` | stacks, source image and whether it is ready |
| POST | `/api/v1/run` | `{language, files[], stdin}`; `stdin` as a list runs one job per item (batch) |
| POST | `/api/v1/execute` | `{language, code \| files[], stdin, timeout, gpu}` |
| POST | `/api/v1/execute/stream` | same body, response as Server-Sent Events |
| — | `/api/v1/machines/...` | machine lifecycle: create, start, stop, exec, exec/stream, files, logs ([details](/api-reference/overview#machines)) |

Response from `/run` and `/execute`:

```json
{"status":"success","stdout":"1024\n","stderr":null,"exception":null,"executionTime":12}
```

- `status: "success"` says the API executed the job, **not** that the program passed: look at `stderr` and `exception`.
- `exception` is `null`, `"Timeout"`, `"Compilation failed"` or the description of an infrastructure failure.

Streaming (`/execute/stream`):

```
event: stdout
data: "a\n"

event: result
data: {"status":"success","exitCode":0,"exception":null,"executionTime":2034,"timedOut":false}
```

- The `data` of `stdout`/`stderr` is a JSON string: newlines come escaped.
- `result` does not repeat the output.
- A failure after the start becomes `event: error`.

### Error codes

| Code | HTTP | When |
|--------|------|--------|
| `E001: operation timed out` | 200 | the guest did not respond within the timeout plus the host's slack (2 s) |
| `E005: stdin too long` | 400 | stdin over 1,000,000 bytes |
| `E006: unsupported language X` | 400 | stack outside the catalog or without an image built on this host |
| `E007: stack X has no GPU support` | 400 | `gpu` requested on a stack without `gpu: true` |
| other limits (timeout, files, size, name) | 400 | `error` with the reason |
| infrastructure failure | 500 | `exception` with the reason (VM did not boot, channel to the guest dropped) |

## Key defaults

- **A fresh VM per job**: nothing carries over from one job to the next; the pool only takes boot off the critical path.
- **No network**: the vsock is created without TSI. Without disabling the implicit vsock, libkrun would turn on networking by itself.
- **Read-only root** (ext4 over virtio-blk, not virtiofs, which would let the guest reach the rest of the host); `/tmp` is a
  256 MiB tmpfs.
- **Per-job limits**: stdin ≤ 1 MB; up to 32 files; code ≤ 4 MiB in total; 10 s timeout (maximum 300 s).
- **CPU and RAM per stack** are an abuse ceiling; memory is elastic, and an idle VM takes about 44 MB.
- **API without authentication**: listens on `127.0.0.1` by default.

## Key behaviors

- **Timeout = `SIGKILL` on the VM's process group.** No process is left behind; the e2e test checks with `pgrep`.
- **If `rc` dies, the VMs die with it**: `Pdeathsig` on Linux, a watch pipe on fd 3 on macOS.
- **Streaming delivers what the program writes to the pipe.** Python runs with `PYTHONUNBUFFERED=1`. C (`printf`), Ruby and
  Perl buffer stdout when the output is not a terminal, and only deliver it on `flush`.
- **`rc shell` in a terminal** gets a virtio TTY console and puts the host terminal in raw mode; with a pipe, it keeps the regular
  console.
- **Outdated image**: if the image's guest predates streaming, the error tells you to run `rc image build`.
- **Linux**: the `vmm` process of each job VM runs under Landlock + seccomp. With Landlock ABI < 9 there is no Unix
  socket isolation between VMs, and `rc doctor` warns about it. A VM with GPU stays outside the confinement.
- **GPU**: a VM with GPU boots cold, outside the pool, at most 2 at the same time, and shares the host GPU (isolation per
  process, not per VM).

## What does not exist yet

`rc` uses libkrun + libkrunfw, OCI images and no daemon. It runs **disposable jobs** of untrusted code
and also manages persistent **machines**; what follows does not exist yet, or exists only in part:

| Feature | In `rc` today | Notes |
|---------|--------------|------------|
| `machine stats`, `machine monitor` | **no** | Phase D |
| interactive exec over WebSocket in the API | **no** | the terminal is CLI-only today |
| `--error-format json` | partial | the API already returns stable E00x codes |
| RAM and CPU checkpoint (Mac → Linux) | **no** | upstream libkrun has no snapshots; `branch` copies the disk |
| `--cuda` | **no** | requires a Linux host with NVIDIA |
| Windows (WHP) | **no** | upstream libkrun has no WHP |

The final event of the job stream is `result`; on the machine stream it is `exit`. The HTTP API comes with a pre-booted pool, and on
Linux each job VM runs under Landlock + seccomp.

## Evolution plan

Each phase closes like the previous ones: an AIOX story, e2e twice in a row, a defect planted on purpose to prove that
the test catches it, and a 90% gate. **Done:** Phase A (stories 7.1 and 7.2), `runner.code` with an arbitrary image
(story 7.3, from Phase D) and Phase C, filtered network and volumes (story 7.4). Phase B and the rest of D remain.

| Phase | What goes in | Design decision | Known risk |
|------|-------------|--------------------|-----------------|
| **A — machines** ✅ | `rc machine create/start/stop/delete/ls/status/exec/shell/logs/cp`; `/api/v1/machines/...` routes | state in `<cache>/machines/<nome>/`; a writable ext4 disk per machine; overlayfs in the guest (image below, disk on top) so whatever is installed persists; `/workspace` on the disk; the `vmm` runs detached, with a PID record and a liveness check | today the `vmm` dies with its parent on purpose, and ephemeral jobs must keep working that way |
| **B — secrets and SSH** | `--secret-env`, `--secret-file`, `--ssh-agent` | the secret is resolved at start time, never written to the registry; rejected by the API (it would be an arbitrary read of the host); SSH through a vsock port wired to `SSH_AUTH_SOCK`, without the key entering the VM | root in the guest can read the process env; this is defense in depth, not absolute secrecy |
| **C — filtered network** ✅ (7.4) | `--net`, `--allow-host`, `--allow-cidr`, `--block-private`, `-p`, `machine update`, volumes | virtio-net with a userspace network stack on the host (e.g. gvproxy) and filtering done by us; TSI remains only for the build VM | the most expensive and most sensitive part; measure before promising |
| **D — conveniences** | ✅ `runner.code` (7.3), `--image ./img.tar` and `-`, `machine stats`, `--error-format json` | go-containerregistry already reads `docker save`/OCI layout | — |

Suggested order: A → B → D → C, with C last because opening the network is what touches the project's core guarantee the most.

## Going deeper

- [Architecture](/architecture), [Security](/advanced/security), [GPU](/advanced/gpu)
- [Test report](/advanced/test-report): what was measured on each platform
- `docs/prd.md`, `docs/architecture.md`, `docs/stories/`: requirements, decisions and each story with its defect log
