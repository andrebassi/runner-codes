---
title: 'runner.code'
description: 'The runner.code file: the machine declared in TOML (stack or image, resources, env, init and workload)'
---

`rc machine create` accepts flags, but a machine with an environment, setup commands and a long-running process
turns into a line too long to remember and impossible to review. **`runner.code`** is that declaration in a
TOML file that goes into the repository alongside the code.

## At a glance

```toml
# runner.code
stack = "python"                 # or: image = "redis:7-alpine"
cpus = 2
memory = 2048                    # MiB
storage = 10                     # GiB of writable disk

env = ["APP_ENV=dev"]
workdir = "/app"

init = [                         # runs ONCE, on the disk's first start
  "pip install --user flask",
]

entrypoint = ["python3", "-m"]   # workload: entrypoint + cmd,
cmd = ["http.server", "8000"]    # started on every start
stop_signal = "SIGINT"
stop_timeout = 30

[dev]                            # development profile, applied on top
env = ["DEBUG=1"]
```

```bash
rc machine create --name web               # reads ./runner.code
rc machine create --name web -f infra/runner.code
rc machine start  --name web               # init (1st time) + workload
rc machine logs   --name web --source workload
rc machine stop   --name web               # SIGINT to the workload, up to 30 s, sync, power off
```

## Keys

| Key | Type | Default | What it does |
|-------|------|--------|-----------|
| `stack` | string | — | a catalog stack as the base (`python`, `go`, ...). Uses the image with warmup |
| `image` | string | — | **any** OCI image as the base (`redis:7-alpine`). Built on first use, without warmup |
| `cpus` | integer | 4 | vCPUs (idle ones sleep in the hypervisor) |
| `memory` | integer, MiB | 8192 | memory ceiling; libkrun only allocates what the guest touches |
| `storage` | integer, GiB | 10 | the machine's writable disk (sparse: takes on the host only what was written) |
| `gpu` | boolean | `false` | host GPU via Vulkan (Venus). With `stack`, the stack must declare GPU; with `image`, the image must have Mesa with Venus |
| `gpu_vram` | integer, MiB | the stack's | the GPU shared-memory window; ignored without `gpu = true` |
| `env` | `K=V` list | — | environment for init, the workload, `exec` and `shell` |
| `workdir` | absolute path | — | directory for init, the workload and `exec`/`shell` without `-w`. **Created** at start if it does not exist |
| `init` | list of commands | — | each item runs with `sh -c`, in order, **once per disk** |
| `entrypoint` | list | — | start of the workload's argv |
| `cmd` | list | — | rest of the workload's argv |
| `stop_signal` | string | `SIGTERM` | signal sent to the workload on stop: any Linux name (`SIGINT`, `SIGWINCH`, ...), with or without `SIG`, `SIGRTMIN+N` (glibc numbering: `SIGRTMIN+3` = 37, systemd's) or the number (1–64). The table is Linux's, not the macOS host's, where several numbers differ |
| `stop_timeout` | integer, s | 10 | how long the workload has to exit; after that, `SIGKILL`. Maximum 300 |
| `stop_on_exit` | boolean | `false` | when the workload exits, the machine flushes the disk and powers off on its own. Requires a workload |
| `volumes` | `HOST:GUEST[:ro\|:staged]` or `/GUEST` list | — | host directories over virtiofs, mounted on every start; `:staged` copies to the disk and returns on sync; a bare `/GUEST` is a directory on the machine's disk. Relative to the file's folder; `~` becomes HOME. See [Network and volumes](/cli/network#volumes) |
| `net` | boolean | `false` | egress network through the host gateway, with the blocking floor |
| `ports` | `PORT`, `HOST:GUEST`, `A-B:C-D` range, `/udp` list | — | ports published on the host's `127.0.0.1`; a range becomes one port per number (up to 64 in total) |
| `[network]` | table | — | `allow_hosts`, `allow_host_patterns`, `allow_cidrs`, `deny_cidrs`, `block_private`, `allow_private`; any of them implies `net`. `name` joins the machine to a [named network](/cli/network#named-network) (does not imply `net`) |
| `[dev]` | table | — | `env` **adds** to the one above; `workdir`, `init`, `volumes` and `ports` **replace** (an empty list does not replace) |

## Precedence

**flag > file > default.** A flag only counts if it was typed: `--cpus 0` is not the same as saying nothing.

| Flag | Key | How it combines |
|------|-------|--------------|
| `-f`, `--file` | — | without it, uses `./runner.code` if it exists |
| `--stack`, `--image` | `stack`, `image` | each one clears the other |
| `--cpus`, `--memory`, `--disk-gib`, `--gpu`, `--gpu-vram` | `cpus`, `memory`, `storage`, `gpu`, `gpu_vram` | replaces |
| `-e`, `--env` (repeatable) | `env` | **adds** to the file's; inside the VM, the last definition of a key wins |
| `-w`, `--workdir` | `workdir` | replaces |
| `--init` (repeatable) | `init` | replaces the whole list |
| `--stop-signal`, `--stop-timeout`, `--stop-on-exit` | the same | replaces |
| `-v`, `--volume` (repeatable) | `volumes` | **adds** (relative to the current directory) |
| `--net`, `--block-private` | `net`, `block_private` | replaces |
| `--network` | `[network] name` | replaces |
| `-p`, `--port`, `--allow-host`, `--allow-host-pattern`, `--allow-cidr`, `--deny-cidr`, `--allow-private` (repeatable) | `ports`, `[network]` | **adds** |
| `-- CMD ...` | `entrypoint` + `cmd` | replaces the whole workload |

`--name` is deliberately absent from the file: the same `runner.code` serves several machines.

## Lifecycle

```
rc machine start
  │
  ├─ VM boots, guest responds                      (same as a machine without runner.code)
  ├─ mkdir -p <workdir>                            (only if there is an init or a workload)
  ├─ init already ran on this disk?  ── yes ──┐    (marker /var/lib/runner/init-done)
  │        └─ no: each item with sh -c,       │
  │           env + workdir; mark at the end  │
  ├─ workload starts in the background  ◀─────┘    (its own process group)
  │     output → console, each line with "[workload] "
  └─ start returns

rc machine stop
  ├─ stop_signal to the workload's group
  ├─ waits up to stop_timeout  ── did not exit ──▶ SIGKILL
  ├─ disk sync
  └─ guest exits, VM powers off
```

Things that bite:

- **A failing init leaves the machine running** and does **not** mark the disk. `start` returns an error with the command and its stderr;
  `rc machine shell` lets you inspect the state, and the next `start` (after a `stop`) runs init again, from the beginning.
  Write init commands that can safely run twice (`mkdir -p`, `pip install`).
- **Init runs once per disk, not per file.** Changing the `init` in `runner.code` runs nothing on a machine that already
  exists; the file is read at `create`. To redo it: `rc machine delete` and `create` again.
- **The workload starts on every `start`**, including after a `stop_on_exit`.
- Stop gives the guest `stop_timeout` + 5 s before the host kills the VM: the workload gets the full grace period, and the disk
  sync still fits afterwards.
- `--timeout` on `rc machine stop` applies only to that stop and overrides `stop_timeout`.

## Logs

The machine console has what the guest writes and the workload's output, each workload line tagged with `[workload] `.

```bash
rc machine logs --name web                        # everything
rc machine logs --name web --source workload      # only the workload, without the tag
rc machine logs --name web --source workload --tail 20
```

When the workload exits, the last line is `exited with code N`.

## Any image

```toml
image = "redis:7-alpine"
cmd = ["redis-server", "--save", "60", "1"]
```

The image is pulled on the host, flattened and converted to an ext4 by the same build VM the stacks use, with the guest and
`start.sh` injected and the image's `Env` preserved. The internal name is `img-` + 12 hex characters of the reference's sha256: the
same reference reuses the cached ext4.

Difference from `stack`: there is no warmup and no catalog entry, and the image must be Linux for the host architecture
(arm64 on a Mac, amd64 or arm64 on Linux).

## What is rejected

A key that `runner.code` recognizes but does not implement yet is an **error with the reason**, never silently
ignored: an ignored `[network] allow_hosts` would make someone believe the machine has a network filter.

| Key | Situation |
|-------|----------|
| `net_backend = "tsi"` | all networking goes through the filtering gateway; `"virtio-net"` is accepted |
| `allow_system_mounts` | mounting a host system directory (`/etc`, `/var/log`) |
| `nested`, `nested_virt` | nesting is a decision for whoever boots the machine: `--nested` on the CLI |
| `cuda` | rc has no CUDA (`--gpu` is Vulkan) |
| `overlay`, `health`, `restart`, `[artifact]`, `[pack]`, `rosetta`, ... | not implemented |

`user`, `docker_socket`, `[secrets]` and `[auth] ssh_agent` are accepted and behave like the flags
(`--user`, `--docker-socket`, `--secret-env`/`--secret-file`, `--ssh-agent`).
| any other key | `unknown key(s): ...` (typos do not slip through) |

## API

`POST /api/v1/machines` accepts the same fields in JSON (`image`, `env`, `workdir`, `init`, `workload`,
`stop_signal`, `stop_timeout`, `stop_on_exit`, `gpu_vram_mib`) or the whole file in `runner_code`. With both,
the file is the base and every non-empty JSON field wins, like the flags on the CLI.

```bash
curl -s localhost:8080/api/v1/machines -d "$(jq -n --rawfile rc runner.code '{name:"web", runner_code:$rc}')"
curl -s 'localhost:8080/api/v1/machines/web/logs?source=workload&tail=20'
curl -s -X POST localhost:8080/api/v1/machines/web/stop          # uses the machine's stop_timeout
```
