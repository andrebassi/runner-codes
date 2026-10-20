---
title: 'machine'
description: 'Persistent machines: a named VM with its own disk that keeps what you write across commands and across stop and start'
---

`rc run` creates a fresh VM per job and destroys it at the end. For multi-step work (install, compile, test,
fix), use a **machine**: a named VM with its own disk that keeps existing between one command and the next and
across `stop` and `start`.

## At a glance

```bash
rc machine create --name dev --stack python      # registers it and creates the disk (does not boot)
rc machine start  --name dev                     # boots; the VM keeps running after the command exits
rc machine exec   --name dev -- python3 -V       # runs a command (streamed output)
rc machine shell  --name dev                     # interactive terminal (Ctrl+C, vi, resize)
rc machine cp ./app.py dev:/workspace/app.py     # uploads a file
rc machine exec   --name dev -- python3 /workspace/app.py
rc machine cp dev:/workspace/result.json .       # downloads a file
rc machine logs   --name dev --tail 20           # the machine's console (works while stopped)
rc machine stop   --name dev                     # flushes the disk and powers off
rc machine ls                                    # all of them, with their state
rc machine delete --name dev                     # deletes machine and disk (-f if running)
```

`--name` is optional everywhere: the default is `$RC_MACHINE_NAME` or, without it, `default`. With direnv you can pin one
machine per project.

With a [`runner.code`](/cli/runner-code) in the directory, `create` reads the base (stack or image), resources,
environment, init and workload from it; flags you pass override the file.

## What persists

| Path | Persists? | Where it lives |
|---------|-----------|-----------|
| `/` (packages, `/etc`, `/opt`, `/root`...) | **yes** | writable layer (overlay) on the machine's disk, on top of the stack image |
| `/workspace` | **yes** | directory on the machine's disk; the recommended place for code, data and results |
| `/tmp` | **no** | 256 MiB tmpfs, empty on every start |

`HOME` is `/root`, which also persists.

## How it works

```
host (macOS)                                      │ guest
                                                  │
~/Library/Caches/runner-codes/machines/dev/       │
  machine.json   registry (stack, cpus, ...)      │
  disk.ext4      machine disk ── /dev/vdb ────────┼─▶ /persist ─┬─ upper/      (what was written)
  spec.json      VM spec                          │             ├─ work/       (overlayfs)
  console.log    console (rc machine logs)        │             └─ workspace/  ─▶ /workspace
  v.sock         guest channel (vsock 5000)       │
  vmm.pid        VM process                       │  stack image ── /dev/vda (read-only)
                                                  │  overlay: image below + upper on top ─▶ /
```

- **Disk**: the first time, the build VM formats an empty sparse ext4 (`disks/data-10g.ext4`). Each machine
  gets a **clone** of it: clonefile on APFS, FICLONE on btrfs/XFS, a sparse copy everywhere else. Measured on macOS: a
  10 GiB disk takes 6.4 MB on the host right after creation.
- **Boot**: the machine's first process is `/opt/runner/machine.sh`, in place of the jobs' `start.sh`. It mounts the
  disk at `/persist` and an overlayfs with the image underneath and `upper/` on top, binds `/workspace`, and runs the
  guest-runner in a `chroot` there.
- **Detached VM**: the machine's `rc vmm` process runs in its own session (`setsid`), with no watch pipe and no
  `Pdeathsig`. That is why it outlives the `rc` that booted it. The pid lives in `vmm.pid`, and "running" means the pid is alive. The
  ephemeral jobs are unchanged: their VM still dies together with its parent.
- **Stop**: the host asks the guest to `shutdown`; the guest runs `sync` (writes cached data to the ext4), replies and exits, and the
  VM powers off. If it does not finish within `--timeout` (default 10 s), it gets `SIGKILL`. `--timeout 0` kills it immediately.
- **Terminal (`shell`)**: the guest opens a PTY (devpts mounted by `machine.sh`) and runs the shell in it. The connection becomes
  a two-way frame channel: `input` (keystrokes, base64-encoded so no control byte is lost) and `resize` from the
  host; `output` and `exit` from the guest. The host terminal stays in raw mode, and `SIGWINCH` becomes `resize`. Closing the
  terminal kills the program in the guest; the machine stays up.
- **Updated image**: start uses the stack's current image. If the image is rebuilt, the machine starts seeing the
  new one underneath what it wrote itself, like a system update.

## Ephemeral machine: `machine run`

Create, boot and use in one go:

```bash
rc machine run --net --image alpine -- uname -a          # runs and deletes
rc machine run --net -it --image alpine -- /bin/sh       # terminal; deleted on exit
echo dados | rc machine run -i --stack python -- sh -c 'read x; echo $x'
rc machine run -d -s runner.code                         # stays up with the workload
docker save meuapp | rc machine run --image - -- ./app   # local image, no registry
rc machine run --image ./meuapp.tar -- ./app             # docker save tar or OCI layout directory
```

| Point | How it works |
|-------|--------|
| what runs | the command after `--`; otherwise the `runner.code` `cmd`/`entrypoint`; otherwise, with `-t`, a shell |
| without `-d` | name `run-<hex>`; the machine is deleted at the end, also on Ctrl+C (rc 130) and on SIGTERM |
| `-d` | name `default` (or `--name`); the command becomes the workload and the machine stays up |
| `-t` / `-it` | a real terminal (PTY in the guest); `-i` without `-t` only forwards stdin; with neither, stdin is not read |
| local image | a path starting with `/`, `./` or `../`: a `docker save` tar (or OCI tar) or an OCI layout directory; an architecture different from the host's is rejected |
| `--image -` | reads the tar from stdin and stores it in `<cache>/imports/sha256-<hash>.tar`: the same image reuses the cache |
| init | runs (the disk is always new) |

The declaration flags are the same as `create`'s, including `-s`/`-f`, `-v`, `-p`, `--net`, the network policy and
`--display WxH` (see [Display in the browser](/cli/display)).

## Branch and pause

```bash
rc machine pause  --name dev                    # freezes: processes, memory and connections stay as they are
rc machine resume --name dev
rc machine branch --from dev --name try-1       # copy-on-write copy of the disk (running or stopped)
```

| Point | How it works |
|-------|--------|
| pause | SIGSTOP on the VM process: vCPUs, network gateway and viewer stop together; `exec`, `cp`, `update` and `start` refuse with `INVALID_STATE`; `stop` resumes before powering off |
| branch | the child's disk is a copy-on-write clone of the parent's (clonefile on APFS, FICLONE on Linux): instant and taking no space until it diverges. Installed software, `/root` and `/workspace` come along, and init does not run again |
| parent running or paused | the guest runs `sync` and the VM freezes only during the clone; the disk comes out as consistent as after a power loss |
| what does not come | the **execution**: processes and memory. A RAM and CPU checkpoint needs snapshots, which upstream libkrun does not have |
| ports and display | the child inherits the same ports and display port; the two cannot run at the same time unless one of them is changed |

## Commands

| Command | Flags | Notes |
|---------|-------|------------|
| `create` | `--name`, `-f FILE`, `--stack` or `--image`, `--cpus` (4), `--memory` MiB (8192, elastic), `--disk-gib` (10), `--gpu`, `--gpu-vram`, `-e K=V`, `-w DIR`, `-u USER`, `--init CMD`, `--stop-signal`, `--stop-timeout`, `--stop-on-exit`, `-v`, `-p`, `--docker-socket`, `--expose-socket`, `--mount-socket`, `--secret-env`, `--secret-file`, `--ssh-agent`, `--nested`, `-- WORKLOAD` | does not boot the machine. Without `-f`, reads `./runner.code` if it exists. See [runner.code](/cli/runner-code) |
| `start` | `--name` | waits for the guest to respond (up to 30 s); if it does not, kills it and shows the tail of the console. If the VM process dies first (a rejected configuration, like `--nested` on a host without nesting), the error comes right away with the last line of its log. Then runs init (first time on the disk) and starts the workload |
| `stop` | `--name`, `--timeout` (default: the machine's `stop_timeout`, or 10s) | sends `stop_signal` to the workload before the sync; stopping an already stopped machine is not an error |
| `status` | `--name`, `--json` | `running` or `stopped`, with the pid |
| `ls` | `--json` | all machines |
| `delete` | `--name`, `-f` | a running one only with `-f` |
| `exec` | `--name`, `-e K=V` (repeatable), `-w DIR`, `-i` (forwards stdin), `--timeout` (0 = no limit) | argv **with no implicit shell**: for a shell, `-- sh -c '...'`. The `rc` exit code is the command's (124 on timeout) |
| `shell` | `--name`, `-e K=V`, `-w DIR`, optional `-- CMD` | a real terminal (PTY in the guest): Ctrl+C interrupts only the command, the window size follows the host. Without a command, opens `bash` if the image has it, otherwise `sh`. The `rc` exit code is the one from `exit` |
| `cp` | `SRC DST`, with `name:/path` on one side | 1 MiB chunks; atomic on both sides; up to 4 GiB |
| `logs` | `--name`, `--tail N`, `--source all\|workload` | the VM console: what the guest, `machine.sh` and the workload write; `workload` filters to it alone |
| `sync` | `--name` | returns the staged volume copies to the host (see [Network and volumes](/cli/network#staged-volume--v-hostgueststaged)) |
| `data-dir` | `--name` | the machine's directory on the host (registry, disk, published sockets, logs) |
| `prune` | `--all` | the same as `rc image prune`: frees old image builds; with `--all`, also the referenced images no machine uses |

## Errors

Every rejection carries a stable code. Decide by the code, not by the message:

| Code | When |
|--------|--------|
| `NOT_FOUND` | the machine (or the file, in `cp`) does not exist |
| `CONFLICT` | `create` with a name already in use |
| `INVALID_STATE` | `start` on a running one; `start` on a paused one (`is paused; resume it ... or stop it first`); `exec`/`cp` on a stopped one; `delete` on a running one without `-f`; `sync` on a stopped one with a staged volume |
| `CONFIG_ERROR` | invalid name (lowercase letters, digits and `-`, up to 63), stack without an image, `--gpu` on a stack without GPU, relative path in `cp` |

## Measured numbers

macOS 27, Apple M1 Pro, python stack:

| Measurement | Value |
|--------|-------|
| `start` until the guest responds | 204–244 ms |
| freshly created 10 GiB disk, space on the host | 6.4 MB |
| `cp` of 50 MB to the machine | 620 ms (~84 MB/s) |
| `cp` of 50 MB from the machine | 594 ms (~88 MB/s) |

## User, secrets, SSH agent and sockets

| Flag | What it does |
|------|-----------|
| `-u`, `--user NAME\|uid[:gid]` | workload, exec and shell run as that user; init runs as root. `exec --user` and `shell --user` switch just that command |
| `--secret-env VAR=HOST_VAR` | the value of the host variable, read at start, in the environment of every command; it lives only in the guest's memory, never in the registry |
| `--secret-file VAR=/abs/file` | the same, with the content of a host file. A secret with a credential's name feeds the binding and does not enter the VM |
| `--ssh-agent` | the agent of whoever runs start (`SSH_AUTH_SOCK`) appears in the guest; the private key stays on the host |
| `--docker-socket` | the guest's `/var/run/docker.sock` becomes `docker.sock` in the machine's `data-dir` |
| `--expose-socket GUEST[:HOST]` | a Unix socket the workload listens on, reachable from the host (default: its name in the `data-dir`) |
| `--mount-socket HOST:GUEST` | a host socket inside the machine; the guest directory is created if missing. Up to 64 bridges per machine |
| `--nested` | exposes the host's virtualization extensions (`/dev/kvm` in the guest). Needs nested KVM on Linux or an M3+ Mac with macOS 15+; `rc doctor` tells whether this host supports nesting. `runner.code` does not turn it on |

Socket bridges exist only in the CLI: through the API they would be arbitrary paths on the server's disk.

## API

The same operations in `rc serve`. See [API overview](/api-reference/overview#machines).

## Not in this version

The API's interactive WebSocket (the terminal is CLI-only today), `machine stats` and the RAM checkpoint (upstream
libkrun has no snapshots).
Network, ports and volumes are in [Network and volumes](/cli/network). The plan is in the
[Agent reference](/agents#evolution-plan).
