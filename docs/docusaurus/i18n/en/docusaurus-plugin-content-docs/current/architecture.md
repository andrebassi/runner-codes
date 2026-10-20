---
title: 'Architecture'
description: 'The pieces of Runner Codes and the path of a job and of a machine, from HTTP to the microVM'
sidebar_position: 3
---

## Overview

```
 HTTP client / CLI / SDK
        │
        ▼
┌───────────────────────────────┐
│ rc  (host: macOS or Linux)    │
│  ├─ domain: validates job/spec│
│  ├─ pool: ready VMs per       │
│  │   language (jobs only)     │
│  ├─ machines: registry, disk  │
│  └─ launcher: spawns the vmm  │
└──────────────┬────────────────┘
               │ spawn (never fork)
               ▼
┌───────────────────────────────┐
│ rc vmm --spec   (1 per VM)    │
│  libkrun (purego) +           │
│  libkrunfw (Linux kernel)     │
│  HVF on macOS / KVM on Linux  │
│  network gateway (machines    │
│   with --net), screen viewer  │
└──────────────┬────────────────┘
               │ Unix socket  ⇄  vsock port 5000
               ▼
┌───────────────────────────────┐
│ microVM                       │
│  job: read-only ext4 root,    │
│       /tmp tmpfs, no network  │
│  machine: image overlay +     │
│       own disk (/dev/vdb)     │
│  /opt/runner/guest (static)   │
└───────────────────────────────┘
```

## Binaries

| Binary | Where it runs | What it does |
|---------|-----------|-----------|
| `rc` | host | `run`, `shell`, `machine ...`, `pack ...`, `serve` (API + pool), `image build/prune`, `languages`, `doctor` and the internal `vmm` subcommand |
| `runner-guest` | inside the VM | listens on vsock port 5000 and serves each operation: running code, exec, file copy, PTY, workload, published port, secrets, SSH agent, S3 mount. Stored as `bin/rc-guest-linux-<arch>` |

There are two binaries because purego forces dynamic linking against glibc on Linux, and the language images are Alpine
(musl). A dynamic guest fails inside the VM with `not found`; the guest is compiled statically, and `scripts/build.sh`
fails if it comes out dynamic.

## Host layers

The host code follows a hexagonal architecture; `cmd/rc` only wires the pieces together.

| Layer | Package | Responsibility |
|--------|--------|------------------|
| domain | `internal/domain` | job, result, limits (stdin, files, size, timeout) and E00x errors |
| application | `internal/app` | job executor and pool; machine lifecycle, branch, staged volumes, credentials, secrets |
| ports | `internal/ports` | interfaces (`VMSpec` and its validation, launcher, catalog, images) |
| adapters | `internal/adapters/krun` | libkrun loading, VM configuration, launcher, `vmm` process and confinement on Linux |
| | `internal/adapters/oci` | OCI image pull and ext4 build in the build VM |
| | `internal/adapters/catalog` | embedded `languages.yaml` |
| | `internal/adapters/guestproto` | vsock protocol client |
| | `internal/adapters/httpapi` | HTTP API (jobs and machines) |
| | `internal/adapters/machinestore` | each machine's registry and disk (a clone of the base disk) |
| | `internal/adapters/runfile` | reading the `runner.code` file (TOML) |
| | `internal/adapters/netgw` | userspace network gateway: egress policy, DNS, ports, ICMP, named networks, credential interception |
| | `internal/adapters/screen`, `viewer` | the VM's screen in the browser |
| | `internal/adapters/hostfs` | host side of volumes (file modes for `:staged`) |
| others | `internal/credca`, `internal/objectstore`, `internal/s3fs`, `internal/pack` | per-machine CA, S3 client, core of the `s3://` volume, pack format |
| guest | `internal/guest` | vsock server and per-language executor |

SDKs: `pkg/client` (Go) and `sdk/python` (Python), standard library only.

## Path of a job through the API

1. `POST /api/v1/run` reaches `httpapi`, which builds the job.
2. The domain validates it: language in the catalog (otherwise E006), stdin ≤ 1,000,000 bytes (otherwise E005), up to 32 files,
   total code ≤ 4 MiB, valid file names, timeout between 0 and 300 s. A rejection becomes HTTP 400.
3. The pool hands over an already-booted VM for that language. If the pool is empty, it boots a VM on the spot (cold boot):
   it never refuses.
4. The host connects to the Unix socket bound to the VM's vsock port 5000 and sends the job as a frame (4-byte length + JSON).
5. The guest writes the files to a directory under `/tmp`, compiles if the language requires it, runs with the job's stdin and
   timeout, and replies with another frame.
6. The VM is destroyed (SIGKILL on the process group) and the pool replenishes a new VM in the background.
7. The API returns `status`, `stdout`, `stderr`, `exception`, `executionTime`.

## Path of a machine

1. `rc machine create` writes the registry to `<cache>/machines/<nome>/machine.json` and an ext4 disk cloned from the base
   disk (clonefile on APFS, FICLONE on btrfs/XFS, a sparse copy everywhere else).
2. `rc machine start` launches `rc vmm` **detached** (its own session, with no tie to the parent): the machine stays up
   after the command exits. The pid lives in `vmm.pid`.
3. The VM's first process is `/opt/runner/machine.sh`: it mounts the disk at `/persist`, an overlayfs with the image
   underneath and `upper/` on top, binds `/workspace` and runs the guest in a `chroot`.
4. `exec`, `shell`, `cp`, `logs` talk to the guest through the same port 5000 socket. With `--net`, the VM gets `eth0` and
   traffic goes through the gateway in the `vmm` process itself, which applies the egress policy.
5. `rc machine stop` asks the guest to `shutdown`; the guest runs `sync` and exits; if it does not finish within `--timeout`, SIGKILL.

Details in [machine](/cli/machine) and [Network and volumes](/cli/network).

## VM lifecycle

- The launcher creates the `rc vmm --spec <file>` process by **spawn**. A fork of a multi-threaded process is killed by
  Hypervisor.framework, which is why fork is never used.
- The `vmm` process loads libkrun, configures the VM from the spec and calls `krun_start_enter`, which takes over the
  process until the VM ends.
- Job timeout: SIGKILL on the process group.
- A job VM dies with its parent: `Pdeathsig` on Linux; on macOS, a watch pipe on fd 3. A machine VM is detached.
- If the `vmm` dies during boot, start ends immediately, quoting the last line of `vmm.log` written during this start.

Details in [libkrun](/concepts/libkrun).

## Images

Each stack has a read-only ext4 generated from the official OCI image, with the guest, `start.sh` and the warmup
baked in. The build runs in a dedicated Alpine VM. See [Images](/concepts/images).

## Configuration through the environment

| Variable | Default | Purpose |
|----------|--------|-----|
| `RUNNER_LISTEN` | `127.0.0.1:8080` | API address (`serve`) |
| `RUNNER_CACHE_DIR` | `~/Library/Caches/runner-codes` (macOS), `~/.cache/runner-codes` (Linux) | images, build VM, machines, packs and sockets |
| `RUNNER_KRUN_LIB` | `~/.nix-profile/lib/libkrun.dylib` (macOS); on Linux `~/.nix-profile/lib/libkrun.so.1` or the `libkrun.so.1` soname | libkrun path |
| `RUNNER_GUEST_BIN` | `rc-guest-linux-<arch>` next to the host binary | guest injected into the images |
| `RUNNER_SANDBOX` | on | `off` disables Landlock + seccomp on Linux, for diagnostics only |
