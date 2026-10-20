---
title: 'CLI overview'
description: 'The rc subcommands'
---

## The binary

`rc` is the host binary. It runs on macOS on Apple Silicon and on Linux x86_64/aarch64 with KVM.

```bash
rc --help
```

| Subcommand | What it does | Page |
|------------|-----------|--------|
| `run` | runs code in a fresh VM (cold boot), streaming the output | [run and shell](/cli/run) |
| `shell` | interactive shell in a fresh VM of a stack | [run and shell](/cli/run#shell) |
| `machine ...` | persistent machines: `create`, `start`, `exec`, `shell`, `cp`, `logs`, `stop`, `delete`, `run`, `branch`, `pause`, `resume`, `sync`, `update`, `status`, `ls`, `data-dir`, `prune` | [machine](/cli/machine), [runner.code](/cli/runner-code), [Network and volumes](/cli/network) |
| `pack ...` | `create`, `inspect`, `prune`: an image or machine as a portable artifact | [pack](/cli/pack) |
| `serve` | HTTP API with a pool of pre-booted VMs and machine routes | [HTTP API](/cli/http-api) |
| `image build`, `image prune` | builds the ext4 for one or more stacks from the official OCI image; cleans the cache | [image](/cli/image) |
| `languages` | lists the catalog and whether each stack's image has been built | below |
| `doctor` | checks whether the host can run microVMs and what it offers | below |
| `vmm` | a VM's process; internal use, invoked by the launcher | [libkrun](/concepts/libkrun) |

The guest (`runner-guest`) is a separate binary and runs only inside the VM; you never call it by hand.

## languages

```bash
rc languages
```

One line per catalog stack, with `ready=true` when the ext4 is already in the cache, and the source OCI image:

```
awk      ready=true  alpine:3.22
...
llama    ready=true  fedora:43
nodejs   ready=false node:22-alpine
python   ready=true  python:3.12-alpine
```

## doctor

```bash
rc doctor
```

One line per item, with `ok`, `info`, `warn` or `FAIL`. Real output on an M1 Pro Mac:

```
ok    platform
ok    libkrun
ok    gpu: libkrun built with virtio-gpu (Venus); use --gpu on a stack with gpu
info  cuda: not supported (no CUDA remoting; --gpu is Vulkan)
info  nested: not supported on this host; --nested is refused
ok    guest
info  sandbox: macOS has no Landlock/seccomp; the VM process runs unconfined
ok    image awk
...
```

| Item | What it checks |
|------|----------------|
| `platform` | macOS on Apple Silicon, or Linux with `/dev/kvm` open for reading and writing |
| `libkrun` | whether the library loads from the configured path |
| `gpu`, `cuda` | whether libkrun has virtio-gpu (Venus); CUDA is not supported |
| `nested` | whether the host supports nested virtualization (`--nested`) |
| `guest` | whether the guest binary exists next to the host binary |
| `sandbox` | on Linux, the Landlock ABI and seccomp; warns when the ABI is < 9 |
| `image <stack>` | one line per image already built |

A failure prints as `FAIL <item> <reason>`, and the command exits with an error if there is any.

## Environment variables

| Variable | Default | Purpose |
|----------|--------|-----|
| `RUNNER_LISTEN` | `127.0.0.1:8080` | `serve` address |
| `RUNNER_CACHE_DIR` | `~/Library/Caches/runner-codes` (macOS), `~/.cache/runner-codes` (Linux) | images, build VM, machines, packs, sockets |
| `RUNNER_KRUN_LIB` | `~/.nix-profile/lib/libkrun.dylib` (macOS); on Linux `~/.nix-profile/lib/libkrun.so.1` or the `libkrun.so.1` soname | libkrun path |
| `RUNNER_GUEST_BIN` | `rc-guest-linux-<arch>` next to the host binary | guest injected into the images |
| `RUNNER_SANDBOX` | on | `off` disables Landlock + seccomp on Linux, for diagnostics only |

## Through the Taskfile

| Task | Equivalent to |
|------|------------|
| `task run -- <flags>` | `./bin/rc run <flags>` |
| `task shell -- <flags>` | `./bin/rc shell <flags>` |
| `task serve -- <flags>` | `./bin/rc serve <flags>` |
| `task image:build -- <flags>` | `./bin/rc image build <flags>` |
| `task doctor` | `./bin/rc doctor` |
