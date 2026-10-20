---
title: 'Quickstart'
description: 'Install libkrun, compile, build the Python image, run the first job and the first machine'
sidebar_position: 2
---

## Prerequisites

- macOS on Apple Silicon, **or** Linux x86_64/aarch64 with read and write access to `/dev/kvm`.
- [Nix](https://nixos.org) to install libkrun; Go and [Task](https://taskfile.dev) to compile.
- Internet access to pull the languages' OCI images (only during the image build; the job VM has no network).

Windows is not supported.

## 1. Install libkrun and libkrunfw

Through Nix, on both systems (no Homebrew, no Docker):

```bash
task krun:install    # nix profile add .#krun
```

- **macOS:** libkrun 1.19.6 and libkrunfw 5.6.2, from the repo's own derivations (`nix/`). The first time compiles
  libkrun (~10 min).
- **Linux:** libkrun and libkrunfw from nixpkgs, with virtio-blk enabled.

Both libraries go into `~/.nix-profile/lib`, where `rc` looks by default.

## 2. Compile

```bash
task build
```

Produces in `bin/`:

- `rc`: the host (CLI, API, image build, machines and each VM's process);
- `rc-guest-linux-<arch>`: the static guest that runs inside the VM.

On macOS, the build signs `rc` with the `com.apple.security.hypervisor` entitlement. Without it, `krun_start_enter` fails.

## 3. Check the host

```bash
./bin/rc doctor
```

One line per item: `ok` for `platform`, `libkrun` and `guest`, `info` lines for what this host does not offer (CUDA,
nested virtualization, confinement on macOS) and one `ok image <stack>` line per built image. A failure prints as
`FAIL <item> <reason>`.

## 4. Build the language image

```bash
task image:build -- --stack python
```

Pulls `python:3.12-alpine`, generates the read-only ext4 with the guest and the warmup baked in, and stores it in the cache. On the measured
machine, the Python image took ~8–10 s. A second build with the same inputs comes from the cache. For every
language in the catalog: `task image:build -- --all`.

## 5. Run a job

```bash
./bin/rc run --stack python --code 'print(1)'
./bin/rc shell --stack python      # interactive shell in a fresh VM; gone on exit
```

Each `run` boots a fresh VM (cold boot), shows the output while the program runs and destroys the VM at the end. The command
exits with rc 1 if the program fails or runs out of time. See [run and shell](/cli/run).

## 6. Start a persistent machine

```bash
./bin/rc machine create --name demo --stack python
./bin/rc machine start --name demo
./bin/rc machine exec --name demo -- sh -c 'echo persistiu > /etc/minha-config'
./bin/rc machine stop --name demo
./bin/rc machine start --name demo
./bin/rc machine exec --name demo -- cat /etc/minha-config     # persistiu
./bin/rc machine delete --name demo --force   # without --force, a running machine is refused (INVALID_STATE)
```

What is written to `/` and to `/workspace` survives a stop; `/tmp` comes back empty. Without `--net`, the machine has no
network. See [machine](/cli/machine), [runner.code](/cli/runner-code) and [Network and volumes](/cli/network).

## 7. Start the API

```bash
task serve
```

Listens on `127.0.0.1:8080` and keeps a [pool](/concepts/pool) of 2 ready VMs per language with a built
image. In another terminal:

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{"language":"python","files":[{"name":"main.py","content":"print(input()[::-1])"}],"stdin":"abc"}'
```

```json
{"status":"success","stdout":"cba\n","stderr":null,"exception":null,"executionTime":35}
```

The `executionTime` value varies on each run. The API has no authentication: keep it on loopback. See
[HTTP API](/cli/http-api) and the [SDKs](/api-reference/sdks).

## 8. Check everything at once

```bash
task runbook
```

Runs `docs/runbook.md` end to end and fails if any step differs from what is expected. In the latest run
(2026-10-20, macOS 27), there were 116 checks and 0 failures.
