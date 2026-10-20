---
title: 'Introduction'
description: 'Running untrusted code in libkrun microVMs: disposable jobs and persistent machines'
sidebar_position: 1
---

## What Runner Codes is

Runner Codes runs untrusted code (typically LLM-generated) inside microVMs. The binary is `rc`, and
it works in two ways:

- **Disposable job:** a fresh VM per job, with no network and a read-only root, destroyed at the end. This is what
  `rc run`, `rc shell` and the `/api/v1/run` and `/api/v1/execute` routes do.
- **Persistent machine:** a VM with its own disk that keeps whatever was installed and written from one start to the next.
  This is `rc machine ...`, declarable in a `runner.code` file and controllable through the API.

**Source code:** https://github.com/andrebassi/runner-codes

## The pieces

- **VMM:** [libkrun](/concepts/libkrun) 1.19.6 with the libkrunfw 5.6.2 kernel (Linux 6.12.109), loaded directly
  as a library through purego (no cgo). Each VM is an `rc vmm` process.
- **Hypervisor:** Hypervisor.framework on macOS on Apple Silicon; KVM (`/dev/kvm`) on Linux x86_64 and aarch64.
- **Images:** each stack becomes a read-only ext4 generated from the [official OCI image](/concepts/images), without
  Docker. Machines also accept any OCI image (`--image`).
- **Guest channel:** a 4-byte big-endian length + JSON, on vsock port 5000
  ([protocol](/api-reference/vsock-protocol)).
- **Latency:** the `rc serve` [pool](/concepts/pool) keeps already-booted VMs per language, each one used once.

## What `rc` does

| Feature | Where it is |
|---------|-----------|
| ephemeral CLI job with streamed output; interactive shell in a fresh VM | [run and shell](/cli/run) |
| machines: `create`, `start`, `exec`, `shell`, `cp`, `logs`, `stop`, `delete`, `branch`, `pause`, `resume`, `sync`, `update`, `data-dir`, `prune` | [machine](/cli/machine) |
| machine declared in a file | [runner.code](/cli/runner-code) |
| policy-based networking (allow-host, CIDR, `--block-private`), named networks, ICMP, ports and port ranges | [Network and volumes](/cli/network) |
| volumes: host directory, `:ro`, `:staged`, machine disk, S3 | [Network and volumes](/cli/network#volumes) |
| credentials the machine uses without reading, secrets, SSH agent, socket bridges | [Credentials](/cli/credentials), [machine](/cli/machine) |
| GPU over Vulkan (Venus) and the `llama` stack | [GPU](/advanced/gpu) |
| the VM's screen in the browser, with keyboard and mouse (`--display`) | [Display in the browser](/cli/display) |
| an image or machine as a portable artifact | [pack](/cli/pack) |
| HTTP API with pool, SSE and machine routes | [HTTP API](/cli/http-api), [API overview](/api-reference/overview) |
| Go (`pkg/client`) and Python (`sdk/python`) SDKs | [SDKs](/api-reference/sdks) |
| host check | `rc doctor`, in [CLI overview](/cli/overview#doctor) |

## Platforms

| Platform | Status |
|------------|--------|
| macOS, Apple Silicon | supported, tested end to end and measured |
| Linux x86_64 / aarch64 with `/dev/kvm` | supported; Landlock + seccomp confinement has been tested, but a full microVM on Linux **has not been measured yet** |
| Windows | **not supported**: upstream libkrun has no WHP backend, and the Windows build fails on purpose |

## Measured numbers

macOS 27, Apple Silicon. Linux has not been measured yet.

| What | Value |
|-------|-------|
| boot to the first Python response (30 boots, ro ext4 root) | p50 260 ms, p95 266 ms |
| Python job through the CLI, cold boot | ~240 ms total |
| job through the API with a full pool | 26–43 ms for the complete HTTP request (one stack) |
| `machine start` until the guest responds | 204–244 ms |
| footprint of an idle VM | 44 MB |
| `go run` with the build cache warmed in the image | 247 ms (without the cache, 3.4 s) |

Details and method in [Performance](/advanced/performance) and in the [Test report](/advanced/test-report).

## Languages

The catalog has 15 languages: Python, Node.js, Go, Rust, Java, C, C++, Ruby, PHP, Perl, Bash, sh, AWK, jq and SQLite,
plus the `llama` stack (llama.cpp with Vulkan). See [Languages](/languages/overview).

## Next steps

- [Quickstart](/quickstart): install libkrun, compile, build the image, run the first job and the first machine.
- [Agent reference](/agents): every command, flag, route and error on one page.
- [Architecture](/architecture): the pieces and the path of a job and of a machine.
- [Examples and guides](/guides/examples): `runner.code` examples and guides that prove each flow in a real VM.
- [Security](/advanced/security): what isolates the code and what is still missing.
