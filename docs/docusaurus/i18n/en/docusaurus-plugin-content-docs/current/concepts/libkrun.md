---
title: 'libkrun'
description: 'How Runner Codes uses libkrun: boot, one process per VM, network off and read-only root'
---

## What it is

[libkrun](https://github.com/containers/libkrun) is a VMM packaged as a library: the process that calls it becomes the VM. The kernel comes from libkrunfw, also as a library. Versions in use:

| Component | Version |
|-----------|---------|
| libkrun | 1.19.6 (macOS, our own derivation in `nix/`); 1.19.5 in nixpkgs (Linux) |
| libkrunfw | 5.6.2, Linux kernel 6.12.109 (macOS, `nix/`); 5.5.0 in nixpkgs (Linux) |

Runner Codes calls libkrun directly, with no intermediate layer, and loads it at runtime with purego — there is no cgo on the host.

| Platform | Hypervisor |
|----------|------------|
| macOS Apple Silicon | Hypervisor.framework (HVF) |
| Linux x86_64 / aarch64 | KVM (`/dev/kvm`) |
| Windows | not supported: upstream libkrun has no WHP backend |

## One process per VM

`krun_start_enter` does not return while the VM is running: it takes over the process. That is why each VM is its own process, `rc vmm --spec <arquivo>`, created by the launcher.

- **Spawn, never fork.** A fork of a Go (multi-threaded) process is killed by HVF about 2 s later.
- **Timeout:** SIGKILL on the VM's process group.
- **Parent dies, VM dies** (job VMs): on Linux, `Pdeathsig`; on macOS, which has no equivalent, the child watches a pipe inherited on fd 3 and exits when it closes. A persistent machine's VM is the exception: it runs detached, in its own session, and outlives the `rc` that started it.
- **Library on macOS:** the `vmm` process needs `DYLD_LIBRARY_PATH` pointing to the libkrun directory to find `libkrunfw.5.dylib`; the launcher sets it.
- **Entitlement on macOS:** the binary needs `com.apple.security.hypervisor`. Without it, `krun_start_enter` returns `-22`.

## Root: read-only ext4 disk

The VM mounts a read-only ext4 disk as its root:

- `krun_add_disk2` attaches the language's ext4;
- `krun_set_root_disk_remount` mounts `/dev/vda` as root, read-only.

The first process of a job VM is `/opt/runner/start.sh`, which:

1. mounts a 256 MiB tmpfs at `/tmp`;
2. exports the environment from the OCI config (`/opt/runner/env`: `PATH`, `JAVA_HOME`...) and sets `HOME=/tmp/home`;
3. for Go, copies the warm build cache from `/opt/runner/gocache` to `/tmp/go-cache`;
4. runs the guest on port 5000.

Every write goes to the tmpfs, which disappears with the VM. In a machine, the first process is
`/opt/runner/machine.sh`, which mounts the machine disk and an overlayfs on top of the image; see [machine](/cli/machine).

**Why a disk and not virtiofs:** with virtiofs, sharing does not prevent the guest from reaching other host directories (libkrun documentation), and it was not faster.

## Network: off for jobs, policy-driven for machines

By default libkrun turns on TSI (transparent socket impersonation) on its own, which gives the guest network access
through the host. Runner Codes turns it off:

- `krun_disable_implicit_vsock` prevents the implicit vsock;
- `krun_add_vsock(ctx, 0)` adds a vsock without any TSI flag.

Result: the job VM has no network. The end-to-end test confirms this by trying to connect to `1.1.1.1`, and proves the
test itself by failing when the network is turned on deliberately.

A machine with `--net` does not use TSI either: libkrun connects a virtio-net to a socketpair
(`krun_add_net_unixstream`), and on the host side there is a userspace gateway, inside the `vmm` process itself, that
enforces the egress policy. See [Network and volumes](/cli/network).

## Channel to the guest

`krun_add_vsock_port2` with `listen=true` binds the guest's vsock port 5000 to a Unix socket on the host. The protocol is in [vsock communication](/concepts/vsock-communication).

## Arguments of the guest process

libkrun passes the first process's argv through the kernel command line. An argument with a space arrives split in the guest. That is why anything that is a compound command goes into a file (as with `start.sh` and the build warmup), never as an argument.

## Memory and CPU

libkrun memory is elastic: the host only pays for the pages the guest touches. Configuring 256 to 2048 MiB did not change boot time in the measurements. The per-language vCPU and RAM values in the catalog are a ceiling against abuse, not a reservation.
