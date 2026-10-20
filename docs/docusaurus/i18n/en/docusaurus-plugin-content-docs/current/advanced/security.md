---
title: 'Security'
description: 'What isolates untrusted code, what is proven by tests and what is still missing'
---

## Model

Job code is treated as hostile. The main barrier is the VM; the second is the confinement of the process that runs the VM.

The libkrun documentation is explicit: **the guest and the VMM share the same security context.** A VM escape lands inside the `rc vmm` process, with its privileges. That is why isolation is the VM **plus** the confinement of that process — and that confinement currently differs by platform.

## Layers of a job (on both platforms)

| Layer | How |
|-------|-----|
| one VM per job | every job runs in a fresh VM, destroyed at the end; nothing carries over from one job to the next |
| read-only root | the root is an ext4 mounted read-only (`/dev/vda`); writes only go to the `/tmp` tmpfs (256 MiB) |
| ephemeral state | the tmpfs disappears with the VM |
| no network | `krun_disable_implicit_vsock` + `krun_add_vsock(ctx, 0)`: no TSI, no network interface |
| no virtiofs | the root is a disk, not a shared host directory; virtiofs would not stop the guest from reaching other host directories |
| per-language ceilings | vCPU and RAM limited in the catalog; default timeout 10 s, maximum 300 s, with SIGKILL on the process group |
| input limits | stdin ≤ 1,000,000 bytes, up to 32 files, code ≤ 4 MiB, names without `/`, without `..`, not starting with `.` |
| parent dies, VM dies | no VM is left orphaned if the host goes down |

## Per platform

| Item | macOS | Linux |
|------|-------|-------|
| hypervisor | Hypervisor.framework | KVM |
| permission to create a VM | `com.apple.security.hypervisor` entitlement on the binary | access to `/dev/kvm` |
| VM dies with its parent | watch pipe on fd 3 | `Pdeathsig` |
| timeout | SIGKILL on the process group | SIGKILL on the process group |
| unprivileged uid/gid in the VM process | — | supported by the spec (`krun_setuid`/`krun_setgid`) |
| Landlock | does not exist on macOS | applied to the `vmm` process of every job VM |
| seccomp | does not exist on macOS | denylist applied to all threads (TSYNC) |

On macOS, confinement of the `vmm` process comes down to the entitlement and the process group.

### Confinement on Linux

Before handing the process over to libkrun, the `vmm` of a job VM (disk root, no network) applies:

| Mechanism | What is allowed | What is denied |
|---|---|---|
| Landlock (files) | reading the root disk and already-loaded libraries, `/proc`, `/sys`; writing to the console and to the VM's own directory (`run/<id>/`); `/dev/kvm` with ioctl | the rest of the file system |
| Landlock (network, ABI ≥ 4) | nothing | every TCP `bind` and `connect` (and UDP on ABI 10) |
| Landlock (scope, ABI ≥ 6) | — | signals and abstract sockets outside the process itself |
| Landlock (Unix socket, ABI ≥ 9) | the VM's own socket | `connect` to another VM's socket |
| seccomp | everything else | `execve`, `ptrace`, `mount`, `unshare`/`setns`, kernel modules, `kexec`, `bpf`, `perf_event_open`, keyring, clock, swap, reboot, `userfaultfd` and the like (`EPERM`) |

libkrunfw is loaded before Landlock, because libkrun only opens it inside `krun_start_enter` and that `open` would fail
after confinement. seccomp is a denylist, not an allowlist, on purpose: an allowlist would require measuring every
libkrun syscall on a Linux host with KVM, and a single missing one would bring down every VM.

**Measured** on Ubuntu 26.04 (kernel 7.0, aarch64, Landlock ABI 8), with the process itself confined: reading a file
outside the policy, writing next to the disk, TCP, `execve` and `personality` denied; reading the disk and creating
its own socket allowed. Without confinement, the same probe succeeds at everything (proof of failure).

**Not measured:** a full microVM under confinement on a Linux host with KVM (the test VM has no `/dev/kvm`).

**Known gap:** with Landlock ABI < 9 the kernel does not control `connect` on a path-based Unix socket; one VM's
process can still reach another's socket. `rc doctor` warns about it. `RUNNER_SANDBOX=off` disables confinement for
diagnostics, with a warning on stderr.

## Persistent machines

A machine gives up part of a job's isolation (it keeps state and may have network access), and every opening is
declared and has a floor:

| Layer | How |
|-------|-----|
| network off by default | without `--net` (or `net`, or `-p` alone), the machine has no network card; with `-p` and no `--net`, the host reaches the port, but the guest cannot get out |
| egress policy | userspace gateway inside the `vmm` process itself, no TSI: allow-host by name, CIDR, `--block-private`; a name outside the list gets NXDOMAIN; an IP only counts if it came from an allowed name. See [Network and volumes](/cli/network) |
| floor no rule can open | cloud metadata and all of link-local; the host loopback only with `--allow-cidr 127.0.0.1`, never through an IP learned from DNS (DNS rebinding) |
| logged denials | every denied egress goes to `egress-denials.log` in the machine directory |
| credentials without reading them | the guest sees a placeholder; the gateway swaps it for the real value only on TLS to the binding's host, with a CA belonging to the machine itself (name constraints). A placeholder in the URL is rejected with 403. See [Credentials](/cli/credentials) |
| secrets | `--secret-env`/`--secret-file` are read at start and sent to the guest in memory only; they are not kept in the registry or on the machine's disk |
| SSH agent | the guest uses the host's keys through a forwarded socket; the private key never enters the VM |
| `:ro` volume | the protection is on the device, not just the mount option: `mount -o remount,rw` by the guest's root does not open the volume (tested) |
| nested virtualization | `--nested` is rejected where the host does not nest; through the API, only with `rc serve --allow-nested-virt` |
| display | the viewer only listens on the host's `127.0.0.1`, with no password |

What a machine does **not** protect: the guest's root can read the environment of its own processes, so a secret
handed to the machine is within reach of the code running in it. Credential substitution also does not prevent the
server from returning the value in the response body.

## What the end-to-end test proves

| Property | Proof |
|----------|-------|
| no network | a connection to `1.1.1.1` fails inside the VM; with the network turned on deliberately, the test fails |
| read-only root | writing to `/etc` fails |
| no state between jobs | the file written to `/tmp` by one job does not exist in the next job |
| memory ceiling | allocating 2 GiB in a 512 MiB VM fails, without affecting the host |
| timeout | the job in an infinite loop is killed and no VM survives |

## The API

The HTTP API **has no authentication**. By default it only listens on `127.0.0.1`. Do not expose it beyond loopback without an authenticating proxy in front of it.

## Image build

The image pull happens on the host. The rest of the build runs in its own VM, with network access, because installing e2fsprogs and GNU tar and the warmup (`apk add`, for example) need it. The build VM's network never reaches the job VM. The Alpine base tarball used by the build VM has its sha256 verified before use. Language images come from the official OCI images.

## Open items

- validate a full microVM under Landlock + seccomp on a Linux host with KVM;
- API authentication (currently delegated to a proxy);
- Unix socket isolation between VMs on kernels with Landlock ABI < 9.
