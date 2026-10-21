---
title: 'Test report by platform'
description: 'End-to-end tests of rc on macOS, Linux and Windows: environment, method, measured numbers, defects found and what was not tested'
---

# Test report by platform

Last full run: **10/20/2026**, `rc` binary on top of libkrun + libkrunfw. The baseline measurements (boot, memory,
image build, per-stack execution) are from **10/19/2026**, on the same host. Every number on this page was measured;
whatever was not tested is stated as not tested.

## Run of 10/20/2026 (macOS 27, Apple M1 Pro)

| Gate | Result |
|---|---|
| `task test` | 20 packages, all green (unit tests with `-race`) |
| `task test:cov` | **95.2%** total, domain **100%**, guest **75%** (ratchet at 75), Python SDK **100%** in lines and branches |
| `task test:e2e` | **42 of 42** tests green, in two consecutive runs |
| `task runbook` | **116** checks, **0** failures |
| `task guides` | **12** folders, **215** checks, **0** failures |
| `task test:targets` | darwin/arm64, linux/amd64 and linux/arm64 compile; windows/amd64 rejected with the reason |

On Linux, Landlock + seccomp were tested with the process actually confined on Ubuntu 26.04, in a VM without KVM; a
full microVM on Linux **was not measured**. For `--nested`, only the **rejection** was measured (this Mac is an M1);
the path that turns it on was not measured.

> **Update on the same date:** a full microVM **was measured** on x86_64 Linux with KVM (a DigitalOcean droplet,
> nested KVM), with the job under Landlock (per thread, ABI 4) + seccomp, and the Kubernetes runtime passed 19/19 on
> k3s and microk8s. Details in [Linux with KVM and Kubernetes](#linux-com-kvm-e-kubernetes).

## Summary by platform

| Platform | Environment | Real microVM | Result |
|---|---|---|---|
| **macOS** | MacBook M1 Pro, macOS 27.0.1, Hypervisor.framework | **yes** | e2e 42/42 in two consecutive runs; runbook and guides with no failures |
| **Linux** | Ubuntu 26.04.1 LTS on VMPal, kernel 7.0.0-30, aarch64 | **no** (no `/dev/kvm`) | Landlock + seccomp confinement applied and proven on the real process; 30 tests green in two runs |
| **Linux with KVM** | DigitalOcean droplet `s-4vcpu-8gb`, Ubuntu 24.04, kernel 6.8, x86_64, nested KVM | **yes** | Python job under Landlock (ABI 4, per thread) + seccomp; Kubernetes 19/19 on k3s 1.36 and microk8s 1.35, two runs |
| **Windows** | Windows 11 Pro 24H2 (build 26100.9457) ARM64 on VMPal | **no** | out of scope: build rejected on purpose; the hypervisor does not start in the VM (`EL2 not present`) |

The common reason for the "no" on Linux and Windows: both are VMs inside an **M1 Pro**. Virtualization.framework only
provides the EL2 exception level (needed for a hypervisor inside the VM) starting with the **M3**. Without EL2, no
nested VM starts, whether KVM or Hyper-V.

## macOS (target measured end to end)

### Environment

| Item | Value |
|---|---|
| hardware | MacBook Pro M1 Pro (arm64) |
| system | macOS 27.0.1 |
| VMM | libkrun 1.19.6 (the repo's Nix derivation, `nix/`) |
| guest kernel | libkrunfw 5.6.2 → Linux 6.12.109 aarch64 |
| binary | `bin/rc`, ad-hoc signed with `com.apple.security.hypervisor` |
| guest | `bin/rc-guest-linux-arm64`, static, 2.6 MB |

### Boot and resources (spike, 30 boots per measurement)

Measured with a harness that spawns the VM process and times it until the first response to a job over vsock.

| Measurement | Value |
|---|---|
| boot to the first Python response, read-only ext4 root | p50 **260 ms**, p95 **266 ms** (min. 254, max. 279) |
| boot to the first `sh` response, virtiofs root | p50 245 ms, p95 264 ms |
| full cycle spawn → `uname` → shutdown | 221–252 ms with a warm cache; 869 ms on the first one |
| footprint of an idle VM (512 MiB configured) | **44 MB** (`footprint`); RSS of 172 MB includes shared clean pages |
| configured RAM 256 / 1024 / 2048 MiB | boot 260 / 260 / 261 ms; RSS +40 MB in total |
| `SIGKILL` on the process group | no process survives (every measured boot) |

Decision taken from this: root on a read-only ext4 disk. virtiofs was not meaningfully faster and exposes a host
directory to the guest.

### Network

| Configuration | `wget http://1.1.1.1` | `nc 1.1.1.1 80` |
|---|---|---|
| vsock without TSI (default for job VMs) | `Network unreachable` | rc=1 |
| vsock with TSI (build VM only) | connects | rc=0 |

Proof in both directions: the test uses an IP, because with a name the DNS failure would hide the difference.

### Image builds (15 stacks, `rc image build --all`)

| Stack | OCI image | Build | ext4 |
|---|---|---|---|
| awk | alpine:3.22 | 2.6 s | 45 MiB |
| bash | bash:5.2 | 2.9 s | 57 MiB |
| c | alpine:3.22 + build-base | 19.8 s | 332 MiB |
| cpp | alpine:3.22 + build-base | 19.5 s | 332 MiB |
| go | golang:1.25-alpine | 35.3 s | 377 MiB |
| java | eclipse-temurin:21-jdk-alpine | 34.0 s | 478 MiB |
| jq | alpine:3.22 + jq | 2.7 s | 47 MiB |
| nodejs | node:22-alpine | 14.1 s | 234 MiB |
| perl | alpine:3.22 + perl | 8.4 s | 98 MiB |
| php | php:8.3-cli-alpine | 9.4 s | 152 MiB |
| python | python:3.12-alpine | 8.2 s | 128 MiB |
| ruby | ruby:3.3-alpine | 8.2 s | 134 MiB |
| rust | rust:1-alpine | 37.4 s | 1.2 GiB |
| sh | alpine:3.22 | 2.2 s | 45 MiB |
| sqlite | alpine:3.22 + sqlite | 2.8 s | 49 MiB |

`mkfs.ext4` dominates the time for large images (27.8 s of Go's 35 s): the ext4 is written on the host via virtiofs.
Pull and flattening of `python:3.12-alpine`: 2.7 s, 55 MB.

### Per-stack execution (`rc run`, cold boot)

| Stack | Execution | Total (with boot) |
|---|---|---|
| python | 31–44 ms | ~240 ms |
| nodejs | 128–148 ms | 353–371 ms |
| go | **247 ms** with a warm cache (3,429 ms without it) | 510 ms |
| java (JDK 21.0.12) | 522 ms | 739 ms |
| c | 71–72 ms | 282 ms |
| sh | 2 ms | 199 ms |

### API with a pre-booted pool

| Situation | Full HTTP request |
|---|---|
| 1 stack with an image, pool of 2 | 26–43 ms |
| 15 stacks, others still refilling the pool | 73–78 ms |
| 15 stacks, all pools full | 49–175 ms |
| before the global boot ceiling (30 VMs booting at once) | 1.7–2.3 s (regression found and fixed) |

### e2e suite (`task test:e2e`, tag `e2e`)

42 tests, run twice in a row and green both times. Each test locks in a measured behavior. The table below describes
the ones that pin down the contract for jobs and machines; the rest are in the following table.

| Test | What it proves |
|---|---|
| `TestStdoutAndExitCode` | correct output; the program's `exit 3` becomes a non-zero rc from the CLI |
| `TestTimeoutKillsVM` | an infinite loop times out in ~2.2 s and no VM process is left behind |
| `TestNoNetwork` | connection to 1.1.1.1:80 blocked. **Proof of failure:** with `Network: true` planted in the pool, the test failed (the Python probe printed that it connected; the message was in Portuguese at the time and is now `guest reached the network: "CONNECTED\n"`); restored, it passed |
| `TestRootReadOnlyAndIsolation` | `/etc` does not accept writes; a file written to `/tmp` by one job does not exist in the next |
| `TestMemoryCeiling` | allocating 2 GiB in a 512 MiB VM fails inside the VM without affecting the host |
| `TestLanguageFamilies` | 13 stacks run (nodejs, go, java, c, cpp, rust, ruby, php, perl, bash, awk, jq, sqlite); a failed Java compilation exits with a non-zero rc |
| `TestShell` | `rc shell` with stdin from a pipe comes up with a read-only root, writable `/tmp` and no network; on the bash stack it is real bash 5.2 |
| `TestShellTTY` | `rc shell` under a real pseudo-terminal (`creack/pty`, 40×120): the guest's stdin is a terminal, `stty size` returns `40 120`, and Ctrl+C interrupts a `sleep 30` without bringing down the VM (the test takes ~1.4 s; without the interrupt it would take 30 s). **Proof of failure:** without raw mode on the host the test failed (`VM survived the shell exit`); restored, it passed |
| `TestServePool` | `serve` fills the pools, handles `/run` and, on SIGTERM, leaves no VM alive |
| `TestRunStreams` | `rc run` with Python that writes, sleeps 1.5 s and writes again: the 1st line reaches the host's stdout ≥ 1 s before the 2nd (measured: 0.25 s and 1.75 s after start). **Proof of failure:** with `rc run` buffering the output, both lines arrived 3.8 µs apart and the test failed |
| `TestServeStream` | `POST /api/v1/execute/stream` with the real VM: `text/event-stream`, `stdout` events with the 1st line ≥ 1 s before the 2nd, a final `result` with `exitCode` 0; a rejected job responds 400 before the stream |
| `TestMachineLifecycle` | `rc machine`: create, CONFLICT on a repeated name, INVALID_STATE on exec with the machine stopped, start (198–244 ms), writes to `/etc`, `/opt` and `/workspace`, exec rc passed through (7), the stack's Python running inside, stop, logs with the machine stopped, start again: everything persisted and `/tmp` came back empty; delete leaves no VM. **Proof of failure:** with the machine booting without `machine.sh`, the write to `/etc` failed and the test failed |
| `TestMachineShellTTY` | `rc machine shell` under a real pseudo-terminal (40×120) on a running machine: the shell sees a TTY and `40 120`, Ctrl+C interrupts a `sleep 30` without killing the shell, what the shell writes to `/workspace` is visible to exec, `exit 4` becomes rc 4 and the machine stays running. **Proof of failure:** without devpts in `machine.sh` (the real defect found: `open /dev/ptmx: no such device`), the shell did not open and the test failed |
| `TestRunnerCode` | machine from a `runner.code` with `--cpus 1` on top of the file (the flag wins); the `/app` workdir is created on start; the workload comes up with env and workdir and shows in `logs --source workload`; init runs once (counter at 1 after stop+start); stop sends `SIGINT` and the workload's trap writes before the sync. **Proof of failure (two planted defects):** stop sending SIGTERM instead of `stop_signal` → the trap's file does not exist; init ignoring the marker → counter at 2. Both failed |
| `TestRunnerCodeStopOnExit` | with `stop_on_exit`, the end of the workload shuts the machine down by itself (~0.5 s) and the log shows the output and `exited with code 0` |
| `TestRunnerCodeRefusals` | `net = true` rejected with the reason and nothing created; neither stack nor image is an error |
| `TestRunnerCodeImage` | `--image alpine:3.20` (image outside the catalog): built, boots and runs `cat /etc/alpine-release` |
| `TestMachineNoNetwork` | a machine without `--net` has no `eth0` and cannot get out |
| `TestMachineNetwork` | with `--net`: `eth0` `100.96.0.2/30`, DNS on the gateway, `example.com` responds; metadata `169.254.169.254` rejected immediately even with the network open |
| `TestMachineEgressPolicy` | `--allow-host example.com`: the name goes through, `iana.org` gives `bad address` in under 1 s and goes into `egress-denials.log`; `machine update` allows and removes while the machine is running. **Proof of failure:** with the gateway ignoring the policy, metadata and the blocked name went through and the test failed |
| `TestMachinePortsAndVolumes` | `runner.code` with rw and ro volumes, a published port and no `--net`: the workload serves the host file through the port, the guest's write shows up on the host, `:ro` rejects writes **even after `mount -o remount,rw` by the guest's root**, the guest cannot dial out, and the volumes come back on every start. **Proof of failure:** with `:ro` only in the mount option (no flag on the device), the remount opened the volume and the test failed. Before this check, that defect slipped through |
| `TestMachineCopy` | `rc machine cp` of 5 MiB (several chunks) to the machine and back: identical sha256 inside and outside |
| `TestMachineAPI` | `/api/v1/machines` routes: 201/409/200/204/404, exec with rc 4, exec/stream with the 1st line ≥ 1 s before the 2nd and `exit` at the end, identical PUT/GET of 2.6 MB, INVALID_STATE (409) with the machine stopped, stop in under 5 s. **Proof of failure:** with the VM detached without Wait (a zombie in `rc serve`), stop took 10 s and the test failed |
| `TestGPUVulkan` | `--gpu` on the `llama` stack: `vulkaninfo` shows `Virtio-GPU Venus (Apple M1 Pro)`; without `--gpu` there is no `/dev/dri`; `--gpu` on a stack without GPU is rejected with E007. **Proof of failure:** with `Configure` missing the `krun_set_gpu_options2` call, the test failed (`no Venus device with --gpu`) |
| `TestGPULlama` | `llama-bench` with the same model on the GPU (`-ngl 99 -dev Vulkan0`) and on the CPU: the device used is Venus and the prompt comes out ≥ 2× faster (measured 893–1,365 t/s versus 193–264 t/s, 4 runs) |

Tests added with stories 7.4 to 7.6 and with the guides:

| Test | What it proves |
|---|---|
| `TestMachineRun` | `machine run`: the command runs in a machine that disappears at the end, the rc is the command's, `-i` passes stdin through, nothing to run is an error before creating anything |
| `TestMachineRunDetached` | `-d` leaves the machine running with the workload under the requested name |
| `TestMachineRunTTY` | `-it` under a real pseudo-terminal opens the shell with a TTY; its `exit` becomes the rc and the machine disappears |
| `TestMachineRunCallerKilled` | a `machine run` whose CLI gets SIGKILL does not leave the VM alive without an owner (before the fix, it did) |
| `TestMachineBranchAndPause` | when paused, the machine rejects exec with the reason; the branch takes what the parent just wrote; parent and child stay independent; `resume` brings the parent back |
| `TestMachineEgressCIDROnly` | with only `allow_cidrs`, a resolved name does not open a connection (hole closed on 10/19/2026); the allowed CIDR stays reachable |
| `TestMachinePing` | ICMP goes through the gateway under the same policy as TCP; a destination outside the allowlist does not come back and is recorded in the denial log |
| `TestNamedNetworkMachines` | two machines on the same network reach each other by IP and by `<machine>.<network>.internal`, without `--net`; another network cannot reach them; a stopped machine disappears from DNS |
| `TestMachineCredential` | the guest only sees the placeholder and the HTTPS service receives the real value; a placeholder in the URL is rejected with 403; a host outside the binding receives the literal placeholder |
| `TestSecretsAndSSHAgent` | `--secret-env`/`--secret-file` reach exec and are not kept in the registry; a missing variable fails the start; `--ssh-agent` lists the host key without the private key entering the VM |
| `TestExposeAndMountSockets` | `--expose-socket` and `--mount-socket`, in both directions |
| `TestDockerSocketAndDiskVolume` | `docker_socket` responds in the machine directory; a disk volume is real ext4, outside the overlay, and persists |
| `TestStagedVolume` | `:staged`: the host does not change until sync; sync mirrors; graceful stop syncs; the child of a branch returns nothing |
| `TestMachineS3Volume` | `s3://` volume against a real S3 server: reads, byte-by-byte writes (8 MiB), `mv` and `rm`, `:ro`, remount after stop/start, wrong credentials fail the start with the reason |
| `TestMachineDisplay` | the guest's frame reaches the host viewer with the right colors; key presses and clicks via `POST /input` reach the guest's evdev |
| `TestPack` | stack as stub + payload; single-file machine read with the origin deleted; persistent mode; `create --from` and `pack inspect`; rejections |
| `TestDevStdinLinks` | `/dev/stdin`, `/dev/stdout`, `/dev/stderr` and `/dev/fd` exist in the job VM and in the machine |

### `rc shell`

| Check | Result |
|---|---|
| python: `python3`, writable `/tmp`, read-only `/etc`, no network | ok |
| go: `go version` with the image's PATH | `go1.25.14 linux/arm64` |
| bash: prompt and `[[ ]]` | `bash-5.2#`, ok |
| under a pseudo-terminal | guest stdin = `/dev/console` on top of `hvc0`; `test -t 0` true; `stty size` = host terminal size |
| Ctrl+C | interrupts the command in the guest; the VM stays alive |

How it works: with a terminal on stdin, `rc shell` turns on `tty` in the spec (`krun_disable_implicit_console` +
`krun_add_virtio_console_default(0, 1, 2)`; with all three fds being TTYs, libkrun creates a single console port) and
puts the host terminal in raw mode for the session, restoring it before exiting. With stdin from a pipe the behavior
stays as before (implicit console, no TTY). The `tty` command still prints `not a tty`, because it cannot find the
device name; `test -t 0` and `stty` show that it is a terminal.

**Load event:** one run of the suite with `-count=2` failed 6 of 9 tests with a load average of ~24 on the host
(VMPal VMs at 88% CPU and `nix-daemon` at 79%); `TestServePool` took 40 s versus the normal 7–8 s. Run in isolation
and in two subsequent runs, the same suites passed in full.

### Numbers from the 10/20/2026 batch

| Measurement | Value |
|---|---|
| pack | python image 134 MiB → 31 MiB payload; machine with an installed package, single file: 46 MiB |
| S3 volume | 8 MiB written in the guest reach the bucket with the same md5 (local S3 server) |
| security defect found | `allow_cidrs` alone let traffic out to any name; fixed and locked in by `TestMachineEgressCIDROnly` |

## Linux (confinement tested; microVM not)

### Environment

| Item | Value |
|---|---|
| VM | "Ubuntu VM 2" on VMPal, 4 vCPU, 7.9 GB |
| system | Ubuntu 26.04.1 LTS, kernel 7.0.0-30-generic, aarch64 |
| active LSMs | `lockdown,capability,landlock,yama,apparmor,ima,evm` |
| Landlock | **ABI 8** (`landlock_create_ruleset(NULL, 0, LANDLOCK_CREATE_RULESET_VERSION)`) |
| `/dev/kvm` | **absent** (no nested EL2 on the M1 Pro) |

### Method

The test binary of the `internal/adapters/krun` package is compiled on the Mac for `linux/arm64` and run in the VM:

```bash
CGO_ENABLED=0 GOOS=linux GOARCH=arm64 go test -c -cover -o krun.test ./internal/adapters/krun/
vmpal cp krun.test "Ubuntu VM 2:rc-sandbox/krun.test"
# in the VM:
./krun.test -test.v -test.count=1 -test.coverprofile=cover.out
```

The confinement test relaunches its own binary as a **probe**. The probe applies the real policy (`ApplySandbox`, the
same function the `vmm` process calls) and tries each operation. The test process itself is never confined.

### Confinement result (confined probe)

| Operation | Expected | Measured |
|---|---|---|
| read the root disk | allowed | allowed |
| create the VM's own socket (`bind`) | allowed | allowed |
| read a file outside the policy | denied | **denied** (Landlock) |
| write next to the root disk | denied | **denied** (Landlock) |
| TCP `connect` | denied | **denied** (Landlock, network) |
| `execve("/bin/true")` | denied | **denied** (seccomp, `EPERM`) |
| `personality(0xffffffff)` | denied | **denied** (seccomp, `EPERM`) |
| `connect` to another VM's Unix socket | denied from ABI 9 onward | **allowed** (kernel with ABI 8) |

**Proof of failure** (`TestProbeWithoutSandbox`): the same probe without confinement reads the forbidden file, writes
next to the disk, connects to another VM's socket, opens TCP and calls `personality`. In other words, the test tells
them apart.

`ptrace` is not used as seccomp proof: on Ubuntu, Yama already denies `ptrace` to the parent without any confinement.

### Suite result on Linux

30 tests, green in two consecutive runs (`TestLoadRealLibrary` skipped: no libkrun in the VM). Package coverage:
81.9%. Code executed inside the confined subprocess is not included in the measurement.

### `rc doctor` in the VM

```
FAIL  platform     no access to /dev/kvm: open /dev/kvm: no such file or directory
ok    sandbox landlock ABI 8 + seccomp denylist
warn  landlock ABI 8 < 9: a VM process can still connect to another VM's unix socket
```

### Not tested on Linux

- a full microVM (boot, job, timeout) under Landlock + seccomp: it requires KVM;
- the list of paths libkrun opens in `krun_start_enter` on Linux. The policy allows the loaded libraries, `/proc`,
  `/sys`, `/dev/kvm` and the common device nodes, and preloads libkrunfw. If libkrun needs another path, the VM does
  not boot; `RUNNER_SANDBOX=off` disables confinement for diagnostics;
- `linux/amd64` (compiled only).

## Linux with KVM and Kubernetes {#linux-com-kvm-e-kubernetes}

### Environment

| Item | Value |
|---|---|
| machine | DigitalOcean droplet `s-4vcpu-8gb` (4 vCPU, 8 GB), nyc3; `scripts/do-kvm-probe.sh` measured `/dev/kvm` and the `vmx` flags before creating the test node |
| system | Ubuntu 24.04, kernel 6.8.0-142, x86_64; Landlock **ABI 4** |
| libkrun | 1.19.5 and libkrunfw 5.5.0 from nixpkgs (`task krun:install`) |
| Kubernetes | k3s v1.36.5 (containerd 2.3.4, flannel) and microk8s v1.35.6 (containerd 2.1.6, Calico), one droplet each |

### Result

| What | Measured |
|---|---|
| `rc run --stack python`, cold boot | `status=success`, ~1.4 s end to end (nested KVM; ~240 ms on the Mac) |
| `rc image build --stack python` | 60 s (`mkfs` took 45 s) |
| `runtimeClassName: rc` pod to `Running` | ~2 s, image already on the node |
| `deploy/k8s/e2e.sh` | **19/19** on k3s and microk8s, two consecutive runs on each |
| e2e failure proof | exit code fixed at 0 in the shim: the 2 exit-code checks failed |
| `task k8s:defects` | 20 defects planted; 19 caught on the 1st run, the one that slipped through became a test, 20 of 20 after |
| `TestRestrictThreadBlocks` (root, real kernel) | per-thread Landlock blocks the outside file; without `landlock_restrict_self` the test fails |
| `TestAttach` (root, real netns) | tap0 is created in the pod netns, not the host one, with one mirred filter each way |

### Defects found only on real Linux

| Symptom | Cause | Fix |
|---|---|---|
| `nix profile add .#krun` fails with `File exists` | in the nixpkgs libkrun, `lib/` and `lib64/` carry the same files | `ln -sf` in `flake.nix` |
| every job fails: `prctl(PR_SET_NO_NEW_PRIVS): operation not supported` | below ABI 8 go-landlock uses psx, and without cgo `AllThreadsSyscall` returns ENOTSUP (purego makes the runtime see itself "with cgo") | ruleset on the `krun_start_enter` thread (`sandbox_rules.go`); libkrun threads inherit it |
| launcher test did not compile on Linux | `syscall.Getsid` only exists on BSD/macOS | `unix.Getsid` |
| rootfs copy "fails" with `waitid: no child processes` | pkg/shim reaps every child on SIGCHLD | shim children wait through `reaper.Default` |
| kubelet summary breaks for the whole node; namespace stuck in Terminating | empty `Stats` from the shim (`failed to decode container metrics`) | cgroup v2 metrics with the real vmm usage |
| microk8s: `unsupported protocol: Yunix` | containerd 2.1 read the protobuf start reply as an address | legacy JSON for callers of the old protocol |
| microk8s: pod without network (`Network is unreachable`) | Calico gives a `/32` with gateway `169.254.1.1` outside the subnet | link route to the gateway before the default one |

### Not tested on Linux with KVM

- `linux/arm64` with KVM (x86_64 only);
- Landlock with ABI ≥ 8 (TSYNC path) with a real microVM: the droplet has ABI 4;
- `--nested` turning on (the droplet's `rc doctor` says `supported`, without proof);
- the pod VM's vmm runs **without** Landlock/seccomp (declared debt).

## Windows (out of scope, measured)

### Environment

| Item | Value |
|---|---|
| VM | "Windows VM 1" on VMPal |
| system | Windows 11 Pro 24H2, build 26100.9457, ARM64, CPU "Apple Silicon" |

### Building `rc`

```
GOOS=windows GOARCH=arm64|amd64 go build ./cmd/rc
undefined: runnerCodesDoesNotSupportWindows_libkrunUpstreamHasNoWHPBackend
```

The rejection is deliberate: upstream libkrun only has KVM and Hypervisor.framework backends, with no Windows
Hypervisor Platform.

### Hypervisor inside the VM

| Step | Measured |
|---|---|
| initial state | `HypervisorPresent=False`; `VirtualizationFirmwareEnabled`, `VMMonitorModeExtensions` and `SLAT` all `False`; WSL not installed |
| `vmpal set --nested on` | Windows: `nested virtualization is for Linux VMs`; Ubuntu: `Nested virtualization needs a Mac with M3 or later` |
| VMPal restore point `antes-hypervisor-2026-10-19` | created |
| `Enable-WindowsOptionalFeature` HypervisorPlatform, VirtualMachinePlatform, Microsoft-Hyper-V-All | enabled, `RestartNeeded=True` |
| after reboot | `bcdedit hypervisorlaunchtype Auto`; `vmms` and `vmcompute` running; **`HypervisorPresent=False`** |
| system log | `Microsoft-Windows-Hyper-V-Hypervisor id=43: Hypervisor launch failed; EL2 not present.` |
| reverted to the restore point | the three features `Disabled`; `vmms` and `vmcompute` absent |

Conclusion: without EL2 the VM cannot run any hypervisor, and no Windows setting works around that. Even with `rc`
support, this VM would have nowhere to create microVMs.

## What is missing to close the matrix

| Gap | What unblocks it |
|---|---|
| microVM under Landlock with TSYNC (ABI ≥ 8) | Linux with KVM and Landlock ABI ≥ 8 (Ubuntu 26.04, kernel 7.0, has 8) (the droplet's ABI 4 was measured, through the per-thread path) |
| `linux/arm64` running with KVM | arm64 Linux with KVM (`linux/amd64` was measured on the droplet) |
| Unix socket isolation between VMs | kernel with Landlock ABI ≥ 9 |
| `--nested` actually turning on | M3+ Mac or Linux with nestable KVM |
| native Windows | replace upstream libkrun with the fork that has WHP (an architecture decision) and a Windows machine with virtualization |
