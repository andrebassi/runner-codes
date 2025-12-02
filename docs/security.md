# Security Model

## Table of Contents

1. [Threat Model](#threat-model)
2. [Isolation Layers](#isolation-layers)
3. [Attack Surface Analysis](#attack-surface-analysis)
4. [Seccomp Profiles](#seccomp-profiles)
5. [Jailer Configuration](#jailer-configuration)
6. [Network Isolation](#network-isolation)
7. [Resource Limits](#resource-limits)
8. [Secrets Management](#secrets-management)
9. [Hardening Checklist](#hardening-checklist)

---

## Threat Model

### Adversary Profile

```
┌─────────────────────────────────────────────────────────────┐
│                    THREAT MODEL                              │
│                                                              │
│  Adversary: Malicious LLM-generated code                    │
│                                                              │
│  Assumptions:                                                │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  - Code is UNTRUSTED and potentially malicious      │    │
│  │  - Adversary has arbitrary code execution in guest  │    │
│  │  - Adversary knows the sandbox architecture         │    │
│  │  - Adversary may attempt multi-stage attacks        │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  Goals:                                                      │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  - Prevent escape from guest to host                │    │
│  │  - Prevent access to other guests' data             │    │
│  │  - Prevent persistent compromise                    │    │
│  │  - Prevent resource exhaustion (DoS)                │    │
│  │  - Prevent data exfiltration                        │    │
│  └─────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────┘
```

### Attack Scenarios

```
┌─────────────────────────────────────────────────────────────┐
│                    ATTACK SCENARIOS                          │
│                                                              │
│  1. GUEST-TO-HOST ESCAPE                                     │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  Attack Vector:                                      │    │
│  │  - Exploit bug in Firecracker VMM                   │    │
│  │  - Exploit bug in virtio device emulation           │    │
│  │  - Exploit KVM vulnerability                        │    │
│  │                                                      │    │
│  │  Mitigations:                                        │    │
│  │  - Firecracker written in Rust (memory safe)        │    │
│  │  - Minimal device model (small attack surface)      │    │
│  │  - Seccomp filtering in VMM                         │    │
│  │  - Jailer drops privileges                          │    │
│  │  - Regular security updates                         │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  2. LATERAL MOVEMENT (Guest-to-Guest)                        │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  Attack Vector:                                      │    │
│  │  - Side-channel attacks (Spectre, Meltdown)         │    │
│  │  - Shared resource timing attacks                   │    │
│  │  - Covert channels via cache/memory                 │    │
│  │                                                      │    │
│  │  Mitigations:                                        │    │
│  │  - Each job in separate VM (full isolation)         │    │
│  │  - VMs destroyed after execution                    │    │
│  │  - No shared state between executions               │    │
│  │  - CPU microcode patches                            │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  3. DATA EXFILTRATION                                        │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  Attack Vector:                                      │    │
│  │  - Network access to external servers               │    │
│  │  - DNS tunneling                                    │    │
│  │  - Covert timing channels                           │    │
│  │  - Large stdout/stderr output                       │    │
│  │                                                      │    │
│  │  Mitigations:                                        │    │
│  │  - No network interface by default                  │    │
│  │  - Output size limits                               │    │
│  │  - Execution time limits                            │    │
│  │  - No persistent storage                            │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  4. DENIAL OF SERVICE                                        │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  Attack Vector:                                      │    │
│  │  - CPU exhaustion (infinite loops)                  │    │
│  │  - Memory exhaustion (allocation bombs)             │    │
│  │  - Disk exhaustion (write loops)                    │    │
│  │  - Fork bombs                                       │    │
│  │                                                      │    │
│  │  Mitigations:                                        │    │
│  │  - Hard execution timeout                           │    │
│  │  - Memory limit (512 MiB)                           │    │
│  │  - Disk is tmpfs (limited size)                     │    │
│  │  - No fork capability in guest                      │    │
│  │  - Rate limiting at control plane                   │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  5. CRYPTOMINING / RESOURCE ABUSE                            │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  Attack Vector:                                      │    │
│  │  - Use allocated CPU for mining                     │    │
│  │  - Long-running background processes                │    │
│  │                                                      │    │
│  │  Mitigations:                                        │    │
│  │  - Short execution timeout (10s default)            │    │
│  │  - VM destroyed after each job                      │    │
│  │  - No network (can't submit mining results)         │    │
│  │  - CPU accounting/billing                           │    │
│  └─────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────┘
```

---

## Isolation Layers

### Defense in Depth

```
┌─────────────────────────────────────────────────────────────┐
│                    ISOLATION LAYERS                          │
│                                                              │
│  Layer 5: APPLICATION SANDBOX                                │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  - Temp directory per execution                     │    │
│  │  - Limited file system access                       │    │
│  │  - No access to guest-runner internals              │    │
│  │  - Process killed after timeout                     │    │
│  └─────────────────────────────────────────────────────┘    │
│                         │                                    │
│                         ▼                                    │
│  Layer 4: GUEST USERSPACE                                    │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  - Minimal rootfs (no extra tools)                  │    │
│  │  - No setuid binaries                               │    │
│  │  - No sudo/su                                       │    │
│  │  - Read-only rootfs (optional)                      │    │
│  └─────────────────────────────────────────────────────┘    │
│                         │                                    │
│                         ▼                                    │
│  Layer 3: GUEST KERNEL                                       │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  - Minimal kernel config                            │    │
│  │  - No kernel modules loading                        │    │
│  │  - No network stack (optional)                      │    │
│  │  - Hardened kernel options                          │    │
│  └─────────────────────────────────────────────────────┘    │
│                         │                                    │
│                         ▼                                    │
│  Layer 2: FIRECRACKER VMM                                    │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  - Rust (memory safety)                             │    │
│  │  - Minimal device model                             │    │
│  │  - Seccomp-bpf filtering                            │    │
│  │  - No legacy devices                                │    │
│  └─────────────────────────────────────────────────────┘    │
│                         │                                    │
│                         ▼                                    │
│  Layer 1: JAILER                                             │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  - chroot isolation                                 │    │
│  │  - User namespace (unprivileged)                    │    │
│  │  - Dropped capabilities                             │    │
│  │  - Cgroup resource limits                           │    │
│  └─────────────────────────────────────────────────────┘    │
│                         │                                    │
│                         ▼                                    │
│  Layer 0: KVM / HARDWARE                                     │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  - Hardware virtualization (VT-x/AMD-V)             │    │
│  │  - EPT/NPT memory isolation                         │    │
│  │  - Separate address spaces                          │    │
│  │  - Privileged instruction trapping                  │    │
│  └─────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────┘
```

---

## Attack Surface Analysis

### Firecracker Attack Surface

```
┌─────────────────────────────────────────────────────────────┐
│                    ATTACK SURFACE                            │
│                                                              │
│  GUEST → VMM INTERFACES                                      │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  Interface          │ Attack Surface │ Risk Level   │    │
│  ├─────────────────────┼────────────────┼──────────────┤    │
│  │  virtio-blk         │ Low            │ Medium       │    │
│  │  virtio-net         │ Medium         │ High         │    │
│  │  virtio-vsock       │ Low            │ Low          │    │
│  │  Serial console     │ Very Low       │ Low          │    │
│  │  RTC                │ Very Low       │ Very Low     │    │
│  │  KVM ioctls         │ High           │ Critical     │    │
│  └─────────────────────┴────────────────┴──────────────┘    │
│                                                              │
│  Recommendation:                                             │
│  - Disable virtio-net if not needed (we do this)           │
│  - Minimize virtio-blk operations                           │
│  - Use read-only rootfs where possible                      │
│                                                              │
│  HOST → VMM INTERFACES                                       │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  Interface          │ Attack Surface │ Risk Level   │    │
│  ├─────────────────────┼────────────────┼──────────────┤    │
│  │  API socket         │ Low            │ Medium       │    │
│  │  vsock UDS          │ Low            │ Low          │    │
│  │  File paths         │ Medium         │ Medium       │    │
│  └─────────────────────┴────────────────┴──────────────┘    │
│                                                              │
│  Recommendation:                                             │
│  - API socket only accessible by host-agent                 │
│  - Validate all file paths                                  │
│  - Use jailer chroot                                        │
└─────────────────────────────────────────────────────────────┘
```

### Comparison: Firecracker vs Containers

```
┌─────────────────────────────────────────────────────────────┐
│                FIRECRACKER vs CONTAINERS                     │
│                                                              │
│  ┌──────────────────────┬─────────────┬─────────────────┐   │
│  │  Aspect              │ Firecracker │ Containers      │   │
│  ├──────────────────────┼─────────────┼─────────────────┤   │
│  │  Kernel              │ Separate    │ Shared          │   │
│  │  Syscall exposure    │ ~50 VMM     │ ~300+ host      │   │
│  │  Device drivers      │ VMM only    │ All host        │   │
│  │  Memory isolation    │ Hardware    │ Software        │   │
│  │  Escape complexity   │ Very High   │ Medium          │   │
│  │  CVE impact scope    │ Guest only  │ All containers  │   │
│  │  Startup time        │ ~150ms      │ ~50ms           │   │
│  │  Memory overhead     │ ~5 MiB VMM  │ ~0 (shared)     │   │
│  └──────────────────────┴─────────────┴─────────────────┘   │
│                                                              │
│  Verdict: Firecracker provides stronger isolation at        │
│  the cost of slightly higher resource usage and latency.    │
│  For untrusted code execution, this tradeoff is worthwhile. │
└─────────────────────────────────────────────────────────────┘
```

---

## Seccomp Profiles

### Firecracker Seccomp (Default)

```
┌─────────────────────────────────────────────────────────────┐
│                    FIRECRACKER SECCOMP                       │
│                                                              │
│  Allowed Syscalls (~50):                                     │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  File I/O:                                           │    │
│  │    read, write, close, fstat, lseek, pread64,       │    │
│  │    pwrite64, readv, writev, fsync, fdatasync        │    │
│  │                                                      │    │
│  │  Memory:                                             │    │
│  │    mmap, mprotect, munmap, brk, mremap              │    │
│  │                                                      │    │
│  │  Process:                                            │    │
│  │    exit, exit_group, rt_sigaction, rt_sigprocmask,  │    │
│  │    rt_sigreturn, getpid, gettid                     │    │
│  │                                                      │    │
│  │  Synchronization:                                    │    │
│  │    futex, nanosleep, clock_gettime, clock_nanosleep │    │
│  │                                                      │    │
│  │  Event/Timer:                                        │    │
│  │    epoll_create1, epoll_ctl, epoll_wait, epoll_pwait│    │
│  │    eventfd2, timerfd_create, timerfd_settime        │    │
│  │                                                      │    │
│  │  Socket (for API):                                   │    │
│  │    socket, bind, listen, accept4, recvfrom, sendto  │    │
│  │                                                      │    │
│  │  KVM:                                                │    │
│  │    ioctl (filtered to KVM_* only)                   │    │
│  │                                                      │    │
│  │  Misc:                                               │    │
│  │    fcntl, dup, pipe2, getrandom                     │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  Blocked (High Risk):                                        │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  Process creation: fork, clone, execve, vfork       │    │
│  │  Privilege: setuid, setgid, capset, personality     │    │
│  │  Mount: mount, umount2, pivot_root, chroot          │    │
│  │  Modules: init_module, delete_module, finit_module  │    │
│  │  Debug: ptrace, process_vm_readv, process_vm_writev │    │
│  │  Network config: socket(AF_NETLINK), sethostname    │    │
│  │  Namespaces: unshare, setns                         │    │
│  │  Keyring: keyctl, add_key, request_key              │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  Action on violation: SCMP_ACT_TRAP (SIGSYS → kill)         │
└─────────────────────────────────────────────────────────────┘
```

### Custom Seccomp for Guest-Runner

```c
// Additional seccomp for guest-runner (inside VM)
// Applied after fork, before exec of interpreter

#include <seccomp.h>

int apply_seccomp() {
    scmp_filter_ctx ctx = seccomp_init(SCMP_ACT_KILL);

    // Allow basic I/O
    seccomp_rule_add(ctx, SCMP_ACT_ALLOW, SCMP_SYS(read), 0);
    seccomp_rule_add(ctx, SCMP_ACT_ALLOW, SCMP_SYS(write), 0);
    seccomp_rule_add(ctx, SCMP_ACT_ALLOW, SCMP_SYS(close), 0);
    seccomp_rule_add(ctx, SCMP_ACT_ALLOW, SCMP_SYS(fstat), 0);
    seccomp_rule_add(ctx, SCMP_ACT_ALLOW, SCMP_SYS(lseek), 0);

    // Allow memory management
    seccomp_rule_add(ctx, SCMP_ACT_ALLOW, SCMP_SYS(mmap), 0);
    seccomp_rule_add(ctx, SCMP_ACT_ALLOW, SCMP_SYS(mprotect), 0);
    seccomp_rule_add(ctx, SCMP_ACT_ALLOW, SCMP_SYS(munmap), 0);
    seccomp_rule_add(ctx, SCMP_ACT_ALLOW, SCMP_SYS(brk), 0);

    // Allow process exit
    seccomp_rule_add(ctx, SCMP_ACT_ALLOW, SCMP_SYS(exit), 0);
    seccomp_rule_add(ctx, SCMP_ACT_ALLOW, SCMP_SYS(exit_group), 0);

    // Block dangerous syscalls explicitly
    // (Already blocked by default action, but explicit is clearer)

    // NO: fork, clone, execve (except initial exec)
    // NO: socket (no network)
    // NO: ptrace
    // NO: mount, chroot
    // NO: setuid, setgid

    return seccomp_load(ctx);
}
```

---

## Jailer Configuration

### Production Jailer Setup

```bash
#!/bin/bash
# jailer-setup.sh

# Variables
JAIL_DIR="/srv/jailer"
EXEC_FILE="/usr/local/bin/firecracker"
INSTANCE_ID="vm-$(uuidgen)"
UID=65534  # nobody
GID=65534  # nogroup

# Create jailer directory structure
mkdir -p "${JAIL_DIR}/${INSTANCE_ID}/root"

# Run with jailer
/usr/local/bin/jailer \
    --id "${INSTANCE_ID}" \
    --exec-file "${EXEC_FILE}" \
    --uid "${UID}" \
    --gid "${GID}" \
    --chroot-base-dir "${JAIL_DIR}" \
    --netns /var/run/netns/fc-net-${INSTANCE_ID} \
    --daemonize \
    --new-pid-ns \
    --cgroup cpuset.cpus=0 \
    --cgroup cpuset.mems=0 \
    --cgroup cpu.max="100000 100000" \
    --cgroup memory.max=536870912 \
    --cgroup pids.max=100 \
    -- \
    --config-file /config.json

# Jail directory structure after jailer runs:
#
# /srv/jailer/vm-xxx/root/
# ├── dev/
# │   ├── kvm          (bind mount from /dev/kvm)
# │   ├── urandom      (bind mount from /dev/urandom)
# │   └── null         (bind mount from /dev/null)
# ├── firecracker      (hardlink to /usr/local/bin/firecracker)
# ├── kernel.bin       (hardlink)
# ├── rootfs.ext4      (hardlink, cow if needed)
# ├── config.json      (copied)
# └── run/
#     └── firecracker.socket
```

### Jailer Security Features

```
┌─────────────────────────────────────────────────────────────┐
│                    JAILER SECURITY                           │
│                                                              │
│  1. CHROOT ISOLATION                                         │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  - Firecracker sees only its jail directory        │    │
│  │  - Cannot access /etc, /home, or other host paths   │    │
│  │  - Minimal /dev with only required devices          │    │
│  │  - No /proc, /sys access                            │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  2. USER/GROUP MAPPING                                       │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  --uid 65534  (nobody)                              │    │
│  │  --gid 65534  (nogroup)                             │    │
│  │                                                      │    │
│  │  Even if Firecracker is compromised, attacker has   │    │
│  │  no privileges on the host system.                  │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  3. CGROUP LIMITS                                            │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  cpu.max = "100000 100000"                          │    │
│  │    → Max 100% of 1 CPU (100ms per 100ms period)    │    │
│  │                                                      │    │
│  │  memory.max = 536870912 (512 MiB)                   │    │
│  │    → Hard memory limit, OOM kill if exceeded        │    │
│  │                                                      │    │
│  │  pids.max = 100                                      │    │
│  │    → Max 100 processes (prevent fork bombs)         │    │
│  │                                                      │    │
│  │  cpuset.cpus = "0"                                   │    │
│  │    → Pin to specific CPU (cache isolation)          │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  4. NETWORK NAMESPACE                                        │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  --netns /var/run/netns/fc-net-xxx                  │    │
│  │                                                      │    │
│  │  Isolate network stack. For no-network mode:        │    │
│  │    ip netns add fc-net-xxx                          │    │
│  │    (empty namespace with no interfaces)             │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  5. PID NAMESPACE                                            │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  --new-pid-ns                                        │    │
│  │                                                      │    │
│  │  Firecracker is PID 1 in its namespace.             │    │
│  │  Cannot see or signal other host processes.         │    │
│  └─────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────┘
```

---

## Network Isolation

### No-Network Configuration (Default)

```
┌─────────────────────────────────────────────────────────────┐
│                    NETWORK ISOLATION                         │
│                                                              │
│  Default: NO NETWORK                                         │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  - No virtio-net device attached                    │    │
│  │  - Guest has no network interfaces (except lo)      │    │
│  │  - No IP address, no routing                        │    │
│  │  - DNS resolution fails                             │    │
│  │  - Cannot connect to external servers               │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  Guest /etc/hosts (minimal):                                │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  127.0.0.1   localhost                              │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  Why no network?                                             │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  - Prevents C2 (command & control) communication   │    │
│  │  - Prevents data exfiltration                       │    │
│  │  - Prevents cryptocurrency mining submission        │    │
│  │  - Prevents attack amplification (DDoS)             │    │
│  │  - Prevents lateral movement                        │    │
│  │  - Reduces attack surface (no virtio-net)          │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  Communication Channel:                                      │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  Only vsock (host-controlled, no external access)   │    │
│  │                                                      │    │
│  │  Host ◄──── vsock ────► Guest                       │    │
│  │   ▲                                                  │    │
│  │   │                                                  │    │
│  │   └── Control Plane (API, rate limiting, auth)      │    │
│  └─────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────┘
```

### Optional: Limited Network (if needed)

```bash
# Only if network access is required (e.g., package downloads)
# Use host-side firewall to restrict

# Create network namespace with outbound-only rules
ip netns add fc-net-${ID}

# Add veth pair
ip link add veth-host type veth peer name veth-guest
ip link set veth-guest netns fc-net-${ID}

# Configure IP addresses
ip addr add 172.16.0.1/30 dev veth-host
ip netns exec fc-net-${ID} ip addr add 172.16.0.2/30 dev veth-guest

# Enable NAT for outbound
iptables -t nat -A POSTROUTING -s 172.16.0.0/30 -j MASQUERADE

# BLOCK ALL INBOUND
iptables -A FORWARD -d 172.16.0.0/30 -m state --state NEW -j DROP

# Allow only specific outbound (e.g., PyPI)
iptables -A FORWARD -s 172.16.0.0/30 -d pypi.org -p tcp --dport 443 -j ACCEPT
iptables -A FORWARD -s 172.16.0.0/30 -j DROP  # Block everything else
```

---

## Resource Limits

### Limit Configuration

```
┌─────────────────────────────────────────────────────────────┐
│                    RESOURCE LIMITS                           │
│                                                              │
│  VM Configuration:                                           │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  vcpu_count: 1              # Single vCPU           │    │
│  │  mem_size_mib: 512          # 512 MiB RAM           │    │
│  │  ht_enabled: false          # No hyperthreading     │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  Execution Limits:                                           │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  Timeout: 10 seconds (configurable)                 │    │
│  │  Max stdout: 1 MiB                                  │    │
│  │  Max stderr: 1 MiB                                  │    │
│  │  Max code size: 64 KiB                              │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  Cgroup Limits (via Jailer):                                │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  cpu.max: "100000 100000"   # 100% of 1 CPU        │    │
│  │  memory.max: 536870912      # 512 MiB              │    │
│  │  memory.swap.max: 0         # No swap              │    │
│  │  pids.max: 100              # Max processes        │    │
│  │  io.max: "8:0 riops=1000"   # I/O rate limit      │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  Rate Limiters (Firecracker):                               │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  Block device:                                      │    │
│  │    bandwidth: 100 MiB/s                            │    │
│  │    ops: 1000 IOPS                                  │    │
│  │                                                      │    │
│  │  Network (if enabled):                              │    │
│  │    bandwidth: 10 Mbit/s                            │    │
│  │    ops: 1000 packets/s                             │    │
│  └─────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────┘
```

### Enforcement Mechanism

```go
// Timeout enforcement in guest-runner
func (e *Executor) Execute(job Job) Result {
    timeout := time.Duration(job.Timeout) * time.Second
    if timeout <= 0 || timeout > 60*time.Second {
        timeout = 10 * time.Second  // Default/max
    }

    ctx, cancel := context.WithTimeout(context.Background(), timeout)
    defer cancel()

    cmd := exec.CommandContext(ctx, interpreter, scriptPath)

    // Set process group for reliable killing
    cmd.SysProcAttr = &syscall.SysProcAttr{
        Setpgid: true,
    }

    err := cmd.Run()
    if ctx.Err() == context.DeadlineExceeded {
        // Kill process group
        syscall.Kill(-cmd.Process.Pid, syscall.SIGKILL)
        return Result{ExitCode: 124, Stderr: "Execution timed out"}
    }

    // ...
}
```

---

## Secrets Management

### Ephemeral Secrets Pattern

```
┌─────────────────────────────────────────────────────────────┐
│                    SECRETS MANAGEMENT                        │
│                                                              │
│  Principle: NO SECRETS IN GUEST                              │
│                                                              │
│  Default State:                                              │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  - Guest has no access to host secrets              │    │
│  │  - No environment variables with secrets            │    │
│  │  - No mounted secret volumes                        │    │
│  │  - Code executes without credentials                │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  If Secrets Needed (Future):                                │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  1. Host-agent requests ephemeral credentials       │    │
│  │     from Vault/AWS STS/GCP IAM                      │    │
│  │                                                      │    │
│  │  2. Credentials have:                               │    │
│  │     - Short TTL (< execution timeout)               │    │
│  │     - Minimal permissions                           │    │
│  │     - Single-use if possible                        │    │
│  │                                                      │    │
│  │  3. Credentials injected via vsock (not env/file)   │    │
│  │                                                      │    │
│  │  4. Credentials revoked after execution             │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  Secret Flow:                                                │
│                                                              │
│  ┌──────────┐    ┌───────────┐    ┌───────────────────┐    │
│  │  Vault   │───►│ Host-Agent│───►│  Guest (vsock)    │    │
│  │          │    │           │    │                   │    │
│  │ TTL=30s  │    │ Encrypt   │    │ Use immediately   │    │
│  │ scope=X  │    │ in memory │    │ Clear from memory │    │
│  └──────────┘    └───────────┘    └───────────────────┘    │
│                         │                                    │
│                         └──► Revoke after execution         │
└─────────────────────────────────────────────────────────────┘
```

---

## Hardening Checklist

### Production Deployment Checklist

```
┌─────────────────────────────────────────────────────────────┐
│                    HARDENING CHECKLIST                       │
│                                                              │
│  HOST SYSTEM                                                 │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  [ ] Kernel up-to-date with security patches        │    │
│  │  [ ] Spectre/Meltdown mitigations enabled           │    │
│  │  [ ] SELinux/AppArmor configured                    │    │
│  │  [ ] Firewall configured (no unnecessary ports)     │    │
│  │  [ ] SSH hardened (keys only, no root)              │    │
│  │  [ ] Audit logging enabled                          │    │
│  │  [ ] /dev/kvm permissions restricted                │    │
│  │  [ ] Host-agent runs as non-root (if possible)      │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  FIRECRACKER                                                 │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  [ ] Latest Firecracker version                     │    │
│  │  [ ] Jailer enabled in production                   │    │
│  │  [ ] Seccomp filtering enabled                      │    │
│  │  [ ] API socket permissions restricted              │    │
│  │  [ ] No virtio-net unless required                  │    │
│  │  [ ] Rate limiters configured                       │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  GUEST                                                       │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  [ ] Minimal kernel config                          │    │
│  │  [ ] Minimal rootfs (no extra tools)                │    │
│  │  [ ] No setuid binaries                             │    │
│  │  [ ] Read-only rootfs (if possible)                 │    │
│  │  [ ] No persistent storage                          │    │
│  │  [ ] Guest-runner drops privileges                  │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  OPERATIONAL                                                 │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  [ ] VM destroyed after each execution              │    │
│  │  [ ] Execution timeout enforced                     │    │
│  │  [ ] Output size limits enforced                    │    │
│  │  [ ] Rate limiting at control plane                 │    │
│  │  [ ] Monitoring and alerting configured             │    │
│  │  [ ] Incident response plan documented              │    │
│  │  [ ] Regular security audits scheduled              │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  LOGGING & MONITORING                                        │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  [ ] All executions logged with trace_id            │    │
│  │  [ ] Failed executions alerted                      │    │
│  │  [ ] Resource usage monitored                       │    │
│  │  [ ] Anomaly detection enabled                      │    │
│  │  [ ] Logs shipped to secure storage                 │    │
│  │  [ ] Log retention policy defined                   │    │
│  └─────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────┘
```
