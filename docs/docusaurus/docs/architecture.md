---
title: 'Architecture'
description: 'Deep dive into Runner Codes system architecture'
---

## System Overview

Runner Codes uses a layered architecture designed for security, performance, and scalability.

![System Architecture](/img/system-architecture.svg)

## Component Details

### Host Mode (infra.operator host)

The host mode is the main orchestrator running on the EC2 host. It manages the lifecycle of Firecracker microVMs and handles job execution.

```go title="pkg/host/runner.go"
// Runner handles VM lifecycle and code execution
type Runner struct {
    config *RunnerConfig
    fc     *FirecrackerClient
    cache  *SnapshotCache
}

// Run executes code in a microVM
func (r *Runner) Run() (*RunResult, error) {
    // 1. Ensure snapshot is cached (download from S3 if needed)
    // 2. Start Firecracker with snapshot restore
    // 3. Connect via vsock
    // 4. Send job, receive result
    // 5. Shutdown VM
}
```

```bash title="Configure VM machine config"
PUT /machine-config     {"vcpu_count":1,"mem_size_mib":512,"smt":false}
```

```bash title="Configure boot source"
PUT /boot-source        {"kernel_image_path":"...","boot_args":"..."}
```

```bash title="Configure rootfs drive"
PUT /drives/rootfs      {"drive_id":"rootfs","path_on_host":"..."}
```

```bash title="Configure vsock"
PUT /vsock              {"guest_cid":3,"uds_path":"/tmp/fc.vsock"}
```

```bash title="Start VM instance"
PUT /actions            {"action_type":"InstanceStart"}
```

```bash title="Send shutdown signal"
PUT /actions            {"action_type":"SendCtrlAltDel"}
```

### Guest Mode (infra.operator guest)

The guest mode executes inside the microVM and handles code execution requests.

```go title="pkg/guest/executor.go"
type Executor struct {
    languages map[string]LanguageConfig
}

type LanguageConfig struct {
    Name       string   // "python", "node", etc.
    Extension  string   // ".py", ".js", etc.
    Command    string   // "python3", "node", etc.
    Args       []string // Additional arguments
    NeedsBuild bool     // For compiled languages
    BuildCmd   string   // "rustc", "go", etc.
    BuildArgs  []string // Build arguments
}

func (e *Executor) Execute(job Job) Result {
    // 1. Create temp directory
    // 2. Write code to file
    // 3. Execute (or compile then execute)
    // 4. Capture stdout/stderr
    // 5. Return result
}
```

### Vsock Communication

The host and guest communicate via virtio-vsock, a virtual socket for VM-to-host communication.

```text title="1. Host connects to vsock"
connect(/tmp/fc-{instance_id}.vsock)
```

```text title="2. Host sends connect request"
CONNECT 5000\n
```

```text title="3. Guest responds"
OK 5000\n
```

```text title="4. Message format"
Messages use 4-byte big-endian length prefix + JSON payload
```

## Data Flow

### Job Execution Flow

![Data Flow](/img/data-flow.svg)

### Boot Timeline

| Phase | Duration | Description |
|-------|----------|-------------|
| Firecracker API calls | ~100ms | Configure VM via API socket |
| Kernel boot | ~900ms | Linux kernel initialization |
| Systemd init | ~1.5s | Service startup |
| Infra.operator guest ready | ~500ms | Vsock server listening |
| **Total cold boot** | **~3s** | Full boot time |

With snapshot restore:

| Phase | Duration | Description |
|-------|----------|-------------|
| Load snapshot | ~50ms | Restore VM state |
| Resume VM | ~20ms | Continue execution |
| **Total warm start** | **~70ms** | Snapshot restore time |

## Security Model

### Isolation Layers

![Security Isolation Layers](/img/isolation-layers.svg)

### Security Features (Production)

:::warning

The following features are recommended for production but not yet implemented:

:::

| Feature | Status | Description |
|---------|--------|-------------|
| Jailer | Planned | chroot + dropped capabilities |
| Seccomp | Planned | Per-language syscall filtering |
| Read-only rootfs | Planned | Overlay filesystem for writes |
| cgroups | Planned | Resource enforcement |
| eBPF | Planned | Network blocking |

## Storage Architecture

### S3-Backed Rootfs

Rootfs images are stored in S3 and downloaded on demand:

![S3 Storage Architecture](/img/s3-storage-architecture.svg)

### Rootfs Image Structure

Each rootfs is an ext4 filesystem image containing:

```text title="rootfs-python.ext4"
rootfs-python.ext4
├── bin/                    # Core binaries
├── etc/
│   └── systemd/system/
│       └── infra.operator.service
├── lib/                    # Shared libraries
├── usr/
│   ├── bin/
│   │   └── python3
│   └── local/
│       └── bin/
│           └── infra.operator
└── var/                    # Variable data
```

## Environment Variables

The executor sets these environment variables for code execution:

```go title="Environment configuration"
cmd.Env = append(os.Environ(),
    "HOME=/tmp",
    "GOCACHE=/tmp/go-cache",
    "GOPATH=/tmp/go",
    "GOROOT=/usr/local/go",
    "CARGO_HOME=/opt/cargo",
    "RUSTUP_HOME=/opt/rustup",
    "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/local/go/bin:/opt/cargo/bin",
)
```

## Next Steps

- [CLI Reference](/cli/overview) - Command line interface
- [API Reference](/api-reference/overview) - HTTP API documentation
- [Security Guide](/advanced/security) - Security best practices
