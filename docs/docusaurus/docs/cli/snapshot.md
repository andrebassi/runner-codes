---
title: 'Snapshot Commands'
description: 'Create, list, upload and download VM snapshots for fast microVM boot'
---

## Overview

Snapshots capture the complete state of a running Firecracker microVM, including memory and CPU registers. Loading a snapshot is significantly faster than cold booting (~100ms vs ~3s), making them essential for production workloads.

## How Snapshots Work

![Snapshot Creation and Restore Flow](/img/snapshot-sequence.svg)

## Commands

### snapshot create

Create a new snapshot from a rootfs image. **Firecracker and kernel are automatically downloaded if missing.**

```bash title="Create snapshot command"
infra.operator snapshot create --lang <language> [flags]
```

**Flags:**

| Flag | Short | Description | Default |
|------|-------|-------------|---------|
| `--lang` | `-l` | Language runtime (required) | - |
| `--rootfs` | `-r` | Rootfs path | Auto-detect |
| `--mem` | `-m` | Memory in MB | `512` |
| `--vcpus` | `-c` | Number of vCPUs | `1` |
| `--kernel` | `-k` | Path to kernel (auto-downloads if missing) | `/srv/firecracker/vmlinux` |
| `--firecracker` | `-f` | Path to Firecracker binary (auto-downloads if missing) | `/usr/local/bin/firecracker` |

:::note

**Auto-download:** If Firecracker or kernel are not found at the specified paths, they will be automatically downloaded:
- Firecracker v1.7.0 from GitHub releases
- Kernel 5.10 from Google Cloud Storage

:::

**Examples:**

```bash title="Create Python snapshot with default settings"
sudo infra.operator snapshot create --lang python
```

```bash title="Create Node.js snapshot with more memory"
sudo infra.operator snapshot create --lang nodejs --mem 1024
```

```bash title="Create Go snapshot with custom rootfs"
sudo infra.operator snapshot create --lang go --rootfs /custom/rootfs.ext4
```

```bash title="Create snapshot with specific kernel version"
sudo infra.operator snapshot create --lang python --kernel /srv/firecracker/vmlinux-5.10
```

```bash title="Create snapshot with custom Firecracker path"
sudo infra.operator snapshot create --lang python --firecracker /opt/firecracker/firecracker
```

**Output:**

```text title="Snapshot creation output"
Creating snapshot for python (Python)...
  Rootfs: /srv/firecracker/images/rootfs-python.ext4
  Memory: 512 MB
  vCPUs:  1
[1/5] Starting Firecracker...
[2/5] Configuring VM...
[3/5] Starting VM...
[3.5/5] Waiting for guest-runner...
[3.6/5] Syncing filesystem...
[4/5] Pausing VM...
[5/5] Creating snapshot...

Snapshot created successfully!
  vmstate.snapshot: 13516 bytes
  mem.snapshot:     536870912 bytes (512.00 MB)
  Location: /dev/shm/snapshots/python
```

```text title="Auto-download output (when dependencies missing)"
Firecracker not found at /usr/local/bin/firecracker, downloading v1.7.0...
  Downloaded: /usr/local/bin/firecracker
Kernel not found at /srv/firecracker/vmlinux, downloading kernel 5.10...
  Downloaded: /srv/firecracker/vmlinux

Creating snapshot for python (Python)...
...
```

---

### snapshot list

List available snapshots.

```bash title="List snapshots command"
infra.operator snapshot list [flags]
```

**Flags:**

| Flag | Short | Description | Default |
|------|-------|-------------|---------|
| `--remote` | `-r` | List snapshots from S3 | `false` |

**Examples:**

```bash title="List local snapshots"
infra.operator snapshot list
```

```text title="Output"
LANGUAGE  VMSTATE     MEMORY    PATH
python    13.2 KB     512 MB    /dev/shm/snapshots/python/
nodejs    14.1 KB     512 MB    /dev/shm/snapshots/nodejs/
go        12.8 KB     512 MB    /dev/shm/snapshots/go/
```

```bash title="List S3 snapshots"
infra.operator snapshot list --remote
```

```text title="Output"
LANGUAGE  VMSTATE     MEMORY    S3 PATH
python    13.2 KB     512 MB    s3://runner-codes/snapshots/python/
nodejs    14.1 KB     512 MB    s3://runner-codes/snapshots/nodejs/
```

---

### snapshot upload

Upload a snapshot to S3.

```bash title="Upload snapshot command"
infra.operator snapshot upload --lang <language> [flags]
```

**Flags:**

| Flag | Short | Description | Default |
|------|-------|-------------|---------|
| `--lang` | `-l` | Language to upload (required) | - |
| `--bucket` | `-b` | S3 bucket | `s3://runner-codes/snapshots` |

**Example:**

```bash title="Upload Python snapshot"
infra.operator snapshot upload --lang python
```

```text title="Output"
Uploading snapshot for python to S3...
  - Uploading vmstate.snapshot (13.2 KB)...
  - Uploading mem.snapshot (512 MB)...
Uploaded successfully: s3://runner-codes/snapshots/python/
```

---

### snapshot download

Download a snapshot from S3.

```bash title="Download snapshot command"
infra.operator snapshot download --lang <language> [flags]
```

**Flags:**

| Flag | Short | Description | Default |
|------|-------|-------------|---------|
| `--lang` | `-l` | Language to download (required) | - |
| `--bucket` | `-b` | S3 bucket | `s3://runner-codes/snapshots` |
| `--output` | `-o` | Output directory | Auto-detect |

**Example:**

```bash title="Download Python snapshot"
infra.operator snapshot download --lang python
```

```text title="Output"
Downloading snapshot for python from S3...
  - Downloading vmstate.snapshot...
  - Downloading mem.snapshot...
Downloaded successfully in 2.4s: /srv/firecracker/snapshots/python/
```

---

## Snapshot Files

Each snapshot consists of two files:

| File | Description | Typical Size |
|------|-------------|--------------|
| `vmstate.snapshot` | CPU registers, device state | 10-20 KB |
| `mem.snapshot` | Full memory dump | 512 MB (or configured memory) |

```text title="Directory structure"
/dev/shm/snapshots/         # Local cache (fast, RAM-based)
├── python/
│   ├── vmstate.snapshot
│   └── mem.snapshot
├── nodejs/
│   ├── vmstate.snapshot
│   └── mem.snapshot
└── go/
    ├── vmstate.snapshot
    └── mem.snapshot

/srv/firecracker/snapshots/ # Persistent storage
├── python/
│   ├── vmstate.snapshot
│   └── mem.snapshot
...
```

---

## Performance Benchmarks

Snapshot load times measured on AWS EC2 m5zn.metal:

| Operation | Time |
|-----------|------|
| Snapshot restore (warm boot) | ~100ms |
| Cold boot (no snapshot) | ~3s |
| S3 download (first time) | ~2.4s |
| Code execution | ~35ms |

**Total execution time with warm boot:** ~135ms

:::note

S3 download is only needed once. After caching locally in `/dev/shm/snapshots/`, only the restore time applies.

:::

---

## Workflow Example

### Complete Setup for a New Language

```bash title="1. Run setup (installs Docker, Firecracker, kernel)"
sudo infra.operator setup
```

```bash title="2. Create rootfs from Docker image"
sudo infra.operator rootfs from-docker --name python --image python:3.12-alpine --size 150
```

```bash title="3. Create snapshot (auto-downloads Firecracker/kernel if needed)"
sudo infra.operator snapshot create --lang python --mem 512 --vcpus 1
```

```bash title="4. Upload to S3 for distribution"
infra.operator snapshot upload --lang python
```

```bash title="5. Test execution (warm boot)"
sudo infra.operator host --lang python --code "print('Hello!')" --mem 512 --vcpus 1 --snapshot
```

```json title="Expected output"
{
  "trace_id": "tr-xxx",
  "status": "success",
  "stdout": "Hello!\n",
  "executionTime": 34
}
```

### On-Demand Download

The host command automatically downloads snapshots on-demand:

```bash title="First execution: downloads from S3 + loads snapshot (~2.6s)"
sudo infra.operator host --lang ruby --code "puts 'Hello'" --snapshot
```

```bash title="Subsequent executions: loads from cache (~0.15s)"
sudo infra.operator host --lang ruby --code "puts 'Hello again'" --snapshot
```

---

## Advanced Configuration

### Custom Memory Settings

Different languages may need different memory allocations:

| Language | Recommended Memory |
|----------|-------------------|
| bash, python, ruby, lua | 256 MB |
| nodejs, go, rust, c, cpp | 512 MB |
| java, kotlin, scala, dotnet | 1024 MB |
| julia | 2048 MB |

```bash title="Create Java snapshot with 1GB memory"
sudo infra.operator snapshot create --lang java --mem 1024
```

```bash title="Create Julia snapshot with 2GB memory"
sudo infra.operator snapshot create --lang julia --mem 2048
```

### Multiple vCPUs

For CPU-intensive workloads:

```bash title="Create with 2 vCPUs"
sudo infra.operator snapshot create --lang python --vcpus 2 --mem 1024
```

### Kernel Requirements

:::warning

**Node.js requires kernel 5.10+!** Older kernels (4.14) cause Node.js to hang due to libuv/epoll compatibility issues.

:::

```bash title="Use specific kernel"
sudo infra.operator snapshot create --lang nodejs --kernel /srv/firecracker/vmlinux-5.10
```

---

## Troubleshooting

### Firecracker not found

```bash title="Manual download"
curl -L -o /tmp/fc.tgz https://github.com/firecracker-microvm/firecracker/releases/download/v1.7.0/firecracker-v1.7.0-x86_64.tgz
tar -xzf /tmp/fc.tgz -C /tmp
sudo mv /tmp/release-v1.7.0-x86_64/firecracker-v1.7.0-x86_64 /usr/local/bin/firecracker
sudo chmod +x /usr/local/bin/firecracker
```

### Guest runner not starting

```bash title="Check rootfs exists"
ls -la /srv/firecracker/images/rootfs-python.ext4
```

```bash title="Verify infra.operator is in rootfs"
sudo mount -o loop /srv/firecracker/images/rootfs-python.ext4 /mnt
ls -la /mnt/usr/local/bin/infra.operator
sudo umount /mnt
```

### Snapshot version mismatch

```bash title="Check Firecracker version"
firecracker --version
```

```bash title="Recreate snapshot if version changed"
sudo infra.operator snapshot create --lang python
```

### Node.js hangs during snapshot

```bash title="Check kernel version"
ls -la /srv/firecracker/vmlinux*
```

```bash title="Download kernel 5.10 if needed"
sudo infra.operator setup --kernel-version 5.10 --skip-docker --skip-firecracker
```

```bash title="Recreate snapshot with new kernel"
sudo infra.operator snapshot create --lang nodejs --kernel /srv/firecracker/vmlinux
```

### Socket errors

```bash title="Clean up stale sockets"
sudo rm -f /tmp/fc-*.sock /tmp/fc-*.vsock
sudo pkill -f firecracker
```

### S3 download fails

```bash title="Check current region"
echo $AWS_DEFAULT_REGION
```

```bash title="Set correct region (S3 bucket is in us-east-1)"
export AWS_DEFAULT_REGION=us-east-1
```

### Out of memory

```bash title="Increase memory for memory-intensive languages"
sudo infra.operator snapshot create --lang java --mem 1024
```

---

## Next Steps

- [Rootfs Commands](/cli/rootfs) - Create and manage rootfs images
- [Run Commands](/cli/run) - Execute code in microVMs
- [HTTP API](/cli/http-api) - REST API for code execution
