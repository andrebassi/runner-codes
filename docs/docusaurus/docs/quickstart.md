---
title: 'Quickstart'
description: 'Get LLM-Firecracker running in 5 minutes'
---

## Prerequisites

Before you begin, ensure you have:




:::note

  **Mac Users**: Firecracker does not run on macOS. Use an AWS EC2 metal instance or a Linux VM with nested virtualization.

:::

## Step 1: Download infra.operator

Download the latest release:

```bash title="Download binary"
curl -L -o infra.operator https://github.com/your-org/llm-infra-operator/releases/latest/download/infra.operator-linux-amd64
```

```bash title="Make executable and install"
chmod +x infra.operator
```

```bash title="Move to system path"
sudo mv infra.operator /usr/local/bin/
```

## Step 2: Run Setup

The `setup` command automatically installs all dependencies:

```bash title="Run setup command"
sudo infra.operator setup
```

- **Docker** - Required for creating rootfs images (auto-installs if missing)
- **Firecracker v1.7.0** - The microVM hypervisor
- **Linux Kernel 5.10** - Required for Node.js and modern runtimes
- **Directory structure** - `/srv/firecracker/images`, `/dev/shm/snapshots`

**Expected Output:**
```
Setting up Firecracker environment...

[1/5] Creating directories...
  Created: /srv/firecracker
  Created: /srv/firecracker/images
  Created: /dev/shm/snapshots

[2/5] Checking Docker...
  Docker already installed

[3/5] Downloading Firecracker v1.7.0...
  Installed: /usr/local/bin/firecracker

[4/5] Downloading kernel 5.10...
  NOTE: Kernel 5.10+ is REQUIRED for Node.js support!
  Installed: /srv/firecracker/vmlinux

[5/5] Verifying setup...
  Firecracker: OK
  Kernel: OK
  /dev/kvm: OK
  Docker: OK

Setup complete!

Next steps:
  1. Create a rootfs:  infra.operator rootfs from-docker --name nodejs --image node:22-alpine --size 200
  2. Create snapshot:  infra.operator snapshot create --lang nodejs --mem 512 --vcpus 1
  3. Test execution:   infra.operator host --lang nodejs --code "console.log('hello')" --mem 512 --vcpus 1
```

### Setup Options

```bash title="Skip Docker installation (if already installed)"
sudo infra.operator setup --skip-docker
```

```bash title="Skip Firecracker download"
sudo infra.operator setup --skip-firecracker
```

```bash title="Skip kernel download"
sudo infra.operator setup --skip-kernel
```

```bash title="Use specific versions"
sudo infra.operator setup --kernel-version 6.1 --firecracker-version 1.7.0
```

## Step 3: Create Your First Rootfs

Create a Python rootfs from the official Docker image:

```bash title="Create Python rootfs"
sudo infra.operator rootfs from-docker --name python --image python:3.12-alpine --size 150
```

**Expected Output:**
```
Creating rootfs for python from Docker image...
  Docker Image: python:3.12-alpine
  Size: 150 MB
[1/6] Pulling Docker image: python:3.12-alpine...
[2/6] Creating ext4 image (150MB)...
[3/6] Exporting Docker filesystem...
[4/6] Configuring init system...
[5/6] Installing guest-runner...
[6/6] Creating service...
Created: /srv/firecracker/images/rootfs-python.ext4
```

## Step 4: Create a Snapshot

Create a snapshot for fast boot:

```bash title="Create snapshot for fast boot"
sudo infra.operator snapshot create --lang python --mem 512 --vcpus 1
```

:::note

  If Firecracker or kernel are missing, they will be downloaded automatically!

:::

**Expected Output:**
```
Creating snapshot for python (python)...
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

## Step 5: Run Your First Code

Execute Python code in the microVM:

```bash title="Execute Python code in microVM"
sudo infra.operator host --lang python --code "print('Hello from Firecracker!')" --mem 512 --vcpus 1 --snapshot
```

**Expected Output:**
```json title="JSON data"
{
  "trace_id": "tr-1764485208308504533",
  "status": "success",
  "stdout": "Hello from Firecracker!\n",
  "stderr": null,
  "exception": null,
  "executionTime": 34
}
```

## Quick Reference Commands

### Setup Commands

| Command | Description |
|---------|-------------|
| `sudo infra.operator setup` | Full setup (Docker, Firecracker, kernel) |
| `sudo infra.operator setup --skip-docker` | Skip Docker installation |
| `sudo infra.operator setup --kernel-version 6.1` | Use kernel 6.1 instead of 5.10 |

### Rootfs Commands

| Command | Description |
|---------|-------------|
| `infra.operator rootfs from-docker --name X --image Y --size Z` | Create rootfs from Docker image |
| `infra.operator rootfs create --name X --base alpine --packages "pkg1,pkg2"` | Create rootfs from base OS |
| `infra.operator rootfs list` | List local rootfs images |
| `infra.operator rootfs upload --lang X --bucket Y` | Upload to S3 |

### Snapshot Commands

| Command | Description |
|---------|-------------|
| `infra.operator snapshot create --lang X --mem Y --vcpus Z` | Create snapshot |
| `infra.operator snapshot list` | List local snapshots |
| `infra.operator snapshot upload --lang X --bucket Y` | Upload to S3 |

### Execution Commands

| Command | Description |
|---------|-------------|
| `infra.operator host --lang X --code "..." --mem Y --vcpus Z --snapshot` | Execute code (warm boot) |
| `infra.operator host --lang X --code "..." --kernel /path/to/vmlinux` | Execute code (cold boot) |

## What's Next?





