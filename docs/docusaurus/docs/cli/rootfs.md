---
title: 'Rootfs Commands'
description: 'Create, list, upload and download rootfs images for Firecracker microVMs'
---

## Overview

Rootfs (root filesystem) images are the base disk images used by Firecracker microVMs. Each language runtime requires its own rootfs image with the necessary packages and dependencies installed.

## Commands

### rootfs from-docker (Recommended)

Create a rootfs image from any Docker image. This is the **recommended** method for creating rootfs images.

```bash title="Create rootfs from Docker image"
infra.operator rootfs from-docker --name <name> --image <docker-image> --size <size-mb>
```

**Flags:**

| Flag | Short | Description | Default |
|------|-------|-------------|---------|
| `--name` | `-n` | Name for the rootfs (required) | - |
| `--image` | `-i` | Docker image to use (required) | - |
| `--size` | `-s` | Image size in MB (required) | - |

**Examples:**

```bash title="Create Python rootfs from official Alpine image"
sudo infra.operator rootfs from-docker --name python --image python:3.12-alpine --size 150
```

```bash title="Create Node.js rootfs (use kernel 5.10+ for Node.js!)"
sudo infra.operator rootfs from-docker --name nodejs --image node:22-alpine --size 200
```

```bash title="Create Go rootfs"
sudo infra.operator rootfs from-docker --name go --image golang:1.22-alpine --size 300
```

```bash title="Create Rust rootfs"
sudo infra.operator rootfs from-docker --name rust --image rust:1.75-alpine --size 400
```

```bash title="Create Java rootfs"
sudo infra.operator rootfs from-docker --name java --image eclipse-temurin:21-alpine --size 350
```

**Output:**

```text title="Rootfs creation output"
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

:::note

  **Why from-docker?** This method uses official Docker images which are well-maintained, security-patched, and include all necessary dependencies. Alpine-based images are recommended for smaller rootfs sizes.

:::

### rootfs create

Create a new rootfs image for a specific language using debootstrap (Ubuntu-based).

```bash title="Create rootfs for a language"
infra.operator rootfs create --lang <language> [flags]
```

**Flags:**

| Flag | Short | Description | Default |
|------|-------|-------------|---------|
| `--lang` | `-l` | Language runtime (required) | - |
| `--base` | `-b` | Base OS | `alpine` |
| `--packages` | `-p` | Additional packages | - |
| `--output` | `-o` | Output path | `/srv/firecracker/images/rootfs-{lang}.ext4` |
| `--size` | `-s` | Image size in MB | `1024` |

**Examples:**

```bash title="Create Python rootfs with default settings"
sudo infra.operator rootfs create --lang python
```

```bash title="Create Node.js rootfs with custom size"
sudo infra.operator rootfs create --lang nodejs --size 2048
```

```bash title="Create custom rootfs with extra packages"
sudo infra.operator rootfs create --lang python --packages "numpy,pandas"
```

### rootfs list

List available rootfs images.

```bash title="List rootfs images"
infra.operator rootfs list [flags]
```

**Flags:**

| Flag | Short | Description | Default |
|------|-------|-------------|---------|
| `--remote` | `-r` | List images from S3 | `false` |

**Examples:**

```bash title="List local rootfs images"
infra.operator rootfs list
```

```text title="Local rootfs output"
LANGUAGE  SIZE      PATH
python    150 MB    /srv/firecracker/images/rootfs-python.ext4
nodejs    200 MB    /srv/firecracker/images/rootfs-nodejs.ext4
go        300 MB    /srv/firecracker/images/rootfs-go.ext4
```

```bash title="List S3 rootfs images"
infra.operator rootfs list --remote
```

```text title="S3 rootfs output"
LANGUAGE  SIZE      LAST MODIFIED     S3 KEY
python    150 MB    2025-11-30 10:30  rootfs-python.ext4
nodejs    200 MB    2025-11-30 10:35  rootfs-nodejs.ext4
```

### rootfs upload

Upload a rootfs image to S3.

```bash title="Upload rootfs to S3"
infra.operator rootfs upload --lang <language> [flags]
```

**Flags:**

| Flag | Short | Description | Default |
|------|-------|-------------|---------|
| `--lang` | `-l` | Language to upload (required) | - |
| `--bucket` | `-b` | S3 bucket | `s3://runner-codes` |

**Examples:**

```bash title="Upload Python rootfs to S3"
infra.operator rootfs upload --lang python
```

```bash title="Upload to custom bucket"
infra.operator rootfs upload --lang nodejs --bucket s3://my-bucket
```

### rootfs download

Download a rootfs image from S3.

```bash title="Download rootfs from S3"
infra.operator rootfs download --lang <language> [flags]
```

**Flags:**

| Flag | Short | Description | Default |
|------|-------|-------------|---------|
| `--lang` | `-l` | Language to download (required) | - |
| `--bucket` | `-b` | S3 bucket | `s3://runner-codes` |
| `--output` | `-o` | Output path | Auto-detect |

**Examples:**

```bash title="Download Python rootfs"
infra.operator rootfs download --lang python
```

```bash title="Download to custom path"
infra.operator rootfs download --lang nodejs --output /tmp/nodejs.ext4
```

## Recommended Docker Images

Use Alpine-based images for smaller rootfs sizes:

| Language | Docker Image | Size |
|----------|--------------|------|
| Python | `python:3.12-alpine` | 150 MB |
| Node.js | `node:22-alpine` | 200 MB |
| Go | `golang:1.22-alpine` | 300 MB |
| Rust | `rust:1.75-alpine` | 400 MB |
| Java | `eclipse-temurin:21-alpine` | 350 MB |
| Ruby | `ruby:3.3-alpine` | 150 MB |
| PHP | `php:8.3-alpine` | 150 MB |
| .NET | `mcr.microsoft.com/dotnet/sdk:8.0-alpine` | 600 MB |

:::warning

  **Node.js requires kernel 5.10+!** Older kernels (4.14) cause Node.js to hang due to libuv/epoll compatibility issues. Use `--kernel-version 5.10` in the setup command.

:::

## Rootfs Structure

Each rootfs image contains:

```text title="Rootfs directory structure"
/
├── bin/                    # Essential binaries
├── etc/
│   └── init.d/
│       └── S99guest        # Auto-start service
├── usr/
│   └── local/bin/
│       └── infra.operator  # Guest runner binary
├── home/                   # Working directory
├── tmp/                    # Temp files for code execution
└── var/log/               # Logs
```

## Build Process (from-docker)

![Rootfs Build Process](/img/rootfs-build-process.svg)

The `from-docker` command:

1. **Pulls** the specified Docker image
2. **Creates** an empty ext4 image with the specified size
3. **Exports** the Docker filesystem to the ext4 image
4. **Installs** the init system (OpenRC for Alpine)
5. **Copies** the `infra.operator` binary as guest runner
6. **Configures** the service to start on boot

## Troubleshooting

### Rootfs Creation Fails

**Docker not found:**

```bash title="Setup Docker automatically"
sudo infra.operator setup
```

```bash title="Install Docker manually"
curl -fsSL https://get.docker.com | sh
```

**Insufficient disk space:**

```bash title="Check available disk space in /tmp"
df -h /tmp
```

```bash title="Check available disk space in /srv/firecracker"
df -h /srv/firecracker
```

**Cannot pull Docker image:**

```bash title="Test Docker Hub connectivity"
docker pull python:3.12-alpine
```

**Corrupted rootfs:**

```bash title="Remove existing rootfs"
sudo rm /srv/firecracker/images/rootfs-python.ext4
```

```bash title="Create fresh rootfs"
sudo infra.operator rootfs from-docker --name python --image python:3.12-alpine --size 150
```

### S3 Operations Fail

**Invalid AWS credentials:**

```bash title="Verify AWS credentials"
aws sts get-caller-identity
```

**Wrong bucket or region:**

```bash title="List S3 bucket contents"
aws s3 ls s3://runner-codes/
```

## Next Steps



