---
title: 'Rootfs Management'
description: 'Create, modify, and manage per-language rootfs images'
---

## Overview

Rootfs (root filesystem) images are ext4 filesystem images containing the Linux userspace and language runtime. Each supported language has its own optimized image.

## Rootfs Image Specifications

| Language | Image | Size | Contents |
|----------|-------|------|----------|
| Python | `rootfs-python.ext4` | 600 MB | Python 3.10, pip, ca-certificates |
| Node.js | `rootfs-nodejs.ext4` | 700 MB | Node.js 20 LTS, npm |
| Go | `rootfs-go.ext4` | 700 MB | Go 1.22.3 |
| Rust | `rootfs-rust.ext4` | 2 GB | rustc, cargo (stable) |
| Bash | `rootfs-bash.ext4` | 512 MB | GNU coreutils, bash |

All images are based on **Ubuntu 22.04 (Jammy)** using debootstrap with `--variant=minbase`.

---

## aws:create-rootfs-python

Create a Python rootfs image and upload to S3.

```bash title="Create Python rootfs"
task aws:create-rootfs-python 2>&1 | tee /tmp/log.txt
```

```yaml title="Task definition"
aws:create-rootfs-python:
  desc: Create Python rootfs and upload to S3
  cmds:
    - |
      ssh ubuntu@$PUBLIC_IP 'bash -s' << 'ENDSSH'
      # Create 600MB ext4 image
      sudo dd if=/dev/zero of=/tmp/rootfs-python.ext4 bs=1M count=600
      sudo mkfs.ext4 /tmp/rootfs-python.ext4

      # Install Ubuntu base
      sudo debootstrap --variant=minbase jammy /mnt/python http://archive.ubuntu.com/ubuntu/

      # Install Python
      sudo chroot /mnt/python apt-get update
      sudo chroot /mnt/python apt-get install -y --no-install-recommends \
        python3 python3-pip ca-certificates

      # Install infra.operator
      sudo cp infra.operator-linux /mnt/python/usr/local/bin/infra.operator

      # Create systemd service
      # ...

      # Upload to S3
      aws s3 cp rootfs-python.ext4 s3://{{.S3_BUCKET}}/
      ENDSSH
```

```text title="Expected output"
Creating Python rootfs...
=== Creating Python rootfs (600MB) ===
600+0 records in
600+0 records out
629145600 bytes (629 MB, 600 MiB) copied, 1.2 s, 524 MB/s
mke2fs 1.46.5 (30-Dec-2021)
Installing Ubuntu 22.04 base...
I: Retrieving InRelease
I: Validating InRelease
I: Retrieving Packages
...
Installing Python...
Reading package lists... Done
Setting up python3 (3.10.6-1~22.04) ...
Setting up python3-pip (22.0.2+dfsg-1ubuntu0.4) ...
Installing infra.operator...
Creating systemd service...
Uploading to S3...
upload: images/rootfs-python.ext4 to s3://llm-infra-operator-rootfs/rootfs-python.ext4

=== Python rootfs created and uploaded to S3 ===
-rw-r--r-- 1 root root 600M rootfs-python.ext4
```

**Installed packages:**
- `python3` (3.10.12)
- `python3-pip`
- `ca-certificates`

---

## aws:create-rootfs-nodejs

Create a Node.js rootfs image and upload to S3.

```bash title="Create Node.js rootfs"
task aws:create-rootfs-nodejs 2>&1 | tee /tmp/log.txt
```

```yaml title="Task definition"
aws:create-rootfs-nodejs:
  desc: Create Node.js rootfs and upload to S3
  cmds:
    - |
      ssh ubuntu@$PUBLIC_IP 'bash -s' << 'ENDSSH'
      # Create 700MB ext4 image
      sudo dd if=/dev/zero of=/tmp/rootfs-nodejs.ext4 bs=1M count=700
      sudo mkfs.ext4 /tmp/rootfs-nodejs.ext4

      # Install Ubuntu base
      sudo debootstrap --variant=minbase jammy /mnt/nodejs http://archive.ubuntu.com/ubuntu/

      # Install Node.js 20 LTS via NodeSource
      sudo chroot /mnt/nodejs apt-get install -y curl ca-certificates gnupg
      sudo chroot /mnt/nodejs bash -c "curl -fsSL https://deb.nodesource.com/setup_20.x | bash -"
      sudo chroot /mnt/nodejs apt-get install -y nodejs

      # Upload to S3
      aws s3 cp rootfs-nodejs.ext4 s3://{{.S3_BUCKET}}/
      ENDSSH
```

```text title="Expected output"
Creating Node.js rootfs...
=== Creating Node.js rootfs (700MB) ===
Installing Ubuntu 22.04 base...
Installing Node.js 20 LTS...
## Installing the NodeSource Node.js 20.x repo...
...
Setting up nodejs (20.19.6-1nodesource1) ...
Installing infra.operator...
Uploading to S3...

=== Node.js rootfs created and uploaded to S3 ===
-rw-r--r-- 1 root root 700M rootfs-nodejs.ext4
```

**Installed packages:**
- `nodejs` (20.x LTS)
- `npm` (included with nodejs)

---

## aws:create-rootfs-go

Create a Go rootfs image and upload to S3.

```bash title="Create Go rootfs"
task aws:create-rootfs-go 2>&1 | tee /tmp/log.txt
```

```yaml title="Task definition"
aws:create-rootfs-go:
  desc: Create Go rootfs and upload to S3
  cmds:
    - |
      ssh ubuntu@$PUBLIC_IP 'bash -s' << 'ENDSSH'
      # Create 700MB ext4 image
      sudo dd if=/dev/zero of=/tmp/rootfs-go.ext4 bs=1M count=700
      sudo mkfs.ext4 /tmp/rootfs-go.ext4

      # Install Ubuntu base
      sudo debootstrap --variant=minbase jammy /mnt/go http://archive.ubuntu.com/ubuntu/

      # Download and install Go 1.22.3
      wget -q https://go.dev/dl/go1.22.3.linux-amd64.tar.gz -O /tmp/go.tar.gz
      sudo tar -C /mnt/go/usr/local -xzf /tmp/go.tar.gz

      # Setup environment
      echo 'export GOROOT=/usr/local/go' | sudo tee /mnt/go/etc/profile.d/go.sh
      echo 'export PATH=$PATH:/usr/local/go/bin' | sudo tee -a /mnt/go/etc/profile.d/go.sh

      # Create systemd service with Go environment
      cat << 'EOF' | sudo tee /mnt/go/etc/systemd/system/infra.operator.service
      [Service]
      ExecStart=/usr/local/bin/infra.operator guest --port 5000
      Environment="HOME=/tmp"
      Environment="GOCACHE=/tmp/go-cache"
      Environment="GOPATH=/tmp/go"
      Environment="GOROOT=/usr/local/go"
      Environment="PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/local/go/bin"
      EOF

      # Upload to S3
      aws s3 cp rootfs-go.ext4 s3://{{.S3_BUCKET}}/
      ENDSSH
```

```text title="Expected output"
Creating Go rootfs...
=== Creating Go rootfs (700MB) ===
Installing Ubuntu 22.04 base...
Installing Go 1.22.3...
Downloading https://go.dev/dl/go1.22.3.linux-amd64.tar.gz
Installing infra.operator...
Uploading to S3...

=== Go rootfs created and uploaded to S3 ===
-rw-r--r-- 1 root root 700M rootfs-go.ext4
```

:::note

The Go systemd service includes environment variables for `GOCACHE`, `GOPATH`, and `GOROOT` to ensure proper Go execution.

:::

---

## aws:create-rootfs-rust

Create a Rust rootfs image and upload to S3.

```bash title="Create Rust rootfs"
task aws:create-rootfs-rust 2>&1 | tee /tmp/log.txt
```

```yaml title="Task definition"
aws:create-rootfs-rust:
  desc: Create Rust rootfs and upload to S3
  cmds:
    - |
      ssh ubuntu@$PUBLIC_IP 'bash -s' << 'ENDSSH'
      # Create 2GB ext4 image (Rust toolchain is large)
      sudo dd if=/dev/zero of=/tmp/rootfs-rust.ext4 bs=1M count=2048
      sudo mkfs.ext4 /tmp/rootfs-rust.ext4

      # Install Ubuntu base with build tools
      sudo debootstrap --variant=minbase jammy /mnt/rust http://archive.ubuntu.com/ubuntu/
      sudo chroot /mnt/rust apt-get install -y curl ca-certificates build-essential

      # Install Rust via rustup
      sudo chroot /mnt/rust bash -c "curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh -s -- -y --default-toolchain stable --profile minimal"

      # Move to system location
      sudo cp -r /mnt/rust/root/.cargo /mnt/rust/opt/cargo
      sudo cp -r /mnt/rust/root/.rustup /mnt/rust/opt/rustup

      # Create symlinks
      sudo ln -sf /opt/cargo/bin/rustc /mnt/rust/usr/bin/rustc
      sudo ln -sf /opt/cargo/bin/cargo /mnt/rust/usr/bin/cargo

      # Upload to S3
      aws s3 cp rootfs-rust.ext4 s3://{{.S3_BUCKET}}/
      ENDSSH
```

```text title="Expected output"
Creating Rust rootfs...
=== Creating Rust rootfs (2GB) ===
Installing Ubuntu 22.04 base...
Installing Rust build dependencies...
Installing Rust via rustup...
info: profile set to 'minimal'
info: default toolchain set to 'stable'
info: downloading component 'rustc'
info: downloading component 'rust-std'
info: downloading component 'cargo'
info: installing component 'rustc'
info: installing component 'rust-std'
info: installing component 'cargo'
Installing infra.operator...
Uploading to S3...

=== Rust rootfs created and uploaded to S3 ===
-rw-r--r-- 1 root root 2.0G rootfs-rust.ext4
```

:::warning

Rust requires a 2GB image due to the size of the toolchain. Upload times will be significantly longer.

:::

---

## aws:create-rootfs-bash

Create a minimal Bash rootfs image and upload to S3.

```bash title="Create Bash rootfs"
task aws:create-rootfs-bash 2>&1 | tee /tmp/log.txt
```

```yaml title="Task definition"
aws:create-rootfs-bash:
  desc: Create Bash rootfs and upload to S3
  cmds:
    - |
      ssh ubuntu@$PUBLIC_IP 'bash -s' << 'ENDSSH'
      # Create 512MB ext4 image (minimal)
      sudo dd if=/dev/zero of=/tmp/rootfs-bash.ext4 bs=1M count=512
      sudo mkfs.ext4 /tmp/rootfs-bash.ext4

      # Install Ubuntu base only
      sudo debootstrap --variant=minbase jammy /mnt/bash http://archive.ubuntu.com/ubuntu/

      # Install infra.operator
      sudo cp infra.operator-linux /mnt/bash/usr/local/bin/infra.operator

      # Create systemd service
      # ...

      # Upload to S3
      aws s3 cp rootfs-bash.ext4 s3://{{.S3_BUCKET}}/
      ENDSSH
```

```text title="Expected output"
Creating Bash rootfs...
=== Creating Bash rootfs (512MB) ===
Installing Ubuntu 22.04 base...
Installing infra.operator...
Creating systemd service...
Uploading to S3...

=== Bash rootfs created and uploaded to S3 ===
-rw-r--r-- 1 root root 512M rootfs-bash.ext4
```

:::tip

The Bash image is the smallest and fastest to create. It's useful for shell scripting and simple command execution.

:::

---

## aws:create-all-rootfs

Create all language rootfs images in sequence.

```bash title="Create all rootfs images"
task aws:create-all-rootfs 2>&1 | tee /tmp/log.txt
```

```yaml title="Task definition"
aws:create-all-rootfs:
  desc: Create all language rootfs images and upload to S3
  cmds:
    - task: aws:create-rootfs-bash
    - task: aws:create-rootfs-python
    - task: aws:create-rootfs-nodejs
    - task: aws:create-rootfs-go
    - task: aws:create-rootfs-rust
    - task: s3:list
    - echo ""
    - echo "=== All rootfs images created and uploaded to S3 ==="
```

```text title="Expected output"
Creating Bash rootfs...
=== Bash rootfs created and uploaded to S3 ===

Creating Python rootfs...
=== Python rootfs created and uploaded to S3 ===

Creating Node.js rootfs...
=== Node.js rootfs created and uploaded to S3 ===

Creating Go rootfs...
=== Go rootfs created and uploaded to S3 ===

Creating Rust rootfs...
=== Rust rootfs created and uploaded to S3 ===

=== S3 bucket contents (llm-infra-operator-rootfs) ===
2025-11-29 05:00:00  512.0 MiB rootfs-bash.ext4
2025-11-29 05:05:00  700.0 MiB rootfs-go.ext4
2025-11-29 05:10:00  700.0 MiB rootfs-nodejs.ext4
2025-11-29 05:15:00  600.0 MiB rootfs-python.ext4
2025-11-29 05:20:00    2.0 GiB rootfs-rust.ext4
2025-11-29 05:25:00   20.4 MiB vmlinux

=== All rootfs images created and uploaded to S3 ===
```

:::note

Creating all images takes approximately 30-45 minutes due to package downloads and Rust toolchain installation.

:::

---

## Rootfs Image Structure

Each rootfs image follows this structure:

```text title="Rootfs directory structure"
rootfs-{lang}.ext4
├── bin/                    # Essential binaries
├── etc/
│   ├── profile.d/          # Environment setup
│   │   └── {lang}.sh       # Language-specific PATH
│   └── systemd/system/
│       └── infra.operator.service
├── lib/                    # Shared libraries
├── opt/                    # Language installations
│   ├── cargo/              # (Rust only)
│   └── rustup/             # (Rust only)
├── usr/
│   ├── bin/                # User binaries
│   │   └── {lang}          # Language interpreter/compiler
│   └── local/
│       ├── bin/
│       │   └── infra.operator
│       └── go/             # (Go only)
└── var/                    # Variable data
```

---

## Infra.Operator Systemd Service

All rootfs images include a systemd service for auto-starting the infra.operator in guest mode:

```ini title="Systemd service configuration"
[Unit]
Description=LLM Firecracker Infra.Operator Guest
After=network.target

[Service]
Type=simple
ExecStart=/usr/local/bin/infra.operator guest --port 5000
Restart=always
RestartSec=1

# Language-specific environment (example for Go)
Environment="HOME=/tmp"
Environment="GOCACHE=/tmp/go-cache"
Environment="GOPATH=/tmp/go"
Environment="GOROOT=/usr/local/go"
Environment="PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/local/go/bin"

[Install]
WantedBy=multi-user.target
```

---

## Customizing Rootfs Images

To add custom packages or configurations:

```bash title="SSH to EC2 instance"
task aws:ssh
```

```bash title="Mount rootfs image"
sudo mkdir -p /mnt/rootfs
sudo mount -o loop /srv/firecracker/images/rootfs-python.ext4 /mnt/rootfs
```

```bash title="Chroot into rootfs"
sudo chroot /mnt/rootfs /bin/bash
```

```bash title="Install packages"
apt-get update
apt-get install -y numpy pandas
```

```bash title="Unmount rootfs"
exit
sudo umount /mnt/rootfs
```

```bash title="Upload to S3"
task s3:upload-single LANG=python
```


:::warning

Changes to rootfs images will be lost if you recreate them. Document any customizations for reproducibility.

:::

---

## Verifying Rootfs Images

Check installed language versions:

```bash title="Verify language versions"
task aws:rootfs-verify
```

```text title="Expected output"
Verifying language installations...
=== Language Versions ===
Python:  Python 3.10.12
Node.js: v20.19.6
Go:      go version go1.22.3 linux/amd64
Rust:    rustc 1.91.1 (4eb161df7 2025-01-08)
Bash:    GNU bash, version 5.1.16(1)-release (x86_64-pc-linux-gnu)

=== Disk Usage ===
Filesystem      Size  Used Avail Use% Mounted on
/dev/loop0      591M  423M  151M  74% /mnt/rootfs
```

