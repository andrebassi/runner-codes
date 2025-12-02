---
title: 'Rootfs Images'
description: 'Understanding and managing root filesystem images'
---

## What is a Rootfs?

A rootfs (root filesystem) is a disk image containing the complete Linux userspace needed to run code. In Runner Codes, each supported language has its own optimized rootfs image.

![Rootfs Structure](/img/rootfs-structure.svg)

## Image Specifications

| Image | Size | Base | Language | Additional Packages |
|-------|------|------|----------|---------------------|
| `rootfs-bash.ext4` | 512 MB | Ubuntu 22.04 | Bash 5.1 | coreutils |
| `rootfs-python.ext4` | 600 MB | Ubuntu 22.04 | Python 3.10 | pip |
| `rootfs-nodejs.ext4` | 700 MB | Ubuntu 22.04 | Node.js 20 | npm |
| `rootfs-go.ext4` | 700 MB | Ubuntu 22.04 | Go 1.22.3 | - |
| `rootfs-rust.ext4` | 2 GB | Ubuntu 22.04 | Rust stable | cargo, rustc |

## Image Structure

Each rootfs image follows this structure:

```text title="Rootfs directory structure"
rootfs-{lang}.ext4 (ext4 filesystem)
│
├── bin/                        # Essential command binaries
│   ├── bash
│   ├── cat
│   ├── ls
│   └── ...
│
├── etc/
│   ├── profile.d/              # Environment setup scripts
│   │   ├── go.sh              # Go PATH configuration
│   │   └── rust.sh            # Rust environment
│   │
│   └── systemd/system/
│       ├── infra.operator.service
│       └── multi-user.target.wants/
│           └── infra.operator.service -> ../infra.operator.service
│
├── lib/                        # Shared libraries
│   └── x86_64-linux-gnu/
│
├── opt/                        # Optional packages
│   ├── cargo/                 # Rust (only in rust image)
│   └── rustup/                # Rust (only in rust image)
│
├── usr/
│   ├── bin/
│   │   ├── python3            # Language interpreter/compiler
│   │   ├── node
│   │   ├── go
│   │   └── rustc
│   │
│   ├── lib/
│   │
│   └── local/
│       ├── bin/
│       │   └── infra.operator   # Code executor binary (guest mode)
│       │
│       └── go/                # Go installation (only in go image)
│
└── var/
    └── log/                    # Log files
```

## Infra.Operator Guest Service

Each rootfs includes a systemd service that starts the infra.operator in guest mode on boot:

```ini title="/etc/systemd/system/infra.operator.service"
# /etc/systemd/system/infra.operator.service
[Unit]
Description=LLM Firecracker Infra.Operator Guest
After=network.target

[Service]
Type=simple
ExecStart=/usr/local/bin/infra.operator guest --port 5000
Restart=always
RestartSec=1

# Language-specific environment (example: Go)
Environment="HOME=/tmp"
Environment="GOCACHE=/tmp/go-cache"
Environment="GOPATH=/tmp/go"
Environment="GOROOT=/usr/local/go"
Environment="PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/local/go/bin"

[Install]
WantedBy=multi-user.target
```

## Creating Rootfs Images

### Using Taskfile (Recommended)

```bash title="Create individual images"
task aws:create-rootfs-python
task aws:create-rootfs-nodejs
task aws:create-rootfs-go
task aws:create-rootfs-rust
task aws:create-rootfs-bash
```

```bash title="Create all images"
task aws:create-all-rootfs
```

### Manual Process


```bash title="Create 600MB image file"
dd if=/dev/zero of=rootfs-python.ext4 bs=1M count=600
```

```bash title="Format as ext4"
mkfs.ext4 rootfs-python.ext4
```



```bash title="Mount the image"
mkdir -p /mnt/rootfs
mount -o loop rootfs-python.ext4 /mnt/rootfs
```

```bash title="Install Ubuntu base"
debootstrap --variant=minbase jammy /mnt/rootfs http://archive.ubuntu.com/ubuntu/
```



```bash title="Enter chroot"
chroot /mnt/rootfs /bin/bash
```

```bash title="Update and install Python"
apt-get update
apt-get install -y python3 python3-pip ca-certificates
exit
```



```bash title="Copy infra.operator binary"
# Copy infra.operator binary
cp infra.operator-linux /mnt/rootfs/usr/local/bin/infra.operator
chmod +x /mnt/rootfs/usr/local/bin/infra.operator
```



```bash title="Create and enable systemd service"
cat > /mnt/rootfs/etc/systemd/system/infra.operator.service << 'EOF'
[Unit]
Description=LLM Firecracker Infra.Operator Guest
After=network.target

[Service]
Type=simple
ExecStart=/usr/local/bin/infra.operator guest --port 5000
Restart=always

[Install]
WantedBy=multi-user.target
EOF

# Enable service
ln -sf /etc/systemd/system/infra.operator.service \
  /mnt/rootfs/etc/systemd/system/multi-user.target.wants/
```



```bash title="Clean apt cache"
chroot /mnt/rootfs apt-get clean
rm -rf /mnt/rootfs/var/lib/apt/lists/*
```

```bash title="Unmount"
umount /mnt/rootfs
```


## Modifying Existing Images

### Adding Packages

```bash title="Mount the image"
sudo mount -o loop /srv/firecracker/images/rootfs-python.ext4 /mnt/rootfs
```

```bash title="Setup for chroot"
sudo cp /etc/resolv.conf /mnt/rootfs/etc/
sudo mount --bind /dev /mnt/rootfs/dev
sudo mount --bind /proc /mnt/rootfs/proc
sudo mount --bind /sys /mnt/rootfs/sys
```

```bash title="Enter chroot and install packages"
sudo chroot /mnt/rootfs /bin/bash
apt-get update
apt-get install -y numpy pandas  # Example packages
exit
```

```bash title="Cleanup"
sudo umount /mnt/rootfs/sys
sudo umount /mnt/rootfs/proc
sudo umount /mnt/rootfs/dev
sudo umount /mnt/rootfs
```

```bash title="Upload updated image"
task s3:upload-single LANG=python
```

### Updating Infra.Operator Binary

```bash title="Build new infra.operator for Linux"
task build:infra-operator-linux
```

```bash title="Mount image"
sudo mount -o loop /srv/firecracker/images/rootfs-python.ext4 /mnt/rootfs
```

```bash title="Replace binary"
sudo cp bin/infra.operator-linux /mnt/rootfs/usr/local/bin/infra.operator
sudo chmod +x /mnt/rootfs/usr/local/bin/infra.operator
```

```bash title="Unmount"
sudo umount /mnt/rootfs
```

## Storage on S3

Rootfs images are stored in S3 for fast deployment:

```text title="S3 rootfs structure"
s3://llm-infra-operator-rootfs/
├── vmlinux                 # Linux kernel
├── rootfs-bash.ext4
├── rootfs-python.ext4
├── rootfs-nodejs.ext4
├── rootfs-go.ext4
└── rootfs-rust.ext4
```

### Upload to S3

```bash title="Upload all images"
task s3:upload
```

```bash title="Upload single image"
task s3:upload-single LANG=python
```

### Download from S3

```bash title="Download all images from S3"
# Download all images
task s3:download
```

## Image Selection

When executing code, the host component selects the appropriate rootfs based on the language:

```go title="Select rootfs by language"
func (r *Runner) selectRootfs(lang string) string {
    imagesDir := "/srv/firecracker/images"
    switch lang {
    case "python":
        return filepath.Join(imagesDir, "rootfs-python.ext4")
    case "node":
        return filepath.Join(imagesDir, "rootfs-nodejs.ext4")
    case "go":
        return filepath.Join(imagesDir, "rootfs-go.ext4")
    case "rust":
        return filepath.Join(imagesDir, "rootfs-rust.ext4")
    case "bash":
        return filepath.Join(imagesDir, "rootfs-bash.ext4")
    default:
        return filepath.Join(imagesDir, "rootfs-python.ext4")
    }
}
```

## Size Optimization

### Tips for Smaller Images

1. **Use `--variant=minbase`** with debootstrap
2. **Remove documentation** (`rm -rf /usr/share/doc/*`)
3. **Clean apt cache** (`apt-get clean`)
4. **Remove man pages** (`rm -rf /usr/share/man/*`)
5. **Use `--no-install-recommends`** when installing packages

### Example: Minimal Python Image

```bash title="Create 400MB image"
dd if=/dev/zero of=rootfs-python-mini.ext4 bs=1M count=400
mkfs.ext4 rootfs-python-mini.ext4
mount -o loop rootfs-python-mini.ext4 /mnt/rootfs
```

```bash title="Minimal debootstrap"
debootstrap --variant=minbase --include=python3 \
  jammy /mnt/rootfs http://archive.ubuntu.com/ubuntu/
```

```bash title="Remove unnecessary files"
rm -rf /mnt/rootfs/usr/share/doc/*
rm -rf /mnt/rootfs/usr/share/man/*
rm -rf /mnt/rootfs/var/lib/apt/lists/*
rm -rf /mnt/rootfs/var/cache/apt/*
umount /mnt/rootfs
```

## Troubleshooting

### Image Won't Mount

```bash title="Check image integrity"
e2fsck -f rootfs.ext4
```

```bash title="Fix filesystem errors"
e2fsck -y rootfs.ext4
```

### Out of Space During Install

```bash title="Expand rootfs image"
dd if=/dev/zero bs=1M count=500 >> rootfs.ext4
e2fsck -f rootfs.ext4
resize2fs rootfs.ext4
```

### Infra.Operator Not Starting

```bash title="Check service status"
mount -o loop rootfs.ext4 /mnt/rootfs
chroot /mnt/rootfs systemctl status infra.operator
```

```bash title="Check binary"
file /mnt/rootfs/usr/local/bin/infra.operator
```
