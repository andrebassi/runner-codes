#!/bin/bash
# Runner Codes EC2 User Data Script
# Installs Firecracker and downloads rootfs images from S3

set -euo pipefail

LOG_FILE="/var/log/firesandbox-setup.log"
exec > >(tee -a "$LOG_FILE") 2>&1

echo "=== Runner Codes Setup Starting ==="
echo "Date: $(date)"

# S3 Configuration
S3_BUCKET="runner-codes-rootfs"
S3_REGION="us-east-1"

# Update system
echo "Updating system packages..."
apt-get update
apt-get upgrade -y

# Install dependencies
echo "Installing dependencies..."
apt-get install -y \
    build-essential \
    curl \
    jq \
    git \
    wget \
    e2fsprogs \
    debootstrap \
    qemu-utils \
    libseccomp2 \
    awscli

# Check KVM
echo "Checking KVM availability..."
if [ ! -e /dev/kvm ]; then
    echo "Loading KVM modules..."
    modprobe kvm
    modprobe kvm_intel 2>/dev/null || modprobe kvm_amd 2>/dev/null || true
fi

if [ -e /dev/kvm ]; then
    echo "KVM is available"
    chmod 666 /dev/kvm
else
    echo "ERROR: KVM not available"
    exit 1
fi

# Check/load vhost_vsock
echo "Checking vhost_vsock..."
if [ ! -e /dev/vhost-vsock ]; then
    modprobe vhost_vsock
fi

if [ -e /dev/vhost-vsock ]; then
    echo "vhost_vsock is available"
    chmod 666 /dev/vhost-vsock
else
    echo "ERROR: vhost_vsock not available"
    exit 1
fi

# Create working directory
WORKDIR="/srv/firecracker"
mkdir -p "$WORKDIR"
mkdir -p "$WORKDIR/images"
cd "$WORKDIR"

# Download Firecracker
FC_VERSION="v1.7.0"
ARCH=$(uname -m)
echo "Downloading Firecracker ${FC_VERSION} for ${ARCH}..."

curl -L -o firecracker.tgz \
    "https://github.com/firecracker-microvm/firecracker/releases/download/${FC_VERSION}/firecracker-${FC_VERSION}-${ARCH}.tgz"

tar -xzf firecracker.tgz
mv "release-${FC_VERSION}-${ARCH}/firecracker-${FC_VERSION}-${ARCH}" /usr/local/bin/firecracker
mv "release-${FC_VERSION}-${ARCH}/jailer-${FC_VERSION}-${ARCH}" /usr/local/bin/jailer
chmod +x /usr/local/bin/firecracker /usr/local/bin/jailer
rm -rf "release-${FC_VERSION}-${ARCH}" firecracker.tgz

echo "Firecracker version:"
/usr/local/bin/firecracker --version

# Download rootfs images from S3
echo ""
echo "=== Downloading rootfs images from S3 ==="
echo "Bucket: s3://${S3_BUCKET}"

# Download vmlinux kernel
echo "Downloading vmlinux kernel..."
if aws s3 cp s3://${S3_BUCKET}/vmlinux ${WORKDIR}/vmlinux --region ${S3_REGION}; then
    echo "vmlinux downloaded successfully"
else
    echo "WARNING: vmlinux not found in S3, downloading from Firecracker releases..."
    curl -L -o vmlinux \
        "https://s3.amazonaws.com/spec.ccfc.min/img/quickstart_guide/${ARCH}/kernels/vmlinux.bin" || \
    curl -L -o vmlinux \
        "https://s3.amazonaws.com/spec.ccfc.min/firecracker-ci/v1.7.0/${ARCH}/vmlinux-5.10.217"
fi

# Download all rootfs images from S3
echo "Downloading rootfs images..."
aws s3 sync s3://${S3_BUCKET}/ ${WORKDIR}/images/ \
    --exclude "*" --include "rootfs-*.ext4" \
    --region ${S3_REGION} || echo "No rootfs images found in S3"

# List downloaded images
echo ""
echo "=== Downloaded rootfs images ==="
ls -lh ${WORKDIR}/images/ 2>/dev/null || echo "No images downloaded"

# Create symlink to default rootfs (python)
if [ -f ${WORKDIR}/images/rootfs-python.ext4 ]; then
    ln -sf ${WORKDIR}/images/rootfs-python.ext4 ${WORKDIR}/rootfs.ext4
    echo "Default rootfs symlinked to rootfs-python.ext4"
elif [ -f ${WORKDIR}/images/rootfs-bash.ext4 ]; then
    ln -sf ${WORKDIR}/images/rootfs-bash.ext4 ${WORKDIR}/rootfs.ext4
    echo "Default rootfs symlinked to rootfs-bash.ext4"
else
    # Fallback: download basic rootfs from Firecracker
    echo "No rootfs images found in S3, downloading basic rootfs..."
    curl -L -o rootfs.ext4 \
        "https://s3.amazonaws.com/spec.ccfc.min/img/quickstart_guide/${ARCH}/rootfs/bionic.rootfs.ext4" || \
    curl -L -o rootfs.ext4 \
        "https://s3.amazonaws.com/spec.ccfc.min/firecracker-ci/v1.7.0/${ARCH}/ubuntu-22.04.ext4"
fi

# Create directories
mkdir -p /srv/firecracker/jailer
mkdir -p /srv/firecracker/snapshots
mkdir -p /srv/firecracker/logs
mkdir -p /tmp/firecracker

# Set permissions
chmod -R 755 /srv/firecracker

# Install Go 1.21+
echo "Installing Go..."
GO_VERSION="1.21.5"
curl -L -o go.tar.gz "https://go.dev/dl/go${GO_VERSION}.linux-${ARCH}.tar.gz"
rm -rf /usr/local/go
tar -C /usr/local -xzf go.tar.gz
rm go.tar.gz

# Add Go to PATH for all users
echo 'export PATH=$PATH:/usr/local/go/bin' >> /etc/profile.d/go.sh
export PATH=$PATH:/usr/local/go/bin

echo "Go version:"
/usr/local/go/bin/go version

# Clone project (placeholder - will be replaced by actual deployment)
echo "Creating project directory..."
mkdir -p /opt/runner-codes
cat > /opt/runner-codes/README.md << 'EOF'
# Runner Codes

Environment ready. Deploy your code here.

## Quick Test

```bash
# Test Firecracker
firecracker --version

# Check KVM
ls -la /dev/kvm

# Check vsock
ls -la /dev/vhost-vsock

# List rootfs images
ls -la /srv/firecracker/images/
```

## Available rootfs images

Images are downloaded from S3 bucket: runner-codes-rootfs

- rootfs-python.ext4 - Python 3.10
- rootfs-nodejs.ext4 - Node.js 20 LTS
- rootfs-go.ext4 - Go 1.22.3
- rootfs-rust.ext4 - Rust stable
- rootfs-bash.ext4 - Bash (minimal)
EOF

# Create systemd service for persistence
cat > /etc/systemd/system/firesandbox-setup.service << 'EOF'
[Unit]
Description=Runner Codes KVM/Vsock Setup
After=network.target

[Service]
Type=oneshot
ExecStart=/bin/bash -c 'modprobe kvm; modprobe kvm_intel 2>/dev/null || modprobe kvm_amd 2>/dev/null || true; modprobe vhost_vsock; chmod 666 /dev/kvm /dev/vhost-vsock 2>/dev/null || true'
RemainAfterExit=yes

[Install]
WantedBy=multi-user.target
EOF

systemctl daemon-reload
systemctl enable firesandbox-setup.service

echo ""
echo "=== Runner Codes Setup Complete ==="
echo "Firecracker: $(firecracker --version)"
echo "Kernel: $WORKDIR/vmlinux"
echo "Rootfs: $WORKDIR/rootfs.ext4 -> $(readlink -f $WORKDIR/rootfs.ext4 2>/dev/null || echo 'N/A')"
echo "KVM: $(ls -la /dev/kvm)"
echo "Vsock: $(ls -la /dev/vhost-vsock)"
echo ""
echo "Available rootfs images:"
ls -lh $WORKDIR/images/ 2>/dev/null || echo "None"
echo ""
