#!/usr/bin/env bash
set -euo pipefail

# =============================================================================
# LLM-FireSandbox: Firecracker Setup Script for Ubuntu/Zorin
# =============================================================================
# This script prepares a Linux host for running Firecracker microVMs.
# Supports both x86_64 and aarch64 architectures.
# =============================================================================

WORKDIR="${FC_WORKDIR:-/srv/firecracker}"
FC_VERSION="${FC_VERSION:-v1.7.0}"
ARCH=$(uname -m)

# Color output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

log_info() { echo -e "${GREEN}[INFO]${NC} $1"; }
log_warn() { echo -e "${YELLOW}[WARN]${NC} $1"; }
log_error() { echo -e "${RED}[ERROR]${NC} $1"; }

# =============================================================================
# Pre-flight checks
# =============================================================================

check_root() {
    if [[ $EUID -ne 0 ]]; then
        log_error "This script must be run as root (use sudo)"
        exit 1
    fi
}

check_kvm() {
    log_info "Checking KVM availability..."
    if [[ ! -e /dev/kvm ]]; then
        log_warn "/dev/kvm not found. Attempting to load module..."
        modprobe kvm || { log_error "Failed to load kvm module"; exit 1; }

        if [[ "$ARCH" == "x86_64" ]]; then
            modprobe kvm_intel 2>/dev/null || modprobe kvm_amd 2>/dev/null || true
        fi
    fi

    if [[ -e /dev/kvm ]]; then
        log_info "KVM is available: /dev/kvm"
    else
        log_error "KVM is not available. Firecracker requires KVM."
        exit 1
    fi
}

check_vsock() {
    log_info "Checking vhost_vsock availability..."
    if [[ ! -e /dev/vhost_vsock ]]; then
        log_warn "/dev/vhost_vsock not found. Attempting to load module..."
        modprobe vhost_vsock || { log_error "Failed to load vhost_vsock module"; exit 1; }
    fi

    if [[ -e /dev/vhost_vsock ]]; then
        log_info "vhost_vsock is available: /dev/vhost_vsock"
    else
        log_error "vhost_vsock is not available. vsock communication requires this."
        exit 1
    fi
}

# =============================================================================
# Installation
# =============================================================================

install_dependencies() {
    log_info "Installing dependencies..."
    apt-get update
    apt-get install -y \
        build-essential \
        debootstrap \
        qemu-utils \
        curl \
        jq \
        libseccomp2 \
        wget \
        e2fsprogs
    log_info "Dependencies installed successfully"
}

download_firecracker() {
    log_info "Downloading Firecracker ${FC_VERSION} for ${ARCH}..."

    local fc_bin="firecracker-${FC_VERSION}-${ARCH}"
    local jailer_bin="jailer-${FC_VERSION}-${ARCH}"
    local base_url="https://github.com/firecracker-microvm/firecracker/releases/download/${FC_VERSION}"

    mkdir -p "$WORKDIR"
    cd "$WORKDIR"

    # Download firecracker
    if [[ ! -f /usr/local/bin/firecracker ]]; then
        curl -LO "${base_url}/${fc_bin}.tgz" || {
            # Fallback to direct binary download
            curl -LO "${base_url}/${fc_bin}"
            chmod +x "${fc_bin}"
            mv "${fc_bin}" /usr/local/bin/firecracker
        }

        if [[ -f "${fc_bin}.tgz" ]]; then
            tar -xzf "${fc_bin}.tgz"
            mv "release-${FC_VERSION}-${ARCH}/firecracker-${FC_VERSION}-${ARCH}" /usr/local/bin/firecracker
            mv "release-${FC_VERSION}-${ARCH}/jailer-${FC_VERSION}-${ARCH}" /usr/local/bin/jailer
            rm -rf "release-${FC_VERSION}-${ARCH}" "${fc_bin}.tgz"
        fi

        chmod +x /usr/local/bin/firecracker
        log_info "Firecracker installed to /usr/local/bin/firecracker"
    else
        log_info "Firecracker already installed"
    fi

    # Verify installation
    /usr/local/bin/firecracker --version
}

download_kernel_rootfs() {
    log_info "Downloading kernel and sample rootfs..."

    cd "$WORKDIR"

    # Download kernel
    if [[ ! -f "$WORKDIR/vmlinux" ]]; then
        log_info "Downloading vmlinux kernel..."
        curl -LO "https://s3.amazonaws.com/spec.ccfc.min/firecracker-ci/${FC_VERSION}/vmlinux.bin" || \
        curl -LO "https://s3.amazonaws.com/spec.ccfc.min/img/quickstart_guide/${ARCH}/kernels/vmlinux.bin"
        mv vmlinux.bin vmlinux
        log_info "Kernel downloaded: $WORKDIR/vmlinux"
    else
        log_info "Kernel already exists: $WORKDIR/vmlinux"
    fi

    # Download rootfs
    if [[ ! -f "$WORKDIR/rootfs.base.ext4" ]]; then
        log_info "Downloading sample rootfs..."
        curl -LO "https://s3.amazonaws.com/spec.ccfc.min/firecracker-ci/${FC_VERSION}/rootfs.ext4" || \
        curl -LO "https://s3.amazonaws.com/spec.ccfc.min/img/quickstart_guide/${ARCH}/rootfs/bionic.rootfs.ext4"

        if [[ -f "bionic.rootfs.ext4" ]]; then
            mv bionic.rootfs.ext4 rootfs.base.ext4
        else
            mv rootfs.ext4 rootfs.base.ext4
        fi
        log_info "Rootfs downloaded: $WORKDIR/rootfs.base.ext4"
    else
        log_info "Rootfs already exists: $WORKDIR/rootfs.base.ext4"
    fi
}

prepare_rootfs_for_guest_runner() {
    log_info "Preparing rootfs for guest-runner injection..."

    local TMPROOT="/tmp/fc-root-$$"
    mkdir -p "$TMPROOT"

    # Create a copy of the base rootfs for modification
    if [[ ! -f "$WORKDIR/rootfs.ext4" ]]; then
        cp "$WORKDIR/rootfs.base.ext4" "$WORKDIR/rootfs.ext4"
    fi

    # Mount the rootfs
    mount -o loop "$WORKDIR/rootfs.ext4" "$TMPROOT"

    # Copy resolv.conf for DNS resolution during setup
    cp /etc/resolv.conf "$TMPROOT/etc/resolv.conf" 2>/dev/null || true

    # Create directory for guest-runner
    mkdir -p "$TMPROOT/usr/local/bin"

    # Create a placeholder script if guest-runner binary doesn't exist yet
    if [[ ! -f "$WORKDIR/guest-runner" ]]; then
        cat > "$TMPROOT/usr/local/bin/guest-runner" << 'EOF'
#!/bin/sh
echo "ERROR: guest-runner binary not installed"
echo "Please build and copy the guest-runner binary to this location"
exit 1
EOF
        chmod +x "$TMPROOT/usr/local/bin/guest-runner"
        log_warn "Placeholder guest-runner created. Build and copy the real binary."
    else
        cp "$WORKDIR/guest-runner" "$TMPROOT/usr/local/bin/guest-runner"
        chmod +x "$TMPROOT/usr/local/bin/guest-runner"
        log_info "guest-runner copied to rootfs"
    fi

    # Create init script to start guest-runner on boot
    cat > "$TMPROOT/etc/init.d/guest-runner" << 'EOF'
#!/bin/sh
### BEGIN INIT INFO
# Provides:          guest-runner
# Required-Start:    $local_fs
# Required-Stop:     $local_fs
# Default-Start:     2 3 4 5
# Default-Stop:      0 1 6
# Short-Description: LLM FireSandbox Guest Runner
### END INIT INFO

case "$1" in
    start)
        echo "Starting guest-runner..."
        /usr/local/bin/guest-runner &
        ;;
    stop)
        echo "Stopping guest-runner..."
        killall guest-runner 2>/dev/null || true
        ;;
    *)
        echo "Usage: $0 {start|stop}"
        exit 1
        ;;
esac
exit 0
EOF
    chmod +x "$TMPROOT/etc/init.d/guest-runner"

    # Enable guest-runner on boot (for sysvinit systems)
    if [[ -d "$TMPROOT/etc/rc3.d" ]]; then
        ln -sf ../init.d/guest-runner "$TMPROOT/etc/rc3.d/S99guest-runner" 2>/dev/null || true
    fi

    # For systemd-based systems
    mkdir -p "$TMPROOT/etc/systemd/system"
    cat > "$TMPROOT/etc/systemd/system/guest-runner.service" << 'EOF'
[Unit]
Description=LLM FireSandbox Guest Runner
After=network.target

[Service]
Type=simple
ExecStart=/usr/local/bin/guest-runner
Restart=always
RestartSec=1

[Install]
WantedBy=multi-user.target
EOF

    # Unmount
    umount "$TMPROOT"
    rmdir "$TMPROOT"

    log_info "Rootfs prepared successfully at $WORKDIR/rootfs.ext4"
}

create_runtime_dirs() {
    log_info "Creating runtime directories..."

    mkdir -p "$WORKDIR/jailer"
    mkdir -p "$WORKDIR/snapshots"
    mkdir -p "$WORKDIR/logs"
    mkdir -p /tmp/firecracker

    # Set permissions
    chmod 755 "$WORKDIR"
    chmod 755 "$WORKDIR/jailer"

    log_info "Runtime directories created"
}

print_next_steps() {
    echo ""
    echo "============================================================================="
    echo -e "${GREEN}Firecracker setup completed successfully!${NC}"
    echo "============================================================================="
    echo ""
    echo "Next steps:"
    echo ""
    echo "1. Build guest-runner for the guest architecture:"
    echo "   # For x86_64 guest:"
    echo "   GOOS=linux GOARCH=amd64 go build -o $WORKDIR/guest-runner ./guest-runner"
    echo ""
    echo "   # For ARM64 guest:"
    echo "   GOOS=linux GOARCH=arm64 go build -o $WORKDIR/guest-runner ./guest-runner"
    echo ""
    echo "2. Copy guest-runner to rootfs:"
    echo "   sudo mount -o loop $WORKDIR/rootfs.ext4 /mnt"
    echo "   sudo cp $WORKDIR/guest-runner /mnt/usr/local/bin/guest-runner"
    echo "   sudo chmod +x /mnt/usr/local/bin/guest-runner"
    echo "   sudo umount /mnt"
    echo ""
    echo "3. Build and run host-agent:"
    echo "   go build -o host-agent ./host-agent"
    echo "   sudo ./host-agent $WORKDIR/vmlinux $WORKDIR/rootfs.ext4 python 10"
    echo ""
    echo "Environment:"
    echo "  WORKDIR:  $WORKDIR"
    echo "  Kernel:   $WORKDIR/vmlinux"
    echo "  Rootfs:   $WORKDIR/rootfs.ext4"
    echo "  Arch:     $ARCH"
    echo ""
}

# =============================================================================
# Main
# =============================================================================

main() {
    log_info "Starting Firecracker setup for LLM-FireSandbox"
    log_info "Architecture: $ARCH"
    log_info "Workdir: $WORKDIR"
    echo ""

    check_root
    check_kvm
    check_vsock
    install_dependencies
    download_firecracker
    download_kernel_rootfs
    prepare_rootfs_for_guest_runner
    create_runtime_dirs
    print_next_steps
}

# Run main
main "$@"
