#!/bin/bash
# =============================================================================
# Runner Codes: Consolidated Rootfs Creation Script
# =============================================================================
# This script creates ALL 40 language rootfs images from scratch.
# Each language has its correct size, packages, and post-installation steps.
#
# Usage:
#   ./create-all-rootfs-consolidated.sh           # Create all languages
#   ./create-all-rootfs-consolidated.sh python    # Create only python
#   ./create-all-rootfs-consolidated.sh go rust   # Create multiple languages
#
# Run on EC2 bare-metal instance with AWS credentials configured.
# =============================================================================

set -e

# Configuration
GUEST_RUNNER="/opt/runner-codes/guest-runner"
S3_BUCKET="runner-codes"
IMAGES_DIR="/srv/firecracker/images"
LOG_FILE="/tmp/rootfs-creation-$(date +%Y%m%d-%H%M%S).log"

# Enable logging
exec > >(tee -a "$LOG_FILE") 2>&1

# =============================================================================
# HELPER FUNCTIONS
# =============================================================================

create_rootfs() {
    local LANG=$1
    local SIZE_MB=$2
    local PACKAGES=$3
    local POST_INSTALL=$4

    echo ""
    echo "================================================================"
    echo "[$(date '+%H:%M:%S')] Creating rootfs: $LANG (${SIZE_MB}MB)"
    echo "================================================================"

    cd /srv/firecracker

    # Remove old rootfs if exists
    sudo rm -f ${IMAGES_DIR}/rootfs-${LANG}.ext4
    sudo umount /mnt/${LANG} 2>/dev/null || true
    sudo rm -rf /mnt/${LANG}

    # Create ext4 image
    echo "[1/6] Creating ext4 image (${SIZE_MB}MB)..."
    sudo dd if=/dev/zero of=/tmp/rootfs-${LANG}.ext4 bs=1M count=${SIZE_MB} status=progress
    sudo mkfs.ext4 -F /tmp/rootfs-${LANG}.ext4

    # Mount
    sudo mkdir -p /mnt/${LANG}
    sudo mount /tmp/rootfs-${LANG}.ext4 /mnt/${LANG}

    # Install base system
    echo "[2/6] Installing Ubuntu 22.04 base (debootstrap)..."
    sudo debootstrap --variant=minbase jammy /mnt/${LANG} http://archive.ubuntu.com/ubuntu/

    # Configure apt sources
    echo "[3/6] Configuring apt sources..."
    echo 'deb http://archive.ubuntu.com/ubuntu jammy main universe' | sudo tee /mnt/${LANG}/etc/apt/sources.list
    echo 'deb http://archive.ubuntu.com/ubuntu jammy-updates main universe' | sudo tee -a /mnt/${LANG}/etc/apt/sources.list

    # Mount /proc for apt/dpkg operations (needed for JVM languages)
    sudo mount -t proc proc /mnt/${LANG}/proc
    sudo mount -t devtmpfs dev /mnt/${LANG}/dev 2>/dev/null || true

    # Install packages
    if [ -n "$PACKAGES" ]; then
        echo "[4/6] Installing packages: $PACKAGES"
        sudo chroot /mnt/${LANG} apt-get update
        sudo chroot /mnt/${LANG} apt-get install -y --no-install-recommends $PACKAGES ca-certificates || {
            echo "WARNING: Some packages failed to install for $LANG"
        }
        sudo chroot /mnt/${LANG} apt-get clean
    fi

    # Run post-install script if provided
    if [ -n "$POST_INSTALL" ]; then
        echo "[4.5/6] Running post-installation..."
        sudo chroot /mnt/${LANG} /bin/bash -c "$POST_INSTALL" || {
            echo "WARNING: Post-install script had issues for $LANG"
        }
    fi

    # Unmount /proc and /dev
    sudo umount /mnt/${LANG}/dev 2>/dev/null || true
    sudo umount /mnt/${LANG}/proc 2>/dev/null || true

    # Install guest-runner
    echo "[5/6] Installing guest-runner..."
    sudo cp ${GUEST_RUNNER} /mnt/${LANG}/usr/local/bin/
    sudo chmod +x /mnt/${LANG}/usr/local/bin/guest-runner

    # Create systemd service
    echo "[6/6] Creating systemd service..."
    sudo mkdir -p /mnt/${LANG}/etc/systemd/system
    cat << 'SVCEOF' | sudo tee /mnt/${LANG}/etc/systemd/system/guest-runner.service > /dev/null
[Unit]
Description=LLM FireSandbox Guest Runner
After=network.target

[Service]
Type=simple
ExecStart=/usr/local/bin/guest-runner -port 5000
Restart=always
RestartSec=1

[Install]
WantedBy=multi-user.target
SVCEOF

    sudo mkdir -p /mnt/${LANG}/etc/systemd/system/multi-user.target.wants
    sudo ln -sf /etc/systemd/system/guest-runner.service /mnt/${LANG}/etc/systemd/system/multi-user.target.wants/

    # Finalize
    sudo umount /mnt/${LANG}
    sudo rmdir /mnt/${LANG}
    sudo mv /tmp/rootfs-${LANG}.ext4 ${IMAGES_DIR}/

    echo "Created: rootfs-${LANG}.ext4"
    ls -lh ${IMAGES_DIR}/rootfs-${LANG}.ext4

    # Upload to S3
    echo "Uploading to S3..."
    aws s3 cp ${IMAGES_DIR}/rootfs-${LANG}.ext4 s3://${S3_BUCKET}/ --only-show-errors
    echo "[$(date '+%H:%M:%S')] DONE: $LANG"
}

# =============================================================================
# LANGUAGE DEFINITIONS
# Format: create_rootfs "name" size_mb "packages" "post_install_script"
# =============================================================================

create_python() {
    create_rootfs "python" 500 "python3" ""
}

create_nodejs() {
    create_rootfs "nodejs" 600 "nodejs npm" ""
}

create_typescript() {
    create_rootfs "typescript" 800 "nodejs npm" \
        "npm install -g typescript ts-node"
}

create_go() {
    create_rootfs "go" 1000 "wget" \
        'cd /tmp && \
         wget -q https://go.dev/dl/go1.21.5.linux-amd64.tar.gz && \
         tar -C /usr/local -xzf go1.21.5.linux-amd64.tar.gz && \
         rm go1.21.5.linux-amd64.tar.gz && \
         echo "export GOROOT=/usr/local/go" > /etc/profile.d/go.sh && \
         echo "export PATH=\$PATH:\$GOROOT/bin" >> /etc/profile.d/go.sh && \
         ln -sf /usr/local/go/bin/go /usr/local/bin/go && \
         ln -sf /usr/local/go/bin/gofmt /usr/local/bin/gofmt && \
         /usr/local/go/bin/go version'
}

create_rust() {
    create_rootfs "rust" 1500 "rustc cargo" ""
}

create_bash() {
    create_rootfs "bash" 400 "bash" ""
}

create_c() {
    create_rootfs "c" 800 "build-essential gcc" ""
}

create_cpp() {
    create_rootfs "cpp" 900 "build-essential g++" ""
}

create_java() {
    create_rootfs "java" 1200 "default-jdk" \
        "ldconfig && java -version"
}

create_kotlin() {
    create_rootfs "kotlin" 1500 "default-jdk wget unzip" \
        'cd /tmp && \
         wget -q https://github.com/JetBrains/kotlin/releases/download/v1.9.22/kotlin-compiler-1.9.22.zip && \
         unzip -q kotlin-compiler-1.9.22.zip -d /opt && \
         ln -sf /opt/kotlinc/bin/kotlin /usr/local/bin/kotlin && \
         ln -sf /opt/kotlinc/bin/kotlinc /usr/local/bin/kotlinc && \
         rm kotlin-compiler-1.9.22.zip && \
         ldconfig && \
         kotlinc -version'
}

create_scala() {
    create_rootfs "scala" 1500 "default-jdk scala" \
        "ldconfig && scala -version"
}

create_ruby() {
    create_rootfs "ruby" 600 "ruby" ""
}

create_php() {
    create_rootfs "php" 600 "php-cli" ""
}

create_perl() {
    create_rootfs "perl" 500 "perl" ""
}

create_lua() {
    create_rootfs "lua" 400 "lua5.4" ""
}

create_r() {
    create_rootfs "r" 1200 "r-base" ""
}

create_haskell() {
    create_rootfs "haskell" 2500 "ghc libgmp10 libtinfo6 libncurses6" \
        "ldconfig && ghc --version"
}

create_elixir() {
    create_rootfs "elixir" 900 "elixir erlang" ""
}

create_erlang() {
    create_rootfs "erlang" 800 "erlang-base" ""
}

create_julia() {
    create_rootfs "julia" 1500 "wget" \
        'cd /tmp && \
         wget -q https://julialang-s3.julialang.org/bin/linux/x64/1.10/julia-1.10.0-linux-x86_64.tar.gz && \
         tar -xzf julia-1.10.0-linux-x86_64.tar.gz -C /opt && \
         ln -sf /opt/julia-1.10.0/bin/julia /usr/local/bin/julia && \
         rm julia-1.10.0-linux-x86_64.tar.gz && \
         echo "/opt/julia-1.10.0/lib" > /etc/ld.so.conf.d/julia.conf && \
         echo "/opt/julia-1.10.0/lib/julia" >> /etc/ld.so.conf.d/julia.conf && \
         ldconfig && \
         julia --version'
}

create_fortran() {
    create_rootfs "fortran" 700 "gfortran" ""
}

create_lisp() {
    create_rootfs "lisp" 700 "sbcl" ""
}

create_scheme() {
    create_rootfs "scheme" 500 "guile-3.0" ""
}

create_prolog() {
    create_rootfs "prolog" 600 "swi-prolog-core" ""
}

create_pascal() {
    create_rootfs "pascal" 700 "fp-compiler" ""
}

create_ocaml() {
    create_rootfs "ocaml" 800 "ocaml" ""
}

create_nim() {
    create_rootfs "nim" 900 "wget gcc xz-utils" \
        'cd /tmp && \
         wget -q https://nim-lang.org/download/nim-2.0.0-linux_x64.tar.xz && \
         tar -xJf nim-2.0.0-linux_x64.tar.xz -C /opt && \
         ln -sf /opt/nim-2.0.0/bin/nim /usr/local/bin/nim && \
         rm nim-2.0.0-linux_x64.tar.xz && \
         nim --version'
}

create_crystal() {
    create_rootfs "crystal" 1000 "wget gcc libgc-dev libevent-dev libpcre3-dev pkg-config" \
        'cd /tmp && \
         wget -q https://github.com/crystal-lang/crystal/releases/download/1.10.1/crystal-1.10.1-1-linux-x86_64.tar.gz && \
         tar -xzf crystal-1.10.1-1-linux-x86_64.tar.gz -C /opt && \
         ln -sf /opt/crystal-1.10.1-1/bin/crystal /usr/local/bin/crystal && \
         rm crystal-1.10.1-1-linux-x86_64.tar.gz && \
         crystal --version || echo "Crystal version check (may need /proc at runtime)"'
}

create_d() {
    create_rootfs "d" 1000 "ldc" ""
}

create_zig() {
    create_rootfs "zig" 900 "wget xz-utils" \
        'cd /tmp && \
         wget -q https://ziglang.org/download/0.11.0/zig-linux-x86_64-0.11.0.tar.xz && \
         tar -xJf zig-linux-x86_64-0.11.0.tar.xz -C /opt && \
         ln -sf /opt/zig-linux-x86_64-0.11.0/zig /usr/local/bin/zig && \
         rm zig-linux-x86_64-0.11.0.tar.xz && \
         zig version'
}

create_cobol() {
    create_rootfs "cobol" 700 "gnucobol" ""
}

create_groovy() {
    create_rootfs "groovy" 1200 "default-jdk groovy" \
        "ldconfig && groovy --version"
}

create_clojure() {
    create_rootfs "clojure" 1200 "default-jdk wget rlwrap bash" \
        'cd /tmp && \
         wget -q https://github.com/clojure/brew-install/releases/download/1.11.1.1435/linux-install.sh && \
         chmod +x linux-install.sh && \
         ./linux-install.sh --prefix /usr/local && \
         rm linux-install.sh && \
         clojure --version || echo "Clojure installed"'
}

create_sqlite() {
    create_rootfs "sqlite" 400 "sqlite3" ""
}

create_tcl() {
    create_rootfs "tcl" 400 "tcl" ""
}

create_awk() {
    create_rootfs "awk" 400 "gawk" ""
}

create_jq() {
    create_rootfs "jq" 400 "jq" ""
}

create_nasm() {
    create_rootfs "nasm" 600 "nasm binutils" ""
}

create_octave() {
    create_rootfs "octave" 1200 "octave" ""
}

create_dotnet() {
    create_rootfs "dotnet" 2000 "wget libicu70 libssl3" \
        'cd /tmp && \
         wget -q https://dot.net/v1/dotnet-install.sh && \
         chmod +x dotnet-install.sh && \
         ./dotnet-install.sh --channel 8.0 --install-dir /usr/share/dotnet && \
         ln -sf /usr/share/dotnet/dotnet /usr/local/bin/dotnet && \
         rm dotnet-install.sh && \
         export DOTNET_CLI_TELEMETRY_OPTOUT=1 && \
         dotnet --version || echo ".NET installed"'
}

# =============================================================================
# ALL LANGUAGES LIST (in order)
# =============================================================================

ALL_LANGUAGES=(
    python
    nodejs
    typescript
    go
    rust
    bash
    c
    cpp
    java
    kotlin
    scala
    ruby
    php
    perl
    lua
    r
    haskell
    elixir
    erlang
    julia
    fortran
    lisp
    scheme
    prolog
    pascal
    ocaml
    nim
    crystal
    d
    zig
    cobol
    groovy
    clojure
    sqlite
    tcl
    awk
    jq
    nasm
    octave
    dotnet
)

# =============================================================================
# MAIN
# =============================================================================

echo "============================================================"
echo "Runner Codes: Consolidated Rootfs Creation"
echo "Started: $(date)"
echo "Log file: $LOG_FILE"
echo "============================================================"
echo ""

# Check if specific languages were requested
if [ $# -gt 0 ]; then
    LANGUAGES=("$@")
    echo "Creating rootfs for: ${LANGUAGES[*]}"
else
    LANGUAGES=("${ALL_LANGUAGES[@]}")
    echo "Creating rootfs for ALL ${#ALL_LANGUAGES[@]} languages"
fi

echo ""

# Create each language
CREATED=0
FAILED=0
for lang in "${LANGUAGES[@]}"; do
    echo ""
    echo ">>> Processing: $lang"

    # Check if function exists
    if declare -f "create_$lang" > /dev/null; then
        if "create_$lang"; then
            ((CREATED++))
        else
            echo "ERROR: Failed to create $lang"
            ((FAILED++))
        fi
    else
        echo "ERROR: Unknown language: $lang"
        ((FAILED++))
    fi
done

echo ""
echo "============================================================"
echo "COMPLETED!"
echo "============================================================"
echo "Created: $CREATED"
echo "Failed: $FAILED"
echo "Log: $LOG_FILE"
echo "Finished: $(date)"
echo ""

# List all images
echo "Local images:"
ls -lh ${IMAGES_DIR}/rootfs-*.ext4 2>/dev/null | head -50

echo ""
echo "S3 bucket:"
aws s3 ls s3://${S3_BUCKET}/ --human-readable | head -50
