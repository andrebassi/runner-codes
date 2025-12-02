---
title: 'Build Tasks'
description: 'Compile and build the unified infra.operator CLI'
---

## Overview

Build tasks compile the unified `infra.operator` Go binary which contains both host-side (VM control) and guest-side (code execution) functionality.

## build:all

Build all components in sequence.

```bash title="Build all components"
task build:all
```

```yaml title="Task definition"
build:all:
  desc: Build all components
  cmds:
    - task: build:infra-operator
    - task: build:infra-operator-linux
```

**Output:**
```text title="Expected output"
Built bin/infra.operator
Built bin/infra.operator-linux (amd64)
```

---

## build:infra-operator

Build the unified CLI binary for the current operating system.

```bash title="Build infra.operator for current OS"
task build:infra-operator
```

```yaml title="Task definition"
build:infra-operator:
  desc: Build infra.operator for current OS
  cmds:
    - mkdir -p {{.BIN_DIR}}
    - go build -ldflags="-s -w" -o {{.BIN_DIR}}/infra.operator ./cmd/infra.operator/...
    - echo "Built {{.BIN_DIR}}/infra.operator"
```

**What it does:**
1. Creates `bin/` directory if it doesn't exist
2. Compiles unified CLI with stripped debug info (`-ldflags="-s -w"`)
3. Outputs binary to `bin/infra.operator`

**Output:**
```text title="Expected output"
Built bin/infra.operator
```

:::note

The binary is built for your current OS. For AWS deployment, use `task build:infra-operator-linux` which cross-compiles for Linux.

:::

---

## build:infra-operator-linux

Build the unified CLI binary for Linux, auto-detecting architecture.

```bash title="Build infra.operator for Linux"
task build:infra-operator-linux
```

```yaml title="Task definition"
build:infra-operator-linux:
  desc: Build infra.operator for Linux (auto-detect arch)
  vars:
    GOARCH:
      sh: |
        ARCH=$(uname -m)
        if [ "$ARCH" = "x86_64" ]; then echo "amd64"
        elif [ "$ARCH" = "aarch64" ] || [ "$ARCH" = "arm64" ]; then echo "arm64"
        else echo "amd64"; fi
  cmds:
    - mkdir -p {{.BIN_DIR}}
    - GOOS=linux GOARCH={{.GOARCH}} go build -ldflags="-s -w" -o {{.BIN_DIR}}/infra.operator-linux ./cmd/infra.operator/...
    - echo "Built {{.BIN_DIR}}/infra.operator-linux ({{.GOARCH}})"
```

**What it does:**
1. Detects current machine architecture (x86_64 → amd64, arm64 → arm64)
2. Cross-compiles for Linux with detected architecture
3. Outputs binary to `bin/infra.operator-linux`

**Output:**
```text title="Expected output"
Built bin/infra.operator-linux (amd64)
```

---

## build:infra-operator-amd64

Explicitly build for Linux x86_64.

```bash title="Build for Linux/amd64"
task build:infra-operator-amd64
```

```yaml title="Task definition"
build:infra-operator-amd64:
  desc: Build infra.operator for Linux/amd64
  cmds:
    - mkdir -p {{.BIN_DIR}}
    - GOOS=linux GOARCH=amd64 go build -ldflags="-s -w" -o {{.BIN_DIR}}/infra.operator-amd64 ./cmd/infra.operator/...
    - echo "Built {{.BIN_DIR}}/infra.operator-amd64"
```

**Use case:** When building on ARM Mac for x86_64 EC2 instances.

**Output:**
```text title="Expected output"
Built bin/infra.operator-amd64
```

---

## build:infra-operator-arm64

Build for Linux ARM64.

```bash title="Build for Linux/arm64"
task build:infra-operator-arm64
```

```yaml title="Task definition"
build:infra-operator-arm64:
  desc: Build infra.operator for Linux/arm64
  cmds:
    - mkdir -p {{.BIN_DIR}}
    - GOOS=linux GOARCH=arm64 go build -ldflags="-s -w" -o {{.BIN_DIR}}/infra.operator-arm64 ./cmd/infra.operator/...
    - echo "Built {{.BIN_DIR}}/infra.operator-arm64"
```

**Use case:** When building for ARM64 EC2 instances (Graviton).

**Output:**
```text title="Expected output"
Built bin/infra.operator-arm64
```

---

## Development Tasks

### dev:tidy

Tidy Go module dependencies.

```bash title="Tidy Go modules"
task dev:tidy
```

```yaml title="Task definition"
dev:tidy:
  desc: Tidy Go modules
  cmds:
    - go mod tidy
```

---

### dev:fmt

Format all Go code using `go fmt`.

```bash title="Format Go code"
task dev:fmt
```

```yaml title="Task definition"
dev:fmt:
  desc: Format Go code
  cmds:
    - go fmt ./...
```

---

### dev:lint

Run `go vet` on all code.

```bash title="Lint Go code"
task dev:lint
```

```yaml title="Task definition"
dev:lint:
  desc: Lint Go code
  cmds:
    - go vet ./...
```

---

## Clean Tasks

### clean

Remove built binaries.

```bash title="Clean built binaries"
task clean
```

```yaml title="Task definition"
clean:
  desc: Clean built binaries
  cmds:
    - rm -rf {{.BIN_DIR}}
    - echo "Cleaned"
```

**Output:**
```text title="Expected output"
Cleaned
```

---

### clean:all

Remove binaries and Go build cache.

```bash title="Clean everything including Go cache"
task clean:all
```

```yaml title="Task definition"
clean:all:
  desc: Clean everything including Go cache
  cmds:
    - task: clean
    - go clean -cache
    - echo "All cleaned"
```

:::warning

This clears the Go build cache, which will slow down subsequent builds.

:::

---

## Build Artifacts

After running `task build:all`, you'll have:

```text title="Build artifacts directory structure"
llm-firecracker/
├── bin/
│   ├── infra.operator        # Unified CLI for current OS
│   └── infra.operator-linux  # Cross-compiled for Linux
├── cmd/
│   └── infra.operator/       # CLI entry point
├── pkg/
│   ├── host/                 # Host-side VM control
│   └── guest/                # Guest-side code execution
└── internal/                 # Internal packages
```

### Binary Size

| Binary | Approximate Size |
|--------|-----------------|
| infra.operator | ~10 MB |

:::tip

The `-ldflags="-s -w"` flags strip debug info, reducing binary size by ~30%.

:::

---

## Cross-Compilation

The unified binary can be cross-compiled for different architectures:


```bash title="Build for Linux x86_64"
task build:infra-operator-amd64
```

```bash title="Build for Linux ARM64"
task build:infra-operator-arm64
```

```bash title="Build for Linux with auto-detected architecture"
task build:infra-operator-linux
```


:::note

The `task aws:deploy` command automatically cross-compiles for Linux/amd64, regardless of your host machine.

:::

---

## Unified CLI Subcommands

The single `infra.operator` binary contains all functionality:

| Subcommand | Description |
|------------|-------------|
| `host` | Controls Firecracker VMs and executes code (host-side) |
| `guest` | Runs inside microVM, listens on vsock:5000 (guest-side) |
| `rootfs` | Manages rootfs images (create, list, upload, download) |
| `snapshot` | Manages snapshots (create, list, upload, download) |
| `api` | Starts HTTP API server |
| `run` | Executes code in a microVM |
| `benchmark` | Runs performance benchmarks |
