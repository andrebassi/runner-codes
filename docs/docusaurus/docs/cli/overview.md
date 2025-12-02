---
title: 'CLI Overview'
description: 'Manage Firecracker microVMs, rootfs images, and snapshots via command line'
---

## Overview

The `infra.operator` CLI provides a complete interface for managing Firecracker microVMs, including rootfs image creation, snapshot management, S3 synchronization, and code execution.

## Installation

### Build from Source

```bash title="Build for current platform"
task -t Taskfile.cli.yaml build
```

```bash title="Build for Linux (deploy to EC2)"
task -t Taskfile.cli.yaml build-linux
```

### Deploy to EC2

```bash title="Deploy to EC2"
task -t Taskfile.cli.yaml deploy
```

## Command Structure

![CLI Command Structure](/img/cli-structure.svg)

## Quick Reference






## Global Flags

| Flag | Description | Default |
|------|-------------|---------|
| `--config` | Path to config file | `$HOME/.infra-operator.yaml` |
| `--version` | Show version | - |
| `--help` | Show help | - |

## Environment Variables

```bash title="AWS Configuration"
export AWS_ACCESS_KEY_ID="your-key"
export AWS_SECRET_ACCESS_KEY="your-secret"
export AWS_DEFAULT_REGION="us-east-1"
```

```bash title="S3 Bucket Configuration"
export S3_BUCKET="llm-infra-operator-rootfs"
export S3_REGION="us-east-1"
```

```bash title="Firecracker Configuration"
export FC_WORKDIR="/srv/firecracker"
export FC_KERNEL="/srv/firecracker/vmlinux"
```

## Command Categories

### Rootfs Management

Create and manage rootfs images for different programming languages:

```bash title="Create a Python rootfs image"
infra.operator rootfs create --lang python
```

```bash title="List local rootfs images"
infra.operator rootfs list
```

```bash title="List S3 rootfs images"
infra.operator rootfs list --remote
```

```bash title="Upload to S3"
infra.operator rootfs upload --lang python
```

```bash title="Download from S3"
infra.operator rootfs download --lang python
```

### Snapshot Management

Create and manage VM snapshots for fast boot (~200ms):

```bash title="Create a snapshot"
infra.operator snapshot create --lang python
```

```bash title="List local snapshots"
infra.operator snapshot list
```

```bash title="List S3 snapshots"
infra.operator snapshot list --remote
```

```bash title="Upload snapshot to S3"
infra.operator snapshot upload --lang python
```

```bash title="Download snapshot from S3"
infra.operator snapshot download --lang python
```

### Code Execution

Execute code in isolated microVMs:

```bash title="Execute inline code"
infra.operator run --lang python --code "print('Hello!')"
```

```bash title="Execute from file"
infra.operator run --lang nodejs --file script.js
```

```bash title="Execute with custom timeout"
infra.operator run --lang go --code 'package main; func main() { println("hi") }' --timeout 60
```

### API Server

Start the HTTP API server:

```bash title="Start API server on default port 8080"
infra.operator api
```

```bash title="Start API server with custom port and host"
infra.operator api --port 3000 --host 0.0.0.0
```

### Benchmarking

Run performance benchmarks:

```bash title="Benchmark all 45 languages"
infra.operator benchmark --all
```

```bash title="Benchmark specific languages"
infra.operator benchmark --langs python,nodejs,go
```

## Supported Languages

The CLI supports **45 programming languages**:

| Category | Languages |
|----------|-----------|
| **Popular** | Python, Node.js, TypeScript, Go, Rust, Java, C#/.NET |
| **Web** | PHP, Ruby, Perl |
| **Compiled** | C, C++, Fortran, Pascal, COBOL |
| **Functional** | Haskell, OCaml, Elixir, Erlang, Clojure, Common Lisp |
| **JVM** | Java, Kotlin, Scala, Groovy, Clojure |
| **Modern** | Zig, Nim, Crystal, D |
| **Scientific** | Julia, R, Octave |
| **Scripting** | Lua, Tcl, AWK, jq |
| **Other** | Prolog, Scheme, SQLite, NASM, Bash |

## Workflow Examples

### Full Language Setup

```bash title="1. Create rootfs image"
infra.operator rootfs create --lang python
```

```bash title="2. Create snapshot for fast boot"
infra.operator snapshot create --lang python
```

```bash title="3. Upload to S3 for distribution"
infra.operator snapshot upload --lang python
```

```bash title="4. Execute code"
infra.operator run --lang python --code "print('Ready!')"
```

### Download and Execute

```bash title="1. Download snapshot from S3"
infra.operator snapshot download --lang nodejs
```

```bash title="2. Execute code"
infra.operator run --lang nodejs --code "console.log('Hello!')"
```

### Benchmark Performance

```bash title="Run full benchmark with S3 on-demand download"
infra.operator benchmark --all
```

```text title="Expected output"
[1/45] python: LOAD=209ms [OK]
[2/45] nodejs: LOAD=215ms [OK]
...
Average Load Time: 220ms
```

## Taskfile Integration

Use Taskfile for common operations:

```bash title="List available tasks"
task -t Taskfile.cli.yaml --list
```

```bash title="Build the CLI"
task -t Taskfile.cli.yaml build
```

```bash title="Run code locally"
task -t Taskfile.cli.yaml run LANG=python CODE='print(1)'
```

```bash title="Execute code on remote EC2"
task -t Taskfile.cli.yaml remote:run LANG=python CODE='print(1)'
```

```bash title="Run benchmark on remote EC2"
task -t Taskfile.cli.yaml remote:benchmark
```

## Output Format

All commands output structured information:

```bash title="List rootfs images (table format)"
infra.operator rootfs list
```

```text title="Rootfs list output"
LANGUAGE  SIZE      PATH
python    1.2 GB    /srv/firecracker/images/rootfs-python.ext4
nodejs    800 MB    /srv/firecracker/images/rootfs-nodejs.ext4
```

```bash title="Execute code"
infra.operator run --lang python --code "print('hello')"
```

```json title="Execution output (JSON)"
{
  "stdout": "hello\n",
  "stderr": "",
  "exit_code": 0
}
```

## Next Steps



