---
title: 'Guest Mode'
description: 'The code execution engine inside microVMs (infra.operator guest)'
---

## What is Guest Mode?

Guest Mode (`infra.operator guest`) is part of the unified CLI that runs inside each Firecracker microVM. It listens for code execution requests via vsock and executes code in the specified language. The guest binary is the same `infra.operator` binary, just invoked with the `guest` subcommand.






## Architecture

![Infra.Operator Guest Architecture](/img/infra-operator-guest-architecture.svg)

## Source Code

The guest mode is part of the unified `infra.operator` CLI located in `pkg/guest/`:

```text title="Guest source code structure"
pkg/guest/
├── server.go         # Vsock server entry point
├── executor.go       # Code execution logic (40+ languages)
├── types.go          # Request/response types
├── server_test.go    # Unit tests
├── executor_test.go  # Unit tests
└── types_test.go     # Unit tests
```

## Main Components

### Vsock Server (main.go)

```go title="Main function"
package main

import (
    "log"
    "github.com/mdlayher/vsock"
)

func main() {
    // Listen on vsock CID=3, port=5000
    listener, err := vsock.Listen(3, 5000)
    if err != nil {
        log.Fatal(err)
    }
    defer listener.Close()

    log.Println("Infra.operator guest listening on vsock CID=3, port=5000")

    for {
        conn, err := listener.Accept()
        if err != nil {
            log.Printf("Accept error: %v", err)
            continue
        }
        go handleConnection(conn)
    }
}

func handleConnection(conn net.Conn) {
    defer conn.Close()

    // Read job request
    job, err := readJob(conn)
    if err != nil {
        log.Printf("Read error: %v", err)
        return
    }

    // Execute code
    result := execute(job)

    // Send result
    if err := writeResult(conn, result); err != nil {
        log.Printf("Write error: %v", err)
    }
}
```

### Executor (executor.go)

```go title="Code execution engine"
package main

import (
    "context"
    "os"
    "os/exec"
    "path/filepath"
    "time"
)

type LanguageConfig struct {
    Extension string
    Command   string
    Args      []string
}

var languages = map[string]LanguageConfig{
    "python": {".py", "python3", nil},
    "node":   {".js", "node", nil},
    "go":     {".go", "go", []string{"run"}},
    "rust":   {".rs", "rustc", nil},
    "bash":   {".sh", "bash", nil},
}

func execute(job Job) Result {
    config, ok := languages[job.Lang]
    if !ok {
        return Result{
            TraceID:  job.TraceID,
            Error:    "unsupported language",
            ExitCode: -1,
        }
    }

    // Create temp directory
    workDir := filepath.Join("/tmp", "job-"+job.TraceID)
    os.MkdirAll(workDir, 0755)
    defer os.RemoveAll(workDir)

    // Write code to file
    scriptPath := filepath.Join(workDir, "script"+config.Extension)
    if err := os.WriteFile(scriptPath, []byte(job.Code), 0755); err != nil {
        return Result{
            TraceID:  job.TraceID,
            Error:    err.Error(),
            ExitCode: -1,
        }
    }

    // Prepare command
    var cmd *exec.Cmd
    ctx, cancel := context.WithTimeout(context.Background(),
        time.Duration(job.Timeout)*time.Second)
    defer cancel()

    if job.Lang == "rust" {
        // Compile first
        binaryPath := filepath.Join(workDir, "runbin")
        compileCmd := exec.CommandContext(ctx, "rustc", scriptPath, "-o", binaryPath)
        compileCmd.Dir = workDir
        if output, err := compileCmd.CombinedOutput(); err != nil {
            return Result{
                TraceID:  job.TraceID,
                Stderr:   string(output),
                ExitCode: 1,
            }
        }
        cmd = exec.CommandContext(ctx, binaryPath)
    } else if config.Args != nil {
        args := append(config.Args, scriptPath)
        cmd = exec.CommandContext(ctx, config.Command, args...)
    } else {
        cmd = exec.CommandContext(ctx, config.Command, scriptPath)
    }

    cmd.Dir = workDir
    cmd.Env = getEnv()

    // Capture output
    var stdout, stderr bytes.Buffer
    cmd.Stdout = &stdout
    cmd.Stderr = &stderr

    // Execute
    err := cmd.Run()
    exitCode := 0
    if err != nil {
        if exitErr, ok := err.(*exec.ExitError); ok {
            exitCode = exitErr.ExitCode()
        } else if ctx.Err() == context.DeadlineExceeded {
            return Result{
                TraceID:  job.TraceID,
                Error:    "execution timeout",
                ExitCode: -1,
            }
        }
    }

    return Result{
        TraceID:  job.TraceID,
        Stdout:   stdout.String(),
        Stderr:   stderr.String(),
        ExitCode: exitCode,
    }
}

func getEnv() []string {
    return []string{
        "HOME=/tmp",
        "GOCACHE=/tmp/go-cache",
        "GOPATH=/tmp/go",
        "GOROOT=/usr/local/go",
        "CARGO_HOME=/opt/cargo",
        "RUSTUP_HOME=/opt/rustup",
        "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/local/go/bin:/opt/cargo/bin",
    }
}
```

### Types (types.go)

```go title="Job and Result type definitions"
package main

type Job struct {
    TraceID string `json:"trace_id"`
    Lang    string `json:"lang"`
    Code    string `json:"code"`
    Timeout int    `json:"timeout"`
}

type Result struct {
    TraceID  string `json:"trace_id"`
    Stdout   string `json:"stdout"`
    Stderr   string `json:"stderr"`
    ExitCode int    `json:"exit_code"`
    Error    string `json:"error,omitempty"`
}
```

## Building Infra.Operator

### For x86_64 (Most Common)

```bash title="Build"
GOOS=linux GOARCH=amd64 CGO_ENABLED=0 \
  go build -o bin/infra.operator-linux ./cmd/infra.operator
```

### For ARM64

```bash title="Build"
GOOS=linux GOARCH=arm64 CGO_ENABLED=0 \
  go build -o bin/infra.operator-arm64 ./cmd/infra.operator
```

### Using Taskfile

```bash title="Build for Linux amd64"
task build:infra-operator-linux
```

```bash title="Build for current OS"
task build:infra-operator
```

```bash title="Build all"
task build:all
```

## Installing in Rootfs

The infra.operator binary is installed to `/usr/local/bin/infra.operator` in each rootfs image:

```bash title="Mount rootfs"
sudo mount -o loop rootfs-python.ext4 /mnt
```

```bash title="Copy binary"
sudo cp bin/infra.operator-linux /mnt/usr/local/bin/infra.operator
sudo chmod +x /mnt/usr/local/bin/infra.operator
```

```bash title="Unmount"
sudo umount /mnt
```

## Systemd Service

Infra.operator guest mode starts automatically via systemd:

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

# Environment variables
Environment="HOME=/tmp"
Environment="GOCACHE=/tmp/go-cache"
Environment="GOPATH=/tmp/go"
Environment="GOROOT=/usr/local/go"
Environment="CARGO_HOME=/opt/cargo"
Environment="RUSTUP_HOME=/opt/rustup"
Environment="PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/usr/local/go/bin:/opt/cargo/bin"

[Install]
WantedBy=multi-user.target
```

## Execution Flow

1. **Receive Request**: Host mode connects to vsock and sends JSON job request with length prefix

2. **Parse & Validate**: Infra.operator guest parses the JSON and validates the language

3. **Create Workspace**: A temporary directory `/tmp/job-{trace_id}/` is created

4. **Write Code**: Code is written to `script.{ext}` in the workspace

5. **Execute**: The appropriate interpreter/compiler is invoked with timeout

6. **Capture Output**: stdout, stderr, and exit code are captured

7. **Cleanup**: Workspace directory is deleted

8. **Return Response**: JSON response is sent back with length prefix

## Error Handling

### Timeout

```go title="Handle execution timeout"
ctx, cancel := context.WithTimeout(context.Background(),
    time.Duration(job.Timeout)*time.Second)
defer cancel()

if ctx.Err() == context.DeadlineExceeded {
    return Result{
        Error:    "execution timeout",
        ExitCode: -1,
    }
}
```

### Unsupported Language

```go title="Handle unsupported language"
if _, ok := languages[job.Lang]; !ok {
    return Result{
        Error:    "unsupported language",
        ExitCode: -1,
    }
}
```

### Compilation Error

```go title="Handle compilation error"
if output, err := compileCmd.CombinedOutput(); err != nil {
    return Result{
        Stderr:   string(output),
        ExitCode: 1,
    }
}
```

## Resource Limits

Guest runner operates within VM constraints:

| Resource | Limit | Enforced By |
|----------|-------|-------------|
| Memory | 512 MiB | VM config |
| CPU | 1 vCPU | VM config |
| Time | Job timeout | Guest runner |
| Disk | Rootfs size | ext4 image |

## Logging

Infra.operator guest logs to stdout (captured by systemd):

```go title="Guest logging example"
log.Printf("[%s] Received job: lang=%s, code=%d bytes",
    job.TraceID, job.Lang, len(job.Code))

log.Printf("[%s] Execution complete: exit_code=%d, stdout=%d bytes",
    job.TraceID, result.ExitCode, len(result.Stdout))
```

View logs:
```bash title="View guest logs"
# On guest (if you have serial console access)
journalctl -u infra.operator -f
```

## Security Considerations

:::warning

  Infra.operator guest executes arbitrary code. Security relies on VM isolation.

:::

1. **Process isolation**: Each job runs in a separate process
2. **Filesystem isolation**: Jobs use separate temp directories
3. **Timeout enforcement**: Prevents infinite loops
4. **No network**: VM has no network access
5. **VM destruction**: VM is destroyed after execution

## Debugging

### Check if Running

```bash title="Inside VM"
systemctl status infra.operator
```

```bash title="or"
ps aux | grep infra.operator
```

### Check Listening

```bash title="Terminal"
# Inside VM
ss -lnp | grep 5000
```

### Test Manually

```bash title="From host, via vsock"
echo -e "CONNECT 5000\n" | nc -U /tmp/fc-12345.vsock
```


## Performance

| Operation | Time |
|-----------|------|
| Job parsing | < 1ms |
| File write | < 1ms |
| Python startup | ~50ms |
| Node.js startup | ~100ms |
| Go compilation | ~500ms |
| Rust compilation | ~2000ms |
| Bash startup | ~10ms |

## Future Improvements

- [ ] Concurrent job execution
- [ ] Job queue
- [ ] Resource monitoring
- [ ] Pre-loaded interpreters
- [ ] Caching compiled binaries
