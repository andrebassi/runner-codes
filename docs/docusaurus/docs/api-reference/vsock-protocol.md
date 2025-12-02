---
title: 'Vsock Protocol'
description: 'Low-level virtio-vsock communication protocol'
---

## Overview

Virtio-vsock provides a communication channel between the host and guest without requiring a network stack. LLM-Firecracker uses Firecracker's vsock implementation through Unix Domain Sockets (UDS).

## Architecture

![Vsock Protocol Architecture](/img/vsock-protocol.svg)

## Connection Sequence

When starting the VM, Firecracker creates a vsock UDS:
```bash title="Configure vsock via Firecracker API"
PUT /vsock
{
  "guest_cid": 3,
  "uds_path": "/tmp/fc-{instance_id}.vsock"
}
```

Inside the microVM, infra.operator guest listens on vsock:
```go title="Guest listens on vsock port"
listener, _ := vsock.Listen(3, 5000)  // CID=3, Port=5000
conn, _ := listener.Accept()
```

Host agent connects to Firecracker's vsock UDS:
```go title="Connect to vsock Unix socket"
conn, _ := net.Dial("unix", "/tmp/fc-12345.vsock")
```

Host requests connection to specific guest port:
```text title="Send CONNECT command"
CONNECT 5000\n
```

Firecracker multiplexes the connection:
```text title="Firecracker acknowledges connection"
OK 5000\n
```

After handshake, the connection becomes a raw byte stream.


## Firecracker Vsock UDS Protocol

### CONNECT Command

**Format:**
```text title="CONNECT command format"
CONNECT <port>\n
```

**Examples:**
```text title="CONNECT command examples"
CONNECT 5000\n
CONNECT 8080\n
```

**Rules:**
- Port must be a valid 32-bit unsigned integer
- Command terminated by newline (`\n`)
- Single space between CONNECT and port

### OK Response

**Format:**
```text title="OK response format"
OK <port>\n
```

**Example:**
```text title="Successful connection response"
OK 5000\n
```

### Error Responses

| Response | Meaning |
|----------|---------|
| `NO <port>\n` | Connection refused |
| `BUSY <port>\n` | Port already in use |
| `INVALID\n` | Malformed command |

## Message Framing

After the CONNECT/OK handshake, messages use length-prefix framing:

```text title="Message framing structure"
+------------------+----------------------+
| Length (4 bytes) | Payload              |
| Big-Endian       | (JSON, UTF-8)        |
+------------------+----------------------+
```

### Byte Order

Length is encoded as **big-endian** (network byte order):

| Value | Bytes (hex) |
|-------|-------------|
| 100 | `00 00 00 64` |
| 1000 | `00 00 03 E8` |
| 65535 | `00 00 FF FF` |

### Example Message

JSON payload:
```json title="Example job request JSON"
{"trace_id":"t1","lang":"python","code":"print(1)","timeout":10}
```

Wire format:
```text title="Message on wire with length prefix"
00 00 00 3F                              # Length: 63 bytes
7B 22 74 72 61 63 65 5F 69 64 22 3A ...  # JSON payload
```

## Implementation Examples

### Host Side (Go)

```go title="Complete host-side vsock client"
package main

import (
    "bufio"
    "encoding/binary"
    "encoding/json"
    "fmt"
    "net"
    "strings"
)

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

func connectVsock(socketPath string, port int) (net.Conn, error) {
    // Connect to Firecracker UDS
    conn, err := net.Dial("unix", socketPath)
    if err != nil {
        return nil, fmt.Errorf("dial unix: %w", err)
    }

    // Send CONNECT command
    fmt.Fprintf(conn, "CONNECT %d\n", port)

    // Read response
    reader := bufio.NewReader(conn)
    response, err := reader.ReadString('\n')
    if err != nil {
        conn.Close()
        return nil, fmt.Errorf("read response: %w", err)
    }

    // Verify OK response
    if !strings.HasPrefix(response, "OK") {
        conn.Close()
        return nil, fmt.Errorf("connect failed: %s", response)
    }

    return conn, nil
}

func sendJob(conn net.Conn, job Job) error {
    payload, err := json.Marshal(job)
    if err != nil {
        return err
    }

    // Write length prefix
    length := make([]byte, 4)
    binary.BigEndian.PutUint32(length, uint32(len(payload)))
    if _, err := conn.Write(length); err != nil {
        return err
    }

    // Write payload
    _, err = conn.Write(payload)
    return err
}

func recvResult(conn net.Conn) (*Result, error) {
    // Read length prefix
    lengthBuf := make([]byte, 4)
    if _, err := conn.Read(lengthBuf); err != nil {
        return nil, err
    }
    length := binary.BigEndian.Uint32(lengthBuf)

    // Read payload
    payload := make([]byte, length)
    if _, err := conn.Read(payload); err != nil {
        return nil, err
    }

    // Parse JSON
    var result Result
    if err := json.Unmarshal(payload, &result); err != nil {
        return nil, err
    }

    return &result, nil
}

func main() {
    // Connect to vsock
    conn, err := connectVsock("/tmp/fc-12345.vsock", 5000)
    if err != nil {
        panic(err)
    }
    defer conn.Close()

    // Send job
    job := Job{
        TraceID: "tr-001",
        Lang:    "python",
        Code:    "print('Hello')",
        Timeout: 30,
    }
    if err := sendJob(conn, job); err != nil {
        panic(err)
    }

    // Receive result
    result, err := recvResult(conn)
    if err != nil {
        panic(err)
    }

    fmt.Printf("Result: %+v\n", result)
}
```

### Guest Side (Go)

```go title="Complete guest-side vsock server"
package main

import (
    "encoding/binary"
    "encoding/json"
    "log"
    "net"

    "github.com/mdlayher/vsock"
)

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

func recvJob(conn net.Conn) (*Job, error) {
    // Read length prefix
    lengthBuf := make([]byte, 4)
    if _, err := conn.Read(lengthBuf); err != nil {
        return nil, err
    }
    length := binary.BigEndian.Uint32(lengthBuf)

    // Read payload
    payload := make([]byte, length)
    if _, err := conn.Read(payload); err != nil {
        return nil, err
    }

    // Parse JSON
    var job Job
    if err := json.Unmarshal(payload, &job); err != nil {
        return nil, err
    }

    return &job, nil
}

func sendResult(conn net.Conn, result Result) error {
    payload, err := json.Marshal(result)
    if err != nil {
        return err
    }

    // Write length prefix
    length := make([]byte, 4)
    binary.BigEndian.PutUint32(length, uint32(len(payload)))
    if _, err := conn.Write(length); err != nil {
        return err
    }

    // Write payload
    _, err = conn.Write(payload)
    return err
}

func main() {
    // Listen on vsock (CID=3 is Firecracker default for guest)
    listener, err := vsock.Listen(3, 5000)
    if err != nil {
        log.Fatal(err)
    }
    defer listener.Close()

    log.Println("Guest runner listening on vsock CID=3, port=5000")

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

    // Receive job
    job, err := recvJob(conn)
    if err != nil {
        log.Printf("Receive error: %v", err)
        return
    }

    // Execute code (simplified)
    result := executeCode(*job)

    // Send result
    if err := sendResult(conn, result); err != nil {
        log.Printf("Send error: %v", err)
    }
}

func executeCode(job Job) Result {
    // ... execution logic ...
    return Result{
        TraceID:  job.TraceID,
        Stdout:   "Hello\n",
        Stderr:   "",
        ExitCode: 0,
    }
}
```

## Configuration

### Firecracker VM Config

```json title="Vsock configuration"
{
  "vsock": {
    "guest_cid": 3,
    "uds_path": "/tmp/fc-{instance_id}.vsock"
  }
}
```

### API Call

```bash title="Configure vsock via Firecracker API"
curl --unix-socket /tmp/fc.sock -X PUT \
  "http://localhost/vsock" \
  -H "Content-Type: application/json" \
  -d '{
    "guest_cid": 3,
    "uds_path": "/tmp/fc-12345.vsock"
  }'
```

## Context ID (CID)

| CID | Meaning |
|-----|---------|
| 0 | Hypervisor (reserved) |
| 1 | Reserved |
| 2 | Host |
| 3+ | Guest VMs |

:::note

  Firecracker uses CID=3 by default for the guest VM. This is configurable but rarely changed.

:::

## Performance Characteristics

| Metric | Value |
|--------|-------|
| Connection latency | < 1ms |
| Message overhead | 4 bytes per message |
| Bandwidth | Limited by VM memory bandwidth |
| Max concurrent connections | Implementation dependent |

## Debugging

### Check Vsock UDS Exists

```bash title="List vsock socket files"
ls -la /tmp/fc-*.vsock
```

### Test Connection with netcat

```bash title="Connect to vsock UDS"
nc -U /tmp/fc-12345.vsock
```


### Trace Vsock Traffic

```bash title="Trace system calls on infra.operator"
# On host, trace the infra.operator process
sudo strace -e trace=read,write,connect -s 1000 -p $(pgrep infra.operator)
```

### Guest-Side Debugging

```bash title="Inside VM, check vsock module"
lsmod | grep vsock
```

```bash title="Check listening ports"
ss -lnp | grep vsock
```

## Troubleshooting

**Symptoms:** `NO 5000\n` response or connection error

**Causes:**
- infra.operator (guest mode) not started
- Wrong port number
- VM not fully booted

**Solutions:**
- Wait for VM to boot completely
- Verify infra.operator service is running
- Check port number matches

**Symptoms:** `dial unix: no such file or directory`

**Causes:**
- Firecracker not started
- Wrong socket path
- VM crashed

**Solutions:**
- Verify Firecracker process is running
- Check socket path in Firecracker config
- Check Firecracker logs for errors

**Symptoms:** Connection hangs after sending job

**Causes:**
- infra.operator (guest mode) crashed
- Code execution taking too long
- Message framing mismatch

**Solutions:**
- Check infra.operator guest logs
- Verify timeout setting
- Ensure length-prefix framing matches


