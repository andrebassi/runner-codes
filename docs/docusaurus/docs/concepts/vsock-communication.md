---
title: 'Vsock Communication'
description: 'Host-guest communication using virtio-vsock'
---

## What is Vsock?

Virtio-vsock is a host/guest communication device that provides a socket-like interface for communication between a virtual machine and its host without requiring a network stack.

## How It Works

![Vsock Communication](/img/vsock-communication.svg)

## Addressing

Vsock uses a simple addressing scheme:

| Component | Description |
|-----------|-------------|
| CID (Context ID) | Identifies the endpoint (host or guest) |
| Port | Application-level port number |

### Reserved CIDs

| CID | Meaning |
|-----|---------|
| 0 | Hypervisor (reserved) |
| 1 | Reserved |
| 2 | Host |
| 3+ | Guest VMs |

:::note

  Firecracker uses CID=3 for the guest by default. This is configured via the `/vsock` API endpoint.

:::

## Communication Flow in LLM-Firecracker

**Step 1:** When configuring the VM, Firecracker creates a Unix socket:

```text title="Vsock Unix socket path"
/tmp/fc-{instance_id}.vsock
```

**Step 2:** Inside the VM, infra.operator (guest mode) starts a vsock server:

```go title="Guest vsock listener"
listener, _ := vsock.Listen(3, 5000)
conn, _ := listener.Accept()
```

**Step 3:** Host component connects to Firecracker's Unix socket:

```go title="Host connects to UDS"
conn, _ := net.Dial("unix", "/tmp/fc-12345.vsock")
```

**Step 4:** Host requests connection to guest port:

```text title="CONNECT command"
CONNECT 5000\n
```

**Step 5:** Firecracker multiplexes the connection:

```text title="Connection acknowledgment"
OK 5000\n
```

**Step 6:** Messages flow between host and guest using length-prefixed framing.


## Firecracker Vsock Protocol

### CONNECT Command

The host sends a text command to connect to a specific guest port:

```text title="CONNECT command format"
CONNECT <port>\n
```

Example:
```text title="CONNECT example"
CONNECT 5000\n
```

### Response

On success:
```text title="Successful connection response"
OK 5000\n
```

On failure:
```text title="Connection failed response"
NO 5000\n
```

### After Handshake

After successful connection, the socket becomes a raw byte stream. LLM-Firecracker uses length-prefixed JSON messages.

## Message Framing

All messages use a 4-byte big-endian length prefix:

![Vsock Message Framing](/img/vsock-framing.svg)

### Example

Message:
```json title="Example message payload"
{"trace_id":"t1","lang":"python","code":"print(1)"}
```

Wire format:
```text title="Message wire format with length prefix"
00 00 00 31  # Length: 49 bytes (big-endian)
7B 22 74 72 61 63 65 5F 69 64 22 ...  # JSON payload
```

## Implementation

### Host Side (Go)

```go title="Host-side vsock client implementation"
package main

import (
    "bufio"
    "encoding/binary"
    "encoding/json"
    "fmt"
    "net"
)

// Connect to guest via vsock UDS
func connectToGuest(socketPath string, port int) (net.Conn, error) {
    // Connect to Firecracker's vsock UDS
    conn, err := net.Dial("unix", socketPath)
    if err != nil {
        return nil, err
    }

    // Send CONNECT command
    _, err = fmt.Fprintf(conn, "CONNECT %d\n", port)
    if err != nil {
        conn.Close()
        return nil, err
    }

    // Read response
    reader := bufio.NewReader(conn)
    response, err := reader.ReadString('\n')
    if err != nil {
        conn.Close()
        return nil, err
    }

    // Check for OK
    if response[:2] != "OK" {
        conn.Close()
        return nil, fmt.Errorf("connect failed: %s", response)
    }

    return conn, nil
}

// Send length-prefixed message
func sendMessage(conn net.Conn, data interface{}) error {
    payload, err := json.Marshal(data)
    if err != nil {
        return err
    }

    // Write 4-byte length prefix
    length := make([]byte, 4)
    binary.BigEndian.PutUint32(length, uint32(len(payload)))
    if _, err := conn.Write(length); err != nil {
        return err
    }

    // Write payload
    _, err = conn.Write(payload)
    return err
}

// Receive length-prefixed message
func recvMessage(conn net.Conn, result interface{}) error {
    // Read 4-byte length prefix
    lengthBuf := make([]byte, 4)
    if _, err := conn.Read(lengthBuf); err != nil {
        return err
    }
    length := binary.BigEndian.Uint32(lengthBuf)

    // Read payload
    payload := make([]byte, length)
    if _, err := conn.Read(payload); err != nil {
        return err
    }

    return json.Unmarshal(payload, result)
}
```

### Guest Side (Go)

```go title="Guest-side vsock server implementation"
package main

import (
    "encoding/binary"
    "encoding/json"
    "log"
    "net"

    "github.com/mdlayher/vsock"
)

func main() {
    // Listen on vsock port 5000
    listener, err := vsock.Listen(3, 5000)
    if err != nil {
        log.Fatal(err)
    }
    defer listener.Close()

    log.Println("Listening on vsock CID=3, port=5000")

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

    // Read length
    lengthBuf := make([]byte, 4)
    conn.Read(lengthBuf)
    length := binary.BigEndian.Uint32(lengthBuf)

    // Read payload
    payload := make([]byte, length)
    conn.Read(payload)

    // Parse job
    var job map[string]interface{}
    json.Unmarshal(payload, &job)

    // Execute and respond...
    result := map[string]interface{}{
        "trace_id":  job["trace_id"],
        "stdout":    "Hello\n",
        "stderr":    "",
        "exit_code": 0,
    }

    // Send response
    respPayload, _ := json.Marshal(result)
    respLen := make([]byte, 4)
    binary.BigEndian.PutUint32(respLen, uint32(len(respPayload)))
    conn.Write(respLen)
    conn.Write(respPayload)
}
```

## Comparison with Alternatives

| Method | Latency | Setup | Security | Use Case |
|--------|---------|-------|----------|----------|
| **Vsock** | ~0.1ms | Low | High | Firecracker, direct VM-host |
| TCP/IP | ~1ms | Medium | Medium | Network-accessible VMs |
| Shared Memory | ~0.01ms | High | Low | High-performance IPC |
| Serial Console | ~10ms | Very Low | Medium | Debug, low bandwidth |

## Debugging

### Check Vsock Socket

```bash title="Verify socket exists"
ls -la /tmp/fc-*.vsock
```

```bash title="Test connection with netcat"
echo -e "CONNECT 5000\n" | nc -U /tmp/fc-12345.vsock
```

### Monitor Traffic

```bash title="Trace system calls"
# Trace system calls
sudo strace -e read,write -s 1000 -p $(pgrep infra.operator)
```

### Guest-Side Verification

```bash title="Inside VM, check vsock module"
lsmod | grep vsock
```

```bash title="Check listening"
ss -lnp | grep vsock
```

## Troubleshooting

### Connection Refused

**Error:** `NO 5000\n` or connection timeout

**Causes:**
- infra.operator (guest mode) not started
- Wrong port number
- VM not fully booted

**Solutions:**
- Wait for VM to fully boot (check dmesg)
- Verify infra.operator service is enabled
- Check port number in both host and guest

### Socket Not Found

**Error:** `dial unix: no such file or directory`

**Causes:**
- Firecracker not started
- Vsock not configured
- Wrong socket path

**Solutions:**
- Verify Firecracker process is running
- Check vsock configuration in API calls
- Ensure uds_path matches expected path

### Message Format Errors

**Error:** JSON unmarshal error or garbage data

**Causes:**
- Length prefix mismatch
- Byte order wrong
- Incomplete read

**Solutions:**
- Ensure both sides use big-endian
- Use `io.ReadFull` for complete reads
- Verify message framing matches


## Performance

| Operation | Latency |
|-----------|---------|
| Connect to UDS | < 1ms |
| CONNECT handshake | < 1ms |
| Small message (< 1KB) | < 0.1ms |
| Large message (1MB) | ~5ms |

:::tip

  For maximum performance, reuse connections instead of reconnecting for each message.

:::
