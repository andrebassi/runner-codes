# Vsock Protocol Deep Dive

## Table of Contents

1. [Overview](#overview)
2. [AF_VSOCK Socket Family](#af_vsock-socket-family)
3. [Kernel Implementation](#kernel-implementation)
4. [Virtio-vsock Device](#virtio-vsock-device)
5. [Connection Lifecycle](#connection-lifecycle)
6. [Flow Control](#flow-control)
7. [Go Implementation](#go-implementation)
8. [Debugging](#debugging)

---

## Overview

AF_VSOCK (Address Family - Virtual Socket) provides bidirectional communication between host and guest without requiring network configuration. It was designed by VMware and later adopted by other hypervisors including QEMU/KVM and Firecracker.

### Key Properties

```
┌─────────────────────────────────────────────────────────────┐
│                    VSOCK PROPERTIES                          │
│                                                              │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  Transport Layer: SOCK_STREAM (connection-oriented)  │    │
│  │  Addressing: CID (Context ID) + Port                 │    │
│  │  No IP stack required                                │    │
│  │  Zero-copy capable (with virtio optimizations)       │    │
│  │  Secure: no network exposure                         │    │
│  │  Low latency: ~10-50 microseconds                    │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  Use Cases:                                                  │
│  - Host-guest RPC                                            │
│  - Guest agent communication                                 │
│  - Metrics/logging export                                    │
│  - Secure secrets injection                                  │
│  - Code execution (this project!)                           │
└─────────────────────────────────────────────────────────────┘
```

---

## AF_VSOCK Socket Family

### Address Structure

```c
#include <linux/vm_sockets.h>

struct sockaddr_vm {
    sa_family_t    svm_family;     // AF_VSOCK (40)
    unsigned short svm_reserved1;  // Always 0
    unsigned int   svm_port;       // Port number (32-bit)
    unsigned int   svm_cid;        // Context ID (32-bit)
    unsigned char  svm_zero[sizeof(struct sockaddr) -
                            sizeof(sa_family_t) -
                            sizeof(unsigned short) -
                            sizeof(unsigned int) -
                            sizeof(unsigned int)];
};

// Socket creation
int sock = socket(AF_VSOCK, SOCK_STREAM, 0);

// Bind (server)
struct sockaddr_vm addr = {
    .svm_family = AF_VSOCK,
    .svm_port = 5000,
    .svm_cid = VMADDR_CID_ANY,  // Accept from any CID
};
bind(sock, (struct sockaddr *)&addr, sizeof(addr));
listen(sock, 10);

// Connect (client)
struct sockaddr_vm addr = {
    .svm_family = AF_VSOCK,
    .svm_port = 5000,
    .svm_cid = 3,  // Guest CID
};
connect(sock, (struct sockaddr *)&addr, sizeof(addr));
```

### Reserved CIDs

```
┌─────────────────────────────────────────────────────────────┐
│                    CONTEXT IDs (CID)                         │
│                                                              │
│  ┌────────────────┬────────────────────────────────────┐    │
│  │ CID            │ Description                        │    │
│  ├────────────────┼────────────────────────────────────┤    │
│  │ -1 (0xFFFFFFFF)│ VMADDR_CID_ANY - Any CID          │    │
│  │ 0              │ VMADDR_CID_HYPERVISOR             │    │
│  │ 1              │ VMADDR_CID_LOCAL (loopback)       │    │
│  │ 2              │ VMADDR_CID_HOST                   │    │
│  │ 3+             │ Guest VMs (assigned by VMM)       │    │
│  └────────────────┴────────────────────────────────────┘    │
│                                                              │
│  Firecracker Default:                                        │
│  - Guest CID = 3 (configurable via /vsock API)              │
│  - Host uses CID 2 when connecting to guest                 │
│                                                              │
│  Connection Direction:                                       │
│  - Host → Guest: connect(CID=3, port=5000)                  │
│  - Guest → Host: connect(CID=2, port=5000)                  │
└─────────────────────────────────────────────────────────────┘
```

### Port Allocation

```
┌─────────────────────────────────────────────────────────────┐
│                    PORT NUMBERS                              │
│                                                              │
│  ┌────────────────────┬────────────────────────────────┐    │
│  │ Range              │ Description                    │    │
│  ├────────────────────┼────────────────────────────────┤    │
│  │ 0 - 1023           │ Privileged (require root)      │    │
│  │ 1024 - 49151       │ Registered services            │    │
│  │ 49152 - 65535      │ Dynamic/private                │    │
│  │ > 65535            │ Extended range (32-bit ports)  │    │
│  └────────────────────┴────────────────────────────────┘    │
│                                                              │
│  LLM-FireSandbox:                                            │
│  - Port 5000: GuestRunner service                           │
│  - Port 5001+: Reserved for future services                 │
└─────────────────────────────────────────────────────────────┘
```

---

## Kernel Implementation

### Host-side (vhost-vsock)

```
┌─────────────────────────────────────────────────────────────┐
│                    HOST KERNEL STACK                         │
│                                                              │
│  User Space                                                  │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  host-agent (Go application)                         │    │
│  │    └── vsock.Dial(CID=3, port=5000)                 │    │
│  └─────────────────────────────────────────────────────┘    │
│                    │ socket(), connect()                     │
│                    ▼                                         │
│  Kernel Space                                                │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  AF_VSOCK Socket Layer                               │    │
│  │  (net/vmw_vsock/af_vsock.c)                         │    │
│  │    └── Connection state machine                      │    │
│  │    └── Socket buffer management                      │    │
│  └─────────────────────────────────────────────────────┘    │
│                    │                                         │
│                    ▼                                         │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  VSOCK Transport Layer                               │    │
│  │  (virtio_transport_common.c)                        │    │
│  │    └── Packet framing                               │    │
│  │    └── Flow control (credit-based)                  │    │
│  └─────────────────────────────────────────────────────┘    │
│                    │                                         │
│                    ▼                                         │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  vhost-vsock                                         │    │
│  │  (drivers/vhost/vsock.c)                            │    │
│  │    └── vring handling                               │    │
│  │    └── eventfd signaling                            │    │
│  │    └── Memory mapping to guest                      │    │
│  └─────────────────────────────────────────────────────┘    │
│                    │                                         │
│                    ▼                                         │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  /dev/vhost-vsock                                    │    │
│  │  (Firecracker opens this device)                    │    │
│  └─────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────┘
```

### Guest-side (virtio-vsock)

```
┌─────────────────────────────────────────────────────────────┐
│                    GUEST KERNEL STACK                        │
│                                                              │
│  User Space                                                  │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  guest-runner (Go application)                       │    │
│  │    └── vsock.Listen(port=5000)                      │    │
│  └─────────────────────────────────────────────────────┘    │
│                    │ socket(), bind(), listen()              │
│                    ▼                                         │
│  Kernel Space                                                │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  AF_VSOCK Socket Layer                               │    │
│  │  (net/vmw_vsock/af_vsock.c)                         │    │
│  └─────────────────────────────────────────────────────┘    │
│                    │                                         │
│                    ▼                                         │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  VSOCK Transport Layer                               │    │
│  │  (virtio_transport_common.c)                        │    │
│  └─────────────────────────────────────────────────────┘    │
│                    │                                         │
│                    ▼                                         │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  virtio-vsock Driver                                 │    │
│  │  (drivers/net/virtio_vsock.c)                       │    │
│  │    └── virtqueue management                         │    │
│  │    └── Interrupt handling                           │    │
│  └─────────────────────────────────────────────────────┘    │
│                    │                                         │
│                    ▼                                         │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  Virtio-MMIO                                         │    │
│  │  (MMIO address: 0xD0002000)                         │    │
│  └─────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────┘
```

### Kernel Configuration

```
# Required kernel config for guest
CONFIG_VSOCKETS=y
CONFIG_VIRTIO_VSOCKETS=y
CONFIG_VIRTIO_VSOCKETS_COMMON=y

# Required kernel config for host
CONFIG_VSOCKETS=y
CONFIG_VHOST_VSOCK=m  # or =y
CONFIG_VIRTIO_VSOCKETS_COMMON=y

# Load host modules
modprobe vhost_vsock
```

---

## Virtio-vsock Device

### Device Configuration

```
┌─────────────────────────────────────────────────────────────┐
│                    VIRTIO-VSOCK DEVICE                       │
│                                                              │
│  Device ID: 19 (0x13)                                        │
│  Vendor ID: 0x1AF4 (Red Hat / virtio)                       │
│                                                              │
│  Feature Bits:                                               │
│  ┌─────────────┬───────────────────────────────────────┐    │
│  │ Bit 0       │ VIRTIO_VSOCK_F_STREAM (always set)    │    │
│  │ Bit 1       │ VIRTIO_VSOCK_F_SEQPACKET (optional)   │    │
│  └─────────────┴───────────────────────────────────────┘    │
│                                                              │
│  Config Space (offset 0x100):                               │
│  ┌─────────────┬───────────────────────────────────────┐    │
│  │ Offset 0x0  │ guest_cid (64-bit LE)                 │    │
│  └─────────────┴───────────────────────────────────────┘    │
│                                                              │
│  Virtqueues:                                                 │
│  ┌─────────────┬───────────────────────────────────────┐    │
│  │ Queue 0     │ RX (receive from host)                │    │
│  │ Queue 1     │ TX (transmit to host)                 │    │
│  │ Queue 2     │ Event (state change notifications)    │    │
│  └─────────────┴───────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────┘
```

### Packet Format

```
┌─────────────────────────────────────────────────────────────┐
│                    VSOCK PACKET HEADER                       │
│                         (44 bytes)                           │
│                                                              │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  Offset │ Size │ Field       │ Description          │    │
│  ├─────────┼──────┼─────────────┼──────────────────────┤    │
│  │  0      │ 8    │ src_cid     │ Source CID           │    │
│  │  8      │ 8    │ dst_cid     │ Destination CID      │    │
│  │  16     │ 4    │ src_port    │ Source port          │    │
│  │  20     │ 4    │ dst_port    │ Destination port     │    │
│  │  24     │ 4    │ len         │ Payload length       │    │
│  │  28     │ 2    │ type        │ Socket type          │    │
│  │  30     │ 2    │ op          │ Operation            │    │
│  │  32     │ 4    │ flags       │ Flags                │    │
│  │  36     │ 4    │ buf_alloc   │ Buffer space alloc   │    │
│  │  40     │ 4    │ fwd_cnt     │ Forward count        │    │
│  └─────────┴──────┴─────────────┴──────────────────────┘    │
│                                                              │
│  Followed by payload (0 to len bytes)                       │
│                                                              │
│  Type Values:                                                │
│  ┌─────────┬────────────────────────────────────────────┐   │
│  │ 1       │ VIRTIO_VSOCK_TYPE_STREAM                   │   │
│  │ 2       │ VIRTIO_VSOCK_TYPE_SEQPACKET               │   │
│  └─────────┴────────────────────────────────────────────┘   │
│                                                              │
│  Operation Values:                                           │
│  ┌─────────┬────────────────────────────────────────────┐   │
│  │ 0       │ VIRTIO_VSOCK_OP_INVALID                    │   │
│  │ 1       │ VIRTIO_VSOCK_OP_REQUEST   (connect)        │   │
│  │ 2       │ VIRTIO_VSOCK_OP_RESPONSE  (accept)         │   │
│  │ 3       │ VIRTIO_VSOCK_OP_RST       (reset)          │   │
│  │ 4       │ VIRTIO_VSOCK_OP_SHUTDOWN  (half-close)     │   │
│  │ 5       │ VIRTIO_VSOCK_OP_RW        (data)           │   │
│  │ 6       │ VIRTIO_VSOCK_OP_CREDIT_UPDATE              │   │
│  │ 7       │ VIRTIO_VSOCK_OP_CREDIT_REQUEST             │   │
│  └─────────┴────────────────────────────────────────────┘   │
│                                                              │
│  Shutdown Flags:                                             │
│  ┌─────────┬────────────────────────────────────────────┐   │
│  │ Bit 0   │ VIRTIO_VSOCK_SHUTDOWN_RCV (no more recv)   │   │
│  │ Bit 1   │ VIRTIO_VSOCK_SHUTDOWN_SEND (no more send)  │   │
│  └─────────┴────────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
```

---

## Connection Lifecycle

### Connection Establishment

```
┌─────────────────────────────────────────────────────────────┐
│                    CONNECTION HANDSHAKE                      │
│                                                              │
│  Host (Client)                    Guest (Server)             │
│  ┌──────────────┐                ┌──────────────┐           │
│  │   CLOSED     │                │   LISTEN     │           │
│  └──────┬───────┘                └──────┬───────┘           │
│         │                               │                    │
│         │ connect()                     │                    │
│         │                               │                    │
│  ┌──────▼───────┐                       │                    │
│  │  SYN_SENT    │                       │                    │
│  └──────┬───────┘                       │                    │
│         │                               │                    │
│         │ ─────── REQUEST ──────────────▶                    │
│         │       (op=1, flags=0)         │                    │
│         │                               │                    │
│         │                        ┌──────▼───────┐           │
│         │                        │   SYN_RECV   │           │
│         │                        └──────┬───────┘           │
│         │                               │                    │
│         │ ◀────── RESPONSE ─────────────│                    │
│         │       (op=2, flags=0)         │                    │
│         │                               │                    │
│  ┌──────▼───────┐                ┌──────▼───────┐           │
│  │ ESTABLISHED  │                │ ESTABLISHED  │           │
│  └──────────────┘                └──────────────┘           │
│         │                               │                    │
│         │ ◀══════ DATA (op=5) ═════════▶│                    │
│         │                               │                    │
└─────────────────────────────────────────────────────────────┘
```

### Connection Termination

```
┌─────────────────────────────────────────────────────────────┐
│                    CONNECTION TEARDOWN                       │
│                                                              │
│  Graceful Shutdown (half-close):                            │
│                                                              │
│  Host                                 Guest                  │
│  ┌──────────────┐                ┌──────────────┐           │
│  │ ESTABLISHED  │                │ ESTABLISHED  │           │
│  └──────┬───────┘                └──────┬───────┘           │
│         │                               │                    │
│         │ close(SHUT_WR)                │                    │
│         │                               │                    │
│         │ ─────── SHUTDOWN ─────────────▶                    │
│         │    (op=4, flags=SEND)         │                    │
│         │                               │                    │
│  ┌──────▼───────┐                ┌──────▼───────┐           │
│  │  FIN_WAIT    │                │  CLOSE_WAIT  │           │
│  └──────┬───────┘                └──────┬───────┘           │
│         │                               │                    │
│         │                               │ close()           │
│         │                               │                    │
│         │ ◀────── SHUTDOWN ─────────────│                    │
│         │    (op=4, flags=SEND)         │                    │
│         │                               │                    │
│  ┌──────▼───────┐                ┌──────▼───────┐           │
│  │    CLOSED    │                │    CLOSED    │           │
│  └──────────────┘                └──────────────┘           │
│                                                              │
│  Abrupt Reset:                                               │
│                                                              │
│         │ ─────── RST (op=3) ───────────▶                    │
│         │    (immediate termination)    │                    │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

---

## Flow Control

Vsock uses credit-based flow control to prevent buffer overflow.

### Credit System

```
┌─────────────────────────────────────────────────────────────┐
│                    CREDIT-BASED FLOW CONTROL                 │
│                                                              │
│  Concept:                                                    │
│  - Each endpoint advertises buffer space (buf_alloc)        │
│  - Sender tracks how much peer has consumed (fwd_cnt)       │
│  - Available credit = buf_alloc - (tx_cnt - fwd_cnt)        │
│  - Sender blocks when credit exhausted                      │
│                                                              │
│  ┌─────────────────────────────────────────────────────┐    │
│  │                    SENDER                            │    │
│  │                                                      │    │
│  │  tx_cnt: bytes sent so far                          │    │
│  │  peer_buf_alloc: peer's buffer size                 │    │
│  │  peer_fwd_cnt: bytes peer has consumed              │    │
│  │                                                      │    │
│  │  credit = peer_buf_alloc - (tx_cnt - peer_fwd_cnt)  │    │
│  │                                                      │    │
│  │  if credit <= 0:                                     │    │
│  │      wait for CREDIT_UPDATE from peer               │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  ┌─────────────────────────────────────────────────────┐    │
│  │                    RECEIVER                          │    │
│  │                                                      │    │
│  │  buf_alloc: our buffer size (e.g., 262144 = 256KB)  │    │
│  │  fwd_cnt: bytes we've consumed (read by app)        │    │
│  │                                                      │    │
│  │  When app reads data:                               │    │
│  │      fwd_cnt += bytes_read                          │    │
│  │      if should_send_credit_update():                │    │
│  │          send CREDIT_UPDATE (op=6)                  │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  Packet Fields Used:                                         │
│  - buf_alloc: receiver's total buffer size                  │
│  - fwd_cnt: receiver's consumed byte count                  │
│                                                              │
│  Every DATA packet (op=5) includes current buf_alloc and    │
│  fwd_cnt, so credit updates happen implicitly with data.    │
└─────────────────────────────────────────────────────────────┘
```

### Flow Control Example

```
┌─────────────────────────────────────────────────────────────┐
│                    FLOW CONTROL EXAMPLE                      │
│                                                              │
│  Initial State:                                              │
│  - Guest buf_alloc = 65536 (64 KB)                          │
│  - Host tx_cnt = 0                                          │
│  - Host knows peer_fwd_cnt = 0                              │
│  - Credit = 65536 - (0 - 0) = 65536 bytes available         │
│                                                              │
│  Step 1: Host sends 32768 bytes                             │
│  - Host tx_cnt = 32768                                      │
│  - Credit = 65536 - (32768 - 0) = 32768 bytes remaining     │
│                                                              │
│  Step 2: Host sends another 32768 bytes                     │
│  - Host tx_cnt = 65536                                      │
│  - Credit = 65536 - (65536 - 0) = 0 bytes remaining         │
│  - Host BLOCKS (cannot send more)                           │
│                                                              │
│  Step 3: Guest app reads 16384 bytes                        │
│  - Guest fwd_cnt = 16384                                    │
│  - Guest sends CREDIT_UPDATE with fwd_cnt=16384             │
│                                                              │
│  Step 4: Host receives CREDIT_UPDATE                        │
│  - Host peer_fwd_cnt = 16384                                │
│  - Credit = 65536 - (65536 - 16384) = 16384 bytes           │
│  - Host UNBLOCKS (can send 16384 more bytes)                │
│                                                              │
└─────────────────────────────────────────────────────────────┘
```

---

## Go Implementation

### mdlayher/vsock Library

```go
// https://github.com/mdlayher/vsock

import "github.com/mdlayher/vsock"

// Server (Guest-side)
func server() {
    // Listen on port 5000, accept from any CID
    listener, err := vsock.Listen(5000, nil)
    if err != nil {
        log.Fatal(err)
    }
    defer listener.Close()

    for {
        conn, err := listener.Accept()
        if err != nil {
            log.Println("accept error:", err)
            continue
        }

        // conn is *vsock.Conn, implements net.Conn
        go handleConnection(conn)
    }
}

// Client (Host-side)
func client() {
    // Connect to guest CID 3, port 5000
    conn, err := vsock.Dial(3, 5000, nil)
    if err != nil {
        log.Fatal(err)
    }
    defer conn.Close()

    // Use like any net.Conn
    conn.Write([]byte("hello"))

    buf := make([]byte, 1024)
    n, _ := conn.Read(buf)
    fmt.Println(string(buf[:n]))
}
```

### Low-level Syscalls

```go
// How mdlayher/vsock works internally (simplified)

import (
    "syscall"
    "unsafe"
)

const (
    AF_VSOCK = 40
    VMADDR_CID_ANY = 0xFFFFFFFF
)

type sockaddrVM struct {
    Family    uint16
    Reserved1 uint16
    Port      uint32
    CID       uint32
    Zero      [4]byte
}

func dial(cid, port uint32) (int, error) {
    // Create socket
    fd, err := syscall.Socket(AF_VSOCK, syscall.SOCK_STREAM, 0)
    if err != nil {
        return -1, err
    }

    // Prepare address
    addr := sockaddrVM{
        Family: AF_VSOCK,
        Port:   port,
        CID:    cid,
    }

    // Connect
    _, _, errno := syscall.Syscall(
        syscall.SYS_CONNECT,
        uintptr(fd),
        uintptr(unsafe.Pointer(&addr)),
        unsafe.Sizeof(addr),
    )
    if errno != 0 {
        syscall.Close(fd)
        return -1, errno
    }

    return fd, nil
}
```

### Connection with Timeout

```go
func dialWithTimeout(cid, port uint32, timeout time.Duration) (*vsock.Conn, error) {
    // Create a context with timeout
    ctx, cancel := context.WithTimeout(context.Background(), timeout)
    defer cancel()

    // Channel for result
    type result struct {
        conn *vsock.Conn
        err  error
    }
    ch := make(chan result, 1)

    // Dial in goroutine
    go func() {
        conn, err := vsock.Dial(cid, port, nil)
        ch <- result{conn, err}
    }()

    // Wait for result or timeout
    select {
    case r := <-ch:
        return r.conn, r.err
    case <-ctx.Done():
        return nil, ctx.Err()
    }
}
```

---

## Debugging

### Host-side Tools

```bash
# Check if vsock module is loaded
lsmod | grep vsock

# Check vsock device
ls -la /dev/vhost-vsock

# View vsock connections (requires ss with vsock support)
ss -x | grep vsock

# Trace vsock syscalls
strace -e socket,connect,bind,listen,accept \
    -p $(pgrep host-agent)

# Debug with socat (if vsock support compiled in)
socat - VSOCK-CONNECT:3:5000
```

### Guest-side Tools

```bash
# Check virtio-vsock driver
lsmod | grep virtio_vsock

# View vsock device
ls -la /sys/class/vsock/

# Check CID
cat /sys/class/vsock/vsock0/guest_cid

# List vsock sockets
ss -a --vsock

# Test connection
nc-vsock 2 5000  # Connect to host CID 2, port 5000
```

### Common Issues

```
┌─────────────────────────────────────────────────────────────┐
│                    TROUBLESHOOTING                           │
│                                                              │
│  Issue: "dial vsock: connection refused"                    │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  Causes:                                             │    │
│  │  - GuestRunner not started                          │    │
│  │  - Wrong port number                                │    │
│  │  - Guest not fully booted                           │    │
│  │  Solution: Wait for guest boot, verify port         │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  Issue: "dial vsock: no such device"                        │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  Causes:                                             │    │
│  │  - vhost_vsock module not loaded                    │    │
│  │  - /dev/vhost-vsock not accessible                  │    │
│  │  Solution: modprobe vhost_vsock                     │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  Issue: "vsock: operation not permitted"                    │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  Causes:                                             │    │
│  │  - Running as non-root without capabilities         │    │
│  │  - Seccomp blocking vsock syscalls                  │    │
│  │  Solution: Run as root or adjust capabilities       │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  Issue: Connection timeout                                   │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  Causes:                                             │    │
│  │  - VM crashed or not responding                     │    │
│  │  - virtio-vsock driver issue in guest               │    │
│  │  - Wrong CID                                        │    │
│  │  Solution: Check VM status, verify CID              │    │
│  └─────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────┘
```

### Debug Logging

```go
// Enable debug logging in guest-runner
func handleConnection(conn *vsock.Conn) {
    // Log connection details
    local := conn.LocalAddr().(*vsock.Addr)
    remote := conn.RemoteAddr().(*vsock.Addr)

    log.Printf("Connection: local CID=%d port=%d, remote CID=%d port=%d",
        local.ContextID, local.Port,
        remote.ContextID, remote.Port)

    // ... handle connection ...
}
```
