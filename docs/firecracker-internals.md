# Firecracker Internals

## Table of Contents

1. [Firecracker Overview](#firecracker-overview)
2. [KVM Interface](#kvm-interface)
3. [Device Model](#device-model)
4. [Virtio Implementation](#virtio-implementation)
5. [API Server](#api-server)
6. [Jailer](#jailer)
7. [Snapshots](#snapshots)
8. [Rate Limiters](#rate-limiters)

---

## Firecracker Overview

Firecracker is a Virtual Machine Monitor (VMM) built on top of Linux KVM. It is designed for serverless workloads requiring strong security, minimal overhead, and fast startup times.

### Architecture Overview

```
┌─────────────────────────────────────────────────────────────┐
│                    FIRECRACKER PROCESS                       │
│                                                              │
│  ┌─────────────────────────────────────────────────────┐    │
│  │                    MAIN THREAD                       │    │
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  │    │
│  │  │  API Server │  │   VMM Core  │  │   Event     │  │    │
│  │  │  (HTTP/UDS) │  │   (Rust)    │  │   Loop      │  │    │
│  │  └──────┬──────┘  └──────┬──────┘  └──────┬──────┘  │    │
│  │         │                │                │         │    │
│  │         ▼                ▼                ▼         │    │
│  │  ┌─────────────────────────────────────────────┐   │    │
│  │  │              EVENT MANAGER                   │   │    │
│  │  │  (epoll-based async I/O multiplexing)       │   │    │
│  │  └─────────────────────────────────────────────┘   │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  ┌─────────────────────────────────────────────────────┐    │
│  │                    vCPU THREADS                      │    │
│  │  ┌─────────────┐  ┌─────────────┐                   │    │
│  │  │   vCPU 0    │  │   vCPU 1    │  ...              │    │
│  │  │  (KVM_RUN)  │  │  (KVM_RUN)  │                   │    │
│  │  └─────────────┘  └─────────────┘                   │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  ┌─────────────────────────────────────────────────────┐    │
│  │                    DEVICE MODEL                      │    │
│  │  ┌──────┐ ┌──────┐ ┌──────┐ ┌──────┐ ┌──────────┐  │    │
│  │  │Serial│ │ RTC  │ │v-blk │ │v-net │ │ v-vsock  │  │    │
│  │  └──────┘ └──────┘ └──────┘ └──────┘ └──────────┘  │    │
│  └─────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────┘
        │                    │
        │ ioctl(KVM_*)       │ ioctl(KVM_RUN)
        ▼                    ▼
┌─────────────────────────────────────────────────────────────┐
│                      LINUX KERNEL                            │
│  ┌─────────────────────────────────────────────────────┐    │
│  │                      KVM MODULE                      │    │
│  │  ┌─────────────┐  ┌─────────────┐  ┌─────────────┐  │    │
│  │  │   VMCS/     │  │   Memory    │  │   Interrupt │  │    │
│  │  │   VMCB      │  │   Manager   │  │   Injection │  │    │
│  │  │  (per vCPU) │  │   (EPT/NPT) │  │             │  │    │
│  │  └─────────────┘  └─────────────┘  └─────────────┘  │    │
│  └─────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────┘
        │
        │ VT-x/AMD-V
        ▼
┌─────────────────────────────────────────────────────────────┐
│                      HARDWARE (CPU)                          │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  VMX/SVM Extensions, EPT/NPT, APIC Virtualization   │    │
│  └─────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────┘
```

### Comparison with Other VMMs

| Feature | Firecracker | QEMU | Cloud Hypervisor |
|---------|-------------|------|------------------|
| Language | Rust | C | Rust |
| Boot time | ~125ms | ~500ms+ | ~150ms |
| Memory | ~5 MiB | ~30+ MiB | ~10 MiB |
| Devices | Minimal | Full | Moderate |
| Seccomp | Yes | Optional | Yes |
| Jailer | Built-in | No | No |
| Snapshots | Yes | Yes | Yes |
| PCI | No | Yes | Yes |
| ACPI | No | Yes | Optional |

---

## KVM Interface

Firecracker uses the Linux KVM (Kernel-based Virtual Machine) interface to create and manage virtual machines.

### KVM System Calls

```c
// Open KVM device
int kvm_fd = open("/dev/kvm", O_RDWR);

// Create VM
int vm_fd = ioctl(kvm_fd, KVM_CREATE_VM, 0);

// Configure VM memory
struct kvm_userspace_memory_region region = {
    .slot = 0,
    .flags = 0,
    .guest_phys_addr = 0,
    .memory_size = 512 * 1024 * 1024,  // 512 MiB
    .userspace_addr = (uint64_t)mmap(...),
};
ioctl(vm_fd, KVM_SET_USER_MEMORY_REGION, &region);

// Create vCPU
int vcpu_fd = ioctl(vm_fd, KVM_CREATE_VCPU, 0);

// Configure vCPU
struct kvm_sregs sregs;
ioctl(vcpu_fd, KVM_GET_SREGS, &sregs);
// ... setup segments, control registers ...
ioctl(vcpu_fd, KVM_SET_SREGS, &sregs);

struct kvm_regs regs = {
    .rip = 0x100000,  // Kernel entry point
    .rflags = 0x2,
};
ioctl(vcpu_fd, KVM_SET_REGS, &regs);

// Run vCPU (in vCPU thread)
struct kvm_run *run = mmap(..., vcpu_mmap_size, ...);
while (1) {
    ioctl(vcpu_fd, KVM_RUN, NULL);
    switch (run->exit_reason) {
        case KVM_EXIT_IO:
            // Handle port I/O
            break;
        case KVM_EXIT_MMIO:
            // Handle memory-mapped I/O
            break;
        case KVM_EXIT_HLT:
            // CPU halted
            break;
        // ...
    }
}
```

### VM Exit Handling

```
┌─────────────────────────────────────────────────────────────┐
│                    KVM_RUN LOOP                              │
│                                                              │
│  ┌─────────────────┐                                        │
│  │   Guest Mode    │◄──────────────────────────────┐        │
│  │  (ring 0 guest) │                               │        │
│  └────────┬────────┘                               │        │
│           │ VM Exit                                │        │
│           ▼                                        │        │
│  ┌─────────────────┐                               │        │
│  │   KVM Handler   │                               │        │
│  │   (kernel)      │                               │        │
│  └────────┬────────┘                               │        │
│           │                                        │        │
│           ├── Handled in kernel ───────────────────┤        │
│           │   (fast path: most exits)              │        │
│           │                                        │        │
│           └── Exit to userspace ──────┐            │        │
│               (slow path: I/O, etc.)  │            │        │
│                                       ▼            │        │
│                              ┌─────────────────┐   │        │
│                              │   Firecracker   │   │        │
│                              │   VMM Handler   │   │        │
│                              └────────┬────────┘   │        │
│                                       │            │        │
│                                       │ ioctl      │        │
│                                       │ KVM_RUN    │        │
│                                       └────────────┘        │
│                                                              │
│  Exit Reasons:                                               │
│  ┌──────────────────┬───────────────────────────────────┐   │
│  │ KVM_EXIT_IO      │ Port I/O (in/out instructions)    │   │
│  │ KVM_EXIT_MMIO    │ Memory-mapped I/O                 │   │
│  │ KVM_EXIT_HLT     │ HLT instruction (idle)            │   │
│  │ KVM_EXIT_IRQ_... │ Interrupt window                  │   │
│  │ KVM_EXIT_SHUTDOWN│ Triple fault                      │   │
│  │ KVM_EXIT_FAIL_..│ Entry failure                     │   │
│  └──────────────────┴───────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
```

### Memory Management (EPT/NPT)

```
┌─────────────────────────────────────────────────────────────┐
│                MEMORY ADDRESS TRANSLATION                    │
│                                                              │
│  Guest Virtual Address (GVA)                                 │
│           │                                                  │
│           │ Guest Page Tables (CR3)                          │
│           ▼                                                  │
│  Guest Physical Address (GPA)                                │
│           │                                                  │
│           │ Extended Page Tables (EPT) / Nested Page Tables  │
│           ▼                                                  │
│  Host Physical Address (HPA)                                 │
│                                                              │
│  ┌─────────────────────────────────────────────────────┐    │
│  │                    EPT STRUCTURE                     │    │
│  │                                                      │    │
│  │  EPTP ──► PML4 ──► PDPT ──► PD ──► PT ──► Page      │    │
│  │          (512)    (512)    (512)  (512)              │    │
│  │                                                      │    │
│  │  Each entry: 8 bytes                                 │    │
│  │  Bits: Read, Write, Execute, User, Accessed, Dirty   │    │
│  │                                                      │    │
│  │  Page sizes: 4KB, 2MB, 1GB                          │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  Memory Regions in Firecracker:                              │
│  ┌──────────────────┬───────────────┬───────────────────┐   │
│  │ Slot 0           │ GPA 0x0       │ Main memory       │   │
│  │ Slot 1 (opt)     │ GPA varies    │ Device MMIO       │   │
│  └──────────────────┴───────────────┴───────────────────┘   │
└─────────────────────────────────────────────────────────────┘
```

---

## Device Model

Firecracker implements a minimal device model with only essential devices.

### Device List

```
┌─────────────────────────────────────────────────────────────┐
│                    FIRECRACKER DEVICES                       │
│                                                              │
│  Legacy Devices (Port I/O):                                  │
│  ┌──────────────────┬───────────────┬───────────────────┐   │
│  │ Device           │ I/O Ports     │ Purpose           │   │
│  ├──────────────────┼───────────────┼───────────────────┤   │
│  │ Serial (COM1)    │ 0x3F8-0x3FF   │ Console output    │   │
│  │ i8042 (stub)     │ 0x60, 0x64    │ Keyboard ctrl     │   │
│  │ RTC              │ 0x70-0x71     │ Real-time clock   │   │
│  └──────────────────┴───────────────┴───────────────────┘   │
│                                                              │
│  MMIO Devices:                                               │
│  ┌──────────────────┬───────────────┬───────────────────┐   │
│  │ Device           │ MMIO Address  │ Size              │   │
│  ├──────────────────┼───────────────┼───────────────────┤   │
│  │ virtio-blk       │ 0xD0000000    │ 0x1000            │   │
│  │ virtio-net       │ 0xD0001000    │ 0x1000            │   │
│  │ virtio-vsock     │ 0xD0002000    │ 0x1000            │   │
│  │ virtio-balloon   │ 0xD0003000    │ 0x1000            │   │
│  └──────────────────┴───────────────┴───────────────────┘   │
│                                                              │
│  NOT Implemented (reduced attack surface):                   │
│  - PCI bus                                                   │
│  - ACPI                                                      │
│  - USB                                                       │
│  - VGA/GPU                                                   │
│  - Sound                                                     │
│  - Legacy IDE/SATA                                           │
└─────────────────────────────────────────────────────────────┘
```

### Serial Console Implementation

```
┌─────────────────────────────────────────────────────────────┐
│                    SERIAL CONSOLE (16550A)                   │
│                                                              │
│  Registers (I/O port 0x3F8 base):                           │
│  ┌──────────┬─────────────────────────────────────────┐     │
│  │ Offset   │ Register                                │     │
│  ├──────────┼─────────────────────────────────────────┤     │
│  │ 0x0      │ THR (Transmit) / RBR (Receive) / DLL    │     │
│  │ 0x1      │ IER (Interrupt Enable) / DLM            │     │
│  │ 0x2      │ IIR (Interrupt ID) / FCR (FIFO Ctrl)    │     │
│  │ 0x3      │ LCR (Line Control)                      │     │
│  │ 0x4      │ MCR (Modem Control)                     │     │
│  │ 0x5      │ LSR (Line Status)                       │     │
│  │ 0x6      │ MSR (Modem Status)                      │     │
│  │ 0x7      │ SCR (Scratch)                           │     │
│  └──────────┴─────────────────────────────────────────┘     │
│                                                              │
│  Data Flow:                                                  │
│  Guest write to THR ──► Firecracker ──► stdout/file         │
│  stdin/file ──► Firecracker ──► Guest read from RBR         │
│                                                              │
│  Kernel boot arg: console=ttyS0                             │
└─────────────────────────────────────────────────────────────┘
```

---

## Virtio Implementation

Firecracker uses virtio-mmio (not virtio-pci) for paravirtualized devices.

### Virtio-MMIO Layout

```
┌─────────────────────────────────────────────────────────────┐
│                    VIRTIO-MMIO REGISTERS                     │
│                                                              │
│  Offset    Register              Description                 │
│  ┌─────────┬───────────────────┬───────────────────────────┐│
│  │ 0x000   │ MagicValue        │ 0x74726976 ("virt")       ││
│  │ 0x004   │ Version           │ 0x2 (virtio 1.0)          ││
│  │ 0x008   │ DeviceID          │ 2=blk, 1=net, 19=vsock    ││
│  │ 0x00C   │ VendorID          │ Vendor identifier         ││
│  │ 0x010   │ DeviceFeatures    │ Device feature bits       ││
│  │ 0x014   │ DeviceFeaturesSel │ Feature selector          ││
│  │ 0x020   │ DriverFeatures    │ Driver accepted features  ││
│  │ 0x024   │ DriverFeaturesSel │ Feature selector          ││
│  │ 0x030   │ QueueSel          │ Queue selector            ││
│  │ 0x034   │ QueueNumMax       │ Max queue size            ││
│  │ 0x038   │ QueueNum          │ Current queue size        ││
│  │ 0x044   │ QueueReady        │ Queue ready flag          ││
│  │ 0x050   │ QueueNotify       │ Queue notification        ││
│  │ 0x060   │ InterruptStatus   │ Interrupt status          ││
│  │ 0x064   │ InterruptACK      │ Interrupt acknowledge     ││
│  │ 0x070   │ Status            │ Device status             ││
│  │ 0x080   │ QueueDescLow      │ Descriptor table addr    ││
│  │ 0x084   │ QueueDescHigh     │                          ││
│  │ 0x090   │ QueueDriverLow    │ Available ring addr      ││
│  │ 0x094   │ QueueDriverHigh   │                          ││
│  │ 0x0A0   │ QueueDeviceLow    │ Used ring addr           ││
│  │ 0x0A4   │ QueueDeviceHigh   │                          ││
│  │ 0x100+  │ Config Space      │ Device-specific config   ││
│  └─────────┴───────────────────┴───────────────────────────┘│
└─────────────────────────────────────────────────────────────┘
```

### Virtqueue Structure

```
┌─────────────────────────────────────────────────────────────┐
│                    VIRTQUEUE (vring)                         │
│                                                              │
│  ┌─────────────────────────────────────────────────────┐    │
│  │                DESCRIPTOR TABLE                      │    │
│  │  ┌─────┬─────┬─────┬─────┬─────┐                    │    │
│  │  │  0  │  1  │  2  │ ... │ N-1 │  (N = queue size)  │    │
│  │  └─────┴─────┴─────┴─────┴─────┘                    │    │
│  │                                                      │    │
│  │  struct vring_desc {                                 │    │
│  │      __le64 addr;   // Guest physical address       │    │
│  │      __le32 len;    // Buffer length                │    │
│  │      __le16 flags;  // NEXT, WRITE, INDIRECT        │    │
│  │      __le16 next;   // Next descriptor index        │    │
│  │  };                                                  │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  ┌─────────────────────────────────────────────────────┐    │
│  │                AVAILABLE RING (driver → device)      │    │
│  │  ┌───────────────────────────────────────────┐       │    │
│  │  │ flags │ idx │ ring[0] │ ring[1] │ ... │   │       │    │
│  │  └───────────────────────────────────────────┘       │    │
│  │                                                      │    │
│  │  Driver writes descriptor chain head to ring[idx]    │    │
│  │  Then increments idx                                 │    │
│  │  Then writes to QueueNotify register                 │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  ┌─────────────────────────────────────────────────────┐    │
│  │                USED RING (device → driver)           │    │
│  │  ┌───────────────────────────────────────────┐       │    │
│  │  │ flags │ idx │ ring[0] │ ring[1] │ ... │   │       │    │
│  │  └───────────────────────────────────────────┘       │    │
│  │                                                      │    │
│  │  struct vring_used_elem {                            │    │
│  │      __le32 id;    // Descriptor chain head         │    │
│  │      __le32 len;   // Bytes written                 │    │
│  │  };                                                  │    │
│  │                                                      │    │
│  │  Device writes completed descriptor to ring[idx]     │    │
│  │  Then increments idx                                 │    │
│  │  Then triggers interrupt (if enabled)                │    │
│  └─────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────┘
```

### Virtio-vsock Specifics

```
┌─────────────────────────────────────────────────────────────┐
│                    VIRTIO-VSOCK                              │
│                                                              │
│  Device ID: 19 (0x13)                                        │
│                                                              │
│  Queues:                                                     │
│  ┌──────────┬────────────────────────────────────────────┐  │
│  │ Queue 0  │ RX (host → guest packets)                  │  │
│  │ Queue 1  │ TX (guest → host packets)                  │  │
│  │ Queue 2  │ Event (connection state changes)           │  │
│  └──────────┴────────────────────────────────────────────┘  │
│                                                              │
│  Packet Header:                                              │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  struct virtio_vsock_hdr {                           │    │
│  │      __le64 src_cid;        // Source CID           │    │
│  │      __le64 dst_cid;        // Destination CID      │    │
│  │      __le32 src_port;       // Source port          │    │
│  │      __le32 dst_port;       // Destination port     │    │
│  │      __le32 len;            // Data length          │    │
│  │      __le16 type;           // STREAM (1)           │    │
│  │      __le16 op;             // Operation code       │    │
│  │      __le32 flags;          // Flags                │    │
│  │      __le32 buf_alloc;      // Buffer allocation    │    │
│  │      __le32 fwd_cnt;        // Forward count        │    │
│  │  };                                                  │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  Operations:                                                 │
│  ┌─────────────┬────────────────────────────────────────┐   │
│  │ INVALID (0) │ Invalid operation                      │   │
│  │ REQUEST (1) │ Connection request                     │   │
│  │ RESPONSE(2) │ Connection response                    │   │
│  │ RST     (3) │ Connection reset                       │   │
│  │ SHUTDOWN(4) │ Shutdown (half-close)                  │   │
│  │ RW      (5) │ Read/Write data                        │   │
│  │ CREDIT_U(6) │ Credit update                          │   │
│  │ CREDIT_R(7) │ Credit request                         │   │
│  └─────────────┴────────────────────────────────────────┘   │
└─────────────────────────────────────────────────────────────┘
```

---

## API Server

Firecracker exposes an HTTP API over a Unix domain socket.

### API Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    API SERVER                                │
│                                                              │
│  Unix Socket: /tmp/firecracker.sock (or specified path)     │
│                                                              │
│  ┌─────────────────────────────────────────────────────┐    │
│  │                  HTTP Server                         │    │
│  │  (micro_http crate - minimal HTTP 1.1)              │    │
│  └─────────────────────────────────────────────────────┘    │
│           │                                                  │
│           ▼                                                  │
│  ┌─────────────────────────────────────────────────────┐    │
│  │                  Request Router                      │    │
│  │  ┌─────────────┬─────────────────────────────────┐  │    │
│  │  │ PUT         │ /machine-config                 │  │    │
│  │  │ PUT         │ /boot-source                    │  │    │
│  │  │ PUT         │ /drives/{drive_id}              │  │    │
│  │  │ PUT/PATCH   │ /network-interfaces/{iface_id}  │  │    │
│  │  │ PUT         │ /vsock                          │  │    │
│  │  │ PUT         │ /actions                        │  │    │
│  │  │ PUT         │ /snapshot/create                │  │    │
│  │  │ PUT         │ /snapshot/load                  │  │    │
│  │  │ GET         │ /                               │  │    │
│  │  │ GET         │ /machine-config                 │  │    │
│  │  │ GET         │ /mmds                           │  │    │
│  │  │ PUT/PATCH   │ /mmds                           │  │    │
│  │  └─────────────┴─────────────────────────────────┘  │    │
│  └─────────────────────────────────────────────────────┘    │
│           │                                                  │
│           ▼                                                  │
│  ┌─────────────────────────────────────────────────────┐    │
│  │                  VMM Controllers                     │    │
│  │  (Translate API requests to VMM operations)         │    │
│  └─────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────┘
```

### API State Machine

```
┌─────────────────────────────────────────────────────────────┐
│                    VM STATE MACHINE                          │
│                                                              │
│  ┌─────────────┐                                            │
│  │   Not       │                                            │
│  │ Configured  │                                            │
│  └──────┬──────┘                                            │
│         │ PUT /machine-config                               │
│         │ PUT /boot-source                                  │
│         │ PUT /drives/*                                     │
│         ▼                                                   │
│  ┌─────────────┐                                            │
│  │ Configured  │ ◄────────────────────────────────┐        │
│  │  (Ready)    │                                  │        │
│  └──────┬──────┘                                  │        │
│         │ PUT /actions {InstanceStart}            │        │
│         ▼                                         │        │
│  ┌─────────────┐                                  │        │
│  │   Running   │                                  │        │
│  └──────┬──────┘                                  │        │
│         │                                         │        │
│         ├── PUT /snapshot/create ───► Snapshot created     │
│         │                                         │        │
│         ├── PUT /actions {SendCtrlAltDel}        │        │
│         │                                         │        │
│         ▼                                         │        │
│  ┌─────────────┐                                  │        │
│  │  Halted /   │                                  │        │
│  │ Terminated  │                                  │        │
│  └─────────────┘                                  │        │
│                                                   │        │
│  ┌─────────────┐                                  │        │
│  │  Snapshot   │ ── PUT /snapshot/load ───────────┘        │
│  │   File      │                                            │
│  └─────────────┘                                            │
└─────────────────────────────────────────────────────────────┘
```

---

## Jailer

The Jailer provides additional isolation for production deployments.

### Jailer Isolation Mechanisms

```
┌─────────────────────────────────────────────────────────────┐
│                    JAILER ISOLATION                          │
│                                                              │
│  1. CHROOT ISOLATION                                         │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  /srv/jailer/{exec_id}/{id}/root/                    │    │
│  │  ├── dev/                                            │    │
│  │  │   ├── kvm        (bind mount)                     │    │
│  │  │   ├── urandom    (bind mount)                     │    │
│  │  │   └── null       (bind mount)                     │    │
│  │  ├── firecracker    (hardlink)                       │    │
│  │  ├── kernel.bin     (hardlink)                       │    │
│  │  ├── rootfs.ext4    (hardlink)                       │    │
│  │  └── run/                                            │    │
│  │      └── firecracker.socket                          │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  2. USER NAMESPACE                                           │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  --uid <uid>          Map to unprivileged user       │    │
│  │  --gid <gid>          Map to unprivileged group      │    │
│  │  Outside: uid=0       Inside: uid=<uid>              │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  3. CGROUP LIMITS                                            │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  --cgroup <controller>=<path>                        │    │
│  │  Examples:                                           │    │
│  │    cpu.max = "100000 100000"    (100% of 1 CPU)     │    │
│  │    memory.max = "536870912"     (512 MiB)           │    │
│  │    pids.max = "100"             (max processes)     │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  4. SECCOMP-BPF                                              │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  Whitelist of ~50 syscalls                           │    │
│  │  All other syscalls → SIGSYS (kill process)         │    │
│  │                                                      │    │
│  │  Key allowed: read, write, mmap, ioctl (KVM),       │    │
│  │               epoll_*, eventfd, timerfd, ...        │    │
│  │                                                      │    │
│  │  Blocked: execve, fork, clone (no new processes)    │    │
│  │           mount, pivot_root, chroot                 │    │
│  │           ptrace, personality                       │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  5. DROPPED CAPABILITIES                                     │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  All capabilities dropped except:                    │    │
│  │    (none in default configuration)                   │    │
│  │                                                      │    │
│  │  Result: Firecracker runs as unprivileged process   │    │
│  └─────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────┘
```

### Jailer Command Line

```bash
jailer \
    --id my-vm \
    --exec-file /usr/bin/firecracker \
    --uid 1000 \
    --gid 1000 \
    --chroot-base-dir /srv/jailer \
    --daemonize \
    -- \
    --api-sock /run/firecracker.socket \
    --config-file /config.json
```

---

## Snapshots

Firecracker supports VM snapshots for fast restore.

### Snapshot Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    SNAPSHOT SYSTEM                           │
│                                                              │
│  Snapshot Creation (PUT /snapshot/create):                   │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  1. Pause vCPUs                                      │    │
│  │  2. Serialize VM state:                              │    │
│  │     ├── vCPU registers (VMCS/VMCB)                  │    │
│  │     ├── Memory (full or diff)                       │    │
│  │     ├── Device state (virtio queues, etc.)          │    │
│  │     └── KVM state (LAPIC, clock, etc.)              │    │
│  │  3. Write to snapshot file                           │    │
│  │  4. Resume vCPUs (if not halted)                     │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  Snapshot Files:                                             │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  snapshot_file:   VM state (vCPU, devices)          │    │
│  │  mem_file:        Guest memory contents             │    │
│  │  (diff optional): Memory diff from base             │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  Snapshot Restore (PUT /snapshot/load):                      │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  1. Create new Firecracker process                   │    │
│  │  2. Load VM state from snapshot file                 │    │
│  │  3. Map memory from mem_file                         │    │
│  │  4. Restore device state                             │    │
│  │  5. Resume vCPUs                                     │    │
│  │                                                      │    │
│  │  Time: ~5-10ms (vs ~150ms cold boot)                │    │
│  └─────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────┘
```

### Snapshot API

```json
// Create snapshot
PUT /snapshot/create
{
    "snapshot_type": "Full",
    "snapshot_path": "/tmp/snapshot",
    "mem_file_path": "/tmp/mem",
    "version": "1.0.0"
}

// Load snapshot
PUT /snapshot/load
{
    "snapshot_path": "/tmp/snapshot",
    "mem_backend": {
        "backend_type": "File",
        "backend_path": "/tmp/mem"
    },
    "enable_diff_snapshots": false,
    "resume_vm": true
}
```

---

## Rate Limiters

Firecracker supports rate limiting for network and block devices.

### Rate Limiter Configuration

```
┌─────────────────────────────────────────────────────────────┐
│                    RATE LIMITERS                             │
│                                                              │
│  Token Bucket Algorithm:                                     │
│  ┌─────────────────────────────────────────────────────┐    │
│  │                                                      │    │
│  │    ┌─────────────────────┐                          │    │
│  │    │  Bucket             │  size = capacity         │    │
│  │    │  ████████░░░░░░░░░  │  tokens = current        │    │
│  │    └─────────────────────┘                          │    │
│  │           ▲         │                               │    │
│  │           │         │                               │    │
│  │    refill │         │ consume                       │    │
│  │    (rate) │         │ (I/O ops)                    │    │
│  │           │         ▼                               │    │
│  │                                                      │    │
│  │  If tokens < required: block until refilled         │    │
│  │  Refill: tokens += (elapsed_time * refill_rate)     │    │
│  │  Consume: tokens -= operation_cost                  │    │
│  │                                                      │    │
│  └─────────────────────────────────────────────────────┘    │
│                                                              │
│  Configuration:                                              │
│  ┌─────────────────────────────────────────────────────┐    │
│  │  {                                                   │    │
│  │    "bandwidth": {                                    │    │
│  │      "size": 1048576,         // 1 MiB bucket       │    │
│  │      "one_time_burst": 0,                           │    │
│  │      "refill_time": 1000      // 1 sec (1 MiB/s)   │    │
│  │    },                                                │    │
│  │    "ops": {                                          │    │
│  │      "size": 100,             // 100 ops bucket     │    │
│  │      "one_time_burst": 0,                           │    │
│  │      "refill_time": 1000      // 1 sec (100 IOPS)  │    │
│  │    }                                                 │    │
│  │  }                                                   │    │
│  └─────────────────────────────────────────────────────┘    │
└─────────────────────────────────────────────────────────────┘
```
