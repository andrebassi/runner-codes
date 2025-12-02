---
title: 'Swift'
description: 'Swift 5.10 code execution'
---

## Overview

Swift is Apple's modern programming language for building safe, fast, and expressive code. LLM-Firecracker provides Swift 5.10.1 runtime for Linux.

## Specifications

| Property | Value |
|----------|-------|
| Base OS | Debian Bookworm |
| Version | Swift 5.10.1 |
| Rootfs Size | 4 GB |
| Execution | Interpreted (swift) |
| File Extension | `.swift` |
| Run Command | `swift script.swift` |
| Execution Time | ~2.9s |

## Infrastructure

:::warning

  Swift requires manual installation because:
  1. **Debian's "swift" package is OpenStack Swift** (object storage), not Apple Swift
  2. **Swift binaries use AVX2/AVX-512 instructions** - requires T2CL CPU template in Firecracker
  3. **Large toolchain** (~1.9GB) requires 4GB rootfs

:::

### Manual Installation Steps

```bash title="1. Create 4GB Rootfs Image"
sudo dd if=/dev/zero of=/srv/firecracker/images/rootfs-swift.ext4 bs=1M count=4096 status=progress
sudo mkfs.ext4 /srv/firecracker/images/rootfs-swift.ext4
sudo mkdir -p /mnt/swift
sudo mount /srv/firecracker/images/rootfs-swift.ext4 /mnt/swift
```

```bash title="2. Install Debian Base System"
sudo debootstrap --variant=minbase bookworm /mnt/swift http://deb.debian.org/debian
```

```bash title="3. Install Swift Dependencies"
sudo chroot /mnt/swift apt-get update
sudo chroot /mnt/swift apt-get install -y --no-install-recommends \
    binutils \
    libc6-dev \
    libcurl4-openssl-dev \
    libedit-dev \
    libncurses-dev \
    libsqlite3-dev \
    libxml2-dev \
    libicu-dev \
    ca-certificates \
    zlib1g
```

```bash title="4. Download Swift 5.10.1 from swift.org (NOT from Debian repos!)"
cd /tmp
wget https://download.swift.org/swift-5.10.1-release/ubuntu2204/swift-5.10.1-RELEASE/swift-5.10.1-RELEASE-ubuntu22.04.tar.gz
tar -xzf swift-5.10.1-RELEASE-ubuntu22.04.tar.gz
```

```bash title="5. Copy Swift to Rootfs"
sudo cp -r /tmp/swift-5.10.1-RELEASE-ubuntu22.04/usr/* /mnt/swift/usr/
```

```bash title="6. Configure Dynamic Linker for Swift Libraries"
echo '/usr/lib/swift/linux
/usr/lib/swift/host
/usr/lib/swift/clang/lib
/usr/lib/swift' | sudo tee /mnt/swift/etc/ld.so.conf.d/swift.conf
sudo chroot /mnt/swift ldconfig
```

```bash title="7. Copy infra.operator Binary"
sudo cp /usr/local/bin/infra.operator /mnt/swift/usr/local/bin/
sudo chmod +x /mnt/swift/usr/local/bin/infra.operator
```

```bash title="8. Unmount and Create Snapshot"
sudo sync
sudo umount /mnt/swift
sudo infra.operator snapshot create --lang swift --mem 1024 --vcpus 1
```

```bash title="9. Upload to S3"
sudo infra.operator snapshot upload --lang swift --bucket llm-firecracker
aws s3 cp /srv/firecracker/images/rootfs-swift.ext4 s3://llm-firecracker/rootfs/swift/
```

```bash title="10. Test Execution"
sudo infra.operator host --lang swift --code 'print("Hello from Swift")' \
    --mem 1024 --vcpus 1 --snapshot --cache-dir /dev/shm/snapshots
```


### CPU Template Requirement

:::note

  Swift binaries are compiled with AVX2/AVX-512 instructions. Firecracker must use the **T2CL CPU template**
  to expose these instructions to the guest VM. This was configured in `pkg/host/firecracker.go`:

:::

```go title="Function"
// ConfigureMachine sets the machine configuration
func (fc *FirecrackerClient) ConfigureMachine(vcpus, memMiB int) error {
    config := MachineConfig{
        VCPUCount:   vcpus,
        MemSizeMiB:  memMiB,
        SMT:         false,
        CPUTemplate: "T2CL", // Expose Cascade Lake features (AVX, AVX2, AVX-512) to guest
    }
    return fc.putJSON("/machine-config", config)
}
```

### Troubleshooting

| Issue | Cause | Solution |
|-------|-------|----------|
| `Illegal instruction` | Missing CPU features | Use T2CL CPU template |
| `libSwiftCore.so not found` | Missing library path | Configure `/etc/ld.so.conf.d/swift.conf` |
| `No space left on device` | Rootfs too small | Use 4GB rootfs (Swift needs ~2.5GB) |
| `swift: command not found` | Wrong package | Use swift.org binaries, not Debian's OpenStack Swift |

## Examples

### Hello World

```json title="Request"
{
  "trace_id": "swift-hello-001",
  "lang": "swift",
  "code": "print(\"Hello from Swift!\")",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "swift-hello-001",
  "stdout": "Hello from Swift!\n",
  "stderr": "",
  "exit_code": 0
}
```

### Variables and Types

```json title="Request"
{
  "trace_id": "swift-vars-001",
  "lang": "swift",
  "code": "let name = \"Swift\"\nlet version = 5.10\nvar count = 0\ncount += 1\nprint(\"Language: \\(name)\")\nprint(\"Version: \\(version)\")\nprint(\"Count: \\(count)\")",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "swift-vars-001",
  "stdout": "Language: Swift\nVersion: 5.1\nCount: 1\n",
  "stderr": "",
  "exit_code": 0
}
```

### Collections

```json title="Request"
{
  "trace_id": "swift-coll-001",
  "lang": "swift",
  "code": "let numbers = [1, 2, 3, 4, 5]\nlet squares = numbers.map { $0 * $0 }\nlet sum = squares.reduce(0, +)\n\nprint(\"Numbers: \\(numbers)\")\nprint(\"Squares: \\(squares)\")\nprint(\"Sum: \\(sum)\")",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "swift-coll-001",
  "stdout": "Numbers: [1, 2, 3, 4, 5]\nSquares: [1, 4, 9, 16, 25]\nSum: 55\n",
  "stderr": "",
  "exit_code": 0
}
```

### Optionals and Nil Safety

```json title="Request"
{
  "trace_id": "swift-opt-001",
  "lang": "swift",
  "code": "var name: String? = nil\nprint(name ?? \"Name is nil\")\n\nname = \"Alice\"\nif let unwrapped = name {\n    print(\"Name is \\(unwrapped)\")\n}",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "swift-opt-001",
  "stdout": "Name is nil\nName is Alice\n",
  "stderr": "",
  "exit_code": 0
}
```

### Structs and Methods

```json title="Request"
{
  "trace_id": "swift-struct-001",
  "lang": "swift",
  "code": "struct Person {\n    let name: String\n    let age: Int\n    \n    func greet() -> String {\n        return \"Hello, I'm \\(name) and I'm \\(age) years old\"\n    }\n}\n\nlet alice = Person(name: \"Alice\", age: 30)\nprint(alice.greet())",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "swift-struct-001",
  "stdout": "Hello, I'm Alice and I'm 30 years old\n",
  "stderr": "",
  "exit_code": 0
}
```

### Closures and Higher-Order Functions

```json title="Request"
{
  "trace_id": "swift-closure-001",
  "lang": "swift",
  "code": "let numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]\n\nlet evens = numbers.filter { $0 % 2 == 0 }\nlet doubled = numbers.map { $0 * 2 }\nlet sum = numbers.reduce(0) { $0 + $1 }\n\nprint(\"Evens: \\(evens)\")\nprint(\"Doubled: \\(doubled)\")\nprint(\"Sum: \\(sum)\")",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "swift-closure-001",
  "stdout": "Evens: [2, 4, 6, 8, 10]\nDoubled: [2, 4, 6, 8, 10, 12, 14, 16, 18, 20]\nSum: 55\n",
  "stderr": "",
  "exit_code": 0
}
```

## Limitations

:::warning

  The Swift environment has the following limitations:

:::

1. **Large rootfs**: Swift toolchain requires ~4GB
2. **No external packages**: Only Swift standard library
3. **Memory limit**: 1024 MiB recommended
4. **Interpreted mode**: Uses `swift` interpreter (not compiled)
5. **No Foundation extras**: Network operations will fail

## Best Practices


    Always use optional binding (if let, guard let) to safely unwrap optionals.



    For simple data types, prefer structs for value semantics and performance.



    Swift interpretation is slower than compiled; use 30+ second timeouts.


