---
title: 'Crystal'
description: 'Crystal 1.18 code execution'
---

## Overview

Crystal is a programming language with Ruby-like syntax but compiled to efficient native code via LLVM. Runner Codes provides Crystal 1.18.2 runtime.

## Specifications

| Property | Value |
|----------|-------|
| Base OS | Debian Bookworm |
| Version | Crystal 1.18.2 |
| LLVM | 18.1.8 |
| Rootfs Size | 1.5 GB |
| Execution | Compiled (LLVM) |
| File Extension | `.cr` |
| Run Command | `crystal run script.cr` |
| Memory | 2048 MiB (minimum) |
| vCPUs | 2 (minimum) |
| Timeout | 180s+ (recommended) |

## Known Limitation

:::warning

  **Crystal has significant performance limitations in microVMs due to LLVM compilation:**

  1. **Every execution requires full LLVM compilation** - there's no interpreter mode in official builds
  2. **Compilation takes 60-180+ seconds** even for simple "Hello World" programs
  3. **High resource requirements** - needs 2+ vCPUs and 2+ GB RAM for acceptable performance
  4. **No runtime eval** - Crystal compiles to native code, no dynamic code execution

:::

### Why This Happens

Crystal uses LLVM for compilation, which is designed for producing highly optimized native code. This makes Crystal programs very fast once compiled, but the compilation step itself is resource-intensive.

According to Crystal documentation:
- Crystal was compiled without interpreter support in official builds
- The `crystal i` (interpreter) mode requires special compilation flags
- Each `crystal run` invokes the full LLVM pipeline

### Recommendations


    Set timeout to 180+ seconds. Simple programs may compile in 60-120s, but complex code takes longer.



    Configure at least 2 vCPUs and 2048 MB memory for the microVM.



    For faster Ruby-like syntax execution, consider using Ruby instead (interpreted, ~200ms).



    If running the same code multiple times, compile once with `crystal build` and cache the binary.


```bash title="1. Create 1.5GB Rootfs Image"
sudo dd if=/dev/zero of=/srv/firecracker/images/rootfs-crystal.ext4 bs=1M count=1536
sudo mkfs.ext4 /srv/firecracker/images/rootfs-crystal.ext4
sudo mkdir -p /mnt/crystal
sudo mount /srv/firecracker/images/rootfs-crystal.ext4 /mnt/crystal
```

```bash title="2. Install Debian Base System"
sudo debootstrap --variant=minbase bookworm /mnt/crystal http://deb.debian.org/debian
```

```bash title="3. Install Crystal Dependencies"
sudo chroot /mnt/crystal apt-get update
sudo chroot /mnt/crystal apt-get install -y --no-install-recommends \
    wget gcc libgc-dev libevent-dev libpcre2-dev libpcre3-dev \
    libyaml-dev libssl-dev libgmp-dev libz-dev pkg-config ca-certificates
```

```bash title="4. Download Crystal 1.18.2 from GitHub releases"
cd /tmp
wget -O crystal.tar.gz https://github.com/crystal-lang/crystal/releases/download/1.18.2/crystal-1.18.2-1-linux-x86_64.tar.gz
tar -xzf crystal.tar.gz
```

```bash title="5. Install Crystal in Rootfs"
sudo cp -r /tmp/crystal-1.18.2-1 /mnt/crystal/opt/
sudo ln -sf /opt/crystal-1.18.2-1/bin/crystal /mnt/crystal/usr/local/bin/crystal
```

```bash title="6. Configure Library Path"
echo '/opt/crystal-1.18.2-1/lib/crystal' | sudo tee /mnt/crystal/etc/ld.so.conf.d/crystal.conf
sudo chroot /mnt/crystal ldconfig
```

```bash title="7. Copy infra.operator Binary"
sudo cp /usr/local/bin/infra.operator /mnt/crystal/usr/local/bin/
sudo chmod +x /mnt/crystal/usr/local/bin/infra.operator
```

```bash title="8. Unmount and Create Snapshot (with high resources)"
sudo sync
sudo umount /mnt/crystal
sudo infra.operator snapshot create --lang crystal --mem 2048 --vcpus 2
```

```bash title="9. Upload to S3"
sudo infra.operator snapshot upload --lang crystal --bucket runner-codes
aws s3 cp /srv/firecracker/images/rootfs-crystal.ext4 s3://runner-codes/rootfs/crystal/
```

```bash title="10. Test Execution (with high timeout)"
sudo infra.operator host --lang crystal --code 'puts "Hello from Crystal"' \
    --mem 2048 --vcpus 2 --snapshot --cache-dir /dev/shm/snapshots --timeout 180
```



## Examples

### Hello World

```json title="Request"
{
  "trace_id": "crystal-hello-001",
  "lang": "crystal",
  "code": "puts \"Hello from Crystal!\"",
  "timeout": 180,
  "mem": 2048,
  "vcpus": 2
}
```

```json title="Response"
{
  "trace_id": "crystal-hello-001",
  "stdout": "Hello from Crystal!\n",
  "stderr": "",
  "exit_code": 0,
  "executionTime": "~90-120s"
}
```

### Variables and Types

```json title="Request"
{
  "trace_id": "crystal-vars-001",
  "lang": "crystal",
  "code": "name = \"Crystal\"\nversion = 1.18\ncount = 0\ncount += 1\nputs \"Language: #{name}\"\nputs \"Version: #{version}\"\nputs \"Count: #{count}\"",
  "timeout": 180
}
```

```json title="Response"
{
  "trace_id": "crystal-vars-001",
  "stdout": "Language: Crystal\nVersion: 1.18\nCount: 1\n",
  "stderr": "",
  "exit_code": 0
}
```

### Arrays and Iteration

```json title="Request"
{
  "trace_id": "crystal-arrays-001",
  "lang": "crystal",
  "code": "numbers = [1, 2, 3, 4, 5]\nsquares = numbers.map { |n| n * n }\nsum = squares.sum\n\nputs \"Numbers: #{numbers}\"\nputs \"Squares: #{squares}\"\nputs \"Sum: #{sum}\"",
  "timeout": 180
}
```

```json title="Response"
{
  "trace_id": "crystal-arrays-001",
  "stdout": "Numbers: [1, 2, 3, 4, 5]\nSquares: [1, 4, 9, 16, 25]\nSum: 55\n",
  "stderr": "",
  "exit_code": 0
}
```

### Classes and Methods

```json title="Request"
{
  "trace_id": "crystal-class-001",
  "lang": "crystal",
  "code": "class Person\n  property name : String\n  property age : Int32\n\n  def initialize(@name, @age)\n  end\n\n  def greet\n    \"Hello, I'm #{@name} and I'm #{@age} years old\"\n  end\nend\n\nalice = Person.new(\"Alice\", 30)\nputs alice.greet",
  "timeout": 180
}
```

```json title="Response"
{
  "trace_id": "crystal-class-001",
  "stdout": "Hello, I'm Alice and I'm 30 years old\n",
  "stderr": "",
  "exit_code": 0
}
```

## Technical Details

### LLVM Compilation Pipeline

Each `crystal run` execution goes through:
1. **Parsing** - Crystal source to AST
2. **Type inference** - Full program type analysis
3. **LLVM IR generation** - Convert to LLVM intermediate representation
4. **LLVM optimization** - Multiple optimization passes
5. **Native code generation** - Produce x86_64 machine code
6. **Linking** - Link with system libraries
7. **Execution** - Run the binary

Steps 3-6 are the bottleneck in resource-constrained microVMs.

## Limitations

:::warning

  The Crystal environment has the following limitations:

:::

1. **Slow compilation**: 60-180+ seconds per execution
2. **High resources**: Requires 2+ vCPUs and 2048+ MB RAM
3. **No shards**: Only Crystal standard library
4. **No network**: Network operations will fail
5. **No interpreter**: Official builds lack interpreter support

## Comparison with Ruby

| Feature | Crystal | Ruby |
|---------|---------|------|
| Syntax | Ruby-like | Ruby |
| Execution | Compiled (LLVM) | Interpreted |
| Startup time | 60-180s | ~200ms |
| Runtime speed | Very fast | Moderate |
| Type system | Static | Dynamic |
| Memory | 2048+ MB | 512 MB |
| Use case | Performance-critical | Rapid development |

:::note

  For quick code execution with Ruby-like syntax, consider using Ruby instead.
  Crystal is better suited for scenarios where execution speed matters more than startup time.

:::

## Best Practices

**Always set timeout to 180+ seconds for Crystal executions:**

More code means more types to infer and more LLVM work.

**If you just need Ruby-like syntax quickly, use Ruby instead:**

Crystal's type inference is powerful - let the compiler figure out types.
