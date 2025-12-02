---
title: 'Performance Tuning'
description: 'Optimize execution speed and resource usage'
---

## Performance Overview

Runner Codes performance depends on three main factors:
1. **VM Boot Time**: Time to start a microVM (~125ms base)
2. **Guest Ready**: Time until infra.operator guest accepts connections (~2.5s)
3. **Code Execution**: Actual code execution time (varies)

![Performance Timeline](/img/performance-timeline.svg)

## Current Performance

### Baseline Metrics

| Metric | Value |
|--------|-------|
| VM boot (Firecracker start) | ~125ms |
| Kernel boot | ~500ms |
| Systemd + infra.operator ready | ~2.4s |
| **Total warm-up** | **~3s** |

### Language-Specific

| Language | Hello World | Fibonacci(30) | Notes |
|----------|-------------|---------------|-------|
| Python | ~50ms | ~300ms | Interpreter startup |
| Node.js | ~100ms | ~100ms | V8 JIT |
| Go | ~500ms | ~10ms | Compilation overhead |
| Rust | ~2000ms | ~5ms | Long compilation |
| Bash | ~10ms | ~8s | Fast start, slow compute |

## Optimization Strategies

### 1. Snapshot Restore (Recommended)

Instead of booting VMs from scratch, restore from a pre-warmed snapshot:

![Boot Time Comparison](/img/boot-comparison.svg)

**Benefits:**
- 30x faster startup
- Consistent startup time
- Pre-loaded dependencies

**Implementation:**

```bash title="Create snapshot (one-time setup)"
task aws:create-snapshot
```

```bash title="Restore snapshot for each execution"
task aws:restore-snapshot
```

:::note

  Snapshot support requires additional implementation. See the [Snapshots](/advanced/snapshots) page.

:::

### 2. VM Warm Pool

Maintain a pool of pre-booted VMs ready for execution:

![VM Warm Pool Architecture](/img/warm-pool.svg)

**Configuration:**

```go title="Warm pool configuration"
type WarmPool struct {
    Size       int           // Number of VMs to keep warm
    RefillRate time.Duration // How often to check and refill
    Languages  []string      // Languages to pre-warm
}

pool := WarmPool{
    Size:       10,
    RefillRate: time.Second * 5,
    Languages:  []string{"python", "node"},
}
```

### 3. Kernel Optimization

Use an optimized kernel configuration:

```bash title="Kernel configuration optimizations"
# Key optimizations:
CONFIG_PRINTK=n              # Disable kernel logs
CONFIG_MODULES=n             # No loadable modules
CONFIG_SMP=n                 # Single CPU (for 1 vCPU VMs)
CONFIG_DEBUG_KERNEL=n        # No debug
CONFIG_OPTIMIZE_FOR_SIZE=y   # Size optimization
```

**Boot Arguments:**

```bash title="Minimal boot args for speed"
boot_args="reboot=k panic=1 pci=off root=/dev/vda rw quiet"
```

```bash title="Debug boot args (alternative)"
boot_args="console=ttyS0 reboot=k panic=1 pci=off root=/dev/vda rw"
```

### 4. Rootfs Optimization

Minimize rootfs size and content:

```bash title="Remove documentation"
rm -rf /usr/share/doc/* /usr/share/man/*
```

```bash title="Remove locales (keep only en_US)"
rm -rf /usr/share/locale/*
```

```bash title="Clear package cache"
apt-get clean
rm -rf /var/lib/apt/lists/*
```

```bash title="Use minimal base variant"
debootstrap --variant=minbase jammy rootfs
```

### 5. Memory Configuration

Optimize memory allocation:

```json title="Memory configuration example"
{
  "vcpu_count": 1,
  "mem_size_mib": 256,  // Minimum for Python
  "smt": false
}
```

**Language-Specific Memory:**

| Language | Minimum | Recommended |
|----------|---------|-------------|
| Bash | 128 MiB | 256 MiB |
| Python | 256 MiB | 512 MiB |
| Node.js | 256 MiB | 512 MiB |
| Go | 256 MiB | 512 MiB |
| Rust | 512 MiB | 1024 MiB |

### 6. Disable Unnecessary Services

In the guest systemd:

```bash title="Disable everything except infra.operator"
systemctl disable systemd-networkd
systemctl disable systemd-resolved
systemctl disable rsyslog
systemctl disable cron
```

```bash title="Keep only essential services"
systemctl enable infra.operator
```

## Benchmarking

### Run Benchmarks

```bash title="Full benchmark suite"
task aws:benchmark
```

```bash title="Single language benchmark"
task aws:test LANG=python CODE='print("hello")'
```

### Expected Results

```
Benchmark Results (c5.metal instance):
=======================================

VM Boot Time:
  Cold boot:      3.1s ± 0.2s
  Snapshot:       0.1s ± 0.02s  (requires implementation)

Python:
  Hello World:    50ms
  Fibonacci(20):  10ms
  Fibonacci(30):  300ms

Node.js:
  Hello World:    100ms
  Fibonacci(20):  5ms
  Fibonacci(30):  100ms

Go:
  Hello World:    500ms (includes compilation)
  Fibonacci(20):  510ms
  Fibonacci(30):  515ms

Rust:
  Hello World:    2000ms (includes compilation)
  Fibonacci(20):  2005ms
  Fibonacci(30):  2005ms

Bash:
  Hello World:    10ms
  File ops:       20ms
```

## Performance Monitoring

### Metrics to Track

```go title="Prometheus metrics configuration"
// Prometheus metrics
vmBootDuration := prometheus.NewHistogram(prometheus.HistogramOpts{
    Name:    "infra-operator_vm_boot_seconds",
    Help:    "VM boot time distribution",
    Buckets: []float64{0.1, 0.5, 1, 2, 3, 5, 10},
})

codeExecutionDuration := prometheus.NewHistogramVec(prometheus.HistogramOpts{
    Name:    "infra-operator_code_execution_seconds",
    Help:    "Code execution time by language",
    Buckets: []float64{0.01, 0.1, 0.5, 1, 5, 10, 30, 60},
}, []string{"language"})

vmPoolSize := prometheus.NewGauge(prometheus.GaugeOpts{
    Name: "infra-operator_warm_pool_size",
    Help: "Current number of warm VMs",
})
```

### Logging

```go title="Execution metrics structure"
type ExecutionMetrics struct {
    TraceID       string        `json:"trace_id"`
    Language      string        `json:"language"`
    VMBootTime    time.Duration `json:"vm_boot_time"`
    ReadyTime     time.Duration `json:"ready_time"`
    ExecutionTime time.Duration `json:"execution_time"`
    TotalTime     time.Duration `json:"total_time"`
}
```

## Hardware Recommendations

### Production Deployment

| Component | Recommendation |
|-----------|----------------|
| Instance | AWS c5.metal, c6i.metal |
| CPU | Intel Xeon (KVM support) |
| Memory | 8GB+ for warm pool |
| Storage | NVMe SSD for rootfs |
| Network | Not required for execution |

### Capacity Planning

```text title="VM capacity calculation"
Concurrent VMs = Available Memory / VM Memory

Example:
  Host Memory:   32 GB
  Reserved:       2 GB (host OS + Firecracker)
  VM Memory:    512 MB
  Max VMs:      (32 - 2) / 0.5 = 60 concurrent VMs

With warm pool of 10:
  Available for execution: 50 VMs
```

## Cost Optimization

### EC2 Instance Selection

| Instance | vCPUs | Memory | Cost/hr | Use Case |
|----------|-------|--------|---------|----------|
| c5.metal | 96 | 192 GB | ~$4 | Production |
| c5n.metal | 72 | 192 GB | ~$4 | High network |
| c6i.metal | 128 | 256 GB | ~$5 | High capacity |
| m5.metal | 96 | 384 GB | ~$5 | Memory-heavy |

### Cost per Execution

```text title="Cost calculation per execution"
Assuming c5.metal ($4/hr):
  - 3600 seconds/hour
  - 3s per execution (cold boot)

  Cold boot: $4 / (3600/3) = $0.0033 per execution

With warm pool:
  - 0.1s per execution

  Warm pool: $4 / (3600/0.1) = $0.0001 per execution
```

## Optimization Roadmap

### Phase 1: Current State
- [x] Basic VM boot
- [x] Code execution
- [x] Per-language rootfs

### Phase 2: Quick Wins
- [ ] Optimized kernel
- [ ] Minimal rootfs
- [ ] Quiet boot

### Phase 3: Snapshots
- [ ] Create base snapshots
- [ ] Snapshot restore
- [ ] Delta snapshots

### Phase 4: Warm Pool
- [ ] Pool management
- [ ] Language-specific pools
- [ ] Auto-scaling

### Phase 5: Advanced
- [ ] CPU pinning
- [ ] NUMA awareness
- [ ] Huge pages
