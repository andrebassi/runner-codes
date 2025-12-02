---
title: 'AWS Overview'
description: 'Deploy Runner Codes on AWS EC2 metal instances'
---

## Why AWS?

Firecracker requires direct access to KVM (Kernel-based Virtual Machine), which is only available on **bare metal** instances. AWS provides several metal instance types suitable for Firecracker workloads.






## Architecture on AWS

![AWS Architecture](/img/aws-architecture.svg)

## Supported Instance Types

| Instance Type | vCPUs | Memory | Network | Use Case |
|---------------|-------|--------|---------|----------|
| `m5zn.metal` | 48 | 192 GB | 25 Gbps | **Recommended** - Best performance |
| `m5.metal` | 96 | 384 GB | 25 Gbps | High memory workloads |
| `c5.metal` | 96 | 192 GB | 25 Gbps | Compute intensive |
| `i3.metal` | 72 | 512 GB | 25 Gbps | Storage intensive |

:::note

  The default configuration uses `m5zn.metal` (~$3.96/hour in us-east-1). You can change this in `Taskfile.yaml`:
  ```yaml title="Configure instance type in Taskfile"

  AWS_INSTANCE_TYPE: 'm5.metal'
  ```

:::

## Networking Configuration

### Security Group Rules

The project creates a security group `infra-operator-sg` with:

| Type | Protocol | Port | Source | Description |
|------|----------|------|--------|-------------|
| SSH | TCP | 22 | 0.0.0.0/0 | SSH access |

:::warning

  For production, restrict SSH access to your IP address:
  ```yaml title="Restrict SSH to specific IP"

  --cidr YOUR_IP/32
  ```

:::

### VPC Requirements

- Public subnet with internet gateway
- Route table with 0.0.0.0/0 → igw

The default configuration uses:
- VPC: `vpc-0dd1e4d3c5ed4f3c7`
- Subnet: `subnet-004de53997864f0a3`

## Storage Layout

### EC2 Instance Storage

```
/srv/firecracker/
├── vmlinux                    # Linux kernel
├── rootfs.ext4               # Symlink to active rootfs
├── images/
│   ├── rootfs-python.ext4
│   ├── rootfs-nodejs.ext4
│   ├── rootfs-go.ext4
│   ├── rootfs-rust.ext4
│   └── rootfs-bash.ext4
├── jailer/                    # (future) Jailer workdir
├── snapshots/                 # VM snapshots
└── logs/                      # Firecracker logs

/opt/llm-infra-operator/
└── bin/
    └── infra.operator        # Unified CLI (host and guest)
```

### S3 Bucket

```
s3://llm-infra-operator-rootfs/
├── vmlinux
├── rootfs-python.ext4
├── rootfs-nodejs.ext4
├── rootfs-go.ext4
├── rootfs-rust.ext4
└── rootfs-bash.ext4
```

## Cost Breakdown

### Hourly Costs (us-east-1)

| Resource | Cost |
|----------|------|
| m5zn.metal instance | $3.9616/hour |
| 50GB gp3 EBS | $0.0057/hour |
| **Total** | **~$4.00/hour** |

### Monthly Costs (if running 24/7)

| Resource | Cost |
|----------|------|
| m5zn.metal instance | $2,892/month |
| 50GB gp3 EBS | $4/month |
| S3 storage (~5GB) | $0.12/month |
| **Total** | **~$2,896/month** |

:::tip

  For development and testing:
  - Use `task aws:stop` to stop the instance when not in use
  - You only pay for EBS storage while stopped (~$4/month)
  - Use `task aws:cleanup` to terminate completely when done

:::

## Quick Start Commands

```bash title="Launch instance"
task aws:launch 2>&1 | tee /tmp/log.txt
```

```bash title="Check status"
task aws:status
```

```bash title="SSH into instance"
task aws:ssh
```

```bash title="View setup logs"
task aws:logs
```

```bash title="Deploy code"
task aws:deploy
```

```bash title="Run test"
task aws:test
```

```bash title="Stop (pause billing for compute)"
task aws:stop
```

```bash title="Cleanup (remove all resources)"
task aws:cleanup
```

## Region Support

The project defaults to `us-east-1`. To use a different region:

```bash title="Set region"
export AWS_REGION=eu-west-1
```

```bash title="Or pass to task"
AWS_REGION=eu-west-1 task aws:launch
```

:::note

  When changing regions, you may need to update the VPC and subnet IDs in `Taskfile.yaml`.

:::

## Next Steps





