---
title: 'EC2 Setup'
description: 'Setting up AWS EC2 instances for Firecracker'
---

## Overview

Firecracker requires bare metal instances with KVM support. AWS EC2 provides several metal instance types suitable for running Firecracker microVMs.

:::note

  Firecracker cannot run on regular EC2 instances due to the lack of nested virtualization support. You must use `.metal` instances.

:::

## Instance Requirements

### Minimum Requirements

| Requirement | Value |
|-------------|-------|
| Instance Type | `.metal` suffix |
| KVM Support | Required |
| Operating System | Amazon Linux 2023 / Ubuntu |
| Architecture | x86_64 or ARM64 |

### Recommended Instances

| Instance | vCPUs | Memory | Network | Cost/hr | Use Case |
|----------|-------|--------|---------|---------|----------|
| c5.metal | 96 | 192 GB | 25 Gbps | ~$4.08 | General purpose |
| c5n.metal | 72 | 192 GB | 100 Gbps | ~$3.89 | High network |
| c6i.metal | 128 | 256 GB | 50 Gbps | ~$5.44 | Latest generation |
| m5.metal | 96 | 384 GB | 25 Gbps | ~$4.61 | Memory intensive |
| i3.metal | 72 | 512 GB | 25 Gbps | ~$4.99 | High I/O |

## Launch Methods

### Method 1: Task Command (Recommended)

```bash title="Set AWS credentials"
export AWS_ACCESS_KEY_ID="your-key"
export AWS_SECRET_ACCESS_KEY="your-secret"
export AWS_DEFAULT_REGION="us-east-1"
```

```bash title="Launch instance"
task aws:launch
```

This automatically:
- Creates security group
- Generates SSH key pair
- Launches c5.metal instance
- Configures instance with user-data script
- Downloads rootfs images from S3

### Method 2: AWS Console


    Select **Amazon Linux 2023** or **Ubuntu 22.04 LTS**

    AMI IDs (us-east-1):
    - Amazon Linux 2023: `ami-0c7217cdde317cfec`
    - Ubuntu 22.04: `ami-0fc5d935ebf8bc3bc`



    Choose a `.metal` instance:
    - `c5.metal` (recommended)
    - `c5n.metal`
    - `c6i.metal`



    Allow inbound SSH:
```text title="Configure inbound SSH rule"
Type: SSH
Protocol: TCP
Port: 22
Source: Your IP
```



Paste the contents of `aws/user-data.sh` in Advanced Details → User data



Review and launch the instance


### Method 3: AWS CLI

```bash title="Create security group"
aws ec2 create-security-group \
  --group-name firecracker-sg \
  --description "Firecracker security group"
```

```bash title="Allow SSH"
aws ec2 authorize-security-group-ingress \
  --group-name firecracker-sg \
  --protocol tcp \
  --port 22 \
  --cidr 0.0.0.0/0
```

```bash title="Create key pair"
aws ec2 create-key-pair \
  --key-name firecracker-key \
  --query 'KeyMaterial' \
  --output text > firecracker-key.pem
chmod 600 firecracker-key.pem
```

```bash title="Launch instance"
aws ec2 run-instances \
  --image-id ami-0c7217cdde317cfec \
  --instance-type c5.metal \
  --key-name firecracker-key \
  --security-groups firecracker-sg \
  --user-data file://aws/user-data.sh \
  --tag-specifications 'ResourceType=instance,Tags=[{Key=Name,Value=firecracker-host}]'
```

## User Data Script

The user-data script configures the instance:

```bash title="aws/user-data.sh"
set -ex
```

```bash title="Install dependencies"
yum update -y
yum install -y docker git jq
```

```bash title="Start Docker"
systemctl start docker
systemctl enable docker
```

```bash title="Install Firecracker"
FC_VERSION="v1.7.0"
curl -L https://github.com/firecracker-microvm/firecracker/releases/download/${FC_VERSION}/firecracker-${FC_VERSION}-x86_64.tgz | tar -xz
mv release-${FC_VERSION}-x86_64/firecracker-${FC_VERSION}-x86_64 /usr/local/bin/firecracker
mv release-${FC_VERSION}-x86_64/jailer-${FC_VERSION}-x86_64 /usr/local/bin/jailer
chmod +x /usr/local/bin/firecracker /usr/local/bin/jailer
```

```bash title="Create directories"
mkdir -p /srv/firecracker/images
```

```bash title="Load kernel modules"
modprobe kvm
modprobe kvm_intel || modprobe kvm_amd
modprobe vhost_vsock
```

```bash title="Set permissions"
chmod 666 /dev/kvm
chmod 666 /dev/vhost-vsock
```

```bash title="Download rootfs images from S3"
S3_BUCKET="llm-infra-operator-rootfs"
aws s3 sync s3://${S3_BUCKET}/ /srv/firecracker/images/ --region us-east-1
```

```bash title="Signal ready"
touch /tmp/instance-ready
```

## Post-Launch Setup

### Connect via SSH

```bash title="Using task command"
task aws:ssh
```

```bash title="Or manually"
ssh -i aws/keys/firecracker.pem ec2-user@<public-ip>
```

### Verify Setup

```bash title="Check KVM"
ls -la /dev/kvm
```

```bash title="Check vhost_vsock"
ls -la /dev/vhost-vsock
```

```bash title="Check Firecracker"
firecracker --version
```

```bash title="Check images"
ls -la /srv/firecracker/images/
```


### Deploy Application

```bash title="Deploy binaries"
task aws:deploy
```

```bash title="Inject infra.operator"
task aws:inject-infra-operator
```

```bash title="Run test"
task aws:test LANG=python CODE='print("Hello")'
```

## Security Configuration

### Security Group Rules

| Type | Protocol | Port | Source | Purpose |
|------|----------|------|--------|---------|
| SSH | TCP | 22 | Your IP | SSH access |

:::warning

  Never expose SSH to 0.0.0.0/0 in production. Use your specific IP or a VPN.

:::

### IAM Role

For S3 access, attach this policy:

```json title="S3 read access IAM policy"
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Action": [
        "s3:GetObject",
        "s3:ListBucket"
      ],
      "Resource": [
        "arn:aws:s3:::llm-infra-operator-rootfs",
        "arn:aws:s3:::llm-infra-operator-rootfs/*"
      ]
    }
  ]
}
```

## Cost Optimization

### Spot Instances

Use spot instances for development:

```bash title="Launch spot instance"
aws ec2 run-instances \
  --instance-market-options '{"MarketType":"spot"}' \
  --instance-type c5.metal \
  ...
```

Spot savings: ~60-70% compared to on-demand

### Cleanup After Use

Always terminate instances when done:

```bash title="Using task"
task aws:cleanup
```

```bash title="Or manually"
aws ec2 terminate-instances --instance-ids <instance-id>
```

## Regional Availability

Metal instances availability varies by region:

| Region | c5.metal | c5n.metal | c6i.metal |
|--------|----------|-----------|-----------|
| us-east-1 | ✅ | ✅ | ✅ |
| us-west-2 | ✅ | ✅ | ✅ |
| eu-west-1 | ✅ | ✅ | ✅ |
| ap-northeast-1 | ✅ | ✅ | ✅ |

## Troubleshooting

### Instance Stuck in Pending

Metal instances take longer to start:
- Initial launch: 5-10 minutes
- Subsequent launches: 3-5 minutes

### KVM Not Available

```bash title="Load modules"
sudo modprobe kvm
sudo modprobe kvm_intel  # or kvm_amd
sudo modprobe vhost_vsock
```

```bash title="Make permanent"
echo -e "kvm\nkvm_intel\nvhost_vsock" | sudo tee /etc/modules-load.d/kvm.conf
```

### Permission Denied on /dev/kvm

```bash title="Set permissions"
sudo chmod 666 /dev/kvm
sudo chmod 666 /dev/vhost-vsock
```

```bash title="Or add user to kvm group"
sudo usermod -aG kvm $USER
```

### Images Not Downloaded

```bash title="Check S3 access"
aws s3 ls s3://llm-infra-operator-rootfs/
```

```bash title="Download manually"
aws s3 sync s3://llm-infra-operator-rootfs/ /srv/firecracker/images/
```

## Monitoring

### Instance Metrics

Monitor via CloudWatch:
- CPU Utilization
- Network I/O
- Disk I/O

### Application Metrics

Export to Prometheus:

```go title="Define Prometheus metrics"
// Metrics to expose
vm_boot_duration_seconds
code_execution_duration_seconds
vm_memory_usage_bytes
concurrent_vms_total
```

## Best Practices

:::info Best Practices

- **Choose the right instance** — c6i.metal offers better price/performance than c5.metal.
- **Enable detailed monitoring** — Enable 1-minute CloudWatch metrics for production.
- **Use placement groups** — For multi-instance deployments, use cluster placement groups.
- **Use Auto Scaling** — Use Auto Scaling groups with scheduled scaling for cost optimization.

:::