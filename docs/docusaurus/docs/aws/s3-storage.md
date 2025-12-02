---
title: 'S3 Storage'
description: 'Managing rootfs images and kernel storage in Amazon S3'
---

## Overview

S3 provides centralized storage for rootfs images and the Linux kernel, enabling:

- **Fast deployment**: New instances download pre-built images instead of building from scratch
- **Consistency**: All instances use identical rootfs images
- **Version control**: Store multiple versions of rootfs images
- **Cost efficiency**: Store once, download many times

## Bucket Configuration

### Default Settings

| Setting | Value |
|---------|-------|
| Bucket Name | `llm-infra-operator-rootfs` |
| Region | `us-east-1` |
| Storage Class | Standard |
| Versioning | Disabled (enable for production) |
| Encryption | SSE-S3 (default) |

### Creating the Bucket

The bucket was created with:

```bash title="Create S3 bucket"
aws s3api create-bucket \
    --bucket llm-infra-operator-rootfs \
    --region us-east-1
```

:::note

  For regions other than us-east-1, you need `--create-bucket-configuration LocationConstraint=REGION`

:::

## Bucket Contents

```text title="S3 bucket structure"
s3://llm-infra-operator-rootfs/
├── vmlinux                    # Linux kernel (20.4 MiB)
├── rootfs-base.ext4          # Base image (512 MiB)
├── rootfs-bash.ext4          # Bash minimal (512 MiB)
├── rootfs-python.ext4        # Python 3.10 (600 MiB)
├── rootfs-nodejs.ext4        # Node.js 20 (700 MiB)
├── rootfs-go.ext4            # Go 1.22.3 (700 MiB)
└── rootfs-rust.ext4          # Rust stable (2.0 GiB)
```

### List Contents

```bash title="List S3 bucket contents"
task s3:list
```

Output:
```text title="S3 bucket listing output"
=== S3 bucket contents (llm-infra-operator-rootfs) ===
2025-11-29 04:31:21  512.0 MiB rootfs-base.ext4
2025-11-29 04:31:23    2.0 GiB rootfs-bash.ext4
2025-11-29 04:31:30  700.0 MiB rootfs-go.ext4
2025-11-29 04:31:33  700.0 MiB rootfs-nodejs.ext4
2025-11-29 04:31:36  600.0 MiB rootfs-python.ext4
2025-11-29 04:31:42    2.0 GiB rootfs-rust.ext4
2025-11-29 04:31:49   20.4 MiB vmlinux

Total Objects: 7
   Total Size: 6.5 GiB
```

## Workflow Integration

### Instance Launch Flow

When a new EC2 instance launches via `task aws:launch`, the user-data script automatically downloads from S3:

![Instance Launch Flow](/img/s3-instance-launch-flow.svg)

### Rootfs Creation Flow

When creating new rootfs images with `task aws:create-rootfs-*`:

![Rootfs Creation Flow](/img/s3-rootfs-creation-flow.svg)

## User-Data S3 Integration

The EC2 user-data script (`aws/user-data.sh`) handles S3 downloads:

```bash title="S3 Configuration"
S3_BUCKET="llm-infra-operator-rootfs"
S3_REGION="us-east-1"
```

```bash title="Download vmlinux kernel"
echo "Downloading vmlinux kernel..."
if aws s3 cp s3://${S3_BUCKET}/vmlinux ${WORKDIR}/vmlinux --region ${S3_REGION}; then
    echo "vmlinux downloaded successfully"
else
    echo "WARNING: vmlinux not found in S3, downloading from Firecracker releases..."
    curl -L -o vmlinux "https://s3.amazonaws.com/spec.ccfc.min/..."
fi
```

```bash title="Download all rootfs images"
echo "Downloading rootfs images..."
aws s3 sync s3://${S3_BUCKET}/ ${WORKDIR}/images/ \
    --exclude "*" --include "rootfs-*.ext4" \
    --region ${S3_REGION}
```

```bash title="Create symlink to default rootfs (python)"
if [ -f ${WORKDIR}/images/rootfs-python.ext4 ]; then
    ln -sf ${WORKDIR}/images/rootfs-python.ext4 ${WORKDIR}/rootfs.ext4
    echo "Default rootfs symlinked to rootfs-python.ext4"
fi
```

**Key features:**
- Downloads vmlinux with fallback to official Firecracker releases
- Uses `aws s3 sync` for efficient incremental downloads
- Creates symlink to default rootfs
- Supports custom bucket and region configuration

## Manual Operations

### Upload All Images

```bash title="Upload all images to S3"
task s3:upload 2>&1 | tee /tmp/log.txt
```

This uploads all `rootfs-*.ext4` files and vmlinux from the EC2 instance to S3.

### Download All Images

```bash title="Download all images from S3"
task s3:download 2>&1 | tee /tmp/log.txt
```

This downloads all images from S3 to the EC2 instance.

### Upload Single Image

```bash title="Upload specific language rootfs"
task s3:upload-single LANG=python
```


### Direct AWS CLI Commands

```bash title="List bucket contents"
aws s3 ls s3://llm-infra-operator-rootfs/ --human-readable
```

```bash title="Upload a file"
aws s3 cp /srv/firecracker/images/rootfs-python.ext4 s3://llm-infra-operator-rootfs/
```

```bash title="Download a file"
aws s3 cp s3://llm-infra-operator-rootfs/rootfs-python.ext4 /srv/firecracker/images/
```

```bash title="Sync directory"
aws s3 sync /srv/firecracker/images/ s3://llm-infra-operator-rootfs/ --exclude "*" --include "rootfs-*.ext4"
```

## IAM Permissions

### Required Permissions

The EC2 instance role (or your AWS credentials) needs:

```json title="IAM policy for S3 rootfs access"
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Sid": "S3RootfsAccess",
      "Effect": "Allow",
      "Action": [
        "s3:GetObject",
        "s3:PutObject",
        "s3:ListBucket",
        "s3:DeleteObject"
      ],
      "Resource": [
        "arn:aws:s3:::llm-infra-operator-rootfs",
        "arn:aws:s3:::llm-infra-operator-rootfs/*"
      ]
    }
  ]
}
```

### Instance Profile (Production)

For production, attach an IAM role to the EC2 instance:

```bash title="Create IAM role"
aws iam create-role \
    --role-name FirecrackerEC2Role \
    --assume-role-policy-document file://trust-policy.json
```

```bash title="Attach S3 policy"
aws iam put-role-policy \
    --role-name FirecrackerEC2Role \
    --policy-name S3Access \
    --policy-document file://s3-policy.json
```

```bash title="Create instance profile"
aws iam create-instance-profile \
    --instance-profile-name FirecrackerProfile
```

```bash title="Add role to profile"
aws iam add-role-to-instance-profile \
    --instance-profile-name FirecrackerProfile \
    --role-name FirecrackerEC2Role
```

## Cost Analysis

### Storage Costs

| Image | Size | Monthly Cost |
|-------|------|--------------|
| vmlinux | 20.4 MiB | $0.0005 |
| rootfs-base.ext4 | 512 MiB | $0.012 |
| rootfs-bash.ext4 | 512 MiB | $0.012 |
| rootfs-python.ext4 | 600 MiB | $0.014 |
| rootfs-nodejs.ext4 | 700 MiB | $0.016 |
| rootfs-go.ext4 | 700 MiB | $0.016 |
| rootfs-rust.ext4 | 2.0 GiB | $0.046 |
| **Total** | **~5.0 GiB** | **~$0.12/month** |

### Transfer Costs

| Transfer Type | Cost |
|---------------|------|
| Upload (EC2 → S3, same region) | Free |
| Download (S3 → EC2, same region) | Free |
| Download (S3 → Internet) | $0.09/GB |
| Cross-region transfer | $0.02/GB |

:::tip

  Keep your S3 bucket and EC2 instances in the same region to avoid transfer costs.

:::

## Best Practices

### Versioning

Enable versioning for production to maintain history:

```bash title="Enable S3 bucket versioning"
aws s3api put-bucket-versioning \
    --bucket llm-infra-operator-rootfs \
    --versioning-configuration Status=Enabled
```

### Lifecycle Rules

Set up lifecycle rules to manage old versions:

```bash title="Apply lifecycle configuration"
aws s3api put-bucket-lifecycle-configuration \
    --bucket llm-infra-operator-rootfs \
    --lifecycle-configuration file://lifecycle.json
```

Example lifecycle.json:
```json title="lifecycle.json"
{
  "Rules": [
    {
      "ID": "DeleteOldVersions",
      "Status": "Enabled",
      "Filter": {},
      "NoncurrentVersionExpiration": {
        "NoncurrentDays": 30
      }
    }
  ]
}
```

### Cross-Region Replication

For multi-region deployments:

```bash title="Enable cross-region replication"
# Enable replication to eu-west-1
aws s3api put-bucket-replication \
    --bucket llm-infra-operator-rootfs \
    --replication-configuration file://replication.json
```

## Troubleshooting

### Access Denied

```
fatal error: An error occurred (AccessDenied) when calling the GetObject operation
```

**Solutions:**
1. Check IAM permissions include `s3:GetObject`
2. Verify bucket policy allows access
3. Check if bucket is in correct region

### Slow Downloads

If downloads are slow:
1. Verify EC2 and S3 are in same region
2. Use larger instance type for better network
3. Consider S3 Transfer Acceleration

### Missing Images

If images aren't found in S3:
```bash title="Check bucket exists"
aws s3 ls s3://llm-infra-operator-rootfs/
```

```bash title="Upload images manually"
task s3:upload
```

## Multi-Tenant Configuration

For multiple environments (dev, staging, prod):

```bash title="Create buckets per environment"
aws s3 mb s3://llm-infra-operator-rootfs-dev
aws s3 mb s3://llm-infra-operator-rootfs-staging
aws s3 mb s3://llm-infra-operator-rootfs-prod
```

```bash title="Use environment variable"
S3_BUCKET=llm-infra-operator-rootfs-dev task s3:list
```

Or use prefixes:
```text title="Multi-environment bucket structure"
s3://llm-infra-operator-rootfs/
├── dev/
│   ├── rootfs-python.ext4
│   └── ...
├── staging/
│   └── ...
└── prod/
    └── ...
```
