---
title: 'S3 Operations'
description: 'Upload, download, and manage rootfs images in S3'
---

## Overview

S3 operations tasks manage the storage and retrieval of rootfs images from Amazon S3. This enables:

- **Fast deployment**: New instances download pre-built images instead of building from scratch
- **Version control**: Store multiple versions of rootfs images
- **Distribution**: Share images across regions and accounts

## S3 Bucket Structure

```text title="S3 bucket directory structure"
s3://llm-infra-operator-rootfs/
├── vmlinux                    # Linux kernel (20.4 MB)
├── rootfs-python.ext4         # Python 3.10 rootfs (600 MB)
├── rootfs-nodejs.ext4         # Node.js 20 rootfs (700 MB)
├── rootfs-go.ext4             # Go 1.22.3 rootfs (700 MB)
├── rootfs-rust.ext4           # Rust stable rootfs (2 GB)
└── rootfs-bash.ext4           # Minimal bash rootfs (512 MB)
```

---

## s3:list

List all rootfs images in the S3 bucket.

```bash title="List S3 bucket contents"
task s3:list
```

```yaml title="Task definition for s3:list"
s3:list:
  desc: List rootfs images in S3 bucket
  cmds:
    - |
      echo "=== S3 bucket contents ({{.S3_BUCKET}}) ==="
      aws s3 ls s3://{{.S3_BUCKET}}/ --human-readable --summarize
```

```text title="Example output showing bucket contents"
=== S3 bucket contents (llm-infra-operator-rootfs) ===
2025-11-29 04:31:21  512.0 MiB rootfs-bash.ext4
2025-11-29 04:31:30  700.0 MiB rootfs-go.ext4
2025-11-29 04:31:33  700.0 MiB rootfs-nodejs.ext4
2025-11-29 04:31:36  600.0 MiB rootfs-python.ext4
2025-11-29 04:31:42    2.0 GiB rootfs-rust.ext4
2025-11-29 04:31:49   20.4 MiB vmlinux

Total Objects: 6
   Total Size: 4.5 GiB
```

---

## s3:upload

Upload all rootfs images from EC2 instance to S3.

```bash title="Upload all rootfs images to S3"
task s3:upload 2>&1 | tee /tmp/log.txt
```

```yaml title="Task definition for s3:upload"
s3:upload:
  desc: Upload rootfs images from EC2 to S3
  cmds:
    - |
      ssh ubuntu@$PUBLIC_IP 'bash -s' << 'ENDSSH'
      cd /srv/firecracker/images

      echo "=== Uploading rootfs images to S3 ==="
      for img in rootfs-*.ext4; do
        if [ -f "$img" ]; then
          echo "Uploading $img..."
          aws s3 cp "$img" s3://{{.S3_BUCKET}}/ --only-show-errors
          echo "$img uploaded"
        fi
      done

      if [ -f "../vmlinux" ]; then
        echo "Uploading vmlinux kernel..."
        aws s3 cp ../vmlinux s3://{{.S3_BUCKET}}/ --only-show-errors
        echo "vmlinux uploaded"
      fi
      ENDSSH
```

```text title="Example output showing upload progress"
Uploading rootfs images to S3...
=== Uploading rootfs images to S3 ===
Uploading rootfs-bash.ext4...
rootfs-bash.ext4 uploaded
Uploading rootfs-go.ext4...
rootfs-go.ext4 uploaded
Uploading rootfs-nodejs.ext4...
rootfs-nodejs.ext4 uploaded
Uploading rootfs-python.ext4...
rootfs-python.ext4 uploaded
Uploading rootfs-rust.ext4...
rootfs-rust.ext4 uploaded
Uploading vmlinux kernel...
vmlinux uploaded

=== S3 bucket contents ===
2025-11-29 04:31:21  512.0 MiB rootfs-bash.ext4
2025-11-29 04:31:30  700.0 MiB rootfs-go.ext4
2025-11-29 04:31:33  700.0 MiB rootfs-nodejs.ext4
2025-11-29 04:31:36  600.0 MiB rootfs-python.ext4
2025-11-29 04:31:42    2.0 GiB rootfs-rust.ext4
2025-11-29 04:31:49   20.4 MiB vmlinux
```

:::note

Upload time depends on image size and network bandwidth. Rust (2GB) takes significantly longer than Bash (512MB).

:::

---

## s3:download

Download all rootfs images from S3 to EC2 instance.

```bash title="Download all rootfs images from S3"
task s3:download 2>&1 | tee /tmp/log.txt
```

```yaml title="Task definition for s3:download"
s3:download:
  desc: Download rootfs images from S3 to EC2
  cmds:
    - |
      ssh ubuntu@$PUBLIC_IP 'bash -s' << 'ENDSSH'
      cd /srv/firecracker

      echo "=== Downloading rootfs images from S3 ==="
      sudo mkdir -p /srv/firecracker/images

      # Download vmlinux kernel
      echo "Downloading vmlinux..."
      sudo aws s3 cp s3://{{.S3_BUCKET}}/vmlinux /srv/firecracker/vmlinux

      # Download all rootfs images
      echo "Downloading rootfs images..."
      sudo aws s3 sync s3://{{.S3_BUCKET}}/ /srv/firecracker/images/ \
        --exclude "*" --include "rootfs-*.ext4"

      # Create symlink to default rootfs (python)
      if [ -f /srv/firecracker/images/rootfs-python.ext4 ]; then
        sudo ln -sf /srv/firecracker/images/rootfs-python.ext4 /srv/firecracker/rootfs.ext4
      fi
      ENDSSH
```

```text title="Example output showing download progress"
Downloading rootfs images from S3...
=== Downloading rootfs images from S3 ===
Downloading vmlinux...
download: s3://llm-infra-operator-rootfs/vmlinux to /srv/firecracker/vmlinux
Downloading rootfs images...
download: s3://llm-infra-operator-rootfs/rootfs-bash.ext4 to images/rootfs-bash.ext4
download: s3://llm-infra-operator-rootfs/rootfs-go.ext4 to images/rootfs-go.ext4
download: s3://llm-infra-operator-rootfs/rootfs-nodejs.ext4 to images/rootfs-nodejs.ext4
download: s3://llm-infra-operator-rootfs/rootfs-python.ext4 to images/rootfs-python.ext4
download: s3://llm-infra-operator-rootfs/rootfs-rust.ext4 to images/rootfs-rust.ext4

=== Downloaded files ===
-rw-r--r-- 1 root root  21M Nov 29 04:31 /srv/firecracker/vmlinux
-rw-r--r-- 1 root root 512M Nov 29 04:31 rootfs-bash.ext4
-rw-r--r-- 1 root root 700M Nov 29 04:31 rootfs-go.ext4
-rw-r--r-- 1 root root 700M Nov 29 04:31 rootfs-nodejs.ext4
-rw-r--r-- 1 root root 600M Nov 29 04:31 rootfs-python.ext4
-rw-r--r-- 1 root root 2.0G Nov 29 04:31 rootfs-rust.ext4
```

:::tip

The `aws s3 sync` command only downloads files that don't exist locally or have changed, making subsequent syncs much faster.

:::

---

## s3:upload-single

Upload a single rootfs image to S3.

```bash title="Upload a single rootfs image to S3"
task s3:upload-single LANG=python
```

```yaml title="Task definition for s3:upload-single"
s3:upload-single:
  desc: Upload a single rootfs image to S3 (usage: task s3:upload-single LANG=python)
  cmds:
    - |
      LANG={{.LANG | default "python"}}
      echo "Uploading rootfs-${LANG}.ext4 to S3..."
      ssh ubuntu@$PUBLIC_IP "aws s3 cp /srv/firecracker/images/rootfs-${LANG}.ext4 s3://{{.S3_BUCKET}}/"
```

**Available languages:**
- `python`
- `nodejs`
- `go`
- `rust`
- `bash`

```text title="Example output for single image upload"
Uploading rootfs-python.ext4 to S3...
upload: images/rootfs-python.ext4 to s3://llm-infra-operator-rootfs/rootfs-python.ext4
Upload complete!
```

---

## Automatic Upload After Creation

When using the `aws:create-rootfs-*` tasks, images are automatically uploaded to S3 after creation:

```bash title="Creates Python rootfs AND uploads to S3"
task aws:create-rootfs-python
```

```bash title="Creates all rootfs images AND uploads each to S3"
task aws:create-all-rootfs
```

```text title="Example output showing rootfs creation and upload"
=== Creating Python rootfs (600MB) ===
dd if=/dev/zero of=/tmp/rootfs-python.ext4 bs=1M count=600
mkfs.ext4 /tmp/rootfs-python.ext4
...
Installing Python...
Installing infra.operator...
Creating systemd service...

Uploading to S3...
upload: images/rootfs-python.ext4 to s3://llm-infra-operator-rootfs/rootfs-python.ext4

=== Python rootfs created and uploaded to S3 ===
-rw-r--r-- 1 root root 600M rootfs-python.ext4
```

---

## User-Data S3 Integration

New EC2 instances automatically download rootfs images from S3 during initialization. This is configured in `aws/user-data.sh`:

```bash title="Set S3 bucket configuration"
S3_BUCKET="llm-infra-operator-rootfs"
S3_REGION="us-east-1"
```

```bash title="Download vmlinux kernel from S3"
aws s3 cp s3://${S3_BUCKET}/vmlinux ${WORKDIR}/vmlinux --region ${S3_REGION}
```

```bash title="Sync all rootfs images from S3"
aws s3 sync s3://${S3_BUCKET}/ ${WORKDIR}/images/ \
    --exclude "*" --include "rootfs-*.ext4" \
    --region ${S3_REGION}
```

```bash title="Create symlink to default Python rootfs"
ln -sf ${WORKDIR}/images/rootfs-python.ext4 ${WORKDIR}/rootfs.ext4
```

:::note

If S3 images aren't available, the user-data script falls back to downloading a basic rootfs from AWS Firecracker releases.

:::

---

## S3 Configuration

### Bucket Settings

| Setting | Value |
|---------|-------|
| Bucket Name | `llm-infra-operator-rootfs` |
| Region | `us-east-1` |
| Access | Private (default) |
| Versioning | Disabled (recommended to enable) |

### IAM Permissions Required

The EC2 instance role (or your AWS credentials) needs these S3 permissions:

```json title="Required IAM policy for S3 access"
{
  "Version": "2012-10-17",
  "Statement": [
    {
      "Effect": "Allow",
      "Action": [
        "s3:GetObject",
        "s3:PutObject",
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

---

## Cost Optimization

### Storage Costs

| Image | Size | Monthly Cost |
|-------|------|--------------|
| rootfs-bash.ext4 | 512 MB | $0.012 |
| rootfs-python.ext4 | 600 MB | $0.014 |
| rootfs-nodejs.ext4 | 700 MB | $0.016 |
| rootfs-go.ext4 | 700 MB | $0.016 |
| rootfs-rust.ext4 | 2 GB | $0.046 |
| vmlinux | 20 MB | $0.001 |
| **Total** | **~4.5 GB** | **~$0.10/month** |

### Transfer Costs

- **Upload**: Free (from EC2 to S3 in same region)
- **Download**: Free (from S3 to EC2 in same region)
- **Cross-region**: $0.02/GB

:::tip

Keep your S3 bucket in the same region as your EC2 instances to avoid transfer costs.

:::

---

## Workflow Diagram

![S3 Storage Workflow](/img/s3-workflow.svg)

