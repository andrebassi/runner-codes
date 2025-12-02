---
title: 'AWS Infrastructure Tasks'
description: 'Launch, manage, and terminate EC2 instances for Firecracker testing'
---

## Overview

AWS infrastructure tasks manage the lifecycle of EC2 metal instances required for running Firecracker microVMs.

:::note

Firecracker requires KVM support, which is only available on **bare metal** instances like `m5zn.metal`.

:::

## aws:launch

Launch a new EC2 metal instance with Firecracker pre-configured.

```bash title="Launch EC2 instance"
task aws:launch 2>&1 | tee /tmp/log.txt
```

```yaml title="Task definition"
aws:launch:
  desc: Launch EC2 instance for Firecracker testing
  deps:
    - aws:check-creds
    - aws:create-keypair
    - aws:create-security-group
    - aws:get-ami
  cmds:
    - |
      # Check for existing instance
      # Launch new instance with user-data script
      # Wait for instance to be running
      # Get public IP
```

**What it does:**
1. Verifies AWS credentials (`aws:check-creds`)
2. Creates SSH key pair (`aws:create-keypair`)
3. Creates security group with SSH access (`aws:create-security-group`)
4. Finds latest Ubuntu 22.04 AMI (`aws:get-ami`)
5. Launches metal instance with user-data script
6. Waits for instance to be running
7. Saves instance ID and public IP

```text title="Expected output"
Checking AWS credentials...
{
    "UserId": "AIDAXXXXXXXXXXXXXXXXX",
    "Account": "123456789012",
    "Arn": "arn:aws:iam::123456789012:user/your-user"
}
Creating SSH key pair...
Key pair infra-operator-key already exists
Creating security group...
Security group already exists: sg-0123456789abcdef0
Finding Ubuntu 22.04 AMI...
Ubuntu 22.04 AMI: ami-0c7217cdde317cfec
Launching EC2 instance...
Instance launched: i-0925a8816b6711753
Waiting for instance to be running...

===================================
Instance ready!
Instance ID: i-0925a8816b6711753
Public IP: 54.159.149.186

Connect with:
  ssh -i /path/to/aws/keys/infra-operator-key.pem ubuntu@54.159.149.186

Wait ~3-5 minutes for user-data script to complete
===================================
```

### Instance Configuration

| Parameter | Value |
|-----------|-------|
| Instance Type | `m5zn.metal` |
| AMI | Ubuntu 22.04 LTS |
| Region | `us-east-1` (configurable) |
| Volume | 50GB gp3 |
| Security Group | SSH (port 22) from anywhere |

:::warning

Metal instances cost approximately **$3-4/hour**. Remember to run `task aws:cleanup` when done!

:::

---

## aws:status

Check the current instance status.

```bash title="Check instance status"
task aws:status
```

```text title="Expected output"
Instance Status:
-------------------------------------------------
|                DescribeInstances              |
+------+----------------------+-------------+---+
|  ID  |  i-0925a8816b6711753 |             |   |
| State|  running             |             |   |
| Type |  m5zn.metal          |             |   |
|  IP  |  54.159.149.186      |             |   |
+------+----------------------+-------------+---+
```

---

## aws:ssh

SSH into the running instance.

```bash title="SSH into EC2 instance"
task aws:ssh
```

```yaml title="Task definition"
aws:ssh:
  desc: SSH into EC2 instance
  cmds:
    - |
      PUBLIC_IP=$(cat {{.PROJECT_DIR}}/aws/.public-ip)
      ssh -i {{.PROJECT_DIR}}/aws/keys/{{.AWS_KEY_NAME}}.pem \
        -o StrictHostKeyChecking=no ubuntu@$PUBLIC_IP
```

**What it does:**
1. Reads public IP from `aws/.public-ip`
2. Connects via SSH using the generated key pair
3. Skips host key verification for convenience

---

## aws:logs

View the Firecracker setup logs from the instance.

```bash title="View setup logs"
task aws:logs
```

```text title="Expected output"
=== LLM-Firecracker Setup Starting ===
Date: Sat Nov 29 04:25:00 UTC 2025
Updating system packages...
Installing dependencies...
Checking KVM availability...
KVM is available
Checking vhost_vsock...
vhost_vsock is available
Downloading Firecracker v1.7.0 for x86_64...
Firecracker version:
Firecracker v1.7.0

=== Downloading rootfs images from S3 ===
Downloading vmlinux kernel...
vmlinux downloaded successfully
Downloading rootfs images...

=== Downloaded rootfs images ===
-rw-r--r-- 1 root root 600M rootfs-python.ext4
-rw-r--r-- 1 root root 700M rootfs-nodejs.ext4
-rw-r--r-- 1 root root 700M rootfs-go.ext4
-rw-r--r-- 1 root root 2.0G rootfs-rust.ext4
-rw-r--r-- 1 root root 512M rootfs-bash.ext4

Default rootfs symlinked to rootfs-python.ext4

=== LLM-Firecracker Setup Complete ===
```

:::tip

  Run this task after `aws:launch` to verify the user-data script completed successfully.

:::

---

## aws:deploy

Build and deploy application code to the EC2 instance.

```bash title="Deploy code to EC2"
task aws:deploy 2>&1 | tee /tmp/log.txt
```

```yaml title="Task definition"
aws:deploy:
  desc: Deploy project code to EC2 instance
  cmds:
    - |
      # Build infra.operator for Linux (includes both host and guest)
      GOOS=linux GOARCH=amd64 go build -ldflags="-s -w" -o bin/infra.operator-linux ./cmd/infra.operator

      # Create directories on instance
      ssh ubuntu@$PUBLIC_IP "sudo mkdir -p /opt/llm-infra-operator/bin"

      # SCP binary
      scp bin/infra.operator-linux ubuntu@$PUBLIC_IP:/opt/llm-infra-operator/bin/infra.operator
```

**What it does:**
1. Cross-compiles infra.operator for Linux/amd64 (unified binary with host and guest)
2. Creates `/opt/llm-infra-operator/` on instance
3. SCPs binary to instance

```text title="Expected output"
Building infra.operator for Linux amd64...
Deploying to 54.159.149.186...
Deployment complete!

Next steps on the instance:
  1. SSH: task aws:ssh
  2. Inject infra.operator into rootfs
  3. Run test: task aws:test
```

---

## aws:inject-infra-operator

Inject the infra.operator binary into the rootfs image.

```bash title="Inject binary into rootfs"
task aws:inject-infra-operator 2>&1 | tee /tmp/log.txt
```

```yaml title="Task definition"
aws:inject-infra-operator:
  desc: Inject infra.operator into rootfs on EC2 instance
  cmds:
    - |
      ssh ubuntu@$PUBLIC_IP 'bash -s' << 'ENDSSH'
      # Mount rootfs
      sudo mount -o loop /srv/firecracker/rootfs.ext4 /mnt/rootfs

      # Copy infra.operator
      sudo cp /opt/llm-infra-operator/bin/infra.operator /mnt/rootfs/usr/local/bin/
      sudo chmod +x /mnt/rootfs/usr/local/bin/infra.operator

      # Create systemd service for guest mode
      # Link to multi-user.target

      # Unmount
      sudo umount /mnt/rootfs
      ENDSSH
```

**What it does:**
1. Mounts the rootfs image
2. Copies infra.operator to `/usr/local/bin/`
3. Creates systemd service for auto-start (runs `infra.operator guest`)
4. Links service to multi-user.target
5. Unmounts rootfs

```text title="Expected output"
Injecting infra.operator into rootfs...
Mounting rootfs...
Copying infra.operator...
Creating systemd service...
Unmounting...
Infra.operator injected successfully!
```

---

## aws:stop

Stop the instance (keeps it for later).

```bash title="Stop EC2 instance"
task aws:stop
```

**What it does:** Stops the instance without terminating. Instance can be restarted.

:::note

You still incur charges for EBS storage while the instance is stopped.

:::

---

## aws:start

Start a stopped instance.

```bash title="Start stopped instance"
task aws:start
```

```text title="Expected output"
Starting instance i-0925a8816b6711753...
Waiting for instance to be running...
Instance running. New IP: 52.90.123.45
```

:::warning

The public IP may change after stop/start. The task automatically updates `aws/.public-ip`.

:::

---

## aws:terminate

Terminate the instance (permanent delete).

```bash title="Terminate instance permanently"
task aws:terminate
```

```text title="Expected output"
WARNING: This will permanently delete instance i-0925a8816b6711753
Are you sure? (yes/no): yes
Instance terminated.
```

---

## aws:cleanup

Clean up all AWS resources created by the project.

```bash title="Clean up all AWS resources"
task aws:cleanup
```

```yaml title="Task definition"
aws:cleanup:
  desc: Clean up all AWS resources
  cmds:
    - |
      # Terminate instance
      # Wait for termination
      # Delete security group
      # Delete key pair
      # Clean local files
```

**What it cleans:**
1. Terminates EC2 instance
2. Deletes security group `infra-operator-sg`
3. Deletes SSH key pair
4. Removes local state files (`aws/.instance-id`, `aws/.public-ip`, etc.)
5. Removes SSH key from `aws/keys/`

```text title="Expected output"
Cleaning up AWS resources...
Terminating instance i-0925a8816b6711753...
Deleting security group sg-0123456789abcdef0...
Deleting key pair infra-operator-key...
Cleanup complete.
```

---

## aws:full-test

Complete test pipeline: launch, deploy, and test.

```bash title="Run complete test pipeline"
task aws:full-test 2>&1 | tee /tmp/log.txt
```

```yaml title="Task definition"
aws:full-test:
  desc: Complete AWS test pipeline (launch, deploy, test)
  cmds:
    - task: aws:launch
    - echo "Waiting 180 seconds for user-data to complete..."
    - sleep 180
    - task: aws:deploy
    - task: aws:inject-infra-operator
    - task: aws:test
```

**What it does:**
1. Launches new EC2 instance
2. Waits 3 minutes for user-data to complete
3. Deploys application code
4. Injects infra.operator into rootfs
5. Runs basic test

:::tip

This is the fastest way to go from zero to a working test environment.

:::

---

## Local State Files

The AWS tasks create these local files to track state:

| File | Contents |
|------|----------|
| `aws/.instance-id` | EC2 Instance ID |
| `aws/.public-ip` | Instance public IP |
| `aws/.ami-id` | Ubuntu AMI ID |
| `aws/.sg-id` | Security Group ID |
| `aws/keys/infra-operator-key.pem` | SSH private key |

:::warning

The SSH key file contains sensitive credentials. It's gitignored by default.

:::

---

## Cost Considerations

| Resource | Cost (us-east-1) |
|----------|------------------|
| m5zn.metal | ~$3.96/hour |
| 50GB gp3 EBS | ~$4.00/month |
| S3 storage | ~$0.023/GB/month |
| Data transfer | Varies |

**Tips to minimize costs:**
- Use `task aws:stop` when not actively testing
- Run `task aws:cleanup` when done
- Use smaller instance types for non-Firecracker testing
- Consider spot instances for development (not yet implemented)
