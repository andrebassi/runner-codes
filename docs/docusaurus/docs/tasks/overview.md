---
title: 'Tasks Overview'
description: 'Complete reference for all Taskfile commands'
---

## Task Categories

Runner Codes uses [Taskfile](https://taskfile.dev) for command automation. Tasks are organized into logical categories:








## Configuration Variables

All tasks use these configurable variables defined in `Taskfile.yaml`:

```yaml title="Taskfile.yaml configuration variables"
vars:
  # Firecracker Configuration
  FC_WORKDIR: '/srv/firecracker'
  FC_VERSION: 'v1.7.0'
  GUEST_CID: '3'
  VSOCK_PORT: '5000'

  # Build Directories
  BIN_DIR: 'bin'
  ROOTFS_DIR: 'rootfs'

  # AWS Configuration
  AWS_REGION: 'us-east-1'
  AWS_INSTANCE_TYPE: 'm5zn.metal'
  AWS_KEY_NAME: 'infra-operator-key'
  AWS_INSTANCE_NAME: 'llm-infra-operator-test'
  AWS_VPC_ID: 'vpc-0dd1e4d3c5ed4f3c7'
  AWS_SUBNET_ID: 'subnet-004de53997864f0a3'

  # S3 Configuration
  S3_BUCKET: 'llm-infra-operator-rootfs'
  S3_REGION: 'us-east-1'
```

:::tip

Override any variable by setting it in your environment or passing it to the task:
```bash title="Override AWS region"
AWS_REGION=eu-west-1 task aws:launch
```

:::

## Quick Reference

### Essential Commands

| Command | Description |
|---------|-------------|
| `task` | List all available tasks |
| `task aws:launch` | Launch EC2 instance with Firecracker |
| `task aws:deploy` | Deploy code to instance |
| `task aws:test` | Run basic execution test |
| `task aws:cleanup` | Terminate instance and cleanup |

### Build Commands

| Command | Description |
|---------|-------------|
| `task build:all` | Build all components |
| `task build:infra-operator` | Build infra.operator for current OS |
| `task build:infra-operator-linux` | Build infra.operator for Linux amd64 |

### S3 Commands

| Command | Description |
|---------|-------------|
| `task s3:list` | List rootfs images in S3 |
| `task s3:upload` | Upload images from EC2 to S3 |
| `task s3:download` | Download images from S3 to EC2 |

### Rootfs Commands

| Command | Description |
|---------|-------------|
| `task aws:create-rootfs-python` | Create Python rootfs + upload to S3 |
| `task aws:create-rootfs-nodejs` | Create Node.js rootfs + upload to S3 |
| `task aws:create-rootfs-go` | Create Go rootfs + upload to S3 |
| `task aws:create-rootfs-rust` | Create Rust rootfs + upload to S3 |
| `task aws:create-rootfs-bash` | Create Bash rootfs + upload to S3 |
| `task aws:create-all-rootfs` | Create all rootfs images |

### Testing Commands

| Command | Description |
|---------|-------------|
| `task aws:test` | Run basic Python test |
| `task aws:test-languages` | Quick language version check |
| `task aws:test-all-languages` | Full language execution test |
| `task test:unit` | Run unit tests |

## Task Execution Flow

### Standard Workflow

![Standard Task Workflow](/img/task-workflow.svg)

### Rootfs Creation Flow

![Rootfs Creation Flow](/img/rootfs-creation-flow.svg)

## Environment Requirements

Before running tasks, ensure you have:


```bash title="Install Task on macOS"
brew install go-task/tap/go-task
```

```bash title="Install Task on Linux"
sh -c "$(curl --location https://taskfile.dev/install.sh)" -- -d
```

```bash title="Install Go on macOS"
brew install go
```

```bash title="Install Go on Linux"
wget https://go.dev/dl/go1.21.5.linux-amd64.tar.gz
sudo tar -C /usr/local -xzf go1.21.5.linux-amd64.tar.gz
```

```bash title="Configure AWS credentials"
export AWS_ACCESS_KEY_ID="your-key"
export AWS_SECRET_ACCESS_KEY="your-secret"
export AWS_DEFAULT_REGION="us-east-1"
```


## Logging Best Practices

All tasks should be run with output logging for troubleshooting:

```bash title="Run task with logging"
task aws:launch 2>&1 | tee /tmp/log.txt
```

This ensures:
- Real-time output to terminal
- Full log saved for later analysis
- stderr captured alongside stdout

:::warning

When commands run in background (`run_in_background: true`), use `BashOutput` to monitor progress.

:::
