---
title: 'Full Pipeline'
description: 'Complete AWS deployment workflow from build to execution'
---

## Pipeline Overview

The complete pipeline takes code from source to execution in a secure microVM on AWS.

![Runner Codes Pipeline](/img/pipeline-overview.svg)

## Quick Start

Run the complete pipeline with these commands:

```bash title="1. Set AWS credentials"
export AWS_ACCESS_KEY_ID="your-key"
export AWS_SECRET_ACCESS_KEY="your-secret"
export AWS_DEFAULT_REGION="us-east-1"
```

```bash title="2. Build everything locally"
task build:all
```

```bash title="3. Upload rootfs to S3"
task s3:upload
```

```bash title="4. Launch EC2 instance"
task aws:launch
```

```bash title="5. Deploy binaries"
task aws:deploy
```

```bash title="6. Inject infra.operator into rootfs"
task aws:inject-infra-operator
```

```bash title="7. Test execution"
task aws:test LANG=python CODE='print("Hello from microVM!")'
```

## Pipeline Stages

### Stage 1: Build

Build all binaries locally:

```bash title="Build all binaries"
task build:all
```

This creates:
- `bin/infra.operator` - Host-side CLI (darwin)
- `bin/infra.operator-linux` - Linux binary for guest (x86_64)

**Output:**
```text title="Build output"
Building infra.operator...
go build -o bin/infra.operator ./cmd/infra.operator
Building infra.operator for Linux amd64...
GOOS=linux GOARCH=amd64 CGO_ENABLED=0 go build -o bin/infra.operator-linux ./cmd/infra.operator
Build complete!
```

### Stage 2: Create Rootfs Images

Create language-specific rootfs images (requires Linux or EC2):

```bash title="Create all rootfs images"
task aws:create-all-rootfs
```

```bash title="Or individual languages"
task aws:create-rootfs-python
task aws:create-rootfs-nodejs
task aws:create-rootfs-go
task aws:create-rootfs-rust
task aws:create-rootfs-bash
```

Each task:
1. Creates empty ext4 image
2. Installs Ubuntu minimal
3. Installs language runtime
4. Installs infra.operator
5. Configures systemd service
6. Uploads to S3

**Output:**
```text title="Rootfs creation output"
Creating Python rootfs...
Creating 600MB image file...
Installing Ubuntu 22.04 base...
Installing Python 3.10...
Installing infra.operator...
Configuring systemd service...
Uploading to S3...
upload: rootfs-python.ext4 to s3://llm-infra-operator-rootfs/rootfs-python.ext4
Done!
```

### Stage 3: Upload to S3

Upload all images to S3:

```bash title="Upload all"
task s3:upload
```

```bash title="Upload single image"
task s3:upload-single LANG=python
```

**S3 Structure:**
```text title="S3 bucket structure"
s3://llm-infra-operator-rootfs/
├── vmlinux              # Linux kernel
├── rootfs-python.ext4   # Python rootfs
├── rootfs-nodejs.ext4   # Node.js rootfs
├── rootfs-go.ext4       # Go rootfs
├── rootfs-rust.ext4     # Rust rootfs
└── rootfs-bash.ext4     # Bash rootfs
```

### Stage 4: Launch EC2

Launch a metal instance:

```bash title="Launch EC2 instance"
task aws:launch
```

This:
1. Creates security group (firecracker-sg)
2. Generates SSH key pair
3. Launches c5.metal instance
4. Runs user-data script which:
   - Installs Firecracker
   - Loads kernel modules
   - Downloads images from S3
5. Saves instance ID and IP

**Output:**
```text title="Instance launch output"
Creating security group...
Creating key pair...
Launching c5.metal instance...
Waiting for instance to be running...
Instance ID: i-0abc123def456
Public IP: 54.123.45.67
Waiting for setup to complete...
Instance ready!
```

### Stage 5: Deploy Binaries

Deploy infra.operator to EC2:

```bash title="Deploy binaries to EC2"
task aws:deploy
```

This copies:
- `bin/infra.operator-linux` → `/usr/local/bin/infra.operator`

**Output:**
```text title="Deployment output"
Deploying to 54.123.45.67...
Copying infra.operator...
Setting permissions...
Deployment complete!
```

### Stage 6: Inject Infra.Operator

Inject infra.operator into all rootfs images:

```bash title="Inject infra.operator into rootfs"
task aws:inject-infra-operator
```

For each rootfs image:
1. Mount the ext4 image
2. Copy infra.operator to /usr/local/bin/
3. Unmount

**Output:**
```text title="Injection output"
Injecting infra.operator into rootfs images...
Processing rootfs-python.ext4...
Processing rootfs-nodejs.ext4...
Processing rootfs-go.ext4...
Processing rootfs-rust.ext4...
Processing rootfs-bash.ext4...
Injection complete!
```

### Stage 7: Test

Run test executions:

```bash title="Test Python"
task aws:test LANG=python CODE='print("Hello Python!")'
```

```bash title="Test Node.js"
task aws:test LANG=node CODE='console.log("Hello Node!")'
```

```bash title="Test Go"
task aws:test LANG=go CODE='package main
import "fmt"
func main() { fmt.Println("Hello Go!") }'
```

```bash title="Test Rust"
task aws:test LANG=rust CODE='fn main() { println!("Hello Rust!"); }'
```

```bash title="Test Bash"
task aws:test LANG=bash CODE='echo "Hello Bash!"'
```

**Output:**
```text title="Test execution output"
Testing Python execution...
{
  "trace_id": "test-py-001",
  "stdout": "Hello Python!\n",
  "stderr": "",
  "exit_code": 0
}
Test successful!
```

## Complete Example

Full pipeline from scratch:

```bash title="run-pipeline.sh"
set -e
echo "=== Runner Codes Full Pipeline ==="
```

```bash title="Set AWS credentials"
echo "1. Setting up AWS credentials..."
export AWS_ACCESS_KEY_ID="your-key"
export AWS_SECRET_ACCESS_KEY="your-secret"
export AWS_DEFAULT_REGION="us-east-1"
```

```bash title="Build binaries"
echo "2. Building binaries..."
task build:all
```

```bash title="Check S3 bucket"
echo "3. Checking S3 bucket..."
task s3:list || {
  echo "Creating S3 bucket..."
  aws s3 mb s3://llm-infra-operator-rootfs --region us-east-1
}
```

```bash title="Upload rootfs images to S3"
echo "4. Uploading images to S3..."
task s3:upload
```

```bash title="Launch EC2 metal instance"
echo "5. Launching EC2 instance..."
task aws:launch
```

```bash title="Wait for instance setup"
echo "6. Waiting for instance setup (5 minutes)..."
sleep 300
```

```bash title="Deploy binaries to EC2"
echo "7. Deploying binaries..."
task aws:deploy
```

```bash title="Inject infra.operator into rootfs"
echo "8. Injecting infra.operator..."
task aws:inject-infra-operator
```

```bash title="Run tests for all languages"
echo "9. Running tests..."
echo "Testing Python..."
task aws:test LANG=python CODE='print("Hello Python!")'
echo "Testing Node.js..."
task aws:test LANG=node CODE='console.log("Hello Node!")'
echo "Testing Go..."
task aws:test LANG=go CODE='package main; import "fmt"; func main() { fmt.Println("Hello Go!") }'
echo "Testing Bash..."
task aws:test LANG=bash CODE='echo "Hello Bash!"'
echo "=== Pipeline Complete! ==="
echo "Instance IP: $(cat aws/.public-ip)"
```

## Pipeline Diagram

![Runner Codes Detailed Pipeline](/img/pipeline-detailed.svg)

## CI/CD Integration

### GitHub Actions

```yaml title=".github/workflows/deploy.yml"
# .github/workflows/deploy.yml
name: Deploy Runner Codes

on:
  push:
    branches: [main]

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
      - uses: actions/checkout@v4

      - uses: actions/setup-go@v5
        with:
          go-version: '1.22'

      - name: Install Task
        run: |
          sh -c "$(curl --location https://taskfile.dev/install.sh)" -- -d -b /usr/local/bin

      - name: Build
        run: task build:all

      - name: Upload to S3
        env:
          AWS_ACCESS_KEY_ID: ${{ secrets.AWS_ACCESS_KEY_ID }}
          AWS_SECRET_ACCESS_KEY: ${{ secrets.AWS_SECRET_ACCESS_KEY }}
          AWS_DEFAULT_REGION: us-east-1
        run: |
          task s3:upload

      - name: Deploy to EC2
        env:
          EC2_HOST: ${{ secrets.EC2_HOST }}
          SSH_KEY: ${{ secrets.SSH_KEY }}
        run: |
          echo "$SSH_KEY" > key.pem
          chmod 600 key.pem
          scp -i key.pem bin/infra.operator-linux ec2-user@$EC2_HOST:/usr/local/bin/infra.operator
```

### GitLab CI

```yaml title=".gitlab-ci.yml"
# .gitlab-ci.yml
stages:
  - build
  - deploy
  - test

build:
  stage: build
  image: golang:1.22
  script:
    - go build -o bin/infra.operator ./cmd/infra.operator
    - GOOS=linux GOARCH=amd64 CGO_ENABLED=0 go build -o bin/infra.operator-linux ./cmd/infra.operator
  artifacts:
    paths:
      - bin/

deploy:
  stage: deploy
  script:
    - aws s3 sync bin/ s3://llm-infra-operator-rootfs/bin/
    - scp bin/infra.operator-linux ec2-user@$EC2_HOST:/usr/local/bin/infra.operator
  only:
    - main

test:
  stage: test
  script:
    - curl -X POST http://$EC2_HOST:8080/execute -d '{"lang":"python","code":"print(1)"}'
  only:
    - main
```

## Cleanup

When done, clean up AWS resources:

```bash title="Terminate instance"
task aws:cleanup
```

```bash title="Optionally delete S3 bucket"
aws s3 rb s3://llm-infra-operator-rootfs --force
```

## Cost Summary

| Resource | Cost | Duration |
|----------|------|----------|
| c5.metal | ~$4/hr | While running |
| S3 storage | ~$0.02/GB/month | Persistent |
| Data transfer | ~$0.09/GB | Out to internet |

**Typical development session:**
- 2 hours of c5.metal: ~$8
- S3 (10GB): ~$0.20/month
- Data transfer: ~$0.50
- **Total: ~$8.70**
