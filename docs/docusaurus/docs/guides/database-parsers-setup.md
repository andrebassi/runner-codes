---
title: 'Database Parsers Setup'
description: 'Complete guide to set up SQL and NoSQL syntax validators'
---

## Overview

This guide covers the complete setup process for SQL and NoSQL syntax validators in Runner Codes. These parsers validate query syntax WITHOUT requiring real database connections.

## Architecture

```text title="Database parsers architecture diagram"
┌─────────────────────────────────────────────────────────────┐
│                    Host Machine                              │
│  ┌─────────────────┐    ┌──────────────────────────────────┐│
│  │ infra.operator  │───▶│ Firecracker microVM              ││
│  │  (host mode)    │    │  ┌────────────────────────────┐  ││
│  └─────────────────┘    │  │ guest-runner               │  ││
│                         │  │  └─────────────────────────│  ││
│                         │  │  │ Node.js                 │  ││
│                         │  │  │ └── node-sql-parser     │  ││
│                         │  │  │ └── mysql-run.js        │  ││
│                         │  └──│─────────────────────────│  ││
│                         └─────┴──────────────────────────┴──┘│
└─────────────────────────────────────────────────────────────┘
```

## Prerequisites

- EC2 instance with Firecracker installed
- Docker for building images
- AWS credentials for S3 uploads
- `infra.operator` binary installed

## Step 1: Create Docker Images

### Directory Structure

```text title="Docker directory structure for parsers"
docker/
├── mysql-parser/
│   ├── Dockerfile
│   └── mysql-run.js
├── postgresql-parser/
│   ├── Dockerfile
│   └── psql-run.js
├── mariadb-parser/
│   ├── Dockerfile
│   └── mariadb-run.js
├── sqlite-parser/
│   ├── Dockerfile
│   └── sqlite-run.js
├── mongodb-parser/
│   ├── Dockerfile
│   └── mongodb-run.js
└── redis-parser/
    ├── Dockerfile
    └── redis-run.js
```

### SQL Parser Dockerfile

```dockerfile title="Dockerfile for MySQL parser"
# docker/mysql-parser/Dockerfile
FROM node:22-alpine3.20

# Install node-sql-parser
WORKDIR /opt/sql-parser
RUN npm init -y && npm install node-sql-parser --save

# Copy the runner script
COPY mysql-run.js /usr/local/bin/mysql-run
RUN chmod +x /usr/local/bin/mysql-run

# Set NODE_PATH for node-sql-parser
ENV NODE_PATH=/opt/sql-parser/node_modules

CMD ["node", "--version"]
```

### SQL Parser Script (mysql-run.js)

```javascript title="mysql-run.js - SQL validation script"
#!/usr/bin/env node
const fs = require('fs');
const { Parser } = require('node-sql-parser');

const parser = new Parser();
const DATABASE = 'MySQL';  // or PostgreSQL, MariaDB, sqlite

const args = process.argv.slice(2);
if (args.length === 0) {
    console.error('Usage: mysql-run.js <file.sql>');
    process.exit(1);
}

const sql = fs.readFileSync(args[0], 'utf8').trim();

try {
    const ast = parser.astify(sql, { database: DATABASE });
    const formatted = parser.sqlify(ast, { database: DATABASE });

    console.log('=== SQL Validation Result ===');
    console.log('Status: VALID');
    console.log('');
    console.log('=== Formatted SQL ===');
    console.log(formatted);
    console.log('');
    console.log('=== Statement Summary ===');
    if (Array.isArray(ast)) {
        console.log(`Total statements: ${ast.length}`);
        ast.forEach((stmt, i) => {
            console.log(`  ${i + 1}. ${stmt.type || 'unknown'}`);
        });
    } else {
        console.log(`Statement type: ${ast.type || 'unknown'}`);
    }
    process.exit(0);
} catch (err) {
    console.log('=== SQL Validation Result ===');
    console.log('Status: INVALID');
    console.error(`Syntax Error: ${err.message}`);
    process.exit(1);
}
```

### NoSQL Parser Dockerfile (MongoDB)

```dockerfile title="Dockerfile for MongoDB parser"
# docker/mongodb-parser/Dockerfile
FROM node:22-alpine3.20

WORKDIR /opt/mongodb-parser
COPY mongodb-run.js /usr/local/bin/mongodb-run
RUN chmod +x /usr/local/bin/mongodb-run

CMD ["node", "--version"]
```

## Step 2: Build and Push Docker Images

```bash title="Build and push all parser images to ttl.sh"
# Build and push all images
for parser in mysql-parser postgresql-parser mariadb-parser sqlite-parser mongodb-parser redis-parser; do
    echo "Building ${parser}..."
    docker build -t ttl.sh/llm-fc-${parser}:24h ./docker/${parser}/
    docker push ttl.sh/llm-fc-${parser}:24h
done
```

:::note

Using `ttl.sh` as ephemeral registry. Images auto-delete after 24 hours.
For production, use a permanent registry like ECR or Docker Hub.

:::

## Step 3: Create Rootfs Images

```bash title="SSH to EC2 instance"
ssh -i key.pem ubuntu@EC2_IP
```

```bash title="Create rootfs images for SQL parsers"
# SQL parsers need 500MB for node_modules
for parser in mysql-parser postgresql-parser mariadb-parser sqlite-parser; do
    echo "Creating rootfs for ${parser}..."
    sudo infra.operator rootfs from-docker \
        --name ${parser} \
        --image ttl.sh/llm-fc-${parser}:24h \
        --size 500
done
```

```bash title="Create rootfs images for NoSQL parsers"
# NoSQL parsers need 300MB
for parser in mongodb-parser redis-parser; do
    echo "Creating rootfs for ${parser}..."
    sudo infra.operator rootfs from-docker \
        --name ${parser} \
        --image ttl.sh/llm-fc-${parser}:24h \
        --size 300
done
```

### Verify Rootfs Creation

```bash title="List created parser rootfs images"
ls -lah /srv/firecracker/images/rootfs-*-parser.ext4
```


## Step 4: Create Snapshots

```bash title="Create snapshots for all parsers"
# Create snapshots (256MB memory is sufficient)
for parser in mysql-parser postgresql-parser mariadb-parser sqlite-parser mongodb-parser redis-parser; do
    echo "Creating snapshot for ${parser}..."
    sudo infra.operator snapshot create \
        --lang ${parser} \
        --mem 256 \
        --vcpus 1
done
```

### Verify Snapshots

```bash title="List created parser snapshots"
ls -la /dev/shm/snapshots/ | grep parser
```


## Step 5: Upload to S3

```bash title="Set AWS credentials"
export AWS_ACCESS_KEY_ID="your-key"
export AWS_SECRET_ACCESS_KEY="your-secret"
export AWS_DEFAULT_REGION="us-east-1"
```

```bash title="Upload rootfs"
for parser in mysql-parser postgresql-parser mariadb-parser sqlite-parser mongodb-parser redis-parser; do
    echo "Uploading rootfs ${parser}..."
    sudo -E infra.operator rootfs upload \
        --lang ${parser} \
        --bucket runner-codes
done
```

```bash title="Upload snapshots to S3"
for parser in mysql-parser postgresql-parser mariadb-parser sqlite-parser mongodb-parser redis-parser; do
    echo "Uploading snapshot ${parser}..."
    sudo -E infra.operator snapshot upload \
        --lang ${parser} \
        --bucket runner-codes
done
```

### Verify S3 Upload

```bash title="Verify rootfs uploads in S3"
aws s3 ls s3://runner-codes/ | grep parser
```

```bash title="Verify snapshot uploads in S3"
aws s3 ls s3://runner-codes/snapshots/ | grep parser
```

## Step 6: Update executor.go

Add the new languages to `pkg/guest/executor.go`:

```go title="Add SQL and NoSQL parsers to executor.go"
// Tier 11: SQL Syntax Validators (node-sql-parser based)
"mysql-parser": {
    Name:      "mysql-parser",
    Extension: ".sql",
    Command:   "/usr/local/bin/node",
    Args:      []string{"/usr/local/bin/mysql-run"},
    Env:       []string{"NODE_PATH=/opt/sql-parser/node_modules"},
},
"postgresql-parser": {
    Name:      "postgresql-parser",
    Extension: ".sql",
    Command:   "/usr/local/bin/node",
    Args:      []string{"/usr/local/bin/psql-run"},
    Env:       []string{"NODE_PATH=/opt/sql-parser/node_modules"},
},
// ... more parsers

// Tier 12: NoSQL Query Validators
"mongodb-parser": {
    Name:      "mongodb-parser",
    Extension: ".json",
    Command:   "/usr/local/bin/node",
    Args:      []string{"/usr/local/bin/mongodb-run"},
},
"redis-parser": {
    Name:      "redis-parser",
    Extension: ".redis",
    Command:   "/usr/local/bin/node",
    Args:      []string{"/usr/local/bin/redis-run"},
},
```

Also add `NODE_PATH` to the environment in `runCommandWithStdin`:

```go title="Add NODE_PATH environment variable"
// node-sql-parser module path for SQL validators
"NODE_PATH=/opt/sql-parser/node_modules",
```

## Step 7: Rebuild and Deploy Binary

```bash title="Build binary for Linux"
cd /path/to/runner-codes
GOOS=linux GOARCH=amd64 go build -o bin/infra.operator-linux ./cmd/infra.operator/...
```

```bash title="Copy binary to EC2 instance"
scp -i key.pem bin/infra.operator-linux ubuntu@EC2_IP:/tmp/infra.operator
```

```bash title="SSH to EC2 instance"
ssh ubuntu@EC2_IP
```

```bash title="Install the new binary"
sudo cp /tmp/infra.operator /usr/local/bin/infra.operator
sudo chmod +x /usr/local/bin/infra.operator
```

```bash title="Update all parser rootfs images with new binary"
for parser in mysql-parser postgresql-parser mariadb-parser sqlite-parser mongodb-parser redis-parser; do
    sudo mkdir -p /mnt/${parser}
    sudo mount -o loop /srv/firecracker/images/rootfs-${parser}.ext4 /mnt/${parser}
    sudo cp /tmp/infra.operator /mnt/${parser}/usr/local/bin/infra.operator
    sudo chmod +x /mnt/${parser}/usr/local/bin/infra.operator
    sync
    sudo umount /mnt/${parser}
    sudo rmdir /mnt/${parser}
done
```

## Step 8: Recreate Snapshots

```bash title="Delete old parser snapshots"
for parser in mysql-parser postgresql-parser mariadb-parser sqlite-parser mongodb-parser redis-parser; do
    sudo rm -rf /dev/shm/snapshots/${parser}
done
```

```bash title="Create fresh snapshots with updated binary"
for parser in mysql-parser postgresql-parser mariadb-parser sqlite-parser mongodb-parser redis-parser; do
    sudo infra.operator snapshot create --lang ${parser} --mem 256 --vcpus 1
done
```

```bash title="Upload updated snapshots to S3"
export AWS_ACCESS_KEY_ID="your-key"
export AWS_SECRET_ACCESS_KEY="your-secret"
for parser in mysql-parser postgresql-parser mariadb-parser sqlite-parser mongodb-parser redis-parser; do
    sudo -E infra.operator snapshot upload --lang ${parser} --bucket runner-codes
done
```

## Step 9: Test Execution

```bash title="Test MySQL Parser"
sudo infra.operator host \
    --lang mysql-parser \
    --snapshot \
    --mem 256 \
    --vcpus 1 \
    --code "SELECT * FROM users WHERE id = 1;"
```

```bash title="Test PostgreSQL Parser"
sudo infra.operator host \
    --lang postgresql-parser \
    --snapshot \
    --mem 256 \
    --vcpus 1 \
    --code "SELECT * FROM products ORDER BY price DESC LIMIT 10;"
```

```bash title="Test MongoDB Parser"
sudo infra.operator host \
    --lang mongodb-parser \
    --snapshot \
    --mem 256 \
    --vcpus 1 \
    --code '{"collection": "users", "filter": {"age": 25}}'
```

```bash title="Test Redis Parser"
sudo infra.operator host \
    --lang redis-parser \
    --snapshot \
    --mem 256 \
    --vcpus 1 \
    --code "SET user:1 john
GET user:1
HSET user:1:profile name John"
```

## Expected Output

### MySQL Parser Success

```json title="Successful MySQL validation response"
{
  "trace_id": "tr-xxx",
  "status": "success",
  "stdout": "=== SQL Validation Result ===\nStatus: VALID\n...",
  "exit_code": 0,
  "executionTime": "~2.8s"
}
```

### Performance Metrics

| Parser | Execution Time | Memory | Rootfs Size |
|--------|---------------|--------|-------------|
| MySQL Parser | ~2.8s | 256MB | 500MB |
| PostgreSQL Parser | ~2.8s | 256MB | 500MB |
| MariaDB Parser | ~2.8s | 256MB | 500MB |
| SQLite Parser | ~2.8s | 256MB | 500MB |
| MongoDB Parser | ~2.7s | 256MB | 300MB |
| Redis Parser | ~2.7s | 256MB | 300MB |

:::note

Execution time includes Node.js cold start (~2.5s). Subsequent executions
with warm Node.js would be faster, but each microVM execution is isolated.

:::

## Troubleshooting

### "unsupported language" Error

**Cause**: The guest-runner doesn't recognize the language.

**Solution**: Ensure:
1. `executor.go` has the language config
2. Binary was recompiled and deployed
3. Rootfs contains the updated binary
4. Snapshots were recreated

### "MODULE_NOT_FOUND" Error

**Cause**: Node.js can't find node-sql-parser.

**Solution**: Ensure:
1. `NODE_PATH` is set in environment
2. Rootfs has `/opt/sql-parser/node_modules/`

### Timeout Errors

**Cause**: Parser execution exceeding timeout.

**Solution**: Increase timeout or check:
1. Memory is sufficient (256MB minimum)
2. CPU is not overloaded
3. Vsock connection is working

## Summary

Database parsers provide a lightweight way to validate SQL and NoSQL query syntax without running actual databases. The complete setup involves:

1. **Docker images** with Node.js + parser scripts
2. **Rootfs** created from Docker images via infra.operator
3. **Snapshots** for fast VM startup
4. **S3 upload** for distribution
5. **executor.go** configuration for language support
6. **Binary deployment** to all rootfs images
