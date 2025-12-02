---
title: 'MySQL Parser'
description: 'MySQL SQL syntax validation without database connection'
---

## Overview

MySQL Parser validates MySQL SQL syntax using [node-sql-parser](https://github.com/taozhi8833998/node-sql-parser) WITHOUT requiring a real MySQL database.

## Specifications

| Property | Value |
|----------|-------|
| Language ID | `mysql-parser` |
| Base OS | Alpine Linux 3.20 |
| Runtime | Node.js 22 |
| Parser | node-sql-parser |
| Rootfs Size | 500 MB |
| Memory | 256 MB |
| File Extension | `.sql` |
| Execution Time | ~2.8s |

## Infrastructure

### Step 1: Create Docker Directory

```bash title="Create working directory for MySQL parser"
mkdir -p /tmp/docker/mysql-parser
cd /tmp/docker/mysql-parser
```

### Step 2: Create mysql-run.js Script

```bash title="Create Node.js parser wrapper script"
cat > mysql-run.js << 'EOF'
#!/usr/bin/env node
const fs = require('fs');
const { Parser } = require('node-sql-parser');

const parser = new Parser();
const DATABASE = 'MySQL';

function main() {
    const args = process.argv.slice(2);
    if (args.length === 0) {
        console.error('Usage: mysql-run.js <file.sql>');
        process.exit(1);
    }

    const sqlFile = args[0];
    let sql;
    try {
        sql = fs.readFileSync(sqlFile, 'utf8').trim();
    } catch (err) {
        console.error(`Error reading file: ${err.message}`);
        process.exit(1);
    }

    if (!sql) {
        console.error('Error: Empty SQL file');
        process.exit(1);
    }

    try {
        const ast = parser.astify(sql, { database: DATABASE });
        const formatted = parser.sqlify(ast, { database: DATABASE });

        console.log('=== SQL Validation Result ===');
        console.log('Status: VALID');
        console.log('');
        console.log('=== Formatted SQL ===');
        console.log(formatted);
        console.log('');

        if (Array.isArray(ast)) {
            console.log(`=== Statement Summary ===`);
            console.log(`Total statements: ${ast.length}`);
            ast.forEach((stmt, i) => {
                console.log(`  ${i + 1}. ${stmt.type || 'unknown'}`);
            });
        } else {
            console.log('=== Statement Summary ===');
            console.log(`Statement type: ${ast.type || 'unknown'}`);
        }
        process.exit(0);
    } catch (err) {
        console.log('=== SQL Validation Result ===');
        console.log('Status: INVALID');
        console.log('');
        console.log('=== Error Details ===');
        console.error(`Syntax Error: ${err.message}`);
        if (err.location) {
            console.error(`Location: Line ${err.location.start.line}, Column ${err.location.start.column}`);
        }
        process.exit(1);
    }
}

main();
EOF
```

### Step 3: Create Dockerfile

```bash title="Create Dockerfile with Node.js and parser dependencies"
cat > Dockerfile << 'EOF'
FROM node:22-alpine3.20

WORKDIR /opt/sql-parser
RUN npm init -y && npm install node-sql-parser --save

COPY mysql-run.js /usr/local/bin/mysql-run
RUN chmod +x /usr/local/bin/mysql-run

ENV NODE_PATH=/opt/sql-parser/node_modules

CMD ["node", "--version"]
EOF
```

### Step 4: Build and Push Docker Image

```bash title="Build and push Docker image to ttl.sh registry"
docker build -t ttl.sh/llm-fc-mysql-parser:24h .
docker push ttl.sh/llm-fc-mysql-parser:24h
```

### Step 5: Create Rootfs

```bash title="Create rootfs from Docker image"
sudo infra.operator rootfs from-docker \
    --name mysql-parser \
    --image ttl.sh/llm-fc-mysql-parser:24h \
    --size 500
```

### Step 6: Create Snapshot

```bash title="Create VM snapshot for fast boot"
sudo infra.operator snapshot create \
    --lang mysql-parser \
    --mem 256 \
    --vcpus 1
```

### Step 7: Upload to S3

```bash title="Upload rootfs and snapshot to S3 bucket"
export AWS_ACCESS_KEY_ID="your-key"
export AWS_SECRET_ACCESS_KEY="your-secret"
export AWS_DEFAULT_REGION="us-east-1"

sudo -E infra.operator rootfs upload --lang mysql-parser --bucket llm-firecracker
sudo -E infra.operator snapshot upload --lang mysql-parser --bucket llm-firecracker
```

### Step 8: Test Execution

```bash title="Test parser with sample SQL"
sudo infra.operator host \
    --lang mysql-parser \
    --snapshot \
    --mem 256 \
    --vcpus 1 \
    --code "SELECT * FROM users WHERE id = 1;"
```

## Examples

### Hello World

```json title="Request"
{
  "trace_id": "mysql-hello-001",
  "lang": "mysql-parser",
  "code": "SELECT 'Hello from MySQL Parser!' AS message;",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "mysql-hello-001",
  "stdout": "=== SQL Validation Result ===\nStatus: VALID\n\n=== Formatted SQL ===\nSELECT 'Hello from MySQL Parser!' AS `message`\n\n=== Statement Summary ===\nTotal statements: 1\n  1. select\n",
  "stderr": "",
  "exit_code": 0
}
```

### Complex Query

```json title="Request"
{
  "trace_id": "mysql-complex-001",
  "lang": "mysql-parser",
  "code": "SELECT u.name, COUNT(o.id) as order_count, SUM(o.total) as total_spent\nFROM users u\nLEFT JOIN orders o ON u.id = o.user_id\nWHERE u.created_at > '2024-01-01'\nGROUP BY u.id, u.name\nHAVING COUNT(o.id) > 5\nORDER BY total_spent DESC;",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "mysql-complex-001",
  "stdout": "=== SQL Validation Result ===\nStatus: VALID\n\n=== Formatted SQL ===\nSELECT `u`.`name`, COUNT(`o`.`id`) AS `order_count`, SUM(`o`.`total`) AS `total_spent` FROM `users` AS `u` LEFT JOIN `orders` AS `o` ON `u`.`id` = `o`.`user_id` WHERE `u`.`created_at` > '2024-01-01' GROUP BY `u`.`id`, `u`.`name` HAVING COUNT(`o`.`id`) > 5 ORDER BY `total_spent` DESC\n\n=== Statement Summary ===\nTotal statements: 1\n  1. select\n",
  "stderr": "",
  "exit_code": 0
}
```

### Invalid SQL Detection

```json title="Request"
{
  "trace_id": "mysql-invalid-001",
  "lang": "mysql-parser",
  "code": "SELEC * FORM users;",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "mysql-invalid-001",
  "stdout": "=== SQL Validation Result ===\nStatus: INVALID\n\n=== Error Details ===\nSyntax Error: ...",
  "stderr": "",
  "exit_code": 1
}
```

## Executor Configuration

```go title="Go code"
// pkg/guest/executor.go
"mysql-parser": {
    Name:      "mysql-parser",
    Extension: ".sql",
    Command:   "/usr/local/bin/node",
    Args:      []string{"/usr/local/bin/mysql-run"},
    Env:       []string{"NODE_PATH=/opt/sql-parser/node_modules"},
},
```
