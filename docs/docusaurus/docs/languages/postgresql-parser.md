---
title: 'PostgreSQL Parser'
description: 'PostgreSQL SQL syntax validation without database connection'
---

## Overview

PostgreSQL Parser validates PostgreSQL SQL syntax using [node-sql-parser](https://github.com/taozhi8833998/node-sql-parser) WITHOUT requiring a real PostgreSQL database.

## Specifications

| Property | Value |
|----------|-------|
| Language ID | `postgresql-parser` |
| Base OS | Alpine Linux 3.20 |
| Runtime | Node.js 22 |
| Parser | node-sql-parser |
| Rootfs Size | 500 MB |
| Memory | 256 MB |
| File Extension | `.sql` |
| Execution Time | ~2.8s |

## Infrastructure

### Step 1: Create Docker Directory

```bash title="Create working directory for PostgreSQL parser"
mkdir -p /tmp/docker/postgresql-parser
cd /tmp/docker/postgresql-parser
```

### Step 2: Create psql-run.js Script

```bash title="Create Node.js parser wrapper script"
cat > psql-run.js << 'EOF'
#!/usr/bin/env node
const fs = require('fs');
const { Parser } = require('node-sql-parser');

const parser = new Parser();
const DATABASE = 'PostgreSQL';

function main() {
    const args = process.argv.slice(2);
    if (args.length === 0) {
        console.error('Usage: psql-run.js <file.sql>');
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
        console.log('Database: PostgreSQL');
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
        console.log('Database: PostgreSQL');
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

COPY psql-run.js /usr/local/bin/psql-run
RUN chmod +x /usr/local/bin/psql-run

ENV NODE_PATH=/opt/sql-parser/node_modules

CMD ["node", "--version"]
EOF
```

### Step 4: Build and Push Docker Image

```bash title="Build and push Docker image to ttl.sh registry"
docker build -t ttl.sh/llm-fc-postgresql-parser:24h .
docker push ttl.sh/llm-fc-postgresql-parser:24h
```

### Step 5: Create Rootfs

```bash title="Create rootfs from Docker image"
sudo infra.operator rootfs from-docker \
    --name postgresql-parser \
    --image ttl.sh/llm-fc-postgresql-parser:24h \
    --size 500
```

### Step 6: Create Snapshot

```bash title="Create VM snapshot for fast boot"
sudo infra.operator snapshot create \
    --lang postgresql-parser \
    --mem 256 \
    --vcpus 1
```

### Step 7: Upload to S3

```bash title="Upload rootfs and snapshot to S3 bucket"
export AWS_ACCESS_KEY_ID="your-key"
export AWS_SECRET_ACCESS_KEY="your-secret"
export AWS_DEFAULT_REGION="us-east-1"

sudo -E infra.operator rootfs upload --lang postgresql-parser --bucket llm-firecracker
sudo -E infra.operator snapshot upload --lang postgresql-parser --bucket llm-firecracker
```

### Step 8: Test Execution

```bash title="Test parser with sample SQL"
sudo infra.operator host \
    --lang postgresql-parser \
    --snapshot \
    --mem 256 \
    --vcpus 1 \
    --code "SELECT * FROM products WHERE price > 100 ORDER BY price DESC LIMIT 10;"
```

## Examples

### Hello World

```json title="Request"
{
  "trace_id": "pg-hello-001",
  "lang": "postgresql-parser",
  "code": "SELECT 'Hello from PostgreSQL Parser!' AS message;",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "pg-hello-001",
  "stdout": "=== SQL Validation Result ===\nStatus: VALID\nDatabase: PostgreSQL\n\n=== Formatted SQL ===\nSELECT 'Hello from PostgreSQL Parser!' AS \"message\"\n\n=== Statement Summary ===\nTotal statements: 1\n  1. select\n",
  "stderr": "",
  "exit_code": 0
}
```

### Window Functions

```json title="Request"
{
  "trace_id": "pg-window-001",
  "lang": "postgresql-parser",
  "code": "SELECT name, salary, department,\n       ROW_NUMBER() OVER (PARTITION BY department ORDER BY salary DESC) as rank\nFROM employees;",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "pg-window-001",
  "stdout": "=== SQL Validation Result ===\nStatus: VALID\nDatabase: PostgreSQL\n\n=== Formatted SQL ===\nSELECT \"name\", \"salary\", \"department\", ROW_NUMBER() OVER (PARTITION BY \"department\" ORDER BY \"salary\" DESC) AS \"rank\" FROM \"employees\"\n\n=== Statement Summary ===\nTotal statements: 1\n  1. select\n",
  "stderr": "",
  "exit_code": 0
}
```

### CTE (Common Table Expression)

```json title="Request"
{
  "trace_id": "pg-cte-001",
  "lang": "postgresql-parser",
  "code": "WITH active_users AS (\n  SELECT * FROM users WHERE status = 'active'\n)\nSELECT * FROM active_users WHERE created_at > '2024-01-01';",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "pg-cte-001",
  "stdout": "=== SQL Validation Result ===\nStatus: VALID\nDatabase: PostgreSQL\n\n=== Formatted SQL ===\nWITH \"active_users\" AS (SELECT * FROM \"users\" WHERE \"status\" = 'active') SELECT * FROM \"active_users\" WHERE \"created_at\" > '2024-01-01'\n\n=== Statement Summary ===\nTotal statements: 1\n  1. select\n",
  "stderr": "",
  "exit_code": 0
}
```

## Executor Configuration

```go title="Go code"
// pkg/guest/executor.go
"postgresql-parser": {
    Name:      "postgresql-parser",
    Extension: ".sql",
    Command:   "/usr/local/bin/node",
    Args:      []string{"/usr/local/bin/psql-run"},
    Env:       []string{"NODE_PATH=/opt/sql-parser/node_modules"},
},
```
