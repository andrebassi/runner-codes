---
title: 'Redis Parser'
description: 'Redis command validation without database connection'
---

## Overview

Redis Parser validates Redis commands WITHOUT requiring a real Redis server. It parses command syntax and validates argument counts.

## Specifications

| Property | Value |
|----------|-------|
| Language ID | `redis-parser` |
| Base OS | Alpine Linux 3.20 |
| Runtime | Node.js 22 |
| Rootfs Size | 300 MB |
| Memory | 256 MB |
| File Extension | `.redis` |
| Execution Time | ~2.7s |

## Supported Commands

| Category | Commands |
|----------|----------|
| **Strings** | SET, GET, MSET, MGET, INCR, DECR, APPEND, STRLEN |
| **Hashes** | HSET, HGET, HMSET, HMGET, HGETALL, HDEL, HEXISTS |
| **Lists** | LPUSH, RPUSH, LPOP, RPOP, LRANGE, LLEN, LINDEX |
| **Sets** | SADD, SREM, SMEMBERS, SISMEMBER, SINTER, SUNION |
| **Sorted Sets** | ZADD, ZREM, ZRANGE, ZRANK, ZSCORE, ZCOUNT |
| **Keys** | DEL, EXISTS, EXPIRE, TTL, KEYS, SCAN, TYPE |
| **Transactions** | MULTI, EXEC, DISCARD, WATCH |
| **Server** | PING, INFO, DBSIZE, FLUSHDB |

## Infrastructure

### Step 1: Create Docker Directory

```bash title="Create working directory for Redis parser"
mkdir -p /tmp/docker/redis-parser
cd /tmp/docker/redis-parser
```

### Step 2: Create redis-run.js Script

```bash title="Create Node.js parser wrapper script"
cat > redis-run.js << 'EOF'
#!/usr/bin/env node
const fs = require('fs');

const REDIS_COMMANDS = {
    // String commands
    'SET': { min: 2, max: -1, desc: 'SET key value [EX|PX|EXAT|PXAT time] [NX|XX] [KEEPTTL] [GET]' },
    'GET': { min: 1, max: 1, desc: 'GET key' },
    'MSET': { min: 2, max: -1, desc: 'MSET key value [key value ...]' },
    'MGET': { min: 1, max: -1, desc: 'MGET key [key ...]' },
    'INCR': { min: 1, max: 1, desc: 'INCR key' },
    'INCRBY': { min: 2, max: 2, desc: 'INCRBY key increment' },
    'DECR': { min: 1, max: 1, desc: 'DECR key' },
    'DECRBY': { min: 2, max: 2, desc: 'DECRBY key decrement' },
    'APPEND': { min: 2, max: 2, desc: 'APPEND key value' },
    'STRLEN': { min: 1, max: 1, desc: 'STRLEN key' },
    'GETRANGE': { min: 3, max: 3, desc: 'GETRANGE key start end' },
    'SETRANGE': { min: 3, max: 3, desc: 'SETRANGE key offset value' },
    'SETNX': { min: 2, max: 2, desc: 'SETNX key value' },
    'SETEX': { min: 3, max: 3, desc: 'SETEX key seconds value' },
    'GETSET': { min: 2, max: 2, desc: 'GETSET key value' },
    'GETDEL': { min: 1, max: 1, desc: 'GETDEL key' },

    // Key commands
    'DEL': { min: 1, max: -1, desc: 'DEL key [key ...]' },
    'EXISTS': { min: 1, max: -1, desc: 'EXISTS key [key ...]' },
    'EXPIRE': { min: 2, max: 3, desc: 'EXPIRE key seconds [NX|XX|GT|LT]' },
    'EXPIREAT': { min: 2, max: 3, desc: 'EXPIREAT key unix-time-seconds [NX|XX|GT|LT]' },
    'TTL': { min: 1, max: 1, desc: 'TTL key' },
    'PTTL': { min: 1, max: 1, desc: 'PTTL key' },
    'PERSIST': { min: 1, max: 1, desc: 'PERSIST key' },
    'KEYS': { min: 1, max: 1, desc: 'KEYS pattern' },
    'SCAN': { min: 1, max: -1, desc: 'SCAN cursor [MATCH pattern] [COUNT count] [TYPE type]' },
    'TYPE': { min: 1, max: 1, desc: 'TYPE key' },
    'RENAME': { min: 2, max: 2, desc: 'RENAME key newkey' },
    'RENAMENX': { min: 2, max: 2, desc: 'RENAMENX key newkey' },
    'UNLINK': { min: 1, max: -1, desc: 'UNLINK key [key ...]' },

    // Hash commands
    'HSET': { min: 3, max: -1, desc: 'HSET key field value [field value ...]' },
    'HGET': { min: 2, max: 2, desc: 'HGET key field' },
    'HMSET': { min: 3, max: -1, desc: 'HMSET key field value [field value ...]' },
    'HMGET': { min: 2, max: -1, desc: 'HMGET key field [field ...]' },
    'HGETALL': { min: 1, max: 1, desc: 'HGETALL key' },
    'HDEL': { min: 2, max: -1, desc: 'HDEL key field [field ...]' },
    'HEXISTS': { min: 2, max: 2, desc: 'HEXISTS key field' },
    'HINCRBY': { min: 3, max: 3, desc: 'HINCRBY key field increment' },
    'HKEYS': { min: 1, max: 1, desc: 'HKEYS key' },
    'HVALS': { min: 1, max: 1, desc: 'HVALS key' },
    'HLEN': { min: 1, max: 1, desc: 'HLEN key' },
    'HSETNX': { min: 3, max: 3, desc: 'HSETNX key field value' },
    'HSCAN': { min: 2, max: -1, desc: 'HSCAN key cursor [MATCH pattern] [COUNT count]' },

    // List commands
    'LPUSH': { min: 2, max: -1, desc: 'LPUSH key element [element ...]' },
    'RPUSH': { min: 2, max: -1, desc: 'RPUSH key element [element ...]' },
    'LPOP': { min: 1, max: 2, desc: 'LPOP key [count]' },
    'RPOP': { min: 1, max: 2, desc: 'RPOP key [count]' },
    'LRANGE': { min: 3, max: 3, desc: 'LRANGE key start stop' },
    'LINDEX': { min: 2, max: 2, desc: 'LINDEX key index' },
    'LLEN': { min: 1, max: 1, desc: 'LLEN key' },
    'LSET': { min: 3, max: 3, desc: 'LSET key index element' },
    'LINSERT': { min: 4, max: 4, desc: 'LINSERT key BEFORE|AFTER pivot element' },
    'LREM': { min: 3, max: 3, desc: 'LREM key count element' },
    'LTRIM': { min: 3, max: 3, desc: 'LTRIM key start stop' },
    'BLPOP': { min: 2, max: -1, desc: 'BLPOP key [key ...] timeout' },
    'BRPOP': { min: 2, max: -1, desc: 'BRPOP key [key ...] timeout' },

    // Set commands
    'SADD': { min: 2, max: -1, desc: 'SADD key member [member ...]' },
    'SREM': { min: 2, max: -1, desc: 'SREM key member [member ...]' },
    'SMEMBERS': { min: 1, max: 1, desc: 'SMEMBERS key' },
    'SISMEMBER': { min: 2, max: 2, desc: 'SISMEMBER key member' },
    'SCARD': { min: 1, max: 1, desc: 'SCARD key' },
    'SPOP': { min: 1, max: 2, desc: 'SPOP key [count]' },
    'SRANDMEMBER': { min: 1, max: 2, desc: 'SRANDMEMBER key [count]' },
    'SUNION': { min: 1, max: -1, desc: 'SUNION key [key ...]' },
    'SINTER': { min: 1, max: -1, desc: 'SINTER key [key ...]' },
    'SDIFF': { min: 1, max: -1, desc: 'SDIFF key [key ...]' },
    'SUNIONSTORE': { min: 2, max: -1, desc: 'SUNIONSTORE destination key [key ...]' },
    'SINTERSTORE': { min: 2, max: -1, desc: 'SINTERSTORE destination key [key ...]' },
    'SDIFFSTORE': { min: 2, max: -1, desc: 'SDIFFSTORE destination key [key ...]' },
    'SMOVE': { min: 3, max: 3, desc: 'SMOVE source destination member' },
    'SSCAN': { min: 2, max: -1, desc: 'SSCAN key cursor [MATCH pattern] [COUNT count]' },

    // Sorted Set commands
    'ZADD': { min: 3, max: -1, desc: 'ZADD key [NX|XX] [GT|LT] [CH] [INCR] score member [score member ...]' },
    'ZREM': { min: 2, max: -1, desc: 'ZREM key member [member ...]' },
    'ZSCORE': { min: 2, max: 2, desc: 'ZSCORE key member' },
    'ZRANK': { min: 2, max: 2, desc: 'ZRANK key member' },
    'ZREVRANK': { min: 2, max: 2, desc: 'ZREVRANK key member' },
    'ZRANGE': { min: 3, max: -1, desc: 'ZRANGE key min max [BYSCORE|BYLEX] [REV] [LIMIT offset count] [WITHSCORES]' },
    'ZREVRANGE': { min: 3, max: -1, desc: 'ZREVRANGE key start stop [WITHSCORES]' },
    'ZRANGEBYSCORE': { min: 3, max: -1, desc: 'ZRANGEBYSCORE key min max [WITHSCORES] [LIMIT offset count]' },
    'ZCARD': { min: 1, max: 1, desc: 'ZCARD key' },
    'ZCOUNT': { min: 3, max: 3, desc: 'ZCOUNT key min max' },
    'ZINCRBY': { min: 3, max: 3, desc: 'ZINCRBY key increment member' },
    'ZSCAN': { min: 2, max: -1, desc: 'ZSCAN key cursor [MATCH pattern] [COUNT count]' },
    'ZPOPMIN': { min: 1, max: 2, desc: 'ZPOPMIN key [count]' },
    'ZPOPMAX': { min: 1, max: 2, desc: 'ZPOPMAX key [count]' },

    // Pub/Sub
    'PUBLISH': { min: 2, max: 2, desc: 'PUBLISH channel message' },
    'SUBSCRIBE': { min: 1, max: -1, desc: 'SUBSCRIBE channel [channel ...]' },
    'UNSUBSCRIBE': { min: 0, max: -1, desc: 'UNSUBSCRIBE [channel [channel ...]]' },
    'PSUBSCRIBE': { min: 1, max: -1, desc: 'PSUBSCRIBE pattern [pattern ...]' },
    'PUNSUBSCRIBE': { min: 0, max: -1, desc: 'PUNSUBSCRIBE [pattern [pattern ...]]' },

    // Transaction
    'MULTI': { min: 0, max: 0, desc: 'MULTI' },
    'EXEC': { min: 0, max: 0, desc: 'EXEC' },
    'DISCARD': { min: 0, max: 0, desc: 'DISCARD' },
    'WATCH': { min: 1, max: -1, desc: 'WATCH key [key ...]' },
    'UNWATCH': { min: 0, max: 0, desc: 'UNWATCH' },

    // Server
    'PING': { min: 0, max: 1, desc: 'PING [message]' },
    'ECHO': { min: 1, max: 1, desc: 'ECHO message' },
    'INFO': { min: 0, max: 1, desc: 'INFO [section]' },
    'DBSIZE': { min: 0, max: 0, desc: 'DBSIZE' },
    'FLUSHDB': { min: 0, max: 1, desc: 'FLUSHDB [ASYNC|SYNC]' },
    'FLUSHALL': { min: 0, max: 1, desc: 'FLUSHALL [ASYNC|SYNC]' },
    'SELECT': { min: 1, max: 1, desc: 'SELECT index' },
    'SAVE': { min: 0, max: 0, desc: 'SAVE' },
    'BGSAVE': { min: 0, max: 1, desc: 'BGSAVE [SCHEDULE]' },
    'TIME': { min: 0, max: 0, desc: 'TIME' },
    'CONFIG': { min: 1, max: -1, desc: 'CONFIG GET|SET|RESETSTAT|REWRITE ...' },
    'CLIENT': { min: 1, max: -1, desc: 'CLIENT ID|KILL|LIST|GETNAME|SETNAME|PAUSE ...' },
};

function parseCommand(line) {
    const tokens = [];
    let current = '';
    let inQuote = false;
    let quoteChar = null;

    for (let i = 0; i < line.length; i++) {
        const char = line[i];

        if (!inQuote && (char === '"' || char === "'")) {
            inQuote = true;
            quoteChar = char;
        } else if (inQuote && char === quoteChar) {
            inQuote = false;
            quoteChar = null;
        } else if (!inQuote && char === ' ') {
            if (current) {
                tokens.push(current);
                current = '';
            }
        } else {
            current += char;
        }
    }

    if (current) {
        tokens.push(current);
    }

    return tokens;
}

function validateCommand(line, lineNumber) {
    const result = {
        line: lineNumber,
        command: null,
        args: [],
        valid: true,
        error: null,
        description: null
    };

    const tokens = parseCommand(line);
    if (tokens.length === 0) {
        return null;
    }

    const cmd = tokens[0].toUpperCase();
    const args = tokens.slice(1);

    result.command = cmd;
    result.args = args;

    const cmdSpec = REDIS_COMMANDS[cmd];
    if (!cmdSpec) {
        result.valid = false;
        result.error = `Unknown command: ${cmd}`;
        return result;
    }

    result.description = cmdSpec.desc;

    if (args.length < cmdSpec.min) {
        result.valid = false;
        result.error = `Too few arguments. Expected at least ${cmdSpec.min}, got ${args.length}`;
        return result;
    }

    if (cmdSpec.max !== -1 && args.length > cmdSpec.max) {
        result.valid = false;
        result.error = `Too many arguments. Expected at most ${cmdSpec.max}, got ${args.length}`;
        return result;
    }

    return result;
}

function main() {
    const args = process.argv.slice(2);
    if (args.length === 0) {
        console.error('Usage: redis-run.js <file.redis>');
        process.exit(1);
    }

    const commandFile = args[0];
    let content;
    try {
        content = fs.readFileSync(commandFile, 'utf8');
    } catch (err) {
        console.error(`Error reading file: ${err.message}`);
        process.exit(1);
    }

    const lines = content.split('\n');
    const results = [];
    let hasErrors = false;

    lines.forEach((line, i) => {
        const trimmed = line.trim();
        if (!trimmed || trimmed.startsWith('#')) {
            return;
        }

        const result = validateCommand(trimmed, i + 1);
        if (result) {
            results.push(result);
            if (!result.valid) {
                hasErrors = true;
            }
        }
    });

    console.log('=== Redis Commands Validation ===');
    console.log(`Status: ${hasErrors ? 'INVALID' : 'VALID'}`);
    console.log(`Total commands: ${results.length}`);
    console.log('');

    results.forEach((r) => {
        const status = r.valid ? 'OK' : 'ERROR';
        console.log(`Line ${r.line}: [${status}] ${r.command} ${r.args.join(' ')}`);

        if (r.error) {
            console.log(`  Error: ${r.error}`);
        }

        if (r.description && r.valid) {
            console.log(`  Syntax: ${r.description}`);
        }
    });

    console.log('');
    console.log('=== Summary ===');
    const valid = results.filter(r => r.valid).length;
    const invalid = results.filter(r => !r.valid).length;
    console.log(`Valid: ${valid}/${results.length}`);
    console.log(`Invalid: ${invalid}/${results.length}`);

    process.exit(hasErrors ? 1 : 0);
}

main();
EOF
```

### Step 3: Create Dockerfile

```bash title="Create Dockerfile with Node.js and parser dependencies"
cat > Dockerfile << 'EOF'
FROM node:22-alpine3.20

WORKDIR /opt/redis-parser

COPY redis-run.js /usr/local/bin/redis-run
RUN chmod +x /usr/local/bin/redis-run

CMD ["node", "--version"]
EOF
```

### Step 4: Build and Push Docker Image

```bash title="Build and push Docker image to ttl.sh registry"
docker build -t ttl.sh/llm-fc-redis-parser:24h .
docker push ttl.sh/llm-fc-redis-parser:24h
```

### Step 5: Create Rootfs

```bash title="Create rootfs from Docker image"
sudo infra.operator rootfs from-docker \
    --name redis-parser \
    --image ttl.sh/llm-fc-redis-parser:24h \
    --size 300
```

### Step 6: Create Snapshot

```bash title="Create VM snapshot for fast boot"
sudo infra.operator snapshot create \
    --lang redis-parser \
    --mem 256 \
    --vcpus 1
```

### Step 7: Upload to S3

```bash title="Upload rootfs and snapshot to S3 bucket"
export AWS_ACCESS_KEY_ID="your-key"
export AWS_SECRET_ACCESS_KEY="your-secret"
export AWS_DEFAULT_REGION="us-east-1"

sudo -E infra.operator rootfs upload --lang redis-parser --bucket llm-firecracker
sudo -E infra.operator snapshot upload --lang redis-parser --bucket llm-firecracker
```

### Step 8: Test Execution

```bash title="Test parser with sample SQL"
sudo infra.operator host \
    --lang redis-parser \
    --snapshot \
    --mem 256 \
    --vcpus 1 \
    --code "SET user:1 john
GET user:1
HSET user:1:profile name John age 30
HGETALL user:1:profile"
```

## Examples

### Basic Commands

```json title="Request"
{
  "trace_id": "redis-basic-001",
  "lang": "redis-parser",
  "code": "SET user:1 john\nGET user:1\nHSET user:1:profile name John age 30",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "redis-basic-001",
  "stdout": "=== Redis Commands Validation ===\nStatus: VALID\nTotal commands: 3\n\nLine 1: [OK] SET user:1 john\n  Syntax: SET key value [EX|PX|EXAT|PXAT time] [NX|XX] [KEEPTTL] [GET]\nLine 2: [OK] GET user:1\n  Syntax: GET key\nLine 3: [OK] HSET user:1:profile name John age 30\n  Syntax: HSET key field value [field value ...]\n\n=== Summary ===\nValid: 3/3\nInvalid: 0/3\n",
  "stderr": "",
  "exit_code": 0
}
```

### List Operations

```json title="Request"
{
  "trace_id": "redis-list-001",
  "lang": "redis-parser",
  "code": "LPUSH mylist item1 item2 item3\nRPUSH mylist item4\nLRANGE mylist 0 -1\nLLEN mylist",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "redis-list-001",
  "stdout": "=== Redis Commands Validation ===\nStatus: VALID\nTotal commands: 4\n\nLine 1: [OK] LPUSH mylist item1 item2 item3\n  Syntax: LPUSH key element [element ...]\nLine 2: [OK] RPUSH mylist item4\n  Syntax: RPUSH key element [element ...]\nLine 3: [OK] LRANGE mylist 0 -1\n  Syntax: LRANGE key start stop\nLine 4: [OK] LLEN mylist\n  Syntax: LLEN key\n\n=== Summary ===\nValid: 4/4\nInvalid: 0/4\n",
  "stderr": "",
  "exit_code": 0
}
```

### Invalid Commands Detection

```json title="Request"
{
  "trace_id": "redis-invalid-001",
  "lang": "redis-parser",
  "code": "SET key\nGET\nUNKNOWNCMD arg1 arg2",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "redis-invalid-001",
  "stdout": "=== Redis Commands Validation ===\nStatus: INVALID\nTotal commands: 3\n\nLine 1: [ERROR] SET key\n  Error: Too few arguments. Expected at least 2, got 1\nLine 2: [ERROR] GET\n  Error: Too few arguments. Expected at least 1, got 0\nLine 3: [ERROR] UNKNOWNCMD arg1 arg2\n  Error: Unknown command: UNKNOWNCMD\n\n=== Summary ===\nValid: 0/3\nInvalid: 3/3\n",
  "stderr": "",
  "exit_code": 1
}
```

## Executor Configuration

```go title="Go code"
// pkg/guest/executor.go
"redis-parser": {
    Name:      "redis-parser",
    Extension: ".redis",
    Command:   "/usr/local/bin/node",
    Args:      []string{"/usr/local/bin/redis-run"},
},
```
