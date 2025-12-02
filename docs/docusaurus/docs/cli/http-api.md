---
title: 'HTTP API'
description: 'REST API for code execution in Firecracker microVMs'
---

## Overview

The HTTP API provides programmatic access to execute code in isolated Firecracker microVMs.

## Quick Start

```bash title="Start the API server"
infra.operator api --port 8080
```

```bash title="Execute Python code"
curl -X POST http://localhost:8080/api/v1/execute \
  -H "Content-Type: application/json" \
  -d '{"language": "python", "code": "print(1 + 1)"}'
```

---

## Endpoints

| Method | Endpoint | Description |
|--------|----------|-------------|
| `GET` | `/health` | Health check |
| `GET` | `/api/v1/languages` | List supported languages |
| `GET` | `/api/v1/languages/:lang` | Get language details |
| `POST` | `/api/v1/execute` | Execute code |
| `GET` | `/api/v1/rootfs` | List rootfs images |
| `POST` | `/api/v1/rootfs` | Create rootfs |
| `GET` | `/api/v1/snapshots` | List snapshots |
| `POST` | `/api/v1/snapshots` | Create snapshot |

---

## Execute Code

The main endpoint for running code in isolated microVMs.

**POST** `/api/v1/execute`

### Request

| Field | Type | Required | Description |
|-------|------|----------|-------------|
| `language` | string | Yes | Programming language |
| `code` | string | Yes | Source code to execute |
| `timeout` | integer | No | Timeout in seconds (default: 30) |

### Example

```bash title="Execute code request"
curl -X POST http://localhost:8080/api/v1/execute \
  -H "Content-Type: application/json" \
  -d '{
    "language": "python",
    "code": "print(\"Hello World!\")",
    "timeout": 30
  }'
```

```json title="Response"
{
  "stdout": "Hello World!\n",
  "stderr": "",
  "exit_code": 0,
  "load_time": "125ms",
  "exec_time": "45ms"
}
```

### Language Examples

```bash title="Execute Python code via API"
curl -X POST http://localhost:8080/api/v1/execute \
  -H "Content-Type: application/json" \
  -d '{"language": "python", "code": "print(sum(range(100)))"}'
```

```bash title="Execute Node.js code via API"
curl -X POST http://localhost:8080/api/v1/execute \
  -H "Content-Type: application/json" \
  -d '{"language": "nodejs", "code": "console.log(Array.from({length:10},(_,i)=>i*i))"}'
```

```bash title="Execute Go code via API"
curl -X POST http://localhost:8080/api/v1/execute \
  -H "Content-Type: application/json" \
  -d '{"language": "go", "code": "package main\nimport \"fmt\"\nfunc main() { fmt.Println(\"Hello Go!\") }"}'
```

```bash title="Execute Rust code via API"
curl -X POST http://localhost:8080/api/v1/execute \
  -H "Content-Type: application/json" \
  -d '{"language": "rust", "code": "fn main() { println!(\"Hello Rust!\"); }"}'
```

---

## Health Check

**GET** `/health`

```bash title="Health check request"
curl http://localhost:8080/health
```

```json title="Response"
{
  "status": "healthy",
  "version": "1.0.0"
}
```

---

## List Languages

**GET** `/api/v1/languages`

```bash title="List languages request"
curl http://localhost:8080/api/v1/languages
```

```json title="Response"
{
  "languages": [
    {"name": "python", "extension": ".py"},
    {"name": "nodejs", "extension": ".js"},
    {"name": "go", "extension": ".go"},
    {"name": "rust", "extension": ".rs"}
  ],
  "count": 45
}
```

---

## List Rootfs

**GET** `/api/v1/rootfs`

| Query Param | Description |
|-------------|-------------|
| `remote=true` | List from S3 instead of local |

```bash title="List local rootfs"
curl http://localhost:8080/api/v1/rootfs
```

```bash title="List S3 rootfs"
curl "http://localhost:8080/api/v1/rootfs?remote=true"
```

```json title="Response"
{
  "rootfs": [
    {"language": "python", "size": 1288490188, "path": "/srv/firecracker/images/rootfs-python.ext4"},
    {"language": "nodejs", "size": 845000000, "path": "/srv/firecracker/images/rootfs-nodejs.ext4"}
  ]
}
```

---

## List Snapshots

**GET** `/api/v1/snapshots`

| Query Param | Description |
|-------------|-------------|
| `remote=true` | List from S3 instead of local |

```bash title="List local snapshots"
curl http://localhost:8080/api/v1/snapshots
```

```json title="Response"
{
  "snapshots": [
    {"language": "python", "vmstate_size": 13516, "mem_size": 536870912},
    {"language": "nodejs", "vmstate_size": 14100, "mem_size": 536870912}
  ]
}
```

---

## Create Rootfs

**POST** `/api/v1/rootfs`

```bash title="Create rootfs request"
curl -X POST http://localhost:8080/api/v1/rootfs \
  -H "Content-Type: application/json" \
  -d '{"language": "python", "size_mb": 1024}'
```

```json title="Response (202 Accepted)"
{
  "message": "rootfs creation started",
  "language": "python"
}
```

---

## Create Snapshot

**POST** `/api/v1/snapshots`

```bash title="Create snapshot request"
curl -X POST http://localhost:8080/api/v1/snapshots \
  -H "Content-Type: application/json" \
  -d '{"language": "python", "mem_mb": 512, "vcpus": 1}'
```

```json title="Response (202 Accepted)"
{
  "message": "snapshot creation started",
  "language": "python"
}
```

---

## Error Handling

All errors return JSON with an `error` field:

```json title="Error response"
{
  "error": "language not found: ruby2"
}
```

| Status | Description |
|--------|-------------|
| `200` | Success |
| `202` | Accepted (async operation) |
| `400` | Bad request |
| `404` | Not found |
| `500` | Server error |

---

## SDK Examples

### Python

```python title="Python SDK"
import requests

class RunnerClient:
    def __init__(self, url="http://localhost:8080"):
        self.url = url

    def execute(self, language, code, timeout=30):
        response = requests.post(
            f"{self.url}/api/v1/execute",
            json={"language": language, "code": code, "timeout": timeout}
        )
        return response.json()

# Usage
client = RunnerClient()
result = client.execute("python", "print(2 ** 10)")
print(result["stdout"])  # 1024
```

### JavaScript

```javascript title="JavaScript SDK"
class RunnerClient {
  constructor(url = "http://localhost:8080") {
    this.url = url;
  }

  async execute(language, code, timeout = 30) {
    const res = await fetch(`${this.url}/api/v1/execute`, {
      method: "POST",
      headers: { "Content-Type": "application/json" },
      body: JSON.stringify({ language, code, timeout })
    });
    return res.json();
  }
}

// Usage
const client = new RunnerClient();
const result = await client.execute("nodejs", "console.log(2 ** 10)");
console.log(result.stdout); // 1024
```

---

## Next Steps

- [CLI Reference](/cli/overview) - Command line interface
- [Snapshot Commands](/cli/snapshot) - Manage snapshots
- [Languages](/languages/overview) - Supported languages
