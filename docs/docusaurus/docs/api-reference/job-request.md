---
title: 'Job Request'
description: 'Job request schema and parameters'
---

## Schema

The job request is a JSON object sent from the host agent to the guest runner.

```json title="Basic job request with Python code"
{
  "trace_id": "tr-1764388648079963068",
  "lang": "python",
  "code": "print('Hello, World!')",
  "timeout": 30
}
```

## Parameters

## Examples

### Python - Simple Print

```json title="Python simple print statement"
{
  "trace_id": "tr-001",
  "lang": "python",
  "code": "print('Hello from Python!')",
  "timeout": 10
}
```

### Python - Multi-line with Functions

```json title="Python function definition and execution"
{
  "trace_id": "tr-002",
  "lang": "python",
  "code": "def greet(name):\n    return f'Hello, {name}!'\n\nresult = greet('World')\nprint(result)",
  "timeout": 10
}
```

### Python - With Imports

```json title="Python with JSON and sys module imports"
{
  "trace_id": "tr-003",
  "lang": "python",
  "code": "import json\nimport sys\n\ndata = {'python_version': sys.version, 'platform': sys.platform}\nprint(json.dumps(data, indent=2))",
  "timeout": 10
}
```

### Node.js - Async/Await

```json title="Node.js async promise with timeout"
{
  "trace_id": "tr-004",
  "lang": "node",
  "code": "async function fetchData() {\n  return new Promise(resolve => {\n    setTimeout(() => resolve('Data loaded'), 100);\n  });\n}\n\nfetchData().then(console.log);",
  "timeout": 15
}
```

### Node.js - ES Modules

```json title="Node.js ES6 arrow functions"
{
  "trace_id": "tr-005",
  "lang": "node",
  "code": "const add = (a, b) => a + b;\nconst multiply = (a, b) => a * b;\n\nconsole.log(`2 + 3 = ${add(2, 3)}`);\nconsole.log(`2 * 3 = ${multiply(2, 3)}`);",
  "timeout": 10
}
```

### Go - Complete Program

```json title="Go program with runtime version info"
{
  "trace_id": "tr-006",
  "lang": "go",
  "code": "package main\n\nimport (\n\t\"fmt\"\n\t\"runtime\"\n)\n\nfunc main() {\n\tfmt.Printf(\"Go version: %s\\n\", runtime.Version())\n\tfmt.Printf(\"OS/Arch: %s/%s\\n\", runtime.GOOS, runtime.GOARCH)\n}",
  "timeout": 30
}
```

### Go - With Goroutines

```json title="Go concurrent execution with WaitGroup"
{
  "trace_id": "tr-007",
  "lang": "go",
  "code": "package main\n\nimport (\n\t\"fmt\"\n\t\"sync\"\n)\n\nfunc main() {\n\tvar wg sync.WaitGroup\n\tfor i := 0; i < 3; i++ {\n\t\twg.Add(1)\n\t\tgo func(n int) {\n\t\t\tdefer wg.Done()\n\t\t\tfmt.Printf(\"Goroutine %d\\n\", n)\n\t\t}(i)\n\t}\n\twg.Wait()\n}",
  "timeout": 30
}
```

### Rust - Simple Program

```json title="Rust vector iteration and sum"
{
  "trace_id": "tr-008",
  "lang": "rust",
  "code": "fn main() {\n    let numbers = vec![1, 2, 3, 4, 5];\n    let sum: i32 = numbers.iter().sum();\n    println!(\"Sum: {}\", sum);\n}",
  "timeout": 60
}
```

### Rust - With Structs

```json title="Rust struct with distance calculation method"
{
  "trace_id": "tr-009",
  "lang": "rust",
  "code": "#[derive(Debug)]\nstruct Point {\n    x: f64,\n    y: f64,\n}\n\nimpl Point {\n    fn distance(&self, other: &Point) -> f64 {\n        ((self.x - other.x).powi(2) + (self.y - other.y).powi(2)).sqrt()\n    }\n}\n\nfn main() {\n    let p1 = Point { x: 0.0, y: 0.0 };\n    let p2 = Point { x: 3.0, y: 4.0 };\n    println!(\"Distance: {}\", p1.distance(&p2));\n}",
  "timeout": 60
}
```

### Bash - System Info

```json title="Bash system information script"
{
  "trace_id": "tr-010",
  "lang": "bash",
  "code": "echo \"Hostname: $(hostname)\"\necho \"Kernel: $(uname -r)\"\necho \"Memory: $(free -h | grep Mem | awk '{print $2}')\"\necho \"Disk: $(df -h / | tail -1 | awk '{print $4}') free\"",
  "timeout": 10
}
```

### Bash - Loop Example

```json title="Bash for loop with sleep delay"
{
  "trace_id": "tr-011",
  "lang": "bash",
  "code": "for i in {1..5}; do\n    echo \"Iteration $i\"\n    sleep 0.1\ndone\necho \"Done!\"",
  "timeout": 10
}
```

## Validation Rules

### Language Validation

If an unsupported language is specified:

**Request:**
```json title="Job request with unsupported language"
{
  "trace_id": "tr-error-001",
  "lang": "java",
  "code": "System.out.println(\"Hello\");",
  "timeout": 10
}
```

**Response:**
```json title="Error response for unsupported language"
{
  "trace_id": "tr-error-001",
  "stdout": "",
  "stderr": "unsupported language: java",
  "exit_code": 127,
  "error": "unsupported language: java"
}
```

### Syntax Errors

Invalid syntax is handled by the language runtime:

**Request:**
```json title="Job request with Python syntax error"
{
  "trace_id": "tr-error-002",
  "lang": "python",
  "code": "print('Hello",
  "timeout": 10
}
```

**Response:**
```json title="Python syntax error response"
{
  "trace_id": "tr-error-002",
  "stdout": "",
  "stderr": "  File \"/tmp/job-xxx/script.py\", line 1\n    print('Hello\n              ^\nSyntaxError: EOL while scanning string literal\n",
  "exit_code": 1,
  "error": ""
}
```

### Timeout

**Request:**
```json title="Job request that will timeout"
{
  "trace_id": "tr-error-003",
  "lang": "python",
  "code": "import time\ntime.sleep(100)",
  "timeout": 5
}
```

**Response:**
```json title="Timeout error response"
{
  "trace_id": "tr-error-003",
  "stdout": "",
  "stderr": "\nExecution timed out",
  "exit_code": 124,
  "error": ""
}
```

## Wire Format

The request is sent over vsock with length-prefix framing:

```
+------------------+----------------------+
| Length (4 bytes) | JSON Request         |
| Big-Endian       | (UTF-8 encoded)      |
+------------------+----------------------+
```

### Example Byte Sequence

For a request:
```json title="Example job request JSON"
{"trace_id":"t1","lang":"python","code":"print(1)","timeout":10}
```

The wire format is:
```
00 00 00 3F  # Length: 63 bytes (big-endian)
7B 22 74 72 61 63 65 5F 69 64 22 3A ...  # JSON payload
```

### Go Implementation

```go title="Send job request with length-prefix framing"
func sendJob(conn net.Conn, job Job) error {
    payload, err := json.Marshal(job)
    if err != nil {
        return err
    }

    // Write length prefix (4 bytes, big-endian)
    length := make([]byte, 4)
    binary.BigEndian.PutUint32(length, uint32(len(payload)))

    if _, err := conn.Write(length); err != nil {
        return err
    }

    // Write payload
    if _, err := conn.Write(payload); err != nil {
        return err
    }

    return nil
}
```

### Python Implementation

```python title="Send job with big-endian length prefix"
import json
import struct

def send_job(sock, job):
    payload = json.dumps(job).encode('utf-8')
    length = struct.pack('>I', len(payload))  # Big-endian unsigned int
    sock.sendall(length + payload)
```
