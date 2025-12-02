---
title: 'Job Response'
description: 'Job response schema and error handling'
---

## Schema

The job response is a JSON object returned from the guest runner to the host agent after code execution.

```json title="Successful job execution response"
{
  "trace_id": "tr-1764388648079963068",
  "stdout": "Hello, World!\n",
  "stderr": "",
  "exit_code": 0,
  "error": ""
}
```

## Fields

## Success Response Examples

### Python - Simple Output

**Request:**
```json title="Python print statement request"
{
  "trace_id": "tr-001",
  "lang": "python",
  "code": "print('Hello, World!')",
  "timeout": 10
}
```

**Response:**
```json title="Successful execution with stdout output"
{
  "trace_id": "tr-001",
  "stdout": "Hello, World!\n",
  "stderr": "",
  "exit_code": 0,
  "error": ""
}
```

### Python - Multi-line Output

**Request:**
```json title="Python for loop with formatted output"
{
  "trace_id": "tr-002",
  "lang": "python",
  "code": "for i in range(3):\n    print(f'Line {i}')",
  "timeout": 10
}
```

**Response:**
```json title="Multi-line stdout response"
{
  "trace_id": "tr-002",
  "stdout": "Line 0\nLine 1\nLine 2\n",
  "stderr": "",
  "exit_code": 0,
  "error": ""
}
```

### Go - Compiled Language

**Request:**
```json title="Go program compilation and execution"
{
  "trace_id": "tr-003",
  "lang": "go",
  "code": "package main\n\nimport \"fmt\"\n\nfunc main() {\n\tfmt.Println(\"Go works!\")\n}",
  "timeout": 30
}
```

**Response:**
```json title="Successful Go execution response"
{
  "trace_id": "tr-003",
  "stdout": "Go works!\n",
  "stderr": "",
  "exit_code": 0,
  "error": ""
}
```

### Rust - Compilation and Execution

**Request:**
```json title="Rust program compilation request"
{
  "trace_id": "tr-004",
  "lang": "rust",
  "code": "fn main() {\n    println!(\"Rust compiles!\");\n}",
  "timeout": 60
}
```

**Response:**
```json title="Successful Rust compilation and execution"
{
  "trace_id": "tr-004",
  "stdout": "Rust compiles!\n",
  "stderr": "",
  "exit_code": 0,
  "error": ""
}
```

## Error Response Examples

### Syntax Error (Python)

**Request:**
```json title="Python code with syntax error"
{
  "trace_id": "tr-err-001",
  "lang": "python",
  "code": "print('Hello",
  "timeout": 10
}
```

**Response:**
```json title="Syntax error in stderr field"
{
  "trace_id": "tr-err-001",
  "stdout": "",
  "stderr": "  File \"/tmp/job-abc123/script.py\", line 1\n    print('Hello\n               ^\nSyntaxError: EOL while scanning string literal\n",
  "exit_code": 1,
  "error": ""
}
```

### Runtime Error (Python)

**Request:**
```json title="Python code causing runtime error"
{
  "trace_id": "tr-err-002",
  "lang": "python",
  "code": "x = 1 / 0",
  "timeout": 10
}
```

**Response:**
```json title="ZeroDivisionError with traceback"
{
  "trace_id": "tr-err-002",
  "stdout": "",
  "stderr": "Traceback (most recent call last):\n  File \"/tmp/job-abc123/script.py\", line 1, in <module>\n    x = 1 / 0\nZeroDivisionError: division by zero\n",
  "exit_code": 1,
  "error": ""
}
```

### Compilation Error (Rust)

**Request:**
```json title="Rust code with type mismatch"
{
  "trace_id": "tr-err-003",
  "lang": "rust",
  "code": "fn main() {\n    let x: i32 = \"not a number\";\n}",
  "timeout": 60
}
```

**Response:**
```json title="Rust compilation error response"
{
  "trace_id": "tr-err-003",
  "stdout": "",
  "stderr": "error[E0308]: mismatched types\n --> script.rs:2:18\n  |\n2 |     let x: i32 = \"not a number\";\n  |            ---   ^^^^^^^^^^^^^^ expected `i32`, found `&str`\n  |            |\n  |            expected due to this\n\nerror: aborting due to previous error\n",
  "exit_code": 1,
  "error": "compilation failed"
}
```

### Timeout

**Request:**
```json title="Python infinite loop exceeding timeout"
{
  "trace_id": "tr-err-004",
  "lang": "python",
  "code": "import time\nwhile True:\n    time.sleep(1)",
  "timeout": 5
}
```

**Response:**
```json title="Execution timeout response (exit code 124)"
{
  "trace_id": "tr-err-004",
  "stdout": "",
  "stderr": "\nExecution timed out",
  "exit_code": 124,
  "error": ""
}
```

### Unsupported Language

**Request:**
```json title="Request with unsupported language"
{
  "trace_id": "tr-err-005",
  "lang": "ruby",
  "code": "puts 'Hello'",
  "timeout": 10
}
```

**Response:**
```json title="Unsupported language error (exit code 127)"
{
  "trace_id": "tr-err-005",
  "stdout": "",
  "stderr": "unsupported language: ruby",
  "exit_code": 127,
  "error": "unsupported language: ruby"
}
```

### Out of Memory

**Request:**
```json title="Python code causing memory error"
{
  "trace_id": "tr-err-006",
  "lang": "python",
  "code": "data = [0] * (10**9)",
  "timeout": 30
}
```

**Response:**
```json title="MemoryError exception response"
{
  "trace_id": "tr-err-006",
  "stdout": "",
  "stderr": "Traceback (most recent call last):\n  File \"/tmp/job-abc123/script.py\", line 1, in <module>\n    data = [0] * (10**9)\nMemoryError\n",
  "exit_code": 1,
  "error": ""
}
```

## Error Handling

### Error Categories


    **Characteristics:**
    - `exit_code`: 1 (or language-specific)
    - `stderr`: Contains parser/compiler error message
    - `error`: Empty (handled by language runtime)

    **Handling:**
```python title="Detect and handle syntax errors"

if result['exit_code'] != 0 and 'SyntaxError' in result['stderr']:
    # Handle syntax error
    show_user_error("Syntax error in code")
```



**Characteristics:**
- `exit_code`: 1 (typically)
- `stderr`: Contains stack trace
- `error`: Empty

**Common types:**
- ZeroDivisionError
- TypeError
- NameError
- IndexError
- KeyError

**Handling:**
```python title="Parse Python traceback from stderr"

if result['exit_code'] != 0 and 'Traceback' in result['stderr']:
    # Handle runtime error
    parse_traceback(result['stderr'])
```



**Characteristics:**
- `exit_code`: 124
- `stderr`: "Execution timed out"
- `error`: Empty

**Handling:**
```python title="Detect execution timeout by exit code"

if result['exit_code'] == 124:
    # Handle timeout
    show_user_error("Code took too long to execute")
```



**Characteristics:**
- `exit_code`: 127 (language not found) or other
- `stderr`: System error message
- `error`: Non-empty description

**Handling:**
```python title="Handle system-level errors from error field"

if result['error']:
    # Handle system-level error
    log_error(f"System error: {result['error']}")
```


### Best Practices

```python title="Comprehensive error categorization handler"
def handle_execution_result(result):
    """Handle execution result with proper error categorization."""

# Check for success
if result['exit_code'] == 0:
    return {
        'status': 'success',
        'output': result['stdout']
    }

# Check for timeout
if result['exit_code'] == 124:
    return {
        'status': 'timeout',
        'message': 'Execution exceeded time limit'
    }

# Check for unsupported language
if result['exit_code'] == 127:
    return {
        'status': 'error',
        'message': result['error'] or 'Unsupported language'
    }

# Check for compilation failure
if result['error'] == 'compilation failed':
    return {
        'status': 'compilation_error',
        'message': result['stderr']
    }

# Runtime error
return {
    'status': 'runtime_error',
    'message': result['stderr']
}
```

## Wire Format

The response uses the same length-prefix framing as the request:

```
+------------------+----------------------+
| Length (4 bytes) | JSON Response        |
| Big-Endian       | (UTF-8 encoded)      |
+------------------+----------------------+
```

### Go Implementation

```go title="Receive result with length-prefix parsing"
func recvResult(conn net.Conn) (*Result, error) {
    // Read length prefix
    lengthBuf := make([]byte, 4)
    if _, err := io.ReadFull(conn, lengthBuf); err != nil {
        return nil, err
    }
    length := binary.BigEndian.Uint32(lengthBuf)

    // Read payload
    payload := make([]byte, length)
    if _, err := io.ReadFull(conn, payload); err != nil {
        return nil, err
    }

    // Parse JSON
    var result Result
    if err := json.Unmarshal(payload, &result); err != nil {
        return nil, err
    }

    return &result, nil
}
```

### Python Implementation

```python title="Receive result with chunked payload reading"
import json
import struct

def recv_result(sock):
    # Read length prefix
    length_bytes = sock.recv(4)
    if len(length_bytes) < 4:
        raise Exception("Connection closed")

    length = struct.unpack('>I', length_bytes)[0]

    # Read payload
    payload = b''
    while len(payload) < length:
        chunk = sock.recv(length - len(payload))
        if not chunk:
            raise Exception("Connection closed")
        payload += chunk

    # Parse JSON
    return json.loads(payload.decode('utf-8'))
```
