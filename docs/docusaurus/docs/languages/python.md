---
title: 'Python'
description: 'Python 3.12 code execution'
---

## Overview

Python is one of the most commonly used languages for LLM-generated code. LLM-Firecracker provides Python 3.12 with the complete standard library.

## Specifications

| Property | Value |
|----------|-------|
| Docker Image | `python:3.12-alpine` |
| Version | Python 3.12.12 |
| Rootfs Size | 150 MB |
| Execution | Interpreted |
| File Extension | `.py` |
| Run Command | `python3 {file}` |
| Execution Time | ~35ms |

## Infrastructure

```bash title="1. Create Rootfs from Docker"
sudo infra.operator rootfs from-docker --name python --image python:3.12-alpine --size 150
```

```bash title="2. Create Snapshot"
sudo infra.operator snapshot create --lang python --mem 512 --vcpus 1
```

```bash title="3. Upload rootfs to S3"
sudo infra.operator rootfs upload --lang python --bucket llm-firecracker
```

```bash title="3. Upload snapshot to S3"
sudo infra.operator snapshot upload --lang python --bucket llm-firecracker
```

```bash title="4. Test Execution"
sudo infra.operator host --lang python --code "import sys; print(sys.version)" --mem 512 --vcpus 1 --snapshot
```

```json title="Expected output"
{
  "status": "success",
  "stdout": "3.12.12 (main, Oct  9 2025, 22:34:32) [GCC 14.2.0]\n",
  "executionTime": "~35ms"
}
```

## Execution Flow

![Python Execution Flow](/img/language-execution-flow.svg)

## Examples

### Hello World

```json title="Request"
{
  "language": "python",
  "files": [
    {
      "name": "main.py",
      "content": "print('Hello from Python!')"
    }
  ]
}
```

```json title="Response"
{
  "status": "success",
  "stdout": "Hello from Python!\n",
  "stderr": null,
  "exception": null,
  "executionTime": 45
}
```

### Variables and Calculations

```json title="Request"
{
  "language": "python",
  "files": [
    {
      "name": "main.py",
      "content": "x = 10\ny = 20\nprint(f'Sum: {x + y}')\nprint(f'Product: {x * y}')"
    }
  ]
}
```

```json title="Response"
{
  "status": "success",
  "stdout": "Sum: 30\nProduct: 200\n",
  "stderr": null,
  "exception": null,
  "executionTime": 42
}
```

### With STDIN Input

```json title="Request"
{
  "language": "python",
  "stdin": "Alice",
  "files": [
    {
      "name": "main.py",
      "content": "import sys\nname = sys.stdin.readline().strip()\nprint(f'Hello, {name}!')"
    }
  ]
}
```

```json title="Response"
{
  "status": "success",
  "stdout": "Hello, Alice!\n",
  "stderr": null,
  "exception": null,
  "executionTime": 38
}
```

### Batch Execution

Execute the same code with multiple inputs:

```json title="Request"
{
  "language": "python",
  "stdin": ["Alice", "Bob", "Charlie"],
  "files": [
    {
      "name": "main.py",
      "content": "import sys\nname = sys.stdin.readline().strip()\nprint(f'Hello, {name}!')"
    }
  ]
}
```

```json title="Response"
[
  {
    "status": "success",
    "stdout": "Hello, Alice!\n",
    "stderr": null,
    "exception": null,
    "executionTime": 35,
    "stdin": "Alice"
  },
  {
    "status": "success",
    "stdout": "Hello, Bob!\n",
    "stderr": null,
    "exception": null,
    "executionTime": 32,
    "stdin": "Bob"
  },
  {
    "status": "success",
    "stdout": "Hello, Charlie!\n",
    "stderr": null,
    "exception": null,
    "executionTime": 31,
    "stdin": "Charlie"
  }
]
```

### Functions

```json title="Request"
{
  "language": "python",
  "files": [
    {
      "name": "main.py",
      "content": "def fibonacci(n):\n    if n <= 1:\n        return n\n    return fibonacci(n-1) + fibonacci(n-2)\n\nfor i in range(10):\n    print(f'F({i}) = {fibonacci(i)}')"
    }
  ]
}
```

```json title="Response"
{
  "status": "success",
  "stdout": "F(0) = 0\nF(1) = 1\nF(2) = 1\nF(3) = 2\nF(4) = 3\nF(5) = 5\nF(6) = 8\nF(7) = 13\nF(8) = 21\nF(9) = 34\n",
  "stderr": null,
  "exception": null,
  "executionTime": 55
}
```

### Classes

```json title="Request"
{
  "language": "python",
  "files": [
    {
      "name": "main.py",
      "content": "class Calculator:\n    def __init__(self):\n        self.result = 0\n    \n    def add(self, x):\n        self.result += x\n        return self\n    \n    def multiply(self, x):\n        self.result *= x\n        return self\n\ncalc = Calculator()\ncalc.add(5).multiply(3).add(10)\nprint(f'Result: {calc.result}')"
    }
  ]
}
```

```json title="Response"
{
  "status": "success",
  "stdout": "Result: 25\n",
  "stderr": null,
  "exception": null,
  "executionTime": 40
}
```

### JSON Processing

```json title="Request"
{
  "language": "python",
  "files": [
    {
      "name": "main.py",
      "content": "import json\n\ndata = {'users': [{'name': 'Alice'}, {'name': 'Bob'}]}\njson_str = json.dumps(data, indent=2)\nprint(json_str)\n\nparsed = json.loads(json_str)\nfor user in parsed['users']:\n    print(f\"User: {user['name']}\")"
    }
  ]
}
```

```json title="Response"
{
  "status": "success",
  "stdout": "{\n  \"users\": [\n    {\n      \"name\": \"Alice\"\n    },\n    {\n      \"name\": \"Bob\"\n    }\n  ]\n}\nUser: Alice\nUser: Bob\n",
  "stderr": null,
  "exception": null,
  "executionTime": 48
}
```

### File Operations (within sandbox)

```json title="Request"
{
  "language": "python",
  "files": [
    {
      "name": "main.py",
      "content": "# Write file\nwith open('/tmp/test.txt', 'w') as f:\n    f.write('Hello, File!\\n')\n    f.write('Line 2\\n')\n\n# Read file\nwith open('/tmp/test.txt', 'r') as f:\n    content = f.read()\n    print(content)"
    }
  ]
}
```

```json title="Response"
{
  "status": "success",
  "stdout": "Hello, File!\nLine 2\n",
  "stderr": null,
  "exception": null,
  "executionTime": 52
}
```

## Available Standard Library Modules

The following standard library modules are available:

| Category | Modules |
|----------|---------|
| Data Types | `json`, `datetime`, `collections`, `itertools`, `functools` |
| Text | `re`, `string`, `textwrap` |
| Math | `math`, `random`, `statistics`, `decimal`, `fractions` |
| File/IO | `os`, `sys`, `io`, `pathlib`, `tempfile` |
| Algorithms | `hashlib`, `base64`, `binascii` |
| Utilities | `copy`, `pprint`, `typing`, `dataclasses` |

:::note

  Network modules (`urllib`, `http`, etc.) are available but won't work since the VM has no network access.

:::

### Multi-File Project

```json title="Request"
{
  "language": "python",
  "files": [
    {
      "name": "main.py",
      "content": "from utils import greet\nfrom math_ops import add, multiply\n\nprint(greet('World'))\nprint(f'2 + 3 = {add(2, 3)}')\nprint(f'4 * 5 = {multiply(4, 5)}')"
    },
    {
      "name": "utils.py",
      "content": "def greet(name):\n    return f'Hello, {name}!'"
    },
    {
      "name": "math_ops.py",
      "content": "def add(a, b):\n    return a + b\n\ndef multiply(a, b):\n    return a * b"
    }
  ]
}
```

```json title="Response"
{
  "status": "success",
  "stdout": "Hello, World!\n2 + 3 = 5\n4 * 5 = 20\n",
  "stderr": null,
  "exception": null,
  "executionTime": 65
}
```

## Error Examples

### Syntax Error

```json title="Request"
{
  "language": "python",
  "files": [
    {
      "name": "main.py",
      "content": "print('Hello'\n"
    }
  ]
}
```

```json title="Response"
{
  "status": "success",
  "stdout": "",
  "stderr": "  File \"/tmp/job-xxx/main.py\", line 1\n    print('Hello'\n                 ^\nSyntaxError: unexpected EOF while parsing\n",
  "exception": null,
  "executionTime": 35
}
```

### Runtime Error

```json title="Request"
{
  "language": "python",
  "files": [
    {
      "name": "main.py",
      "content": "my_list = [1, 2, 3]\nprint(my_list[10])"
    }
  ]
}
```

```json title="Response"
{
  "status": "success",
  "stdout": "",
  "stderr": "Traceback (most recent call last):\n  File \"/tmp/job-xxx/main.py\", line 2, in <module>\n    print(my_list[10])\nIndexError: list index out of range\n",
  "exception": null,
  "executionTime": 40
}
```

### Timeout Error

```json title="Request"
{
  "language": "python",
  "files": [
    {
      "name": "main.py",
      "content": "import time\nwhile True:\n    time.sleep(1)"
    }
  ]
}
```

```json title="Response"
{
  "status": "success",
  "stdout": "",
  "stderr": null,
  "exception": "Timeout",
  "executionTime": 30000
}
```

### Import Error

```json title="Request"
{
  "language": "python",
  "files": [
    {
      "name": "main.py",
      "content": "import numpy\nprint(numpy.array([1, 2, 3]))"
    }
  ]
}
```

```json title="Response"
{
  "status": "success",
  "stdout": "",
  "stderr": "Traceback (most recent call last):\n  File \"/tmp/job-xxx/main.py\", line 1, in <module>\n    import numpy\nModuleNotFoundError: No module named 'numpy'\n",
  "exception": null,
  "executionTime": 38
}
```

### Unsupported Language

```json title="Request"
{
  "language": "unknownlang",
  "files": [
    {
      "name": "main.xyz",
      "content": "print hello"
    }
  ]
}
```

```json title="Response"
{
  "status": "failed",
  "error": "E006: unsupported language unknownlang"
}
```

## Performance

| Operation | Time |
|-----------|------|
| Hello World | ~50ms |
| Fibonacci(30) | ~300ms |
| JSON parse/dump (1KB) | ~5ms |
| List operations (10k items) | ~10ms |

## Limitations

:::warning

  The Python environment has the following limitations:

:::

1. **No pip install**: External packages must be pre-installed in rootfs
2. **No network**: Network operations will fail
3. **Memory limit**: 512 MiB default
4. **Timeout**: Configurable, default 10 seconds
5. **No persistence**: Files are lost after execution

## Adding Packages to Rootfs

To add Python packages to the rootfs:

```bash title="Mount the rootfs for editing"
sudo mount -o loop /srv/firecracker/images/rootfs-python.ext4 /mnt/rootfs
```

```bash title="Copy DNS resolution configuration"
sudo cp /etc/resolv.conf /mnt/rootfs/etc/
```

```bash title="Mount /dev for device access"
sudo mount --bind /dev /mnt/rootfs/dev
```

```bash title="Mount /proc for process information"
sudo mount --bind /proc /mnt/rootfs/proc
```

```bash title="Mount /sys for system information"
sudo mount --bind /sys /mnt/rootfs/sys
```

```bash title="Enter chroot environment"
sudo chroot /mnt/rootfs /bin/bash
```

```bash title="Install additional Python packages with pip"
pip3 install numpy pandas requests
```

```bash title="Exit chroot environment"
exit
```

```bash title="Unmount /sys"
sudo umount /mnt/rootfs/sys
```

```bash title="Unmount /proc"
sudo umount /mnt/rootfs/proc
```

```bash title="Unmount /dev"
sudo umount /mnt/rootfs/dev
```

```bash title="Unmount rootfs"
sudo umount /mnt/rootfs
```

```bash title="Upload modified rootfs to S3 bucket"
task s3:upload-single LANG=python
```

## Best Practices

**Use standard library only:**

Write code that doesn't depend on external packages. Use only standard library modules.

**Handle errors gracefully:**

Use try/except blocks to provide meaningful error messages.

**Output to stdout/stderr:**

All output must go to stdout/stderr. There's no other way to return data.

**Ensure termination:**

Always have termination conditions. Long-running code will be killed by timeout.

