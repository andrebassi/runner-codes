---
title: 'Node.js'
description: 'Node.js 22 Alpine code execution'
---

## Overview

Node.js is ideal for asynchronous operations, JSON processing, and web-related code. Runner Codes provides Node.js 22 LTS with full ES modules support.

## Specifications

| Property | Value |
|----------|-------|
| Docker Image | `node:22-alpine` |
| Version | Node.js v22.21.1 |
| Rootfs Size | 250 MB |
| Execution | Interpreted (V8 JIT) |
| File Extension | `.js` |
| Run Command | `node {file}` |
| Execution Time | ~2.7s |

:::warning

  **Kernel 5.10+ Required**: Node.js requires Linux kernel 5.10 or newer to work correctly in Firecracker microVMs.
  Older kernels (4.14) cause Node.js to hang due to libuv/epoll compatibility issues.

  Run `sudo infra.operator setup` to download the correct kernel version.

:::

```bash title="1. Create Rootfs from Docker"
sudo infra.operator rootfs from-docker --name nodejs --image node:22-alpine --size 250
```

```bash title="2. Create Snapshot"
sudo infra.operator snapshot create --lang nodejs --mem 512 --vcpus 1
```

```bash title="3. Upload rootfs to S3"
sudo infra.operator rootfs upload --lang nodejs --bucket runner-codes
```

```bash title="3. Upload snapshot to S3"
sudo infra.operator snapshot upload --lang nodejs --bucket runner-codes
```

```bash title="4. Test Execution"
sudo infra.operator host --lang nodejs --code "console.log(process.version)" --mem 512 --vcpus 1 --snapshot
```



## Execution Flow

![Node.js Execution Flow](/img/language-execution-flow.svg)

## Examples

### Hello World

```json title="Request"
{
  "language": "node",
  "files": [
    {
      "name": "main.js",
      "content": "console.log('Hello from Node.js!')"
    }
  ]
}
```

```json title="Response"
{
  "status": "success",
  "stdout": "Hello from Node.js!\n",
  "stderr": null,
  "exception": null,
  "executionTime": 45
}
```

### With STDIN Input

```json title="Request"
{
  "language": "node",
  "stdin": "Alice",
  "files": [
    {
      "name": "main.js",
      "content": "const readline = require('readline');\nconst rl = readline.createInterface({ input: process.stdin });\nrl.on('line', (name) => {\n  console.log(`Hello, ${name}!`);\n  rl.close();\n});"
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
  "executionTime": 52
}
```

### Variables and Template Literals

```json title="Request"
{
  "language": "node",
  "files": [
    {
      "name": "main.js",
      "content": "const name = 'Alice';\nconst age = 30;\nconsole.log(`Hello, ${name}! You are ${age} years old.`);"
    }
  ]
}
```

```json title="Response"
{
  "status": "success",
  "stdout": "Hello, Alice! You are 30 years old.\n",
  "stderr": null,
  "exception": null,
  "executionTime": 40
}
```

### Arrow Functions

```json title="Request"
{
  "trace_id": "node-arrow-001",
  "lang": "node",
  "code": "const numbers = [1, 2, 3, 4, 5];\nconst doubled = numbers.map(n => n * 2);\nconst sum = numbers.reduce((a, b) => a + b, 0);\nconsole.log('Doubled:', doubled);\nconsole.log('Sum:', sum);",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "node-arrow-001",
  "stdout": "Doubled: [ 2, 4, 6, 8, 10 ]\nSum: 15\n",
  "stderr": "",
  "exit_code": 0
}
```

### Async/Await

```json title="Request"
{
  "trace_id": "node-async-001",
  "lang": "node",
  "code": "async function delay(ms) {\n  return new Promise(resolve => setTimeout(resolve, ms));\n}\n\nasync function main() {\n  console.log('Start');\n  await delay(100);\n  console.log('After 100ms');\n  await delay(100);\n  console.log('Done');\n}\n\nmain();",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "node-async-001",
  "stdout": "Start\nAfter 100ms\nDone\n",
  "stderr": "",
  "exit_code": 0
}
```

### Promises

```json title="Request"
{
  "trace_id": "node-promise-001",
  "lang": "node",
  "code": "const promise1 = Promise.resolve(10);\nconst promise2 = Promise.resolve(20);\nconst promise3 = Promise.resolve(30);\n\nPromise.all([promise1, promise2, promise3])\n  .then(values => {\n    console.log('Values:', values);\n    console.log('Total:', values.reduce((a, b) => a + b, 0));\n  });",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "node-promise-001",
  "stdout": "Values: [ 10, 20, 30 ]\nTotal: 60\n",
  "stderr": "",
  "exit_code": 0
}
```

### Object Destructuring

```json title="Request"
{
  "trace_id": "node-destruct-001",
  "lang": "node",
  "code": "const user = { name: 'Bob', age: 25, city: 'LA' };\nconst { name, ...rest } = user;\nconsole.log('Name:', name);\nconsole.log('Rest:', rest);",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "node-destruct-001",
  "stdout": "Name: Bob\nRest: { age: 25, city: 'LA' }\n",
  "stderr": "",
  "exit_code": 0
}
```

### Classes

```json title="Request"
{
  "trace_id": "node-class-001",
  "lang": "node",
  "code": "class Animal {\n  constructor(name) {\n    this.name = name;\n  }\n  speak() {\n    console.log(`${this.name} makes a sound`);\n  }\n}\n\nclass Dog extends Animal {\n  speak() {\n    console.log(`${this.name} barks`);\n  }\n}\n\nconst dog = new Dog('Rex');\ndog.speak();",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "node-class-001",
  "stdout": "Rex barks\n",
  "stderr": "",
  "exit_code": 0
}
```

### JSON Processing

```json title="Request"
{
  "trace_id": "node-json-001",
  "lang": "node",
  "code": "const data = {\n  users: [\n    { id: 1, name: 'Alice' },\n    { id: 2, name: 'Bob' }\n  ]\n};\n\nconst json = JSON.stringify(data, null, 2);\nconsole.log('JSON:');\nconsole.log(json);\n\nconst parsed = JSON.parse(json);\nconsole.log('\\nUsers:', parsed.users.map(u => u.name).join(', '));",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "node-json-001",
  "stdout": "JSON:\n{\n  \"users\": [\n    {\n      \"id\": 1,\n      \"name\": \"Alice\"\n    },\n    {\n      \"id\": 2,\n      \"name\": \"Bob\"\n    }\n  ]\n}\n\nUsers: Alice, Bob\n",
  "stderr": "",
  "exit_code": 0
}
```

### Array Methods

```json title="Request"
{
  "trace_id": "node-array-001",
  "lang": "node",
  "code": "const numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];\n\nconst evens = numbers.filter(n => n % 2 === 0);\nconst squared = evens.map(n => n * n);\nconst sum = squared.reduce((a, b) => a + b, 0);\n\nconsole.log('Original:', numbers);\nconsole.log('Evens:', evens);\nconsole.log('Squared:', squared);\nconsole.log('Sum:', sum);",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "node-array-001",
  "stdout": "Original: [\n  1, 2, 3, 4,  5,\n  6, 7, 8, 9, 10\n]\nEvens: [ 2, 4, 6, 8, 10 ]\nSquared: [ 4, 16, 36, 64, 100 ]\nSum: 220\n",
  "stderr": "",
  "exit_code": 0
}
```

### Map and Set

```json title="Request"
{
  "trace_id": "node-mapset-001",
  "lang": "node",
  "code": "// Map example\nconst map = new Map();\nmap.set('a', 1);\nmap.set('b', 2);\nmap.set('c', 3);\nconsole.log('Map size:', map.size);\nconsole.log('Map entries:', [...map.entries()]);\n\n// Set example\nconst set = new Set([1, 2, 2, 3, 3, 3]);\nconsole.log('Set values:', [...set]);",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "node-mapset-001",
  "stdout": "Map size: 3\nMap entries: [ [ 'a', 1 ], [ 'b', 2 ], [ 'c', 3 ] ]\nSet values: [ 1, 2, 3 ]\n",
  "stderr": "",
  "exit_code": 0
}
```

### Regular Expressions

```json title="Request"
{
  "trace_id": "node-regex-001",
  "lang": "node",
  "code": "const text = 'Contact: john@example.com or jane@test.org';\nconst emailRegex = /[\\w.-]+@[\\w.-]+\\.[a-z]{2,}/gi;\nconst emails = text.match(emailRegex);\nconsole.log('Found emails:', emails);",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "node-regex-001",
  "stdout": "Found emails: [ 'john@example.com', 'jane@test.org' ]\n",
  "stderr": "",
  "exit_code": 0
}
```

### File Operations

```json title="Request"
{
  "trace_id": "node-file-001",
  "lang": "node",
  "code": "const fs = require('fs');\n\n// Write file\nfs.writeFileSync('/tmp/test.txt', 'Hello, Node.js!\\nLine 2');\n\n// Read file\nconst content = fs.readFileSync('/tmp/test.txt', 'utf8');\nconsole.log('Content:');\nconsole.log(content);",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "node-file-001",
  "stdout": "Content:\nHello, Node.js!\nLine 2\n",
  "stderr": "",
  "exit_code": 0
}
```

### Complex Test: Comprehensive Node.js

```json title="Request"
{
  "trace_id": "node-complex-001",
  "lang": "node",
  "code": "console.log('=== Node.js Complex Test ===');\nconsole.log();\n\n// Test 1: Fibonacci with memoization\nconsole.log('1. Fibonacci with memoization:');\nconst memo = {};\nfunction fib(n) {\n  if (n in memo) return memo[n];\n  if (n <= 1) return n;\n  memo[n] = fib(n - 1) + fib(n - 2);\n  return memo[n];\n}\nconst fibs = Array.from({length: 15}, (_, i) => fib(i));\nconsole.log(`   First 15: [${fibs.join(', ')}]`);\nconsole.log(`   Fib(50) = ${fib(50)}`);\n\n// Test 2: QuickSort\nconsole.log();\nconsole.log('2. QuickSort:');\nfunction quicksort(arr) {\n  if (arr.length <= 1) return arr;\n  const pivot = arr[Math.floor(arr.length / 2)];\n  const left = arr.filter(x => x < pivot);\n  const middle = arr.filter(x => x === pivot);\n  const right = arr.filter(x => x > pivot);\n  return [...quicksort(left), ...middle, ...quicksort(right)];\n}\nconst unsorted = [64, 34, 25, 12, 22, 11, 90];\nconsole.log(`   Input:  [${unsorted.join(', ')}]`);\nconsole.log(`   Output: [${quicksort(unsorted).join(', ')}]`);\n\n// Test 3: Object operations\nconsole.log();\nconsole.log('3. Object operations:');\nconst people = [\n  { name: 'Alice', age: 30, city: 'NYC' },\n  { name: 'Bob', age: 25, city: 'LA' },\n  { name: 'Charlie', age: 35, city: 'Chicago' }\n];\nconst byCity = Object.fromEntries(people.map(p => [p.city, p.name]));\nconsole.log(`   People: [${people.map(p => p.name).join(', ')}]`);\nconsole.log(`   By city: ${JSON.stringify(byCity)}`);\nconsole.log(`   Avg age: ${people.reduce((s, p) => s + p.age, 0) / people.length}`);\n\n// Test 4: Array methods\nconsole.log();\nconsole.log('4. Array methods:');\nconst numbers = Array.from({length: 10}, (_, i) => i + 1);\nconst squares = numbers.map(x => x * x);\nconst evens = numbers.filter(x => x % 2 === 0);\nconst sum = squares.reduce((a, b) => a + b, 0);\nconsole.log(`   Numbers: [${numbers.join(', ')}]`);\nconsole.log(`   Squares: [${squares.join(', ')}]`);\nconsole.log(`   Evens: [${evens.join(', ')}]`);\nconsole.log(`   Sum of squares: ${sum}`);\n\n// Test 5: Classes and inheritance\nconsole.log();\nconsole.log('5. Classes and inheritance:');\nclass Animal {\n  constructor(name) { this.name = name; }\n  speak() { return 'Some sound'; }\n}\nclass Dog extends Animal {\n  speak() { return `${this.name} says Woof!`; }\n}\nclass Cat extends Animal {\n  speak() { return `${this.name} says Meow!`; }\n}\nconst animals = [new Dog('Rex'), new Cat('Whiskers'), new Dog('Buddy')];\nanimals.forEach(a => console.log(`   ${a.speak()}`));\n\n// Test 6: Promises and async simulation\nconsole.log();\nconsole.log('6. Promise handling:');\nconst asyncOp = (val, delay) => new Promise(r => setTimeout(() => r(val * 2), delay));\nPromise.all([asyncOp(5, 10), asyncOp(10, 10), asyncOp(15, 10)])\n  .then(results => {\n    console.log(`   Results: [${results.join(', ')}]`);\n    console.log(`   Sum: ${results.reduce((a, b) => a + b, 0)}`);\n    \n    // Test 7: Destructuring and spread\n    console.log();\n    console.log('7. Destructuring and spread:');\n    const { name, ...rest } = people[0];\n    console.log(`   Name: ${name}`);\n    console.log(`   Rest: ${JSON.stringify(rest)}`);\n    const merged = [...numbers.slice(0, 3), ...numbers.slice(-3)];\n    console.log(`   Merged: [${merged.join(', ')}]`);\n    \n    // Test 8: String operations\n    console.log();\n    console.log('8. String operations:');\n    const text = 'Hello, World! Welcome to Node.js.';\n    console.log(`   Original: ${text}`);\n    console.log(`   Upper: ${text.toUpperCase()}`);\n    console.log(`   Words: ${text.split(' ').length}`);\n    console.log(`   Includes 'Node': ${text.includes('Node')}`);\n    \n    console.log();\n    console.log('=== All tests passed ===');\n  });",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "node-complex-001",
  "stdout": "=== Node.js Complex Test ===\n\n1. Fibonacci with memoization:\n   First 15: [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]\n   Fib(50) = 12586269025\n\n2. QuickSort:\n   Input:  [64, 34, 25, 12, 22, 11, 90]\n   Output: [11, 12, 22, 25, 34, 64, 90]\n\n3. Object operations:\n   People: [Alice, Bob, Charlie]\n   By city: {\"NYC\":\"Alice\",\"LA\":\"Bob\",\"Chicago\":\"Charlie\"}\n   Avg age: 30\n\n4. Array methods:\n   Numbers: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]\n   Squares: [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]\n   Evens: [2, 4, 6, 8, 10]\n   Sum of squares: 385\n\n5. Classes and inheritance:\n   Rex says Woof!\n   Whiskers says Meow!\n   Buddy says Woof!\n\n6. Promise handling:\n   Results: [10, 20, 30]\n   Sum: 60\n\n7. Destructuring and spread:\n   Name: Alice\n   Rest: {\"age\":30,\"city\":\"NYC\"}\n   Merged: [1, 2, 3, 8, 9, 10]\n\n8. String operations:\n   Original: Hello, World! Welcome to Node.js.\n   Upper: HELLO, WORLD! WELCOME TO NODE.JS.\n   Words: 5\n   Includes 'Node': true\n\n=== All tests passed ===\n",
  "stderr": "",
  "exit_code": 0
}
```

## Available Built-in Modules

| Module | Description |
|--------|-------------|
| `fs` | File system operations |
| `path` | Path manipulation |
| `os` | OS information |
| `util` | Utility functions |
| `crypto` | Cryptographic functions |
| `buffer` | Binary data handling |
| `stream` | Streaming operations |
| `events` | Event emitter |
| `url` | URL parsing |
| `querystring` | Query string parsing |
| `assert` | Assertions |

:::note

  Network modules (`http`, `https`, `net`, etc.) are available but won't work since the VM has no network access.

:::

## Error Examples

### Syntax Error

```json title="Request"
{
  "trace_id": "node-err-syntax",
  "lang": "node",
  "code": "const x = {;",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "node-err-syntax",
  "stdout": "",
  "stderr": "/tmp/job-node-err-syntax/script.js:1\nconst x = {;\n           ^\n\nSyntaxError: Unexpected token ';'\n    at ...\n",
  "exit_code": 1
}
```

### Runtime Error

```json title="Request"
{
  "trace_id": "node-err-runtime",
  "lang": "node",
  "code": "const obj = null;\nconsole.log(obj.property);",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "node-err-runtime",
  "stdout": "",
  "stderr": "TypeError: Cannot read properties of null (reading 'property')\n    at Object.<anonymous> (/tmp/job-node-err-runtime/script.js:2:17)\n    at ...\n",
  "exit_code": 1
}
```

### Module Not Found

```json title="Request"
{
  "trace_id": "node-err-module",
  "lang": "node",
  "code": "const axios = require('axios');\nconsole.log(axios);",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "node-err-module",
  "stdout": "",
  "stderr": "Error: Cannot find module 'axios'\n    at ...\n",
  "exit_code": 1
}
```

## Performance

| Operation | Time |
|-----------|------|
| Hello World | ~100ms |
| Fibonacci(30) | ~100ms (V8 JIT) |
| JSON parse/stringify (1KB) | ~1ms |
| Array operations (10k items) | ~5ms |

## Limitations

:::warning

  The Node.js environment has the following limitations:

:::

1. **Kernel 5.10+ required**: Older kernels cause Node.js to hang (libuv/epoll issues)
2. **No npm install**: External packages must be pre-installed in rootfs
3. **No network**: HTTP requests will fail
4. **Memory limit**: 512 MiB default
5. **Timeout**: Configurable, default 10 seconds
6. **No persistence**: Files are lost after execution
7. **CommonJS only**: ES modules require file extension changes

## Adding npm Packages to Rootfs

### Option 1: Custom Dockerfile (Recommended)

Create a custom Dockerfile with your packages:

```dockerfile title="Create custom Docker image with npm packages"
FROM node:18-alpine
RUN npm install -g lodash moment uuid typescript
```

```bash title="Build Docker image and create rootfs from it"
docker build -t my-nodejs .
infra.operator rootfs from-docker --name nodejs --image my-nodejs --size 500
```

```bash title="Create snapshot and upload to S3"
infra.operator snapshot create --lang nodejs --mem 512 --vcpus 1
infra.operator rootfs upload --lang nodejs --bucket runner-codes
infra.operator snapshot upload --lang nodejs --bucket runner-codes
```

### Option 2: Mount and modify existing rootfs

```bash title="Mount the rootfs for editing"
sudo mount -o loop /srv/firecracker/images/rootfs-nodejs.ext4 /mnt/rootfs
```

```bash title="Prepare chroot environment"
sudo cp /etc/resolv.conf /mnt/rootfs/etc/
sudo mount --bind /dev /mnt/rootfs/dev
sudo mount --bind /proc /mnt/rootfs/proc
sudo mount --bind /sys /mnt/rootfs/sys
```

```bash title="Install packages and cleanup"
sudo chroot /mnt/rootfs /bin/sh
npm install -g lodash moment uuid
exit
sudo umount /mnt/rootfs/sys
sudo umount /mnt/rootfs/proc
sudo umount /mnt/rootfs/dev
sudo umount /mnt/rootfs
```

```bash title="Recreate snapshot and upload to S3"
infra.operator snapshot create --lang nodejs --mem 512 --vcpus 1
infra.operator rootfs upload --lang nodejs --bucket runner-codes
infra.operator snapshot upload --lang nodejs --bucket runner-codes
```

## Troubleshooting

### Node.js hangs or times out with "signal: killed"

**Problem**: Node.js execution hangs and eventually times out with `signal: killed` error.

**Cause**: Linux kernel 4.14 has compatibility issues with Node.js libuv event loop in Firecracker microVMs.

**Solution**: Upgrade to kernel 5.10+

```bash title="Run setup to download kernel 5.10"
sudo infra.operator setup
```

```bash title="Verify kernel path"
ls -la /srv/firecracker/vmlinux
```

```bash title="Recreate snapshots with new kernel"
infra.operator snapshot create --lang nodejs --mem 512 --vcpus 1
```

### EXT4 filesystem corruption after snapshot restore

**Problem**: `Corrupt inode bitmap` or similar errors after restoring snapshot.

**Cause**: Filesystem not synced before snapshot creation.

**Solution**: The snapshot creation now automatically syncs the filesystem. If issues persist:

```bash title="Increase rootfs size"
infra.operator rootfs from-docker --name nodejs --image node:18-alpine --size 500
```

```bash title="Recreate snapshot"
infra.operator snapshot create --lang nodejs --mem 512 --vcpus 1
```

## Best Practices

**Prefer async/await:**

Prefer async/await over callbacks or raw promises for cleaner code.

**Output to stdout/stderr:**

All output must go to stdout/stderr. Use `console.log()` for stdout and `console.error()` for stderr.

**Let scripts complete naturally:**

Let the script complete naturally. Use process.exit() only when needed.

**Keep it simple:**

For simple operations, synchronous code is easier to reason about.

**Use kernel 5.10+:**

Run `sudo infra.operator setup` on new machines to download the correct kernel version.

