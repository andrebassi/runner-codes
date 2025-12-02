---
title: 'Bash'
description: 'Bash 5.2 shell script execution'
---

## Overview

Bash is ideal for system operations, file manipulation, and quick shell scripting. Runner Codes provides Bash 5.2 on Alpine Linux with BusyBox utilities.

## Specifications

| Property | Value |
|----------|-------|
| Docker Image | `alpine:3.20` + bash |
| Version | Bash 5.2.26 |
| Rootfs Size | 100 MB |
| Execution | Interpreted |
| File Extension | `.sh` |
| Run Command | `bash {file}` |
| Execution Time | ~5ms |

```bash title="1. Create Rootfs from Docker"
sudo infra.operator rootfs from-docker --name bash --image alpine:3.20 --size 100
```

```bash title="2. Create Snapshot"
sudo infra.operator snapshot create --lang bash --mem 256 --vcpus 1
```

```bash title="3. Upload rootfs to S3"
sudo infra.operator rootfs upload --lang bash --bucket runner-codes
```

```bash title="3. Upload snapshot to S3"
sudo infra.operator snapshot upload --lang bash --bucket runner-codes
```

```bash title="4. Test Execution"
sudo infra.operator host --lang bash --code "bash --version | head -1" --mem 256 --vcpus 1 --snapshot
```



## Execution Flow

![Bash Execution Flow](/img/language-execution-flow.svg)

## Examples

### Hello World

```json title="Request"
{
  "trace_id": "bash-hello-001",
  "lang": "bash",
  "code": "echo 'Hello from Bash!'",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "bash-hello-001",
  "stdout": "Hello from Bash!\n",
  "stderr": "",
  "exit_code": 0
}
```

### Variables

```json title="Request"
{
  "trace_id": "bash-vars-001",
  "lang": "bash",
  "code": "name=\"Alice\"\nage=30\necho \"Name: $name\"\necho \"Age: $age\"\necho \"Combined: ${name} is ${age} years old\"",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "bash-vars-001",
  "stdout": "Name: Alice\nAge: 30\nCombined: Alice is 30 years old\n",
  "stderr": "",
  "exit_code": 0
}
```

### Arithmetic

```json title="Request"
{
  "trace_id": "bash-math-001",
  "lang": "bash",
  "code": "a=10\nb=20\nsum=$((a + b))\nproduct=$((a * b))\necho \"Sum: $sum\"\necho \"Product: $product\"\necho \"Division: $((a / 2))\"\necho \"Modulo: $((b % 3))\"",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "bash-math-001",
  "stdout": "Sum: 30\nProduct: 200\nDivision: 5\nModulo: 2\n",
  "stderr": "",
  "exit_code": 0
}
```

### Arrays

```json title="Request"
{
  "trace_id": "bash-array-001",
  "lang": "bash",
  "code": "fruits=(apple banana cherry)\necho \"First: ${fruits[0]}\"\necho \"All: ${fruits[@]}\"\necho \"Count: ${#fruits[@]}\"\n\n# Add element\nfruits+=(date)\necho \"After adding: ${fruits[@]}\"\n\n# Loop\nfor fruit in \"${fruits[@]}\"; do\n    echo \"- $fruit\"\ndone",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "bash-array-001",
  "stdout": "First: apple\nAll: apple banana cherry\nCount: 3\nAfter adding: apple banana cherry date\n- apple\n- banana\n- cherry\n- date\n",
  "stderr": "",
  "exit_code": 0
}
```

### Conditionals

```json title="Request"
{
  "trace_id": "bash-cond-001",
  "lang": "bash",
  "code": "x=10\n\nif [ $x -gt 5 ]; then\n    echo \"x is greater than 5\"\nfi\n\nif [ $x -eq 10 ]; then\n    echo \"x equals 10\"\nelse\n    echo \"x is not 10\"\nfi\n\n# String comparison\nname=\"Alice\"\nif [[ \"$name\" == \"Alice\" ]]; then\n    echo \"Hello, Alice!\"\nfi",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "bash-cond-001",
  "stdout": "x is greater than 5\nx equals 10\nHello, Alice!\n",
  "stderr": "",
  "exit_code": 0
}
```

### Loops

```json title="Request"
{
  "trace_id": "bash-loop-001",
  "lang": "bash",
  "code": "# For loop with range\necho \"For loop:\"\nfor i in {1..5}; do\n    echo \"  Iteration: $i\"\ndone\n\n# While loop\necho \"While loop:\"\ncount=0\nwhile [ $count -lt 3 ]; do\n    echo \"  Count: $count\"\n    ((count++))\ndone",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "bash-loop-001",
  "stdout": "For loop:\n  Iteration: 1\n  Iteration: 2\n  Iteration: 3\n  Iteration: 4\n  Iteration: 5\nWhile loop:\n  Count: 0\n  Count: 1\n  Count: 2\n",
  "stderr": "",
  "exit_code": 0
}
```

### Functions

```json title="Request"
{
  "trace_id": "bash-func-001",
  "lang": "bash",
  "code": "greet() {\n    local name=$1\n    echo \"Hello, $name!\"\n}\n\nadd() {\n    local a=$1\n    local b=$2\n    echo $((a + b))\n}\n\ngreet \"World\"\nresult=$(add 5 3)\necho \"5 + 3 = $result\"",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "bash-func-001",
  "stdout": "Hello, World!\n5 + 3 = 8\n",
  "stderr": "",
  "exit_code": 0
}
```

### String Operations

```json title="Request"
{
  "trace_id": "bash-string-001",
  "lang": "bash",
  "code": "str=\"Hello, World!\"\n\necho \"Original: $str\"\necho \"Length: ${#str}\"\necho \"Uppercase: ${str^^}\"\necho \"Lowercase: ${str,,}\"\necho \"Substring (0-5): ${str:0:5}\"\necho \"Replace: ${str/World/Bash}\"",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "bash-string-001",
  "stdout": "Original: Hello, World!\nLength: 13\nUppercase: HELLO, WORLD!\nLowercase: hello, world!\nSubstring (0-5): Hello\nReplace: Hello, Bash!\n",
  "stderr": "",
  "exit_code": 0
}
```

### File Operations

```json title="Request"
{
  "trace_id": "bash-file-001",
  "lang": "bash",
  "code": "# Write file\necho -e \"Line 1\\nLine 2\\nLine 3\" > /tmp/test.txt\n\n# Read file\necho \"File contents:\"\ncat /tmp/test.txt\n\n# Line count\necho \"Line count: $(wc -l < /tmp/test.txt)\"\n\n# Check if file exists\nif [ -f /tmp/test.txt ]; then\n    echo \"File exists\"\nfi",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "bash-file-001",
  "stdout": "File contents:\nLine 1\nLine 2\nLine 3\nLine count: 3\nFile exists\n",
  "stderr": "",
  "exit_code": 0
}
```

### Pipes and Redirection

```json title="Request"
{
  "trace_id": "bash-pipe-001",
  "lang": "bash",
  "code": "# Create sample data\necho -e \"apple\\nbanana\\napricot\\nblueberry\\ncherry\" > /tmp/fruits.txt\n\n# Filter with grep\necho \"Fruits starting with 'a':\"\ngrep '^a' /tmp/fruits.txt\n\n# Count\necho \"Count: $(grep -c '^a' /tmp/fruits.txt)\"\n\n# Sort\necho \"Sorted:\"\nsort /tmp/fruits.txt\n\n# Pipeline\necho \"Pipeline (sort | head -3):\"\nsort /tmp/fruits.txt | head -3",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "bash-pipe-001",
  "stdout": "Fruits starting with 'a':\napple\napricot\nCount: 2\nSorted:\napple\napricot\nbanana\nblueberry\ncherry\nPipeline (sort | head -3):\napple\napricot\nbanana\n",
  "stderr": "",
  "exit_code": 0
}
```

### System Information

```json title="Request"
{
  "trace_id": "bash-sysinfo-001",
  "lang": "bash",
  "code": "echo \"Hostname: $(hostname)\"\necho \"User: $(whoami)\"\necho \"PWD: $(pwd)\"\necho \"Date: $(date)\"\necho \"Uptime: $(uptime)\"\necho \"Kernel: $(uname -r)\"",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "bash-sysinfo-001",
  "stdout": "Hostname: localhost\nUser: root\nPWD: /tmp/job-bash-sysinfo-001\nDate: Fri Nov 29 10:30:45 UTC 2024\nUptime: 10:30:45 up 0 min, 0 users, load average: 0.00, 0.00, 0.00\nKernel: 5.10.0\n",
  "stderr": "",
  "exit_code": 0
}
```

### AWK Processing

```json title="Request"
{
  "trace_id": "bash-awk-001",
  "lang": "bash",
  "code": "# Create CSV data\ncat << 'EOF' > /tmp/data.csv\nname,age,city\nAlice,30,NYC\nBob,25,LA\nCharlie,35,Chicago\nEOF\n\necho \"Original:\"\ncat /tmp/data.csv\n\necho \"\\nNames only:\"\nawk -F',' 'NR>1 {print $1}' /tmp/data.csv\n\necho \"\\nSum of ages:\"\nawk -F',' 'NR>1 {sum+=$2} END {print sum}' /tmp/data.csv",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "bash-awk-001",
  "stdout": "Original:\nname,age,city\nAlice,30,NYC\nBob,25,LA\nCharlie,35,Chicago\n\nNames only:\nAlice\nBob\nCharlie\n\nSum of ages:\n90\n",
  "stderr": "",
  "exit_code": 0
}
```

### Sed Processing

```json title="Request"
{
  "trace_id": "bash-sed-001",
  "lang": "bash",
  "code": "# Create sample text\necho -e \"Hello World\\nHello Universe\\nGoodbye World\" > /tmp/text.txt\n\necho \"Original:\"\ncat /tmp/text.txt\n\necho \"\\nReplace 'World' with 'Earth':\"\nsed 's/World/Earth/g' /tmp/text.txt\n\necho \"\\nDelete lines with 'Goodbye':\"\nsed '/Goodbye/d' /tmp/text.txt\n\necho \"\\nAdd prefix to each line:\"\nsed 's/^/>> /' /tmp/text.txt",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "bash-sed-001",
  "stdout": "Original:\nHello World\nHello Universe\nGoodbye World\n\nReplace 'World' with 'Earth':\nHello Earth\nHello Universe\nGoodbye Earth\n\nDelete lines with 'Goodbye':\nHello World\nHello Universe\n\nAdd prefix to each line:\n>> Hello World\n>> Hello Universe\n>> Goodbye World\n",
  "stderr": "",
  "exit_code": 0
}
```

### JSON with jq

```json title="Request"
{
  "trace_id": "bash-jq-001",
  "lang": "bash",
  "code": "# Create JSON data\ncat << 'EOF' > /tmp/data.json\n{\n  \"users\": [\n    {\"name\": \"Alice\", \"age\": 30},\n    {\"name\": \"Bob\", \"age\": 25}\n  ]\n}\nEOF\n\n# Note: jq might not be available in minimal rootfs\n# This example shows the intent\necho \"JSON content:\"\ncat /tmp/data.json",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "bash-jq-001",
  "stdout": "JSON content:\n{\n  \"users\": [\n    {\"name\": \"Alice\", \"age\": 30},\n    {\"name\": \"Bob\", \"age\": 25}\n  ]\n}\n",
  "stderr": "",
  "exit_code": 0
}
```

### Complex Test: Comprehensive Bash

```json title="Request"
{
  "trace_id": "bash-complex-001",
  "lang": "bash",
  "code": "#!/bin/bash\necho '=== Bash Complex Test ==='\necho\n\n# Test 1: Fibonacci\necho '1. Fibonacci sequence:'\nfib() {\n    local n=$1\n    if [ $n -le 1 ]; then\n        echo $n\n    else\n        local a=$(fib $((n-1)))\n        local b=$(fib $((n-2)))\n        echo $((a + b))\n    fi\n}\necho -n '   First 10: '\nfor i in {0..9}; do\n    echo -n \"$(fib $i) \"\ndone\necho\n\n# Test 2: Array operations and sorting\necho\necho '2. Array sorting (bubble sort):'\narr=(64 34 25 12 22 11 90)\necho \"   Input:  ${arr[*]}\"\nn=${#arr[@]}\nfor ((i=0; i<n-1; i++)); do\n    for ((j=0; j<n-i-1; j++)); do\n        if [ ${arr[j]} -gt ${arr[$((j+1))]} ]; then\n            temp=${arr[j]}\n            arr[j]=${arr[$((j+1))]}\n            arr[$((j+1))]=$temp\n        fi\n    done\ndone\necho \"   Output: ${arr[*]}\"\n\n# Test 3: Associative arrays\necho\necho '3. Associative arrays:'\ndeclare -A user\nuser[name]='Alice'\nuser[age]=30\nuser[city]='NYC'\nfor key in \"${!user[@]}\"; do\n    echo \"   $key: ${user[$key]}\"\ndone\n\n# Test 4: Text processing\necho\necho '4. Text processing:'\ntext='Hello World from Bash Script'\necho \"   Original: $text\"\necho \"   Uppercase: ${text^^}\"\necho \"   Lowercase: ${text,,}\"\necho \"   Word count: $(echo $text | wc -w)\"\necho \"   Char count: ${#text}\"\n\n# Test 5: Arithmetic and math\necho\necho '5. Arithmetic operations:'\nnumbers=(1 2 3 4 5 6 7 8 9 10)\nsum=0\nfor n in \"${numbers[@]}\"; do\n    sum=$((sum + n))\ndone\necho \"   Numbers: ${numbers[*]}\"\necho \"   Sum: $sum\"\necho \"   Average: $((sum / ${#numbers[@]}))\"\necho \"   Max: $(printf '%s\\n' \"${numbers[@]}\" | sort -rn | head -1)\"\n\n# Test 6: Functions with return values\necho\necho '6. Functions:'\nfactorial() {\n    local n=$1\n    if [ $n -le 1 ]; then\n        echo 1\n    else\n        local prev=$(factorial $((n-1)))\n        echo $((n * prev))\n    fi\n}\necho \"   5! = $(factorial 5)\"\necho \"   7! = $(factorial 7)\"\n\n# Test 7: Pattern matching and regex\necho\necho '7. Pattern matching:'\nfiles=('script.sh' 'main.py' 'app.js' 'test.sh' 'data.json')\necho \"   All files: ${files[*]}\"\necho -n '   Shell scripts: '\nfor f in \"${files[@]}\"; do\n    [[ $f == *.sh ]] && echo -n \"$f \"\ndone\necho\n\n# Test 8: Here documents and data\necho\necho '8. Data processing:'\ncat << 'DATA' > /tmp/data.txt\nAlice,30,NYC\nBob,25,LA\nCharlie,35,Chicago\nDATA\necho '   CSV data:'\nwhile IFS=',' read -r name age city; do\n    echo \"   - $name ($age) from $city\"\ndone < /tmp/data.txt\n\necho\necho '=== All tests passed ==='",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "bash-complex-001",
  "stdout": "=== Bash Complex Test ===\n\n1. Fibonacci sequence:\n   First 10: 0 1 1 2 3 5 8 13 21 34 \n\n2. Array sorting (bubble sort):\n   Input:  64 34 25 12 22 11 90\n   Output: 11 12 22 25 34 64 90\n\n3. Associative arrays:\n   name: Alice\n   age: 30\n   city: NYC\n\n4. Text processing:\n   Original: Hello World from Bash Script\n   Uppercase: HELLO WORLD FROM BASH SCRIPT\n   Lowercase: hello world from bash script\n   Word count: 5\n   Char count: 28\n\n5. Arithmetic operations:\n   Numbers: 1 2 3 4 5 6 7 8 9 10\n   Sum: 55\n   Average: 5\n   Max: 10\n\n6. Functions:\n   5! = 120\n   7! = 5040\n\n7. Pattern matching:\n   All files: script.sh main.py app.js test.sh data.json\n   Shell scripts: script.sh test.sh \n\n8. Data processing:\n   CSV data:\n   - Alice (30) from NYC\n   - Bob (25) from LA\n   - Charlie (35) from Chicago\n\n=== All tests passed ===\n",
  "stderr": "",
  "exit_code": 0
}
```

## Available Commands

| Category | Commands |
|----------|----------|
| File | `ls`, `cat`, `cp`, `mv`, `rm`, `mkdir`, `touch`, `chmod` |
| Text | `echo`, `printf`, `grep`, `sed`, `awk`, `cut`, `sort`, `uniq` |
| Archive | `tar`, `gzip`, `gunzip` |
| Process | `ps`, `kill` |
| System | `date`, `hostname`, `whoami`, `uname`, `uptime` |
| Math | `expr`, `bc` (if installed) |
| Utilities | `head`, `tail`, `wc`, `tr`, `tee`, `xargs` |

:::note

  Network utilities (`curl`, `wget`, `ping`, etc.) are not available as there's no network access.

:::

## Error Examples

### Syntax Error

```json title="Request"
{
  "trace_id": "bash-err-syntax",
  "lang": "bash",
  "code": "if [ $x -gt 5 ]\nthen\necho \"test\"",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "bash-err-syntax",
  "stdout": "",
  "stderr": "/tmp/job-bash-err-syntax/script.sh: line 4: syntax error: unexpected end of file\n",
  "exit_code": 2
}
```

### Command Not Found

```json title="Request"
{
  "trace_id": "bash-err-cmd",
  "lang": "bash",
  "code": "curl https://example.com",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "bash-err-cmd",
  "stdout": "",
  "stderr": "/tmp/job-bash-err-cmd/script.sh: line 1: curl: command not found\n",
  "exit_code": 127
}
```

### File Not Found

```json title="Request"
{
  "trace_id": "bash-err-file",
  "lang": "bash",
  "code": "cat /nonexistent/file.txt",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "bash-err-file",
  "stdout": "",
  "stderr": "cat: /nonexistent/file.txt: No such file or directory\n",
  "exit_code": 1
}
```

## Performance

| Operation | Time |
|-----------|------|
| Echo | ~10ms |
| File operations | ~20ms |
| Grep (100 lines) | ~30ms |
| Awk processing | ~50ms |
| Loop (1000 iterations) | ~500ms |

:::tip

  Bash is the fastest to start since there's no compilation, but computationally intensive operations are slower than compiled languages.

:::

## Limitations

:::warning

  The Bash environment has the following limitations:

:::

1. **No network**: `curl`, `wget`, `ping` not available
2. **Limited utilities**: Only basic GNU coreutils
3. **No package manager**: `apt-get` not functional without network
4. **Memory limit**: 512 MiB default
5. **Timeout**: Configurable, default 10 seconds

## Exit Codes

| Code | Meaning |
|------|---------|
| 0 | Success |
| 1 | General error |
| 2 | Syntax error |
| 126 | Permission denied |
| 127 | Command not found |
| 128+N | Fatal error signal N |

## Best Practices

**Always quote variables: `"$var"` instead of `$var` to prevent word splitting.:**

Prefer `[[ ]]` over `[ ]` for safer string comparisons and regex support.

**Use `command -v cmd` to check if a command exists before using it:**

Add `set -e` at the start to exit on first error.
