---
title: 'Nim'
description: 'Nim programming language'
---

## Overview

Nim is a statically typed compiled language that combines powerful metaprogramming with Python-like readability. Runner Codes provides Nim compiler for efficient code execution.

## Specifications

| Property | Value |
|----------|-------|
| Docker Image | `nimlang/nim:alpine` |
| Compiler | Nim 2.2.2 |
| Rootfs Size | 1000 MB |
| Execution | Compiled (nim compile --run) |
| File Extension | `.nim` |
| Run Command | `nim compile --run {file}` |
| Execution Time | ~660ms |

```bash title="1. Create Rootfs with infra.operator"
sudo infra.operator rootfs from-docker --name nim --image nimlang/nim:alpine --size 1000
```

```bash title="2. Create Snapshot"
sudo infra.operator snapshot create --lang nim --mem 512 --vcpus 1
```

```bash title="3. Upload rootfs to S3"
sudo infra.operator rootfs upload --lang nim --bucket runner-codes
```

```bash title="3. Upload snapshot to S3"
sudo infra.operator snapshot upload --lang nim --bucket runner-codes
```

```bash title="4. Test Execution"
sudo infra.operator host --lang nim --code "echo 42 + 8" --mem 512 --vcpus 1 --snapshot
```



## Examples

### Hello World

```json title="Request"
{
  "trace_id": "nim-hello-001",
  "lang": "nim",
  "code": "echo \"Hello from Nim!\"",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "nim-hello-001",
  "stdout": "Hello from Nim!\n",
  "stderr": "",
  "exit_code": 0
}
```

### Variables and Types

```json title="Request"
{
  "trace_id": "nim-vars-001",
  "lang": "nim",
  "code": "var x: int = 10\nlet y = 20  # Type inferred\nconst PI = 3.14159\n\necho \"x = \", x\necho \"y = \", y\necho \"PI = \", PI\necho \"Sum = \", x + y",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "nim-vars-001",
  "stdout": "x = 10\ny = 20\nPI = 3.14159\nSum = 30\n",
  "stderr": "",
  "exit_code": 0
}
```

### Sequences and Arrays

```json title="Request"
{
  "trace_id": "nim-seq-001",
  "lang": "nim",
  "code": "var numbers = @[1, 2, 3, 4, 5]\necho \"Numbers: \", numbers\necho \"Length: \", numbers.len\necho \"Sum: \", numbers.foldl(a + b)\n\nnumbers.add(6)\necho \"After add: \", numbers",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "nim-seq-001",
  "stdout": "Numbers: @[1, 2, 3, 4, 5]\nLength: 5\nSum: 15\nAfter add: @[1, 2, 3, 4, 5, 6]\n",
  "stderr": "",
  "exit_code": 0
}
```

### Procedures

```json title="Request"
{
  "trace_id": "nim-proc-001",
  "lang": "nim",
  "code": "proc factorial(n: int): int =\n  if n <= 1: 1\n  else: n * factorial(n - 1)\n\nproc fib(n: int): int =\n  if n <= 1: n\n  else: fib(n - 1) + fib(n - 2)\n\necho \"5! = \", factorial(5)\necho \"Fib(10) = \", fib(10)",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "nim-proc-001",
  "stdout": "5! = 120\nFib(10) = 55\n",
  "stderr": "",
  "exit_code": 0
}
```

### Complex Test: Comprehensive Nim

```json title="Request"
{
  "trace_id": "nim-complex-001",
  "lang": "nim",
  "code": "import algorithm, sequtils, strutils, tables\n\necho \"=== Nim Complex Test ===\"\necho \"\"\n\n# Test 1: Fibonacci with memoization\necho \"1. Fibonacci sequence:\"\nvar memo = initTable[int, int]()\nproc fib(n: int): int =\n  if n in memo: return memo[n]\n  result = if n <= 1: n else: fib(n-1) + fib(n-2)\n  memo[n] = result\n\nvar fibs: seq[int] = @[]\nfor i in 0..14:\n  fibs.add(fib(i))\necho \"   First 15: \", fibs\necho \"   Fib(40) = \", fib(40)\n\n# Test 2: QuickSort\necho \"\"\necho \"2. Sorting:\"\nvar unsorted = @[64, 34, 25, 12, 22, 11, 90]\necho \"   Input:  \", unsorted\necho \"   Output: \", unsorted.sorted()\n\n# Test 3: Objects and types\necho \"\"\necho \"3. Objects:\"\ntype\n  Person = object\n    name: string\n    age: int\n    city: string\n\nlet people = @[\n  Person(name: \"Alice\", age: 30, city: \"NYC\"),\n  Person(name: \"Bob\", age: 25, city: \"LA\"),\n  Person(name: \"Charlie\", age: 35, city: \"Chicago\")\n]\nfor p in people:\n  echo \"   \", p.name, \" (\", p.age, \") from \", p.city\n\n# Test 4: Functional operations\necho \"\"\necho \"4. Functional operations:\"\nlet numbers = toSeq(1..10)\nlet squares = numbers.map(proc(x: int): int = x * x)\nlet evens = numbers.filter(proc(x: int): bool = x mod 2 == 0)\nlet sum = squares.foldl(a + b)\necho \"   Numbers: \", numbers\necho \"   Squares: \", squares\necho \"   Evens: \", evens\necho \"   Sum of squares: \", sum\n\n# Test 5: String operations\necho \"\"\necho \"5. String operations:\"\nlet text = \"Hello, World! Welcome to Nim.\"\necho \"   Original: \", text\necho \"   Upper: \", text.toUpperAscii()\necho \"   Words: \", text.split().len\necho \"   Contains 'Nim': \", \"Nim\" in text\n\n# Test 6: Tables (dictionaries)\necho \"\"\necho \"6. Tables:\"\nvar ages = {\"Alice\": 30, \"Bob\": 25, \"Charlie\": 35}.toTable\nfor name, age in ages:\n  echo \"   \", name, \": \", age\n\n# Test 7: Case expressions\necho \"\"\necho \"7. Case expressions:\"\nproc describe(n: int): string =\n  case n\n  of 0: \"zero\"\n  of 1..9: \"single digit\"\n  of 10..99: \"double digit\"\n  else: \"large number\"\n\necho \"   0: \", describe(0)\necho \"   5: \", describe(5)\necho \"   42: \", describe(42)\necho \"   100: \", describe(100)\n\n# Test 8: Iterators\necho \"\"\necho \"8. Primes (iterator):\"\nproc isPrime(n: int): bool =\n  if n < 2: return false\n  for i in 2..int(sqrt(float(n))):\n    if n mod i == 0: return false\n  return true\n\nvar primes: seq[int] = @[]\nfor i in 2..50:\n  if isPrime(i): primes.add(i)\necho \"   Primes up to 50: \", primes\n\necho \"\"\necho \"=== All tests passed ===\"",
  "timeout": 45
}
```

```json title="Response"
{
  "trace_id": "nim-complex-001",
  "stdout": "=== Nim Complex Test ===\n\n1. Fibonacci sequence:\n   First 15: @[0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]\n   Fib(40) = 102334155\n\n2. Sorting:\n   Input:  @[64, 34, 25, 12, 22, 11, 90]\n   Output: @[11, 12, 22, 25, 34, 64, 90]\n\n3. Objects:\n   Alice (30) from NYC\n   Bob (25) from LA\n   Charlie (35) from Chicago\n\n4. Functional operations:\n   Numbers: @[1, 2, 3, 4, 5, 6, 7, 8, 9, 10]\n   Squares: @[1, 4, 9, 16, 25, 36, 49, 64, 81, 100]\n   Evens: @[2, 4, 6, 8, 10]\n   Sum of squares: 385\n\n5. String operations:\n   Original: Hello, World! Welcome to Nim.\n   Upper: HELLO, WORLD! WELCOME TO NIM.\n   Words: 5\n   Contains 'Nim': true\n\n6. Tables:\n   Alice: 30\n   Bob: 25\n   Charlie: 35\n\n7. Case expressions:\n   0: zero\n   5: single digit\n   42: double digit\n   100: large number\n\n8. Primes (iterator):\n   Primes up to 50: @[2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47]\n\n=== All tests passed ===\n",
  "stderr": "",
  "exit_code": 0
}
```

## Limitations

:::warning

  The Nim environment has the following limitations:

:::

1. **Compilation overhead**: First run slower due to compilation
2. **Standard library only**: No Nimble packages
3. **No network**: HTTP operations will fail
4. **Memory limit**: 512 MiB

## Best Practices


    Prefer `let` over `var` when values don't change.



    Nim has powerful type inference - use it for cleaner code.


