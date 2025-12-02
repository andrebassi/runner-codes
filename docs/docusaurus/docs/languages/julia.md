---
title: 'Julia'
description: 'Julia high-performance scientific computing'
---

## Overview

Julia is a high-performance language for technical computing. LLM-Firecracker provides Julia for scientific programming.

## Specifications

| Property | Value |
|----------|-------|
| Docker Image | `julia:1.11-alpine` |
| Version | Julia 1.11.6 |
| Rootfs Size | 1200 MB |
| Execution | JIT compiled |
| File Extension | `.jl` |
| Run Command | `julia {file}` |
| Execution Time | ~3.0s |

```bash title="1. Create Rootfs from Docker"
sudo infra.operator rootfs from-docker --name julia --image julia:1.11-alpine --size 1200
```

```bash title="2. Create Snapshot"
sudo infra.operator snapshot create --lang julia --mem 512 --vcpus 1
```

```bash title="3. Upload rootfs to S3"
sudo infra.operator rootfs upload --lang julia --bucket llm-firecracker
```

```bash title="3. Upload snapshot to S3"
sudo infra.operator snapshot upload --lang julia --bucket llm-firecracker
```

```bash title="4. Test Execution"
sudo infra.operator host --lang julia --code 'println("Hello from Julia $(VERSION)")' --mem 512 --vcpus 1 --snapshot
```



## Examples

### Hello World

```json title="Request"
{
  "trace_id": "jl-hello-001",
  "lang": "julia",
  "code": "println(\"Hello from Julia!\")",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "jl-hello-001",
  "stdout": "Hello from Julia!\n",
  "stderr": "",
  "exit_code": 0
}
```

### Complex Test: Scientific Computing

```json title="Request"
{
  "trace_id": "jl-complex-001",
  "lang": "julia",
  "code": "println(\"=== Julia Complex Test ===\")\nprintln()\n\n# Test 1: Fibonacci\nprintln(\"1. Fibonacci sequence:\")\nfunction fib(n)\n    n <= 1 ? n : fib(n-1) + fib(n-2)\nend\nfibs = [fib(i) for i in 0:14]\nprintln(\"   First 15: $fibs\")\nprintln(\"   Fib(25) = $(fib(25))\")\n\n# Test 2: QuickSort\nprintln(\"\\n2. QuickSort:\")\nfunction quicksort(arr)\n    length(arr) <= 1 && return arr\n    pivot = arr[1]\n    smaller = filter(x -> x < pivot, arr[2:end])\n    larger = filter(x -> x >= pivot, arr[2:end])\n    vcat(quicksort(smaller), [pivot], quicksort(larger))\nend\nunsorted = [64, 34, 25, 12, 22, 11, 90]\nprintln(\"   Input:  $unsorted\")\nprintln(\"   Output: $(quicksort(unsorted))\")\n\n# Test 3: Statistical analysis\nprintln(\"\\n3. Statistical analysis:\")\nusing Statistics\nnumbers = 1.0:10.0 |> collect\nprintln(\"   Numbers: $numbers\")\nprintln(\"   Sum: $(sum(numbers))\")\nprintln(\"   Mean: $(mean(numbers))\")\nprintln(\"   Std: $(std(numbers))\")\nprintln(\"   Median: $(median(numbers))\")\nprintln(\"   Variance: $(var(numbers))\")\n\n# Test 4: Matrix operations\nprintln(\"\\n4. Matrix operations:\")\nA = [1 2 3; 4 5 6; 7 8 9]\nprintln(\"   Matrix A:\")\nfor row in eachrow(A)\n    println(\"   $row\")\nend\nprintln(\"   Sum: $(sum(A))\")\nprintln(\"   Trace: $(sum(diag(A)))\")\nprintln(\"   Transpose diagonal: $(diag(A'))\")\n\n# Test 5: Broadcasting and comprehensions\nprintln(\"\\n5. Broadcasting and comprehensions:\")\nnums = 1:10\nsquares = nums .^ 2\nevens = filter(iseven, nums)\nprintln(\"   Squares: $squares\")\nprintln(\"   Evens: $evens\")\nprintln(\"   Sum of squares: $(sum(squares))\")\n\n# Test 6: Multiple dispatch\nprintln(\"\\n6. Multiple dispatch:\")\nstruct Point\n    x::Float64\n    y::Float64\nend\n\ndistance(p1::Point, p2::Point) = sqrt((p2.x - p1.x)^2 + (p2.y - p1.y)^2)\ndistance(p::Point) = sqrt(p.x^2 + p.y^2)  # distance from origin\n\np1 = Point(0.0, 0.0)\np2 = Point(3.0, 4.0)\nprintln(\"   Distance from origin to (3,4): $(distance(p2))\")\nprintln(\"   Distance between points: $(distance(p1, p2))\")\n\n# Test 7: Functional programming\nprintln(\"\\n7. Functional programming:\")\ndata = [(\"Alice\", 30), (\"Bob\", 25), (\"Charlie\", 35)]\nprintln(\"   People: $data\")\nnames = map(x -> x[1], data)\nages = map(x -> x[2], data)\nprintln(\"   Names: $names\")\nprintln(\"   Average age: $(mean(ages))\")\nprintln(\"   Oldest: $(data[argmax(ages)])\")\n\n# Test 8: Prime sieve\nprintln(\"\\n8. Prime sieve (Eratosthenes):\")\nfunction primes_up_to(n)\n    sieve = trues(n)\n    sieve[1] = false\n    for i in 2:isqrt(n)\n        if sieve[i]\n            sieve[i^2:i:n] .= false\n        end\n    end\n    findall(sieve)\nend\nprintln(\"   Primes up to 50: $(primes_up_to(50))\")\n\nprintln(\"\\n=== All tests passed ===\")",
  "timeout": 45
}
```

```json title="Response"
{
  "trace_id": "jl-complex-001",
  "stdout": "=== Julia Complex Test ===\n\n1. Fibonacci sequence:\n   First 15: [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]\n   Fib(25) = 75025\n\n2. QuickSort:\n   Input:  [64, 34, 25, 12, 22, 11, 90]\n   Output: [11, 12, 22, 25, 34, 64, 90]\n\n3. Statistical analysis:\n   Numbers: [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0]\n   Sum: 55.0\n   Mean: 5.5\n   Std: 3.0276503540974917\n   Median: 5.5\n   Variance: 9.166666666666666\n\n4. Matrix operations:\n   Matrix A:\n   [1, 2, 3]\n   [4, 5, 6]\n   [7, 8, 9]\n   Sum: 45\n   Trace: 15\n   Transpose diagonal: [1, 5, 9]\n\n5. Broadcasting and comprehensions:\n   Squares: [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]\n   Evens: [2, 4, 6, 8, 10]\n   Sum of squares: 385\n\n6. Multiple dispatch:\n   Distance from origin to (3,4): 5.0\n   Distance between points: 5.0\n\n7. Functional programming:\n   People: [(\"Alice\", 30), (\"Bob\", 25), (\"Charlie\", 35)]\n   Names: [\"Alice\", \"Bob\", \"Charlie\"]\n   Average age: 30.0\n   Oldest: (\"Charlie\", 35)\n\n8. Prime sieve (Eratosthenes):\n   Primes up to 50: [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47]\n\n=== All tests passed ===\n",
  "stderr": "",
  "exit_code": 0
}
```

## Limitations

:::warning

  - Standard library only
  - No package manager (Pkg)
  - JIT compilation overhead on first run
  - Memory limit: 512 MiB

:::
