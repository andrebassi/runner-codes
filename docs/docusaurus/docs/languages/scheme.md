---
title: 'Scheme'
description: 'Scheme programming with Guile'
---

## Overview

Scheme is a minimalist dialect of Lisp. LLM-Firecracker provides GNU Guile Scheme interpreter.

## Specifications

| Property | Value |
|----------|-------|
| Docker Image | `debian:bookworm-slim` + guile-3.0 |
| Implementation | GNU Guile 3.0.8 |
| Rootfs Size | 300 MB |
| Execution | Interpreted |
| File Extension | `.scm` |
| Run Command | `guile {file}` |
| Execution Time | ~84ms |

```bash title="Build: FROM debian:bookworm-slim + apt-get install guile-3.0"
sudo infra.operator rootfs from-docker --name scheme --image scheme:custom --size 300
```

```bash title="2. Create Snapshot"
sudo infra.operator snapshot create --lang scheme --mem 512 --vcpus 1
```

```bash title="3. Upload rootfs to S3"
sudo infra.operator rootfs upload --lang scheme --bucket llm-firecracker
```

```bash title="3. Upload snapshot to S3"
sudo infra.operator snapshot upload --lang scheme --bucket llm-firecracker
```

```bash title="4. Test Execution"
sudo infra.operator host --lang scheme --code '(display "Hello from Guile Scheme ")(display (version))(newline)' --mem 512 --vcpus 1 --snapshot
```



## Examples

### Hello World

```json title="Request"
{
  "trace_id": "scm-hello-001",
  "lang": "scheme",
  "code": "(display \"Hello from Scheme!\")\n(newline)",
  "timeout": 15
}
```

```json title="Response"
{
  "trace_id": "scm-hello-001",
  "stdout": "Hello from Scheme!\n",
  "stderr": "",
  "exit_code": 0
}
```

### Complex Test: Functional Programming

```json title="Request"
{
  "trace_id": "scm-complex-001",
  "lang": "scheme",
  "code": "(display \"=== Scheme Complex Test ===\")\n(newline)(newline)\n\n;; Test 1: Fibonacci\n(display \"1. Fibonacci sequence:\")\n(newline)\n(define (fib n)\n  (if (<= n 1)\n      n\n      (+ (fib (- n 1)) (fib (- n 2)))))\n\n(define (fib-list n)\n  (map fib (iota n)))\n\n(display \"   First 15: \")\n(display (fib-list 15))\n(newline)\n(display \"   Fib(25) = \")\n(display (fib 25))\n(newline)\n\n;; Test 2: QuickSort\n(newline)\n(display \"2. QuickSort:\")\n(newline)\n(define (quicksort lst)\n  (if (null? lst)\n      '()\n      (let ((pivot (car lst))\n            (rest (cdr lst)))\n        (append (quicksort (filter (lambda (x) (< x pivot)) rest))\n                (list pivot)\n                (quicksort (filter (lambda (x) (>= x pivot)) rest))))))\n\n(define unsorted '(64 34 25 12 22 11 90))\n(display \"   Input:  \")\n(display unsorted)\n(newline)\n(display \"   Output: \")\n(display (quicksort unsorted))\n(newline)\n\n;; Test 3: Higher-order functions\n(newline)\n(display \"3. Higher-order functions:\")\n(newline)\n(define numbers '(1 2 3 4 5 6 7 8 9 10))\n\n(display \"   Squares: \")\n(display (map (lambda (x) (* x x)) numbers))\n(newline)\n\n(display \"   Evens: \")\n(display (filter even? numbers))\n(newline)\n\n(display \"   Sum: \")\n(display (fold + 0 numbers))\n(newline)\n\n(display \"   Product: \")\n(display (fold * 1 numbers))\n(newline)\n\n;; Test 4: List operations\n(newline)\n(display \"4. List operations:\")\n(newline)\n\n(define (my-reverse lst)\n  (if (null? lst)\n      '()\n      (append (my-reverse (cdr lst)) (list (car lst)))))\n\n(define (my-length lst)\n  (if (null? lst)\n      0\n      (+ 1 (my-length (cdr lst)))))\n\n(display \"   Reverse (1 2 3 4 5): \")\n(display (my-reverse '(1 2 3 4 5)))\n(newline)\n\n(display \"   Length (a b c d e): \")\n(display (my-length '(a b c d e)))\n(newline)\n\n;; Test 5: Closures and currying\n(newline)\n(display \"5. Closures and currying:\")\n(newline)\n\n(define (make-adder n)\n  (lambda (x) (+ x n)))\n\n(define add5 (make-adder 5))\n(define add10 (make-adder 10))\n\n(display \"   add5(3) = \")\n(display (add5 3))\n(newline)\n\n(display \"   add10(7) = \")\n(display (add10 7))\n(newline)\n\n;; Test 6: Y combinator (anonymous recursion)\n(newline)\n(display \"6. Y combinator factorial:\")\n(newline)\n\n(define Y\n  (lambda (f)\n    ((lambda (x) (f (lambda (y) ((x x) y))))\n     (lambda (x) (f (lambda (y) ((x x) y)))))))\n\n(define factorial\n  (Y (lambda (fact)\n       (lambda (n)\n         (if (<= n 1)\n             1\n             (* n (fact (- n 1))))))))\n\n(display \"   Factorial 10 = \")\n(display (factorial 10))\n(newline)\n\n(newline)\n(display \"=== All tests passed ===\")\n(newline)",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "scm-complex-001",
  "stdout": "=== Scheme Complex Test ===\n\n1. Fibonacci sequence:\n   First 15: (0 1 1 2 3 5 8 13 21 34 55 89 144 233 377)\n   Fib(25) = 75025\n\n2. QuickSort:\n   Input:  (64 34 25 12 22 11 90)\n   Output: (11 12 22 25 34 64 90)\n\n3. Higher-order functions:\n   Squares: (1 4 9 16 25 36 49 64 81 100)\n   Evens: (2 4 6 8 10)\n   Sum: 55\n   Product: 3628800\n\n4. List operations:\n   Reverse (1 2 3 4 5): (5 4 3 2 1)\n   Length (a b c d e): 5\n\n5. Closures and currying:\n   add5(3) = 8\n   add10(7) = 17\n\n6. Y combinator factorial:\n   Factorial 10 = 3628800\n\n=== All tests passed ===\n",
  "stderr": "",
  "exit_code": 0
}
```

## Limitations

:::warning

  - GNU Guile only
  - No external libraries
  - Memory limit: 512 MiB

:::
