---
title: 'Lisp (SBCL)'
description: 'Common Lisp with Steel Bank Common Lisp'
---

## Overview

Common Lisp is one of the oldest high-level programming languages. LLM-Firecracker provides SBCL (Steel Bank Common Lisp).

## Specifications

| Property | Value |
|----------|-------|
| Docker Image | `clfoundation/sbcl:latest` |
| Implementation | SBCL 2.2.4 |
| Rootfs Size | 1500 MB |
| Execution | Compiled/Interpreted |
| File Extension | `.lisp` |
| Run Command | `sbcl --script {file}` |
| Execution Time | ~35ms |

```bash title="1. Create Rootfs with infra.operator"
sudo infra.operator rootfs from-docker --name lisp --image clfoundation/sbcl:latest --size 1500
```

```bash title="2. Create Snapshot"
sudo infra.operator snapshot create --lang lisp --mem 512 --vcpus 1
```

```bash title="3. Upload rootfs to S3"
sudo infra.operator rootfs upload --lang lisp --bucket llm-firecracker
```

```bash title="3. Upload snapshot to S3"
sudo infra.operator snapshot upload --lang lisp --bucket llm-firecracker
```

```bash title="4. Test Execution"
sudo infra.operator host --lang lisp --code "(format t \"Hello from Common Lisp!~%\")" --mem 512 --vcpus 1 --snapshot
```



## Examples

### Hello World

```json title="Request"
{
  "trace_id": "lisp-hello-001",
  "lang": "lisp",
  "code": "(format t \"Hello from Common Lisp!~%\")",
  "timeout": 15
}
```

```json title="Response"
{
  "trace_id": "lisp-hello-001",
  "stdout": "Hello from Common Lisp!\n",
  "stderr": "",
  "exit_code": 0
}
```

### Complex Test: Symbolic Computation

```json title="Request"
{
  "trace_id": "lisp-complex-001",
  "lang": "lisp",
  "code": "(format t \"=== Common Lisp Complex Test ===~%~%\")\n\n;; Test 1: Fibonacci\n(format t \"1. Fibonacci sequence:~%\")\n(defun fib (n)\n  (if (<= n 1)\n      n\n      (+ (fib (- n 1)) (fib (- n 2)))))\n\n(format t \"   First 15: ~{~a~^ ~}~%\" (loop for i from 0 to 14 collect (fib i)))\n(format t \"   Fib(25) = ~a~%\" (fib 25))\n\n;; Test 2: QuickSort\n(format t \"~%2. QuickSort:~%\")\n(defun quicksort (list)\n  (if (null list)\n      nil\n      (let ((pivot (car list))\n            (rest (cdr list)))\n        (append (quicksort (remove-if-not (lambda (x) (< x pivot)) rest))\n                (list pivot)\n                (quicksort (remove-if-not (lambda (x) (>= x pivot)) rest))))))\n\n(let ((unsorted '(64 34 25 12 22 11 90)))\n  (format t \"   Input:  ~a~%\" unsorted)\n  (format t \"   Output: ~a~%\" (quicksort unsorted)))\n\n;; Test 3: Higher-order functions\n(format t \"~%3. Higher-order functions:~%\")\n(let ((numbers '(1 2 3 4 5 6 7 8 9 10)))\n  (format t \"   Squares: ~a~%\" (mapcar (lambda (x) (* x x)) numbers))\n  (format t \"   Evens: ~a~%\" (remove-if-not #'evenp numbers))\n  (format t \"   Sum: ~a~%\" (reduce #'+ numbers))\n  (format t \"   Product: ~a~%\" (reduce #'* numbers)))\n\n;; Test 4: Symbolic computation\n(format t \"~%4. Symbolic computation:~%\")\n(defun derivative (expr var)\n  (cond\n    ((numberp expr) 0)\n    ((eq expr var) 1)\n    ((eq (car expr) '+)\n     (list '+ (derivative (cadr expr) var) (derivative (caddr expr) var)))\n    ((eq (car expr) '*)\n     (list '+ \n           (list '* (cadr expr) (derivative (caddr expr) var))\n           (list '* (derivative (cadr expr) var) (caddr expr))))\n    (t expr)))\n\n(let ((expr '(+ (* x x) (* 3 x))))\n  (format t \"   Expression: ~a~%\" expr)\n  (format t \"   Derivative: ~a~%\" (derivative expr 'x)))\n\n;; Test 5: List manipulation\n(format t \"~%5. List manipulation:~%\")\n(defun my-reverse (list)\n  (if (null list)\n      nil\n      (append (my-reverse (cdr list)) (list (car list)))))\n\n(defun my-flatten (list)\n  (cond\n    ((null list) nil)\n    ((atom list) (list list))\n    (t (append (my-flatten (car list)) (my-flatten (cdr list))))))\n\n(format t \"   Reverse (1 2 3 4 5): ~a~%\" (my-reverse '(1 2 3 4 5)))\n(format t \"   Flatten ((1 2) (3 (4 5))): ~a~%\" (my-flatten '((1 2) (3 (4 5)))))\n\n;; Test 6: Closures\n(format t \"~%6. Closures:~%\")\n(defun make-counter (start)\n  (let ((count start))\n    (lambda ()\n      (incf count))))\n\n(let ((counter (make-counter 0)))\n  (format t \"   Counter: ~a ~a ~a ~a ~a~%\" \n          (funcall counter)\n          (funcall counter)\n          (funcall counter)\n          (funcall counter)\n          (funcall counter)))\n\n;; Test 7: Association lists\n(format t \"~%7. Association lists:~%\")\n(let ((people '((alice . 30) (bob . 25) (charlie . 35))))\n  (format t \"   People: ~a~%\" people)\n  (format t \"   Alice's age: ~a~%\" (cdr (assoc 'alice people)))\n  (format t \"   All names: ~a~%\" (mapcar #'car people)))\n\n(format t \"~%=== All tests passed ===~%\")",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "lisp-complex-001",
  "stdout": "=== Common Lisp Complex Test ===\n\n1. Fibonacci sequence:\n   First 15: 0 1 1 2 3 5 8 13 21 34 55 89 144 233 377\n   Fib(25) = 75025\n\n2. QuickSort:\n   Input:  (64 34 25 12 22 11 90)\n   Output: (11 12 22 25 34 64 90)\n\n3. Higher-order functions:\n   Squares: (1 4 9 16 25 36 49 64 81 100)\n   Evens: (2 4 6 8 10)\n   Sum: 55\n   Product: 3628800\n\n4. Symbolic computation:\n   Expression: (+ (* X X) (* 3 X))\n   Derivative: (+ (+ (* X 1) (* 1 X)) (+ (* 3 1) (* 0 X)))\n\n5. List manipulation:\n   Reverse (1 2 3 4 5): (5 4 3 2 1)\n   Flatten ((1 2) (3 (4 5))): (1 2 3 4 5)\n\n6. Closures:\n   Counter: 1 2 3 4 5\n\n7. Association lists:\n   People: ((ALICE . 30) (BOB . 25) (CHARLIE . 35))\n   Alice's age: 30\n   All names: (ALICE BOB CHARLIE)\n\n=== All tests passed ===\n",
  "stderr": "",
  "exit_code": 0
}
```

## Limitations

:::warning

  - SBCL only
  - No Quicklisp packages
  - Memory limit: 512 MiB

:::
