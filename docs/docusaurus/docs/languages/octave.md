---
title: 'Octave'
description: 'GNU Octave numerical computing'
---

## Overview

GNU Octave is a high-level language for numerical computations, largely compatible with MATLAB. LLM-Firecracker provides Octave for mathematical programming.

## Specifications

| Property | Value |
|----------|-------|
| Base OS | Debian Bookworm |
| Version | GNU Octave 8.x |
| Rootfs Size | 1200 MB |
| Execution | Interpreted |
| File Extension | `.m` |
| Run Command | `octave --no-gui --quiet {file}` |
| Execution Time | ~157ms |

```bash title="1. Create Rootfs with infra.operator"
sudo infra.operator rootfs create --name octave --size 1200 --base debian --packages "octave"
```

```bash title="2. Create Snapshot"
sudo infra.operator snapshot create --lang octave --mem 512 --vcpus 1
```

```bash title="3. Upload rootfs to S3"
sudo infra.operator rootfs upload --lang octave --bucket llm-firecracker
```

```bash title="3. Upload snapshot to S3"
sudo infra.operator snapshot upload --lang octave --bucket llm-firecracker
```

```bash title="4. Test Execution"
sudo infra.operator host --lang octave --code "disp('Hello from GNU Octave!'); disp(2+2);" --mem 512 --vcpus 1 --snapshot
```



## Examples

### Hello World

```json title="Request"
{
  "trace_id": "oct-hello-001",
  "lang": "octave",
  "code": "disp('Hello from Octave!')",
  "timeout": 20
}
```

```json title="Response"
{
  "trace_id": "oct-hello-001",
  "stdout": "Hello from Octave!\n",
  "stderr": "",
  "exit_code": 0
}
```

### Complex Test: Numerical Computing

```json title="Request"
{
  "trace_id": "oct-complex-001",
  "lang": "octave",
  "code": "disp('=== Octave Complex Test ===')\ndisp('')\n\n% Test 1: Fibonacci\ndisp('1. Fibonacci sequence:')\nfunction f = fib(n)\n  if n <= 1\n    f = n;\n  else\n    f = fib(n-1) + fib(n-2);\n  endif\nendfunction\n\nfibs = arrayfun(@fib, 0:14);\ndisp(['   First 15: ' num2str(fibs)])\ndisp(['   Fib(25) = ' num2str(fib(25))])\n\n% Test 2: Matrix operations\ndisp('')\ndisp('2. Matrix operations:')\nA = [1 2 3; 4 5 6; 7 8 9];\ndisp('   Matrix A:')\ndisp(A)\ndisp(['   Sum: ' num2str(sum(A(:)))])\ndisp(['   Trace: ' num2str(trace(A))])\ndisp(['   Determinant: ' num2str(det(A))])\n\nB = A';\ndisp('   Transpose:')\ndisp(B)\n\n% Test 3: Statistical analysis\ndisp('')\ndisp('3. Statistical analysis:')\nnumbers = 1:10;\ndisp(['   Numbers: ' num2str(numbers)])\ndisp(['   Sum: ' num2str(sum(numbers))])\ndisp(['   Mean: ' num2str(mean(numbers))])\ndisp(['   Std Dev: ' num2str(std(numbers))])\ndisp(['   Median: ' num2str(median(numbers))])\ndisp(['   Variance: ' num2str(var(numbers))])\n\n% Test 4: Vector operations\ndisp('')\ndisp('4. Vector operations:')\nv1 = [1 2 3 4 5];\nv2 = [5 4 3 2 1];\ndisp(['   v1: ' num2str(v1)])\ndisp(['   v2: ' num2str(v2)])\ndisp(['   Dot product: ' num2str(dot(v1, v2))])\ndisp(['   Element-wise product: ' num2str(v1 .* v2)])\ndisp(['   Norm of v1: ' num2str(norm(v1))])\n\n% Test 5: Linear algebra\ndisp('')\ndisp('5. Linear algebra:')\nA = [3 1; 1 2];\nb = [9; 8];\nx = A \\ b;\ndisp('   Solving Ax = b:')\ndisp(['   A = [3 1; 1 2], b = [9; 8]'])\ndisp(['   Solution x: ' num2str(x')])\ndisp(['   Verification Ax: ' num2str((A*x)')])\n\n% Test 6: Polynomial operations\ndisp('')\ndisp('6. Polynomial operations:')\np = [1 -6 11 -6];  % x^3 - 6x^2 + 11x - 6\ndisp(['   Polynomial: x^3 - 6x^2 + 11x - 6'])\nroots_p = roots(p);\ndisp(['   Roots: ' num2str(roots_p')])\ndisp(['   Value at x=2: ' num2str(polyval(p, 2))])\n\n% Test 7: Sorting\ndisp('')\ndisp('7. Sorting:')\nunsorted = [64 34 25 12 22 11 90];\ndisp(['   Input:  ' num2str(unsorted)])\ndisp(['   Sorted: ' num2str(sort(unsorted))])\ndisp(['   Descending: ' num2str(sort(unsorted, 'descend'))])\n\n% Test 8: Function handles and anonymous functions\ndisp('')\ndisp('8. Anonymous functions:')\nsquare = @(x) x.^2;\ncube = @(x) x.^3;\nnums = 1:5;\ndisp(['   Numbers: ' num2str(nums)])\ndisp(['   Squares: ' num2str(square(nums))])\ndisp(['   Cubes: ' num2str(cube(nums))])\n\ndisp('')\ndisp('=== All tests passed ===')",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "oct-complex-001",
  "stdout": "=== Octave Complex Test ===\n\n1. Fibonacci sequence:\n   First 15: 0 1 1 2 3 5 8 13 21 34 55 89 144 233 377\n   Fib(25) = 75025\n\n2. Matrix operations:\n   Matrix A:\n   1   2   3\n   4   5   6\n   7   8   9\n   Sum: 45\n   Trace: 15\n   Determinant: 0\n   Transpose:\n   1   4   7\n   2   5   8\n   3   6   9\n\n3. Statistical analysis:\n   Numbers: 1 2 3 4 5 6 7 8 9 10\n   Sum: 55\n   Mean: 5.5\n   Std Dev: 3.0277\n   Median: 5.5\n   Variance: 9.1667\n\n4. Vector operations:\n   v1: 1 2 3 4 5\n   v2: 5 4 3 2 1\n   Dot product: 35\n   Element-wise product: 5 8 9 8 5\n   Norm of v1: 7.4162\n\n5. Linear algebra:\n   Solving Ax = b:\n   A = [3 1; 1 2], b = [9; 8]\n   Solution x: 2 3\n   Verification Ax: 9 8\n\n6. Polynomial operations:\n   Polynomial: x^3 - 6x^2 + 11x - 6\n   Roots: 3 2 1\n   Value at x=2: 0\n\n7. Sorting:\n   Input:  64 34 25 12 22 11 90\n   Sorted: 11 12 22 25 34 64 90\n   Descending: 90 64 34 25 22 12 11\n\n8. Anonymous functions:\n   Numbers: 1 2 3 4 5\n   Squares: 1 4 9 16 25\n   Cubes: 1 8 27 64 125\n\n=== All tests passed ===\n",
  "stderr": "",
  "exit_code": 0
}
```

## Limitations

:::warning

  - No Octave Forge packages
  - No graphical output
  - Memory limit: 512 MiB
  - MATLAB compatibility may vary

:::
