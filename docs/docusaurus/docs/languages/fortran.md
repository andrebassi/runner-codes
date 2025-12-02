---
title: 'Fortran'
description: 'Fortran scientific computing with GFortran'
---

## Overview

Fortran is one of the oldest programming languages, designed for scientific computing. LLM-Firecracker provides GFortran.

## Specifications

| Property | Value |
|----------|-------|
| Docker Image | `gcc:latest` |
| Compiler | GFortran 15.2.0 |
| Rootfs Size | 2000 MB |
| Execution | Compiled (gfortran + run) |
| File Extension | `.f90` |
| Run Command | `gfortran -o runbin {file} && ./runbin` |
| Execution Time | ~91ms |

```bash title="1. Create Rootfs from Docker"
sudo infra.operator rootfs from-docker --name fortran --image gcc:latest --size 2000
```

```bash title="2. Create Snapshot"
sudo infra.operator snapshot create --lang fortran --mem 512 --vcpus 1
```

```bash title="3. Upload rootfs to S3"
sudo infra.operator rootfs upload --lang fortran --bucket llm-firecracker
```

```bash title="3. Upload snapshot to S3"
sudo infra.operator snapshot upload --lang fortran --bucket llm-firecracker
```

```bash title="4. Test Execution"
sudo infra.operator host --lang fortran --code 'program hello
  print *, "Hello from Fortran!"
end program hello' --mem 512 --vcpus 1 --snapshot
```



## Examples

### Hello World

```json title="Request"
{
  "trace_id": "f90-hello-001",
  "lang": "fortran",
  "code": "program hello\n  print *, 'Hello from Fortran!'\nend program hello",
  "timeout": 20
}
```

```json title="Response"
{
  "trace_id": "f90-hello-001",
  "stdout": " Hello from Fortran!\n",
  "stderr": "",
  "exit_code": 0
}
```

### Complex Test: Scientific Computing

```json title="Request"
{
  "trace_id": "f90-complex-001",
  "lang": "fortran",
  "code": "program complex_test\n  implicit none\n  \n  integer :: i, j, n\n  integer, dimension(7) :: arr, sorted\n  real :: sum_val, avg, std_dev\n  real, dimension(10) :: numbers\n  real, dimension(3,3) :: matrix\n  integer :: fib_result\n  \n  print *, '=== Fortran Complex Test ==='\n  print *\n  \n  ! Test 1: Fibonacci\n  print *, '1. Fibonacci sequence:'\n  print *, '   First 15: ', (fibonacci(i), i=0,14)\n  print *, '   Fib(25) =', fibonacci(25)\n  \n  ! Test 2: QuickSort\n  print *\n  print *, '2. QuickSort:'\n  arr = (/64, 34, 25, 12, 22, 11, 90/)\n  print *, '   Input: ', arr\n  call quicksort(arr, 1, 7)\n  print *, '   Output:', arr\n  \n  ! Test 3: Statistical analysis\n  print *\n  print *, '3. Statistical analysis:'\n  numbers = (/1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0/)\n  sum_val = sum(numbers)\n  avg = sum_val / size(numbers)\n  std_dev = sqrt(sum((numbers - avg)**2) / size(numbers))\n  print *, '   Numbers: 1 to 10'\n  print *, '   Sum:    ', sum_val\n  print *, '   Mean:   ', avg\n  print *, '   Std Dev:', std_dev\n  print *, '   Min:    ', minval(numbers)\n  print *, '   Max:    ', maxval(numbers)\n  \n  ! Test 4: Matrix operations\n  print *\n  print *, '4. Matrix operations:'\n  matrix = reshape((/1,2,3,4,5,6,7,8,9/), (/3,3/))\n  print *, '   Matrix:'\n  do i = 1, 3\n    print *, '   ', (matrix(i,j), j=1,3)\n  end do\n  print *, '   Sum of elements:', sum(matrix)\n  print *, '   Trace:', matrix(1,1) + matrix(2,2) + matrix(3,3)\n  \n  ! Test 5: Factorial\n  print *\n  print *, '5. Factorial:'\n  print *, '   10! =', factorial(10)\n  print *, '   12! =', factorial(12)\n  \n  ! Test 6: Prime numbers\n  print *\n  print *, '6. Prime numbers up to 50:'\n  print *, '   ', (i, i=2,50, merge(1, 50, is_prime(i)))\n  \n  print *\n  print *, '=== All tests passed ==='\n  \ncontains\n  \n  recursive function fibonacci(n) result(fib)\n    integer, intent(in) :: n\n    integer :: fib\n    if (n <= 1) then\n      fib = n\n    else\n      fib = fibonacci(n-1) + fibonacci(n-2)\n    end if\n  end function fibonacci\n  \n  recursive function factorial(n) result(fact)\n    integer, intent(in) :: n\n    integer(8) :: fact\n    if (n <= 1) then\n      fact = 1\n    else\n      fact = n * factorial(n-1)\n    end if\n  end function factorial\n  \n  function is_prime(n) result(prime)\n    integer, intent(in) :: n\n    logical :: prime\n    integer :: i\n    if (n < 2) then\n      prime = .false.\n      return\n    end if\n    prime = .true.\n    do i = 2, int(sqrt(real(n)))\n      if (mod(n, i) == 0) then\n        prime = .false.\n        return\n      end if\n    end do\n  end function is_prime\n  \n  recursive subroutine quicksort(a, lo, hi)\n    integer, intent(inout) :: a(:)\n    integer, intent(in) :: lo, hi\n    integer :: pivot, i, j, temp\n    \n    if (lo < hi) then\n      pivot = a((lo + hi) / 2)\n      i = lo\n      j = hi\n      do while (i <= j)\n        do while (a(i) < pivot)\n          i = i + 1\n        end do\n        do while (a(j) > pivot)\n          j = j - 1\n        end do\n        if (i <= j) then\n          temp = a(i)\n          a(i) = a(j)\n          a(j) = temp\n          i = i + 1\n          j = j - 1\n        end if\n      end do\n      call quicksort(a, lo, j)\n      call quicksort(a, i, hi)\n    end if\n  end subroutine quicksort\n  \nend program complex_test",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "f90-complex-001",
  "stdout": " === Fortran Complex Test ===\n\n 1. Fibonacci sequence:\n    First 15:  0 1 1 2 3 5 8 13 21 34 55 89 144 233 377\n    Fib(25) = 75025\n\n 2. QuickSort:\n    Input:  64 34 25 12 22 11 90\n    Output: 11 12 22 25 34 64 90\n\n 3. Statistical analysis:\n    Numbers: 1 to 10\n    Sum:     55.0\n    Mean:    5.5\n    Std Dev: 2.872281\n    Min:     1.0\n    Max:     10.0\n\n 4. Matrix operations:\n    Matrix:\n    1.0 4.0 7.0\n    2.0 5.0 8.0\n    3.0 6.0 9.0\n    Sum of elements: 45.0\n    Trace: 15.0\n\n 5. Factorial:\n    10! = 3628800\n    12! = 479001600\n\n 6. Prime numbers up to 50:\n    2 3 5 7 11 13 17 19 23 29 31 37 41 43 47\n\n === All tests passed ===\n",
  "stderr": "",
  "exit_code": 0
}
```

## Limitations

:::warning

  - GFortran only
  - No external libraries (LAPACK, BLAS)
  - Memory limit: 512 MiB

:::
