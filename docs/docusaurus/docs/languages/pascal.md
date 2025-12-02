---
title: 'Pascal'
description: 'Pascal programming with Free Pascal'
---

## Overview

Pascal is an educational and procedural programming language. LLM-Firecracker provides Free Pascal Compiler.

## Specifications

| Property | Value |
|----------|-------|
| Base OS | Debian Bookworm |
| Compiler | Free Pascal 3.2.2 |
| Rootfs Size | 600 MB |
| Execution | Compiled (fpc + run) |
| File Extension | `.pas` |
| Run Command | `fpc -o{output} {file} && {output}` |
| Execution Time | ~82ms |

```bash title="1. Create Rootfs with infra.operator"
sudo infra.operator rootfs create --name pascal --size 600 --base debian --packages "fp-compiler"
```

```bash title="2. Create Snapshot"
sudo infra.operator snapshot create --lang pascal --mem 512 --vcpus 1
```

```bash title="3. Upload rootfs to S3"
sudo infra.operator rootfs upload --lang pascal --bucket llm-firecracker
```

```bash title="3. Upload snapshot to S3"
sudo infra.operator snapshot upload --lang pascal --bucket llm-firecracker
```

```bash title="4. Test Execution"
sudo infra.operator host --lang pascal --code "program Hello;
begin
  WriteLn('Hello from Free Pascal!');
end." --mem 512 --vcpus 1 --snapshot
```



## Examples

### Hello World

```json title="Request"
{
  "trace_id": "pas-hello-001",
  "lang": "pascal",
  "code": "program Hello;\nbegin\n  WriteLn('Hello from Pascal!');\nend.",
  "timeout": 20
}
```

```json title="Response"
{
  "trace_id": "pas-hello-001",
  "stdout": "Hello from Pascal!\n",
  "stderr": "",
  "exit_code": 0
}
```

### Complex Test: Algorithms and Data Structures

```json title="Request"
{
  "trace_id": "pas-complex-001",
  "lang": "pascal",
  "code": "program ComplexTest;\n\ntype\n  TIntArray = array of Integer;\n  TPerson = record\n    Name: string;\n    Age: Integer;\n    City: string;\n  end;\n  TPersonArray = array of TPerson;\n\nvar\n  i: Integer;\n  numbers: TIntArray;\n  people: TPersonArray;\n\n{ Fibonacci function }\nfunction Fibonacci(n: Integer): Int64;\nbegin\n  if n <= 1 then\n    Fibonacci := n\n  else\n    Fibonacci := Fibonacci(n - 1) + Fibonacci(n - 2);\nend;\n\n{ QuickSort implementation }\nprocedure QuickSort(var arr: TIntArray; low, high: Integer);\nvar\n  pivot, temp, i, j: Integer;\nbegin\n  if low < high then\n  begin\n    pivot := arr[(low + high) div 2];\n    i := low;\n    j := high;\n    while i <= j do\n    begin\n      while arr[i] < pivot do Inc(i);\n      while arr[j] > pivot do Dec(j);\n      if i <= j then\n      begin\n        temp := arr[i];\n        arr[i] := arr[j];\n        arr[j] := temp;\n        Inc(i);\n        Dec(j);\n      end;\n    end;\n    QuickSort(arr, low, j);\n    QuickSort(arr, i, high);\n  end;\nend;\n\n{ Calculate average age }\nfunction AverageAge(const arr: TPersonArray): Real;\nvar\n  sum, count: Integer;\nbegin\n  sum := 0;\n  for count := 0 to High(arr) do\n    sum := sum + arr[count].Age;\n  AverageAge := sum / Length(arr);\nend;\n\n{ Print array }\nprocedure PrintArray(const arr: TIntArray);\nvar\n  i: Integer;\nbegin\n  for i := 0 to High(arr) do\n  begin\n    Write(arr[i]);\n    if i < High(arr) then Write(', ');\n  end;\n  WriteLn;\nend;\n\n{ Factorial }\nfunction Factorial(n: Integer): Int64;\nbegin\n  if n <= 1 then\n    Factorial := 1\n  else\n    Factorial := n * Factorial(n - 1);\nend;\n\n{ Is Prime }\nfunction IsPrime(n: Integer): Boolean;\nvar\n  i: Integer;\nbegin\n  if n < 2 then\n    IsPrime := False\n  else\n  begin\n    IsPrime := True;\n    for i := 2 to Trunc(Sqrt(n)) do\n      if n mod i = 0 then\n      begin\n        IsPrime := False;\n        Break;\n      end;\n  end;\nend;\n\nbegin\n  WriteLn('=== Pascal Complex Test ===');\n  WriteLn;\n\n  { Test 1: Fibonacci }\n  WriteLn('1. Fibonacci sequence:');\n  Write('   First 15: ');\n  for i := 0 to 14 do\n  begin\n    Write(Fibonacci(i));\n    if i < 14 then Write(', ');\n  end;\n  WriteLn;\n  WriteLn('   Fib(25) = ', Fibonacci(25));\n\n  { Test 2: QuickSort }\n  WriteLn;\n  WriteLn('2. QuickSort:');\n  SetLength(numbers, 7);\n  numbers[0] := 64; numbers[1] := 34; numbers[2] := 25;\n  numbers[3] := 12; numbers[4] := 22; numbers[5] := 11;\n  numbers[6] := 90;\n  Write('   Input:  '); PrintArray(numbers);\n  QuickSort(numbers, 0, High(numbers));\n  Write('   Output: '); PrintArray(numbers);\n\n  { Test 3: People data }\n  WriteLn;\n  WriteLn('3. People data processing:');\n  SetLength(people, 5);\n  people[0].Name := 'Alice'; people[0].Age := 30; people[0].City := 'NYC';\n  people[1].Name := 'Bob'; people[1].Age := 25; people[1].City := 'LA';\n  people[2].Name := 'Charlie'; people[2].Age := 35; people[2].City := 'NYC';\n  people[3].Name := 'Diana'; people[3].Age := 28; people[3].City := 'LA';\n  people[4].Name := 'Eve'; people[4].Age := 32; people[4].City := 'Chicago';\n  \n  for i := 0 to High(people) do\n    WriteLn('   ', people[i].Name, ' (', people[i].Age, ') - ', people[i].City);\n  WriteLn('   Average age: ', AverageAge(people):0:1);\n\n  { Test 4: Mathematical functions }\n  WriteLn;\n  WriteLn('4. Mathematical functions:');\n  WriteLn('   Factorial(10) = ', Factorial(10));\n  WriteLn('   Factorial(12) = ', Factorial(12));\n  \n  { Test 5: Prime numbers }\n  WriteLn;\n  WriteLn('5. Prime numbers up to 50:');\n  Write('   ');\n  for i := 2 to 50 do\n    if IsPrime(i) then\n      Write(i, ' ');\n  WriteLn;\n\n  { Test 6: String operations }\n  WriteLn;\n  WriteLn('6. String operations:');\n  WriteLn('   Length of \"Hello World\": ', Length('Hello World'));\n  WriteLn('   Uppercase: ', UpCase('h'), UpCase('e'), UpCase('l'), UpCase('l'), UpCase('o'));\n\n  WriteLn;\n  WriteLn('=== All tests passed ===');\nend.",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "pas-complex-001",
  "stdout": "=== Pascal Complex Test ===\n\n1. Fibonacci sequence:\n   First 15: 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377\n   Fib(25) = 75025\n\n2. QuickSort:\n   Input:  64, 34, 25, 12, 22, 11, 90\n   Output: 11, 12, 22, 25, 34, 64, 90\n\n3. People data processing:\n   Alice (30) - NYC\n   Bob (25) - LA\n   Charlie (35) - NYC\n   Diana (28) - LA\n   Eve (32) - Chicago\n   Average age: 30.0\n\n4. Mathematical functions:\n   Factorial(10) = 3628800\n   Factorial(12) = 479001600\n\n5. Prime numbers up to 50:\n   2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 \n\n6. String operations:\n   Length of \"Hello World\": 11\n   Uppercase: HELLO\n\n=== All tests passed ===\n",
  "stderr": "",
  "exit_code": 0
}
```

## Limitations

:::warning

  - Free Pascal only
  - No Delphi units
  - Memory limit: 512 MiB

:::
