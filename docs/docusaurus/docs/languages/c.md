---
title: 'C'
description: 'C language code execution with GCC'
---

## Overview

C is a low-level systems programming language. Runner Codes provides GCC for compiling and executing C programs.

## Specifications

| Property | Value |
|----------|-------|
| Docker Image | `frolvlad/alpine-gcc` |
| Compiler | GCC 14.2.0 |
| Rootfs Size | 300 MB |
| Execution | Compiled (gcc + run) |
| File Extension | `.c` |
| Run Command | `gcc -o runbin {file} && ./runbin` |
| Execution Time | ~68ms |

```bash title="1. Create Rootfs from Docker"
sudo infra.operator rootfs from-docker --name c --image frolvlad/alpine-gcc --size 300
```

```bash title="2. Create Snapshot"
sudo infra.operator snapshot create --lang c --mem 512 --vcpus 1
```

```bash title="3. Upload rootfs to S3"
sudo infra.operator rootfs upload --lang c --bucket runner-codes
```

```bash title="3. Upload snapshot to S3"
sudo infra.operator snapshot upload --lang c --bucket runner-codes
```

```bash title="4. Test Execution"
sudo infra.operator host --lang c --code '#include <stdio.h>
int main() {
    printf("Hello from C (GCC %d.%d.%d)\n", __GNUC__, __GNUC_MINOR__, __GNUC_PATCHLEVEL__);
    return 0;
}' --mem 512 --vcpus 1 --snapshot
```



## Execution Flow

![C Execution Flow](/img/execution-compiled.svg)

## Examples

### Hello World

```json title="Request"
{
  "trace_id": "c-hello-001",
  "lang": "c",
  "code": "#include <stdio.h>\n\nint main() {\n    printf(\"Hello from C!\\n\");\n    return 0;\n}",
  "timeout": 15
}
```

```json title="Response"
{
  "trace_id": "c-hello-001",
  "stdout": "Hello from C!\n",
  "stderr": "",
  "exit_code": 0
}
```

### Variables and Pointers

```json title="Request"
{
  "trace_id": "c-ptr-001",
  "lang": "c",
  "code": "#include <stdio.h>\n\nint main() {\n    int x = 42;\n    int *ptr = &x;\n    printf(\"Value: %d\\n\", x);\n    printf(\"Address: %p\\n\", (void*)ptr);\n    printf(\"Via pointer: %d\\n\", *ptr);\n    return 0;\n}",
  "timeout": 15
}
```

```json title="Response"
{
  "trace_id": "c-ptr-001",
  "stdout": "Value: 42\nAddress: 0x7ffd...\nVia pointer: 42\n",
  "stderr": "",
  "exit_code": 0
}
```

### Arrays and Loops

```json title="Request"
{
  "trace_id": "c-array-001",
  "lang": "c",
  "code": "#include <stdio.h>\n\nint main() {\n    int arr[] = {1, 2, 3, 4, 5};\n    int sum = 0;\n    for (int i = 0; i < 5; i++) {\n        sum += arr[i];\n    }\n    printf(\"Sum: %d\\n\", sum);\n    return 0;\n}",
  "timeout": 15
}
```

```json title="Response"
{
  "trace_id": "c-array-001",
  "stdout": "Sum: 15\n",
  "stderr": "",
  "exit_code": 0
}
```

### Structs

```json title="Request"
{
  "trace_id": "c-struct-001",
  "lang": "c",
  "code": "#include <stdio.h>\n\ntypedef struct {\n    char name[50];\n    int age;\n} Person;\n\nint main() {\n    Person p = {\"Alice\", 30};\n    printf(\"Name: %s\\n\", p.name);\n    printf(\"Age: %d\\n\", p.age);\n    return 0;\n}",
  "timeout": 15
}
```

```json title="Response"
{
  "trace_id": "c-struct-001",
  "stdout": "Name: Alice\nAge: 30\n",
  "stderr": "",
  "exit_code": 0
}
```

### Functions

```json title="Request"
{
  "trace_id": "c-func-001",
  "lang": "c",
  "code": "#include <stdio.h>\n\nint fibonacci(int n) {\n    if (n <= 1) return n;\n    return fibonacci(n-1) + fibonacci(n-2);\n}\n\nint main() {\n    for (int i = 0; i < 10; i++) {\n        printf(\"F(%d) = %d\\n\", i, fibonacci(i));\n    }\n    return 0;\n}",
  "timeout": 15
}
```

```json title="Response"
{
  "trace_id": "c-func-001",
  "stdout": "F(0) = 0\nF(1) = 1\nF(2) = 1\nF(3) = 2\nF(4) = 3\nF(5) = 5\nF(6) = 8\nF(7) = 13\nF(8) = 21\nF(9) = 34\n",
  "stderr": "",
  "exit_code": 0
}
```

### Complex Test: Comprehensive C

```json title="Request"
{
  "trace_id": "c-complex-001",
  "lang": "c",
  "code": "#include <stdio.h>\n#include <stdlib.h>\n#include <string.h>\n#include <math.h>\n\n// Function prototypes\nlong long fib_memo[100];\nlong long fib(int n);\nvoid quicksort(int arr[], int low, int high);\nint partition(int arr[], int low, int high);\nint is_prime(int n);\n\ntypedef struct {\n    char name[50];\n    int age;\n    char city[50];\n} Person;\n\nint main() {\n    printf(\"=== C Complex Test ===\\n\\n\");\n    \n    // Test 1: Fibonacci with memoization\n    printf(\"1. Fibonacci with memoization:\\n\");\n    memset(fib_memo, -1, sizeof(fib_memo));\n    printf(\"   First 15: \");\n    for (int i = 0; i < 15; i++) {\n        printf(\"%lld \", fib(i));\n    }\n    printf(\"\\n   Fib(50) = %lld\\n\", fib(50));\n    \n    // Test 2: QuickSort\n    printf(\"\\n2. QuickSort:\\n\");\n    int arr[] = {64, 34, 25, 12, 22, 11, 90};\n    int n = sizeof(arr) / sizeof(arr[0]);\n    printf(\"   Input:  \");\n    for (int i = 0; i < n; i++) printf(\"%d \", arr[i]);\n    printf(\"\\n\");\n    quicksort(arr, 0, n - 1);\n    printf(\"   Output: \");\n    for (int i = 0; i < n; i++) printf(\"%d \", arr[i]);\n    printf(\"\\n\");\n    \n    // Test 3: Structs and arrays\n    printf(\"\\n3. Struct operations:\\n\");\n    Person people[] = {\n        {\"Alice\", 30, \"NYC\"},\n        {\"Bob\", 25, \"LA\"},\n        {\"Charlie\", 35, \"Chicago\"}\n    };\n    int num_people = sizeof(people) / sizeof(people[0]);\n    for (int i = 0; i < num_people; i++) {\n        printf(\"   %s (%d) from %s\\n\", people[i].name, people[i].age, people[i].city);\n    }\n    \n    // Test 4: Array operations\n    printf(\"\\n4. Array operations:\\n\");\n    int numbers[] = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};\n    int sum = 0, count = 10;\n    printf(\"   Numbers: \");\n    for (int i = 0; i < count; i++) {\n        printf(\"%d \", numbers[i]);\n        sum += numbers[i];\n    }\n    printf(\"\\n   Sum: %d\\n\", sum);\n    printf(\"   Average: %.1f\\n\", (float)sum / count);\n    \n    // Test 5: Pointers and memory\n    printf(\"\\n5. Pointer operations:\\n\");\n    int *dynamic = (int*)malloc(5 * sizeof(int));\n    for (int i = 0; i < 5; i++) dynamic[i] = (i + 1) * 10;\n    printf(\"   Dynamic array: \");\n    for (int i = 0; i < 5; i++) printf(\"%d \", dynamic[i]);\n    printf(\"\\n\");\n    free(dynamic);\n    printf(\"   Memory freed successfully\\n\");\n    \n    // Test 6: Prime numbers\n    printf(\"\\n6. Prime numbers up to 50:\\n\");\n    printf(\"   \");\n    for (int i = 2; i <= 50; i++) {\n        if (is_prime(i)) printf(\"%d \", i);\n    }\n    printf(\"\\n\");\n    \n    // Test 7: String operations\n    printf(\"\\n7. String operations:\\n\");\n    char str[] = \"Hello, World!\";\n    printf(\"   Original: %s\\n\", str);\n    printf(\"   Length: %lu\\n\", strlen(str));\n    char copy[50];\n    strcpy(copy, str);\n    for (int i = 0; copy[i]; i++) {\n        if (copy[i] >= 'a' && copy[i] <= 'z') copy[i] -= 32;\n    }\n    printf(\"   Uppercase: %s\\n\", copy);\n    \n    // Test 8: Math operations\n    printf(\"\\n8. Math operations:\\n\");\n    printf(\"   sqrt(144) = %.0f\\n\", sqrt(144));\n    printf(\"   pow(2, 10) = %.0f\\n\", pow(2, 10));\n    printf(\"   ceil(4.3) = %.0f\\n\", ceil(4.3));\n    printf(\"   floor(4.7) = %.0f\\n\", floor(4.7));\n    \n    printf(\"\\n=== All tests passed ===\\n\");\n    return 0;\n}\n\nlong long fib(int n) {\n    if (n <= 1) return n;\n    if (fib_memo[n] != -1) return fib_memo[n];\n    fib_memo[n] = fib(n - 1) + fib(n - 2);\n    return fib_memo[n];\n}\n\nvoid quicksort(int arr[], int low, int high) {\n    if (low < high) {\n        int pi = partition(arr, low, high);\n        quicksort(arr, low, pi - 1);\n        quicksort(arr, pi + 1, high);\n    }\n}\n\nint partition(int arr[], int low, int high) {\n    int pivot = arr[high];\n    int i = low - 1;\n    for (int j = low; j < high; j++) {\n        if (arr[j] < pivot) {\n            i++;\n            int temp = arr[i]; arr[i] = arr[j]; arr[j] = temp;\n        }\n    }\n    int temp = arr[i + 1]; arr[i + 1] = arr[high]; arr[high] = temp;\n    return i + 1;\n}\n\nint is_prime(int n) {\n    if (n < 2) return 0;\n    for (int i = 2; i * i <= n; i++) {\n        if (n % i == 0) return 0;\n    }\n    return 1;\n}",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "c-complex-001",
  "stdout": "=== C Complex Test ===\n\n1. Fibonacci with memoization:\n   First 15: 0 1 1 2 3 5 8 13 21 34 55 89 144 233 377 \n   Fib(50) = 12586269025\n\n2. QuickSort:\n   Input:  64 34 25 12 22 11 90 \n   Output: 11 12 22 25 34 64 90 \n\n3. Struct operations:\n   Alice (30) from NYC\n   Bob (25) from LA\n   Charlie (35) from Chicago\n\n4. Array operations:\n   Numbers: 1 2 3 4 5 6 7 8 9 10 \n   Sum: 55\n   Average: 5.5\n\n5. Pointer operations:\n   Dynamic array: 10 20 30 40 50 \n   Memory freed successfully\n\n6. Prime numbers up to 50:\n   2 3 5 7 11 13 17 19 23 29 31 37 41 43 47 \n\n7. String operations:\n   Original: Hello, World!\n   Length: 13\n   Uppercase: HELLO, WORLD!\n\n8. Math operations:\n   sqrt(144) = 12\n   pow(2, 10) = 1024\n   ceil(4.3) = 5\n   floor(4.7) = 4\n\n=== All tests passed ===\n",
  "stderr": "",
  "exit_code": 0
}
```

## Performance

| Operation | Time |
|-----------|------|
| Compilation | ~500ms |
| Hello World (total) | ~1s |
| Fibonacci(30) | ~50ms |
| Array operations (10k) | ~1ms |

## Limitations

:::warning

  The C environment has the following limitations:

:::

1. **Single file only**: Must be self-contained
2. **No external libraries**: Only standard libc
3. **No networking**: Socket operations will fail
4. **Memory limit**: 512 MiB
5. **Stack size**: Default Linux stack size

## Best Practices

:::info Best Practices

- **Include standard headers** — Always include stdio.h, stdlib.h, string.h as needed.
- **Return proper exit codes** — Always return 0 from main() for success.
- **Check memory allocations** — Always check if malloc/calloc returns NULL.
- **Free allocated memory** — Avoid memory leaks by freeing allocated memory.

:::