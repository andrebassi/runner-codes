---
title: 'COBOL'
description: 'COBOL business programming language'
---

## Overview

COBOL (Common Business-Oriented Language) is one of the oldest programming languages, still widely used in banking and government systems. Runner Codes provides GnuCOBOL for executing COBOL programs.

## Specifications

| Property | Value |
|----------|-------|
| Base OS | Debian Bookworm |
| Compiler | GnuCOBOL 3.1.2 |
| Rootfs Size | 1200 MB |
| Execution | Compiled (cobc + run) |
| File Extension | `.cob` |
| Run Command | `cobc -x -o {output} {file} && {output}` |
| Execution Time | ~147ms |

```bash title="1. Create Rootfs with infra.operator"
sudo infra.operator rootfs create --name cobol --size 1200 --base debian --packages "gnucobol"
```

```bash title="2. Create Snapshot"
sudo infra.operator snapshot create --lang cobol --mem 512 --vcpus 1
```

```bash title="3. Upload rootfs to S3"
sudo infra.operator rootfs upload --lang cobol --bucket runner-codes
```

```bash title="3. Upload snapshot to S3"
sudo infra.operator snapshot upload --lang cobol --bucket runner-codes
```

```bash title="4. Test Execution"
sudo infra.operator host --lang cobol --code "       IDENTIFICATION DIVISION.
       PROGRAM-ID. HELLO.
       PROCEDURE DIVISION.
           DISPLAY \"Hello from GnuCOBOL!\".
           STOP RUN." --mem 512 --vcpus 1 --snapshot
```



## Examples

### Hello World

```json title="Request"
{
  "trace_id": "cobol-hello-001",
  "lang": "cobol",
  "code": "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. HELLO.\n       PROCEDURE DIVISION.\n           DISPLAY \"Hello from COBOL!\".\n           STOP RUN.",
  "timeout": 20
}
```

```json title="Response"
{
  "trace_id": "cobol-hello-001",
  "stdout": "Hello from COBOL!\n",
  "stderr": "",
  "exit_code": 0
}
```

### Variables and Calculations

```json title="Request"
{
  "trace_id": "cobol-calc-001",
  "lang": "cobol",
  "code": "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. CALC.\n       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n       01 WS-NUM1 PIC 9(3) VALUE 100.\n       01 WS-NUM2 PIC 9(3) VALUE 50.\n       01 WS-RESULT PIC 9(4).\n       PROCEDURE DIVISION.\n           ADD WS-NUM1 TO WS-NUM2 GIVING WS-RESULT.\n           DISPLAY \"Sum: \" WS-RESULT.\n           MULTIPLY WS-NUM1 BY WS-NUM2 GIVING WS-RESULT.\n           DISPLAY \"Product: \" WS-RESULT.\n           STOP RUN.",
  "timeout": 20
}
```

```json title="Response"
{
  "trace_id": "cobol-calc-001",
  "stdout": "Sum: 0150\nProduct: 5000\n",
  "stderr": "",
  "exit_code": 0
}
```

### Loops

```json title="Request"
{
  "trace_id": "cobol-loop-001",
  "lang": "cobol",
  "code": "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. LOOPS.\n       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n       01 WS-COUNTER PIC 9(2) VALUE 1.\n       PROCEDURE DIVISION.\n           PERFORM UNTIL WS-COUNTER > 5\n               DISPLAY \"Iteration: \" WS-COUNTER\n               ADD 1 TO WS-COUNTER\n           END-PERFORM.\n           STOP RUN.",
  "timeout": 20
}
```

```json title="Response"
{
  "trace_id": "cobol-loop-001",
  "stdout": "Iteration: 01\nIteration: 02\nIteration: 03\nIteration: 04\nIteration: 05\n",
  "stderr": "",
  "exit_code": 0
}
```

### Tables (Arrays)

```json title="Request"
{
  "trace_id": "cobol-table-001",
  "lang": "cobol",
  "code": "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. TABLES.\n       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n       01 WS-TABLE.\n           05 WS-NUM PIC 9(2) OCCURS 5 TIMES.\n       01 WS-INDEX PIC 9(2).\n       01 WS-SUM PIC 9(3) VALUE 0.\n       PROCEDURE DIVISION.\n           MOVE 10 TO WS-NUM(1).\n           MOVE 20 TO WS-NUM(2).\n           MOVE 30 TO WS-NUM(3).\n           MOVE 40 TO WS-NUM(4).\n           MOVE 50 TO WS-NUM(5).\n           PERFORM VARYING WS-INDEX FROM 1 BY 1 UNTIL WS-INDEX > 5\n               ADD WS-NUM(WS-INDEX) TO WS-SUM\n           END-PERFORM.\n           DISPLAY \"Sum: \" WS-SUM.\n           STOP RUN.",
  "timeout": 20
}
```

```json title="Response"
{
  "trace_id": "cobol-table-001",
  "stdout": "Sum: 150\n",
  "stderr": "",
  "exit_code": 0
}
```

### Complex Test: Comprehensive COBOL

```json title="Request"
{
  "trace_id": "cobol-complex-001",
  "lang": "cobol",
  "code": "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. COMPLEX-TEST.\n       DATA DIVISION.\n       WORKING-STORAGE SECTION.\n       01 WS-HEADER PIC X(30).\n       01 WS-I PIC 9(2).\n       01 WS-J PIC 9(2).\n       01 WS-FIB-PREV PIC 9(10) VALUE 0.\n       01 WS-FIB-CURR PIC 9(10) VALUE 1.\n       01 WS-FIB-NEXT PIC 9(10).\n       01 WS-FIB-RESULT PIC Z(9)9.\n       01 WS-NUMBERS.\n           05 WS-NUM PIC 9(2) OCCURS 7 TIMES.\n       01 WS-TEMP PIC 9(2).\n       01 WS-SWAPPED PIC 9 VALUE 0.\n       01 WS-SUM PIC 9(4) VALUE 0.\n       01 WS-AVG PIC 9(2)V9(2).\n       01 WS-AVG-DISP PIC Z9.99.\n       01 WS-FACTORIAL PIC 9(10) VALUE 1.\n       01 WS-FACT-DISP PIC Z(9)9.\n       01 WS-N PIC 9(2).\n       01 WS-IS-PRIME PIC 9 VALUE 1.\n       01 WS-DIVISOR PIC 9(2).\n       PROCEDURE DIVISION.\n           DISPLAY \"=== COBOL Complex Test ===\".\n           DISPLAY \" \".\n\n           DISPLAY \"1. Fibonacci sequence:\".\n           MOVE 0 TO WS-FIB-PREV.\n           MOVE 1 TO WS-FIB-CURR.\n           DISPLAY \"   First 10: \" WITH NO ADVANCING.\n           DISPLAY WS-FIB-PREV WITH NO ADVANCING.\n           DISPLAY \" \" WITH NO ADVANCING.\n           DISPLAY WS-FIB-CURR WITH NO ADVANCING.\n           DISPLAY \" \" WITH NO ADVANCING.\n           PERFORM 8 TIMES\n               ADD WS-FIB-PREV TO WS-FIB-CURR GIVING WS-FIB-NEXT\n               MOVE WS-FIB-CURR TO WS-FIB-PREV\n               MOVE WS-FIB-NEXT TO WS-FIB-CURR\n               DISPLAY WS-FIB-CURR WITH NO ADVANCING\n               DISPLAY \" \" WITH NO ADVANCING\n           END-PERFORM.\n           DISPLAY \" \".\n           DISPLAY \" \".\n\n           DISPLAY \"2. Bubble Sort:\".\n           MOVE 64 TO WS-NUM(1).\n           MOVE 34 TO WS-NUM(2).\n           MOVE 25 TO WS-NUM(3).\n           MOVE 12 TO WS-NUM(4).\n           MOVE 22 TO WS-NUM(5).\n           MOVE 11 TO WS-NUM(6).\n           MOVE 90 TO WS-NUM(7).\n           DISPLAY \"   Input:  \" WITH NO ADVANCING.\n           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 7\n               DISPLAY WS-NUM(WS-I) WITH NO ADVANCING\n               DISPLAY \" \" WITH NO ADVANCING\n           END-PERFORM.\n           DISPLAY \" \".\n           \n           PERFORM UNTIL WS-SWAPPED = 0\n               MOVE 0 TO WS-SWAPPED\n               PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 6\n                   COMPUTE WS-J = WS-I + 1\n                   IF WS-NUM(WS-I) > WS-NUM(WS-J)\n                       MOVE WS-NUM(WS-I) TO WS-TEMP\n                       MOVE WS-NUM(WS-J) TO WS-NUM(WS-I)\n                       MOVE WS-TEMP TO WS-NUM(WS-J)\n                       MOVE 1 TO WS-SWAPPED\n                   END-IF\n               END-PERFORM\n           END-PERFORM.\n           \n           DISPLAY \"   Output: \" WITH NO ADVANCING.\n           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 7\n               DISPLAY WS-NUM(WS-I) WITH NO ADVANCING\n               DISPLAY \" \" WITH NO ADVANCING\n           END-PERFORM.\n           DISPLAY \" \".\n           DISPLAY \" \".\n\n           DISPLAY \"3. Statistics:\".\n           MOVE 0 TO WS-SUM.\n           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 7\n               ADD WS-NUM(WS-I) TO WS-SUM\n           END-PERFORM.\n           DIVIDE WS-SUM BY 7 GIVING WS-AVG.\n           MOVE WS-SUM TO WS-FACT-DISP.\n           DISPLAY \"   Sum: \" WS-FACT-DISP.\n           MOVE WS-AVG TO WS-AVG-DISP.\n           DISPLAY \"   Average: \" WS-AVG-DISP.\n           DISPLAY \" \".\n\n           DISPLAY \"4. Factorial:\".\n           MOVE 1 TO WS-FACTORIAL.\n           PERFORM VARYING WS-I FROM 1 BY 1 UNTIL WS-I > 10\n               MULTIPLY WS-I BY WS-FACTORIAL\n           END-PERFORM.\n           MOVE WS-FACTORIAL TO WS-FACT-DISP.\n           DISPLAY \"   10! = \" WS-FACT-DISP.\n           DISPLAY \" \".\n\n           DISPLAY \"5. Prime check:\".\n           PERFORM VARYING WS-N FROM 2 BY 1 UNTIL WS-N > 20\n               MOVE 1 TO WS-IS-PRIME\n               IF WS-N > 2\n                   PERFORM VARYING WS-DIVISOR FROM 2 BY 1 \n                       UNTIL WS-DIVISOR >= WS-N OR WS-IS-PRIME = 0\n                       IF FUNCTION MOD(WS-N, WS-DIVISOR) = 0\n                           MOVE 0 TO WS-IS-PRIME\n                       END-IF\n                   END-PERFORM\n               END-IF\n               IF WS-IS-PRIME = 1\n                   DISPLAY \"   \" WS-N \" is prime\"\n               END-IF\n           END-PERFORM.\n           DISPLAY \" \".\n\n           DISPLAY \"=== All tests passed ===\".\n           STOP RUN.",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "cobol-complex-001",
  "stdout": "=== COBOL Complex Test ===\n \n1. Fibonacci sequence:\n   First 10: 0 1 2 3 5 8 13 21 34 55  \n \n2. Bubble Sort:\n   Input:  64 34 25 12 22 11 90  \n   Output: 11 12 22 25 34 64 90  \n \n3. Statistics:\n   Sum:        258\n   Average: 36.85\n \n4. Factorial:\n   10! =    3628800\n \n5. Prime check:\n   02 is prime\n   03 is prime\n   05 is prime\n   07 is prime\n   11 is prime\n   13 is prime\n   17 is prime\n   19 is prime\n \n=== All tests passed ===\n",
  "stderr": "",
  "exit_code": 0
}
```

## Limitations

:::warning

  The COBOL environment has the following limitations:

:::

1. **GnuCOBOL only**: Not IBM or Micro Focus COBOL
2. **No database connectors**: EXEC SQL not available
3. **No network**: File transfer not available
4. **Memory limit**: 512 MiB
5. **Fixed format**: Use columns 7-72 for code

## Best Practices


    COBOL uses fixed columns: 1-6 sequence, 7 indicator, 8-72 code.



    COBOL allows descriptive names like WS-CUSTOMER-NAME.



    Always initialize working storage variables with VALUE clause.


