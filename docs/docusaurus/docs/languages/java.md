---
title: 'Java'
description: 'Java 17 (OpenJDK) code execution'
---

## Overview

Java is a widely-used enterprise programming language. Runner Codes provides OpenJDK 17 for executing Java code with automatic compilation.

## Specifications

| Property | Value |
|----------|-------|
| Base OS | Debian Bookworm |
| Version | OpenJDK 17 |
| Rootfs Size | 600 MB |
| Execution | Compiled (javac + java) |
| File Extension | `.java` |
| Compile Command | `javac Main.java` |
| Run Command | `java Main` |
| Execution Time | ~752ms |

```bash title="1. Create Rootfs with infra.operator"
sudo infra.operator rootfs create --name java --size 600 --base debian --packages "openjdk-17-jdk-headless"
```

```bash title="2. Create Snapshot"
sudo infra.operator snapshot create --lang java --mem 512 --vcpus 1
```

```bash title="3. Upload rootfs to S3"
sudo infra.operator rootfs upload --lang java --bucket runner-codes
```

```bash title="3. Upload snapshot to S3"
sudo infra.operator snapshot upload --lang java --bucket runner-codes
```

```bash title="4. Test Execution"
sudo infra.operator host --lang java --code "public class Main { public static void main(String[] args) { System.out.println(\"Hello from Java\"); } }" --mem 512 --vcpus 1 --snapshot
```



## Execution Flow

![Java Execution Flow](/img/execution-jvm.svg)

## Examples

### Hello World

```json title="Request"
{
  "trace_id": "java-hello-001",
  "lang": "java",
  "code": "public class Main {\n    public static void main(String[] args) {\n        System.out.println(\"Hello from Java!\");\n    }\n}",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "java-hello-001",
  "stdout": "Hello from Java!\n",
  "stderr": "",
  "exit_code": 0
}
```

### Variables and Calculations

```json title="Request"
{
  "trace_id": "java-calc-001",
  "lang": "java",
  "code": "public class Main {\n    public static void main(String[] args) {\n        int x = 10;\n        int y = 20;\n        System.out.println(\"Sum: \" + (x + y));\n        System.out.println(\"Product: \" + (x * y));\n    }\n}",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "java-calc-001",
  "stdout": "Sum: 30\nProduct: 200\n",
  "stderr": "",
  "exit_code": 0
}
```

### Classes and Objects

```json title="Request"
{
  "trace_id": "java-class-001",
  "lang": "java",
  "code": "class Calculator {\n    private int result = 0;\n    \n    Calculator add(int x) { result += x; return this; }\n    Calculator multiply(int x) { result *= x; return this; }\n    int getResult() { return result; }\n}\n\npublic class Main {\n    public static void main(String[] args) {\n        Calculator calc = new Calculator();\n        int result = calc.add(5).multiply(3).add(10).getResult();\n        System.out.println(\"Result: \" + result);\n    }\n}",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "java-class-001",
  "stdout": "Result: 25\n",
  "stderr": "",
  "exit_code": 0
}
```

### Collections

```json title="Request"
{
  "trace_id": "java-list-001",
  "lang": "java",
  "code": "import java.util.*;\n\npublic class Main {\n    public static void main(String[] args) {\n        List<Integer> numbers = Arrays.asList(1, 2, 3, 4, 5);\n        int sum = numbers.stream().mapToInt(Integer::intValue).sum();\n        System.out.println(\"Numbers: \" + numbers);\n        System.out.println(\"Sum: \" + sum);\n    }\n}",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "java-list-001",
  "stdout": "Numbers: [1, 2, 3, 4, 5]\nSum: 15\n",
  "stderr": "",
  "exit_code": 0
}
```

### Exception Handling

```json title="Request"
{
  "trace_id": "java-exception-001",
  "lang": "java",
  "code": "public class Main {\n    public static void main(String[] args) {\n        try {\n            int result = 10 / 0;\n        } catch (ArithmeticException e) {\n            System.out.println(\"Caught: \" + e.getMessage());\n        } finally {\n            System.out.println(\"Cleanup complete\");\n        }\n    }\n}",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "java-exception-001",
  "stdout": "Caught: / by zero\nCleanup complete\n",
  "stderr": "",
  "exit_code": 0
}
```

### Complex Test: Comprehensive Java

```json title="Request"
{
  "trace_id": "java-complex-001",
  "lang": "java",
  "code": "import java.util.*;\nimport java.util.stream.*;\n\npublic class Main {\n    static Map<Integer, Long> memo = new HashMap<>();\n    \n    public static void main(String[] args) {\n        System.out.println(\"=== Java Complex Test ===\");\n        System.out.println();\n        \n        // Test 1: Fibonacci with memoization\n        System.out.println(\"1. Fibonacci with memoization:\");\n        List<Long> fibs = IntStream.range(0, 15)\n            .mapToObj(Main::fib)\n            .collect(Collectors.toList());\n        System.out.println(\"   First 15: \" + fibs);\n        System.out.println(\"   Fib(50) = \" + fib(50));\n        \n        // Test 2: QuickSort\n        System.out.println();\n        System.out.println(\"2. QuickSort:\");\n        int[] arr = {64, 34, 25, 12, 22, 11, 90};\n        System.out.println(\"   Input:  \" + Arrays.toString(arr));\n        quickSort(arr, 0, arr.length - 1);\n        System.out.println(\"   Output: \" + Arrays.toString(arr));\n        \n        // Test 3: Collections and Streams\n        System.out.println();\n        System.out.println(\"3. Streams and Collections:\");\n        List<Integer> numbers = IntStream.rangeClosed(1, 10).boxed().collect(Collectors.toList());\n        List<Integer> squares = numbers.stream().map(x -> x * x).collect(Collectors.toList());\n        int sum = squares.stream().mapToInt(Integer::intValue).sum();\n        System.out.println(\"   Numbers: \" + numbers);\n        System.out.println(\"   Squares: \" + squares);\n        System.out.println(\"   Sum: \" + sum);\n        \n        // Test 4: Map operations\n        System.out.println();\n        System.out.println(\"4. Map operations:\");\n        Map<String, Integer> ages = new LinkedHashMap<>();\n        ages.put(\"Alice\", 30);\n        ages.put(\"Bob\", 25);\n        ages.put(\"Charlie\", 35);\n        ages.forEach((k, v) -> System.out.println(\"   \" + k + \": \" + v));\n        double avg = ages.values().stream().mapToInt(Integer::intValue).average().orElse(0);\n        System.out.println(\"   Average age: \" + avg);\n        \n        // Test 5: Classes and inheritance\n        System.out.println();\n        System.out.println(\"5. Polymorphism:\");\n        Animal[] animals = {new Dog(\"Rex\"), new Cat(\"Whiskers\"), new Dog(\"Buddy\")};\n        for (Animal a : animals) {\n            System.out.println(\"   \" + a.speak());\n        }\n        \n        // Test 6: Optional and functional\n        System.out.println();\n        System.out.println(\"6. Optional handling:\");\n        Optional<String> opt1 = Optional.of(\"Hello\");\n        Optional<String> opt2 = Optional.empty();\n        System.out.println(\"   Present: \" + opt1.orElse(\"default\"));\n        System.out.println(\"   Empty: \" + opt2.orElse(\"default\"));\n        \n        // Test 7: String operations\n        System.out.println();\n        System.out.println(\"7. String operations:\");\n        String text = \"Hello, World! Welcome to Java.\";\n        System.out.println(\"   Original: \" + text);\n        System.out.println(\"   Upper: \" + text.toUpperCase());\n        System.out.println(\"   Words: \" + text.split(\" \").length);\n        System.out.println(\"   Reversed: \" + new StringBuilder(text).reverse());\n        \n        // Test 8: Exception handling\n        System.out.println();\n        System.out.println(\"8. Exception handling:\");\n        try {\n            int result = divide(10, 0);\n        } catch (ArithmeticException e) {\n            System.out.println(\"   Caught: \" + e.getMessage());\n        }\n        System.out.println(\"   Safe divide: \" + divide(10, 2));\n        \n        System.out.println();\n        System.out.println(\"=== All tests passed ===\");\n    }\n    \n    static long fib(int n) {\n        if (memo.containsKey(n)) return memo.get(n);\n        long result = (n <= 1) ? n : fib(n-1) + fib(n-2);\n        memo.put(n, result);\n        return result;\n    }\n    \n    static void quickSort(int[] arr, int low, int high) {\n        if (low < high) {\n            int pi = partition(arr, low, high);\n            quickSort(arr, low, pi - 1);\n            quickSort(arr, pi + 1, high);\n        }\n    }\n    \n    static int partition(int[] arr, int low, int high) {\n        int pivot = arr[high];\n        int i = low - 1;\n        for (int j = low; j < high; j++) {\n            if (arr[j] < pivot) {\n                i++;\n                int temp = arr[i]; arr[i] = arr[j]; arr[j] = temp;\n            }\n        }\n        int temp = arr[i+1]; arr[i+1] = arr[high]; arr[high] = temp;\n        return i + 1;\n    }\n    \n    static int divide(int a, int b) {\n        return a / b;\n    }\n}\n\nabstract class Animal {\n    String name;\n    Animal(String name) { this.name = name; }\n    abstract String speak();\n}\n\nclass Dog extends Animal {\n    Dog(String name) { super(name); }\n    String speak() { return name + \" says Woof!\"; }\n}\n\nclass Cat extends Animal {\n    Cat(String name) { super(name); }\n    String speak() { return name + \" says Meow!\"; }\n}",
  "timeout": 45
}
```

```json title="Response"
{
  "trace_id": "java-complex-001",
  "stdout": "=== Java Complex Test ===\n\n1. Fibonacci with memoization:\n   First 15: [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]\n   Fib(50) = 12586269025\n\n2. QuickSort:\n   Input:  [64, 34, 25, 12, 22, 11, 90]\n   Output: [11, 12, 22, 25, 34, 64, 90]\n\n3. Streams and Collections:\n   Numbers: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]\n   Squares: [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]\n   Sum: 385\n\n4. Map operations:\n   Alice: 30\n   Bob: 25\n   Charlie: 35\n   Average age: 30.0\n\n5. Polymorphism:\n   Rex says Woof!\n   Whiskers says Meow!\n   Buddy says Woof!\n\n6. Optional handling:\n   Present: Hello\n   Empty: default\n\n7. String operations:\n   Original: Hello, World! Welcome to Java.\n   Upper: HELLO, WORLD! WELCOME TO JAVA.\n   Words: 5\n   Reversed: .avaJ ot emocleW !dlroW ,olleH\n\n8. Exception handling:\n   Caught: / by zero\n   Safe divide: 5\n\n=== All tests passed ===\n",
  "stderr": "",
  "exit_code": 0
}
```

## Performance

| Operation | Time |
|-----------|------|
| Compilation | ~2s |
| Hello World (total) | ~3s |
| Stream operations | ~100ms |
| JSON parsing | ~50ms |

## Limitations

:::warning

  The Java environment has the following limitations:

:::

1. **Single file only**: Must be self-contained in one Main.java
2. **No external JARs**: Only standard library available
3. **Class name must be Main**: The main class must be named Main
4. **No network**: Network operations will fail
5. **Memory limit**: 512 MiB default

## Best Practices


    Always name your main class `Main` for proper execution.



    Use full import statements for java.util, java.io, etc.



    Wrap risky operations in try-catch blocks for meaningful errors.


