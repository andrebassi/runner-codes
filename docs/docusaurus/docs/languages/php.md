---
title: 'PHP'
description: 'PHP code execution'
---

## Overview

PHP is a popular server-side scripting language. LLM-Firecracker provides PHP CLI for executing PHP scripts.

## Specifications

| Property | Value |
|----------|-------|
| Docker Image | `php:8.3-cli-alpine` |
| Version | PHP 8.3.28 |
| Rootfs Size | 200 MB |
| Execution | Interpreted |
| File Extension | `.php` |
| Run Command | `php {file}` |
| Execution Time | ~50ms |

```bash title="1. Create Rootfs from Docker"
sudo infra.operator rootfs from-docker --name php --image php:8.3-cli-alpine --size 200
```

```bash title="2. Create Snapshot"
sudo infra.operator snapshot create --lang php --mem 512 --vcpus 1
```

```bash title="3. Upload rootfs to S3"
sudo infra.operator rootfs upload --lang php --bucket llm-firecracker
```

```bash title="3. Upload snapshot to S3"
sudo infra.operator snapshot upload --lang php --bucket llm-firecracker
```

```bash title="4. Test Execution"
sudo infra.operator host --lang php --code '<?php echo "Hello from PHP " . PHP_VERSION . "\n"; ?>' --mem 512 --vcpus 1 --snapshot
```



## Execution Flow

![PHP Execution Flow](/img/language-execution-flow.svg)

## Examples

### Hello World

```json title="Request"
{
  "trace_id": "php-hello-001",
  "lang": "php",
  "code": "<?php\necho \"Hello from PHP!\\n\";",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "php-hello-001",
  "stdout": "Hello from PHP!\n",
  "stderr": "",
  "exit_code": 0
}
```

### Variables and Arrays

```json title="Request"
{
  "trace_id": "php-array-001",
  "lang": "php",
  "code": "<?php\n$numbers = [1, 2, 3, 4, 5];\n$sum = array_sum($numbers);\necho \"Numbers: \" . implode(', ', $numbers) . \"\\n\";\necho \"Sum: $sum\\n\";",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "php-array-001",
  "stdout": "Numbers: 1, 2, 3, 4, 5\nSum: 15\n",
  "stderr": "",
  "exit_code": 0
}
```

### Classes and Objects

```json title="Request"
{
  "trace_id": "php-class-001",
  "lang": "php",
  "code": "<?php\nclass Calculator {\n    private int $result = 0;\n    \n    public function add(int $x): self {\n        $this->result += $x;\n        return $this;\n    }\n    \n    public function multiply(int $x): self {\n        $this->result *= $x;\n        return $this;\n    }\n    \n    public function getResult(): int {\n        return $this->result;\n    }\n}\n\n$calc = new Calculator();\n$result = $calc->add(5)->multiply(3)->add(10)->getResult();\necho \"Result: $result\\n\";",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "php-class-001",
  "stdout": "Result: 25\n",
  "stderr": "",
  "exit_code": 0
}
```

### Associative Arrays

```json title="Request"
{
  "trace_id": "php-assoc-001",
  "lang": "php",
  "code": "<?php\n$user = [\n    'name' => 'Alice',\n    'age' => 30,\n    'city' => 'NYC'\n];\n\nforeach ($user as $key => $value) {\n    echo \"$key: $value\\n\";\n}",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "php-assoc-001",
  "stdout": "name: Alice\nage: 30\ncity: NYC\n",
  "stderr": "",
  "exit_code": 0
}
```

### JSON Operations

```json title="Request"
{
  "trace_id": "php-json-001",
  "lang": "php",
  "code": "<?php\n$data = ['name' => 'Alice', 'scores' => [85, 90, 92]];\n$json = json_encode($data, JSON_PRETTY_PRINT);\necho $json . \"\\n\";\n\n$parsed = json_decode($json, true);\necho \"Name: \" . $parsed['name'] . \"\\n\";\necho \"Average: \" . (array_sum($parsed['scores']) / count($parsed['scores'])) . \"\\n\";",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "php-json-001",
  "stdout": "{\n    \"name\": \"Alice\",\n    \"scores\": [\n        85,\n        90,\n        92\n    ]\n}\nName: Alice\nAverage: 89\n",
  "stderr": "",
  "exit_code": 0
}
```

### Functions and Closures

```json title="Request"
{
  "trace_id": "php-func-001",
  "lang": "php",
  "code": "<?php\nfunction fibonacci(int $n): int {\n    if ($n <= 1) return $n;\n    return fibonacci($n - 1) + fibonacci($n - 2);\n}\n\n$numbers = range(0, 9);\n$fibs = array_map(fn($n) => fibonacci($n), $numbers);\n\nforeach ($fibs as $i => $f) {\n    echo \"F($i) = $f\\n\";\n}",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "php-func-001",
  "stdout": "F(0) = 0\nF(1) = 1\nF(2) = 1\nF(3) = 2\nF(4) = 3\nF(5) = 5\nF(6) = 8\nF(7) = 13\nF(8) = 21\nF(9) = 34\n",
  "stderr": "",
  "exit_code": 0
}
```

### Complex Test: Comprehensive PHP

```json title="Request"
{
  "trace_id": "php-complex-001",
  "lang": "php",
  "code": "<?php\necho \"=== PHP Complex Test ===\\n\\n\";\n\n// Test 1: Fibonacci with memoization\necho \"1. Fibonacci with memoization:\\n\";\n$memo = [];\nfunction fib(int $n): int {\n    global $memo;\n    if (isset($memo[$n])) return $memo[$n];\n    if ($n <= 1) return $n;\n    $memo[$n] = fib($n - 1) + fib($n - 2);\n    return $memo[$n];\n}\n$fibs = array_map('fib', range(0, 14));\necho \"   First 15: [\" . implode(', ', $fibs) . \"]\\n\";\necho \"   Fib(40) = \" . fib(40) . \"\\n\";\n\n// Test 2: QuickSort\necho \"\\n2. QuickSort:\\n\";\nfunction quicksort(array $arr): array {\n    if (count($arr) <= 1) return $arr;\n    $pivot = $arr[floor(count($arr) / 2)];\n    $left = array_filter($arr, fn($x) => $x < $pivot);\n    $middle = array_filter($arr, fn($x) => $x === $pivot);\n    $right = array_filter($arr, fn($x) => $x > $pivot);\n    return array_merge(quicksort(array_values($left)), array_values($middle), quicksort(array_values($right)));\n}\n$unsorted = [64, 34, 25, 12, 22, 11, 90];\necho \"   Input:  [\" . implode(', ', $unsorted) . \"]\\n\";\necho \"   Output: [\" . implode(', ', quicksort($unsorted)) . \"]\\n\";\n\n// Test 3: Classes and interfaces\necho \"\\n3. Classes and interfaces:\\n\";\ninterface Speakable {\n    public function speak(): string;\n}\n\nabstract class Animal implements Speakable {\n    protected string $name;\n    public function __construct(string $name) {\n        $this->name = $name;\n    }\n}\n\nclass Dog extends Animal {\n    public function speak(): string {\n        return \"{$this->name} says Woof!\";\n    }\n}\n\nclass Cat extends Animal {\n    public function speak(): string {\n        return \"{$this->name} says Meow!\";\n    }\n}\n\n$animals = [new Dog('Rex'), new Cat('Whiskers'), new Dog('Buddy')];\nforeach ($animals as $animal) {\n    echo \"   \" . $animal->speak() . \"\\n\";\n}\n\n// Test 4: Array functions\necho \"\\n4. Array functions:\\n\";\n$numbers = range(1, 10);\n$squares = array_map(fn($x) => $x * $x, $numbers);\n$evens = array_filter($numbers, fn($x) => $x % 2 === 0);\n$sum = array_reduce($squares, fn($a, $b) => $a + $b, 0);\necho \"   Numbers: [\" . implode(', ', $numbers) . \"]\\n\";\necho \"   Squares: [\" . implode(', ', $squares) . \"]\\n\";\necho \"   Evens: [\" . implode(', ', $evens) . \"]\\n\";\necho \"   Sum of squares: $sum\\n\";\n\n// Test 5: Associative arrays and objects\necho \"\\n5. Associative arrays:\\n\";\n$people = [\n    ['name' => 'Alice', 'age' => 30, 'city' => 'NYC'],\n    ['name' => 'Bob', 'age' => 25, 'city' => 'LA'],\n    ['name' => 'Charlie', 'age' => 35, 'city' => 'Chicago']\n];\n$byCity = array_column($people, 'name', 'city');\nforeach ($people as $p) {\n    echo \"   {$p['name']} ({$p['age']}) from {$p['city']}\\n\";\n}\necho \"   By city: \" . json_encode($byCity) . \"\\n\";\n\n// Test 6: Closures and callbacks\necho \"\\n6. Closures:\\n\";\n$multiplier = fn($factor) => fn($x) => $x * $factor;\n$double = $multiplier(2);\n$triple = $multiplier(3);\necho \"   double(5) = \" . $double(5) . \"\\n\";\necho \"   triple(5) = \" . $triple(5) . \"\\n\";\n\n// Test 7: String operations\necho \"\\n7. String operations:\\n\";\n$text = 'Hello, World! Welcome to PHP.';\necho \"   Original: $text\\n\";\necho \"   Upper: \" . strtoupper($text) . \"\\n\";\necho \"   Words: \" . str_word_count($text) . \"\\n\";\necho \"   Reversed: \" . strrev($text) . \"\\n\";\n\n// Test 8: JSON and error handling\necho \"\\n8. JSON and error handling:\\n\";\n$data = ['users' => [['id' => 1, 'name' => 'Alice'], ['id' => 2, 'name' => 'Bob']]];\n$json = json_encode($data, JSON_PRETTY_PRINT);\necho \"   JSON:\\n$json\\n\";\ntry {\n    $result = 10 / 0;\n} catch (DivisionByZeroError $e) {\n    echo \"   Caught: Division by zero\\n\";\n}\n\necho \"\\n=== All tests passed ===\\n\";",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "php-complex-001",
  "stdout": "=== PHP Complex Test ===\n\n1. Fibonacci with memoization:\n   First 15: [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]\n   Fib(40) = 102334155\n\n2. QuickSort:\n   Input:  [64, 34, 25, 12, 22, 11, 90]\n   Output: [11, 12, 22, 25, 34, 64, 90]\n\n3. Classes and interfaces:\n   Rex says Woof!\n   Whiskers says Meow!\n   Buddy says Woof!\n\n4. Array functions:\n   Numbers: [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]\n   Squares: [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]\n   Evens: [2, 4, 6, 8, 10]\n   Sum of squares: 385\n\n5. Associative arrays:\n   Alice (30) from NYC\n   Bob (25) from LA\n   Charlie (35) from Chicago\n   By city: {\"NYC\":\"Alice\",\"LA\":\"Bob\",\"Chicago\":\"Charlie\"}\n\n6. Closures:\n   double(5) = 10\n   triple(5) = 15\n\n7. String operations:\n   Original: Hello, World! Welcome to PHP.\n   Upper: HELLO, WORLD! WELCOME TO PHP.\n   Words: 5\n   Reversed: .PHP ot emocleW !dlroW ,olleH\n\n8. JSON and error handling:\n   JSON:\n{\n    \"users\": [\n        {\n            \"id\": 1,\n            \"name\": \"Alice\"\n        },\n        {\n            \"id\": 2,\n            \"name\": \"Bob\"\n        }\n    ]\n}\n   Caught: Division by zero\n\n=== All tests passed ===\n",
  "stderr": "",
  "exit_code": 0
}
```

## Performance

| Operation | Time |
|-----------|------|
| Hello World | ~50ms |
| Array operations | ~10ms |
| JSON encode/decode | ~5ms |
| Fibonacci(25) | ~100ms |

## Limitations

:::warning

  The PHP environment has the following limitations:

:::

1. **No Composer packages**: Only built-in extensions
2. **No network**: cURL/HTTP operations will fail
3. **No database**: No MySQL/PostgreSQL connections
4. **Memory limit**: 512 MiB
5. **CLI only**: No web server context

## Best Practices


    All PHP code must begin with the opening PHP tag.



    Leverage PHP 8's type system for safer code.



    All output must go through echo/print to stdout.


