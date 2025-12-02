---
title: 'Elixir'
description: 'Elixir functional programming on BEAM'
---

## Overview

Elixir is a dynamic, functional language built on the Erlang VM (BEAM). LLM-Firecracker provides Elixir for script execution.

## Specifications

| Property | Value |
|----------|-------|
| Docker Image | `elixir:1.18-alpine` |
| Version | Elixir 1.18.4 |
| Rootfs Size | 250 MB |
| Execution | Interpreted (elixir) |
| File Extension | `.ex` |
| Run Command | `elixir {file}` |
| Execution Time | ~5.5s |

```bash title="1. Create Rootfs from Docker"
sudo infra.operator rootfs from-docker --name elixir --image elixir:1.18-alpine --size 250
```

```bash title="2. Create Snapshot"
sudo infra.operator snapshot create --lang elixir --mem 512 --vcpus 1
```

```bash title="3. Upload rootfs to S3"
sudo infra.operator rootfs upload --lang elixir --bucket llm-firecracker
```

```bash title="3. Upload snapshot to S3"
sudo infra.operator snapshot upload --lang elixir --bucket llm-firecracker
```

```bash title="4. Test Execution"
sudo infra.operator host --lang elixir --code 'IO.puts("Hello from Elixir #{System.version()}")' --mem 512 --vcpus 1 --snapshot
```



## Examples

### Hello World

```json title="Request"
{
  "trace_id": "ex-hello-001",
  "lang": "elixir",
  "code": "IO.puts(\"Hello from Elixir!\")",
  "timeout": 15
}
```

```json title="Response"
{
  "trace_id": "ex-hello-001",
  "stdout": "Hello from Elixir!\n",
  "stderr": "",
  "exit_code": 0
}
```

### Complex Test: Functional Pipeline Processing

```json title="Request"
{
  "trace_id": "ex-complex-001",
  "lang": "elixir",
  "code": "defmodule DataProcessor do\n  # Fibonacci with pattern matching\n  def fib(0), do: 0\n  def fib(1), do: 1\n  def fib(n) when n > 1, do: fib(n - 1) + fib(n - 2)\n\n  # QuickSort with guards\n  def quicksort([]), do: []\n  def quicksort([pivot | rest]) do\n    smaller = Enum.filter(rest, &(&1 < pivot))\n    larger = Enum.filter(rest, &(&1 >= pivot))\n    quicksort(smaller) ++ [pivot] ++ quicksort(larger)\n  end\n\n  # Process a list of people\n  def process_people(people) do\n    people\n    |> Enum.group_by(& &1.city)\n    |> Enum.map(fn {city, persons} ->\n      avg_age = persons |> Enum.map(& &1.age) |> Enum.sum() |> div(length(persons))\n      {city, length(persons), avg_age}\n    end)\n    |> Enum.sort_by(&elem(&1, 1), :desc)\n  end\nend\n\n# Main execution\nIO.puts(\"=== Elixir Complex Test ===\")\n\n# Test 1: Fibonacci\nIO.puts(\"\\n1. Fibonacci sequence:\")\nfibs = Enum.map(0..14, &DataProcessor.fib/1)\nIO.puts(\"   First 15: #{inspect(fibs)}\")\nIO.puts(\"   Fib(25) = #{DataProcessor.fib(25)}\")\n\n# Test 2: QuickSort\nIO.puts(\"\\n2. QuickSort:\")\nunsorted = [64, 34, 25, 12, 22, 11, 90]\nsorted = DataProcessor.quicksort(unsorted)\nIO.puts(\"   Input:  #{inspect(unsorted)}\")\nIO.puts(\"   Output: #{inspect(sorted)}\")\n\n# Test 3: Pipeline operations\nIO.puts(\"\\n3. Pipeline operations on 1..10:\")\nnumbers = 1..10 |> Enum.to_list()\n\nsquares = numbers |> Enum.map(&(&1 * &1))\nevens = numbers |> Enum.filter(&(rem(&1, 2) == 0))\nsum = numbers |> Enum.reduce(0, &+/2)\nproduct = numbers |> Enum.reduce(1, &*/2)\n\nIO.puts(\"   Squares: #{inspect(squares)}\")\nIO.puts(\"   Evens: #{inspect(evens)}\")\nIO.puts(\"   Sum: #{sum}\")\nIO.puts(\"   Product: #{product}\")\n\n# Test 4: Data processing with maps\nIO.puts(\"\\n4. People data processing:\")\npeople = [\n  %{name: \"Alice\", age: 30, city: \"NYC\"},\n  %{name: \"Bob\", age: 25, city: \"LA\"},\n  %{name: \"Charlie\", age: 35, city: \"NYC\"},\n  %{name: \"Diana\", age: 28, city: \"LA\"},\n  %{name: \"Eve\", age: 32, city: \"Chicago\"}\n]\n\nresults = DataProcessor.process_people(people)\nEnum.each(results, fn {city, count, avg} ->\n  IO.puts(\"   #{city}: #{count} people, avg age #{avg}\")\nend)\n\n# Test 5: Pattern matching and guards\nIO.puts(\"\\n5. Pattern matching:\")\ncheck_value = fn\n  x when is_integer(x) and x > 0 -> \"positive integer\"\n  x when is_integer(x) and x < 0 -> \"negative integer\"\n  0 -> \"zero\"\n  x when is_float(x) -> \"float: #{x}\"\n  x when is_binary(x) -> \"string: #{x}\"\n  _ -> \"unknown\"\nend\n\nIO.puts(\"   42 -> #{check_value.(42)}\")\nIO.puts(\"   -5 -> #{check_value.(-5)}\")\nIO.puts(\"   3.14 -> #{check_value.(3.14)}\")\nIO.puts(\"   'hello' -> #{check_value.(\"hello\")}\")\n\n# Test 6: Comprehensions\nIO.puts(\"\\n6. Comprehensions:\")\npairs = for x <- 1..3, y <- [:a, :b], do: {x, y}\nIO.puts(\"   Cartesian: #{inspect(pairs)}\")\n\npythagorean = for a <- 1..10, b <- 1..10, c <- 1..10,\n                  a + b + c <= 20,\n                  a*a + b*b == c*c,\n                  a <= b,\n                  do: {a, b, c}\nIO.puts(\"   Pythagorean triples (sum<=20): #{inspect(pythagorean)}\")\n\nIO.puts(\"\\n=== All tests passed ===\")",
  "timeout": 45
}
```

```json title="Response"
{
  "trace_id": "ex-complex-001",
  "stdout": "=== Elixir Complex Test ===\n\n1. Fibonacci sequence:\n   First 15: [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]\n   Fib(25) = 75025\n\n2. QuickSort:\n   Input:  [64, 34, 25, 12, 22, 11, 90]\n   Output: [11, 12, 22, 25, 34, 64, 90]\n\n3. Pipeline operations on 1..10:\n   Squares: [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]\n   Evens: [2, 4, 6, 8, 10]\n   Sum: 55\n   Product: 3628800\n\n4. People data processing:\n   NYC: 2 people, avg age 32\n   LA: 2 people, avg age 26\n   Chicago: 1 people, avg age 32\n\n5. Pattern matching:\n   42 -> positive integer\n   -5 -> negative integer\n   3.14 -> float: 3.14\n   'hello' -> string: hello\n\n6. Comprehensions:\n   Cartesian: [{1, :a}, {1, :b}, {2, :a}, {2, :b}, {3, :a}, {3, :b}]\n   Pythagorean triples (sum<=20): [{3, 4, 5}]\n\n=== All tests passed ===\n",
  "stderr": "",
  "exit_code": 0
}
```

## Limitations

:::warning

  - No Mix projects or dependencies
  - Script mode only
  - Memory limit: 512 MiB

:::
