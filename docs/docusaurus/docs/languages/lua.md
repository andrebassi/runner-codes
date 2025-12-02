---
title: 'Lua'
description: 'Lua lightweight scripting'
---

## Overview

Lua is a lightweight, embeddable scripting language. Runner Codes provides Lua 5.4 for script execution.

## Specifications

| Property | Value |
|----------|-------|
| Docker Image | `nickblah/lua:5.4-alpine` |
| Version | Lua 5.4 |
| Rootfs Size | 100 MB |
| Execution | Interpreted |
| File Extension | `.lua` |
| Run Command | `lua {file}` |
| Execution Time | ~4ms |

```bash title="1. Create Rootfs from Docker"
sudo infra.operator rootfs from-docker --name lua --image nickblah/lua:5.4-alpine --size 100
```

```bash title="2. Create Snapshot"
sudo infra.operator snapshot create --lang lua --mem 512 --vcpus 1
```

```bash title="3. Upload rootfs to S3"
sudo infra.operator rootfs upload --lang lua --bucket runner-codes
```

```bash title="3. Upload snapshot to S3"
sudo infra.operator snapshot upload --lang lua --bucket runner-codes
```

```bash title="4. Test Execution"
sudo infra.operator host --lang lua --code 'print("Hello from Lua " .. _VERSION)' --mem 512 --vcpus 1 --snapshot
```



## Examples

### Hello World

```json title="Request"
{
  "trace_id": "lua-hello-001",
  "lang": "lua",
  "code": "print(\"Hello from Lua!\")",
  "timeout": 10
}
```

```json title="Response"
{
  "trace_id": "lua-hello-001",
  "stdout": "Hello from Lua!\n",
  "stderr": "",
  "exit_code": 0
}
```

### Complex Test: Tables and Metatables

```json title="Request"
{
  "trace_id": "lua-complex-001",
  "lang": "lua",
  "code": "print(\"=== Lua Complex Test ===\")\n\n-- Test 1: Fibonacci\nprint(\"\\n1. Fibonacci sequence:\")\nlocal function fib(n)\n    if n <= 1 then return n end\n    return fib(n-1) + fib(n-2)\nend\n\nlocal fibs = {}\nfor i = 0, 14 do\n    fibs[#fibs + 1] = fib(i)\nend\nprint(\"   First 15: \" .. table.concat(fibs, \", \"))\nprint(\"   Fib(25) = \" .. fib(25))\n\n-- Test 2: QuickSort\nprint(\"\\n2. QuickSort:\")\nlocal function quicksort(arr)\n    if #arr <= 1 then return arr end\n    local pivot = arr[1]\n    local smaller, larger = {}, {}\n    for i = 2, #arr do\n        if arr[i] < pivot then\n            smaller[#smaller + 1] = arr[i]\n        else\n            larger[#larger + 1] = arr[i]\n        end\n    end\n    local result = quicksort(smaller)\n    result[#result + 1] = pivot\n    for _, v in ipairs(quicksort(larger)) do\n        result[#result + 1] = v\n    end\n    return result\nend\n\nlocal unsorted = {64, 34, 25, 12, 22, 11, 90}\nlocal sorted = quicksort(unsorted)\nprint(\"   Input:  \" .. table.concat(unsorted, \", \"))\nprint(\"   Output: \" .. table.concat(sorted, \", \"))\n\n-- Test 3: Tables as objects\nprint(\"\\n3. Object-oriented with metatables:\")\nlocal Person = {}\nPerson.__index = Person\n\nfunction Person.new(name, age)\n    local self = setmetatable({}, Person)\n    self.name = name\n    self.age = age\n    return self\nend\n\nfunction Person:greet()\n    return string.format(\"Hello, I'm %s, %d years old\", self.name, self.age)\nend\n\nlocal alice = Person.new(\"Alice\", 30)\nlocal bob = Person.new(\"Bob\", 25)\nprint(\"   \" .. alice:greet())\nprint(\"   \" .. bob:greet())\n\n-- Test 4: Higher-order functions\nprint(\"\\n4. Higher-order functions:\")\nlocal function map(arr, fn)\n    local result = {}\n    for i, v in ipairs(arr) do\n        result[i] = fn(v)\n    end\n    return result\nend\n\nlocal function filter(arr, pred)\n    local result = {}\n    for _, v in ipairs(arr) do\n        if pred(v) then\n            result[#result + 1] = v\n        end\n    end\n    return result\nend\n\nlocal function reduce(arr, fn, init)\n    local acc = init\n    for _, v in ipairs(arr) do\n        acc = fn(acc, v)\n    end\n    return acc\nend\n\nlocal numbers = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10}\nlocal squares = map(numbers, function(x) return x * x end)\nlocal evens = filter(numbers, function(x) return x % 2 == 0 end)\nlocal sum = reduce(numbers, function(a, b) return a + b end, 0)\n\nprint(\"   Squares: \" .. table.concat(squares, \", \"))\nprint(\"   Evens: \" .. table.concat(evens, \", \"))\nprint(\"   Sum: \" .. sum)\n\n-- Test 5: Coroutines\nprint(\"\\n5. Coroutines:\")\nlocal function counter(max)\n    return coroutine.create(function()\n        for i = 1, max do\n            coroutine.yield(i)\n        end\n    end)\nend\n\nlocal co = counter(5)\nlocal values = {}\nwhile true do\n    local status, value = coroutine.resume(co)\n    if not value then break end\n    values[#values + 1] = value\nend\nprint(\"   Counter values: \" .. table.concat(values, \", \"))\n\n-- Test 6: String manipulation\nprint(\"\\n6. String operations:\")\nlocal text = \"Hello World from Lua\"\nprint(\"   Original: \" .. text)\nprint(\"   Upper: \" .. string.upper(text))\nprint(\"   Length: \" .. #text)\nprint(\"   Words: \" .. select(2, text:gsub(\"%S+\", \"\")))\n\nprint(\"\\n=== All tests passed ===\")",
  "timeout": 30
}
```

```json title="Response"
{
  "trace_id": "lua-complex-001",
  "stdout": "=== Lua Complex Test ===\n\n1. Fibonacci sequence:\n   First 15: 0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377\n   Fib(25) = 75025\n\n2. QuickSort:\n   Input:  64, 34, 25, 12, 22, 11, 90\n   Output: 11, 12, 22, 25, 34, 64, 90\n\n3. Object-oriented with metatables:\n   Hello, I'm Alice, 30 years old\n   Hello, I'm Bob, 25 years old\n\n4. Higher-order functions:\n   Squares: 1, 4, 9, 16, 25, 36, 49, 64, 81, 100\n   Evens: 2, 4, 6, 8, 10\n   Sum: 55\n\n5. Coroutines:\n   Counter values: 1, 2, 3, 4, 5\n\n6. String operations:\n   Original: Hello World from Lua\n   Upper: HELLO WORLD FROM LUA\n   Length: 20\n   Words: 4\n\n=== All tests passed ===\n",
  "stderr": "",
  "exit_code": 0
}
```

## Limitations

:::warning

  - Standard library only
  - No LuaRocks packages
  - Memory limit: 512 MiB

:::
