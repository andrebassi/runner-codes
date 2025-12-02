---
title: 'Rust'
description: 'Rust stable code execution'
---

## Overview

Rust is ideal for maximum performance, memory safety, and production-quality code. LLM-Firecracker provides Rust stable (1.91+) with rustc compiler.

## Specifications

| Property | Value |
|----------|-------|
| Docker Image | `rust:alpine` |
| Version | Rust 1.91.1 |
| Rootfs Size | 1200 MB |
| Execution | Compiled |
| File Extension | `.rs` |
| Run Command | `rustc -o runbin {file} && ./runbin` |
| Execution Time | ~427ms |

```bash title="1. Create Rootfs from Docker"
sudo infra.operator rootfs from-docker --name rust --image rust:alpine --size 1200
```

```bash title="2. Create Snapshot"
sudo infra.operator snapshot create --lang rust --mem 512 --vcpus 1
```

```bash title="3. Upload rootfs to S3"
sudo infra.operator rootfs upload --lang rust --bucket llm-firecracker
```

```bash title="3. Upload snapshot to S3"
sudo infra.operator snapshot upload --lang rust --bucket llm-firecracker
```

```bash title="4. Test Execution"
sudo infra.operator host --lang rust --code 'fn main() {
    let x = 42;
    println!("{}", x);
}' --mem 512 --vcpus 1 --snapshot
```



## Execution Flow

![Rust Execution Flow](/img/execution-compiled.svg)

## Examples

### Hello World

```json title="Request"
{
  "trace_id": "rust-hello-001",
  "lang": "rust",
  "code": "fn main() {\n    println!(\"Hello from Rust!\");\n}",
  "timeout": 60
}
```

```json title="Response"
{
  "trace_id": "rust-hello-001",
  "stdout": "Hello from Rust!\n",
  "stderr": "",
  "exit_code": 0
}
```

### Complex Test: Full System Demo

```json title="Request"
{
  "trace_id": "rust-complex-001",
  "lang": "rust",
  "code": "use std::collections::HashMap;\n\n// Fibonacci with memoization\nfn fibonacci(n: u64, memo: &mut HashMap<u64, u64>) -> u64 {\n    if n <= 1 {\n        return n;\n    }\n    if let Some(&result) = memo.get(&n) {\n        return result;\n    }\n    let result = fibonacci(n - 1, memo) + fibonacci(n - 2, memo);\n    memo.insert(n, result);\n    result\n}\n\n// QuickSort implementation\nfn quicksort<T: Ord + Clone>(arr: &[T]) -> Vec<T> {\n    if arr.len() <= 1 {\n        return arr.to_vec();\n    }\n    let pivot = arr[0].clone();\n    let smaller: Vec<T> = arr[1..].iter().filter(|x| **x < pivot).cloned().collect();\n    let larger: Vec<T> = arr[1..].iter().filter(|x| **x >= pivot).cloned().collect();\n    let mut result = quicksort(&smaller);\n    result.push(pivot);\n    result.extend(quicksort(&larger));\n    result\n}\n\n// Person struct with traits\n#[derive(Debug, Clone)]\nstruct Person {\n    name: String,\n    age: u32,\n    city: String,\n}\n\nimpl Person {\n    fn new(name: &str, age: u32, city: &str) -> Self {\n        Person {\n            name: name.to_string(),\n            age,\n            city: city.to_string(),\n        }\n    }\n}\n\n// Generic statistics\nfn statistics(numbers: &[f64]) -> (f64, f64, f64, f64) {\n    let sum: f64 = numbers.iter().sum();\n    let mean = sum / numbers.len() as f64;\n    let min = numbers.iter().cloned().fold(f64::INFINITY, f64::min);\n    let max = numbers.iter().cloned().fold(f64::NEG_INFINITY, f64::max);\n    let variance: f64 = numbers.iter().map(|x| (x - mean).powi(2)).sum::<f64>() / numbers.len() as f64;\n    (sum, mean, min, max)\n}\n\nfn main() {\n    println!(\"=== Rust Complex Test ===\");\n    println!();\n\n    // Test 1: Fibonacci with memoization\n    println!(\"1. Fibonacci sequence:\");\n    let mut memo = HashMap::new();\n    let fibs: Vec<u64> = (0..15).map(|i| fibonacci(i, &mut memo)).collect();\n    println!(\"   First 15: {:?}\", fibs);\n    println!(\"   Fib(50) = {}\", fibonacci(50, &mut memo));\n\n    // Test 2: QuickSort\n    println!();\n    println!(\"2. QuickSort:\");\n    let unsorted = vec![64, 34, 25, 12, 22, 11, 90];\n    let sorted = quicksort(&unsorted);\n    println!(\"   Input:  {:?}\", unsorted);\n    println!(\"   Output: {:?}\", sorted);\n\n    // Test 3: Iterator chains\n    println!();\n    println!(\"3. Iterator operations on 1..10:\");\n    let numbers: Vec<i32> = (1..=10).collect();\n    let squares: Vec<i32> = numbers.iter().map(|x| x * x).collect();\n    let evens: Vec<i32> = numbers.iter().filter(|x| *x % 2 == 0).cloned().collect();\n    let sum: i32 = numbers.iter().sum();\n    let product: i64 = numbers.iter().map(|&x| x as i64).product();\n    println!(\"   Squares: {:?}\", squares);\n    println!(\"   Evens: {:?}\", evens);\n    println!(\"   Sum: {}\", sum);\n    println!(\"   Product: {}\", product);\n\n    // Test 4: Struct and collections\n    println!();\n    println!(\"4. People data processing:\");\n    let people = vec![\n        Person::new(\"Alice\", 30, \"NYC\"),\n        Person::new(\"Bob\", 25, \"LA\"),\n        Person::new(\"Charlie\", 35, \"NYC\"),\n        Person::new(\"Diana\", 28, \"LA\"),\n        Person::new(\"Eve\", 32, \"Chicago\"),\n    ];\n    \n    // Group by city\n    let mut by_city: HashMap<String, Vec<&Person>> = HashMap::new();\n    for person in &people {\n        by_city.entry(person.city.clone()).or_insert_with(Vec::new).push(person);\n    }\n    for (city, persons) in &by_city {\n        let names: Vec<&str> = persons.iter().map(|p| p.name.as_str()).collect();\n        println!(\"   {}: {:?}\", city, names);\n    }\n    \n    let avg_age: f64 = people.iter().map(|p| p.age as f64).sum::<f64>() / people.len() as f64;\n    println!(\"   Average age: {:.1}\", avg_age);\n\n    // Test 5: Option and Result\n    println!();\n    println!(\"5. Error handling:\");\n    fn safe_divide(a: f64, b: f64) -> Result<f64, String> {\n        if b == 0.0 {\n            Err(\"Division by zero\".to_string())\n        } else {\n            Ok(a / b)\n        }\n    }\n    println!(\"   10 / 2 = {:?}\", safe_divide(10.0, 2.0));\n    println!(\"   10 / 0 = {:?}\", safe_divide(10.0, 0.0));\n\n    // Test 6: Pattern matching\n    println!();\n    println!(\"6. Pattern matching:\");\n    enum Shape {\n        Circle(f64),\n        Rectangle(f64, f64),\n        Triangle(f64, f64),\n    }\n    \n    fn area(shape: &Shape) -> f64 {\n        match shape {\n            Shape::Circle(r) => std::f64::consts::PI * r * r,\n            Shape::Rectangle(w, h) => w * h,\n            Shape::Triangle(b, h) => 0.5 * b * h,\n        }\n    }\n    \n    let shapes = vec![\n        Shape::Circle(5.0),\n        Shape::Rectangle(4.0, 3.0),\n        Shape::Triangle(6.0, 4.0),\n    ];\n    for (i, shape) in shapes.iter().enumerate() {\n        println!(\"   Shape {} area: {:.2}\", i + 1, area(shape));\n    }\n\n    // Test 7: Closures and higher-order functions\n    println!();\n    println!(\"7. Closures:\");\n    let multiplier = |x: i32, y: i32| x * y;\n    let add = |x: i32| move |y: i32| x + y;\n    println!(\"   multiply(6, 7) = {}\", multiplier(6, 7));\n    println!(\"   add(5)(3) = {}\", add(5)(3));\n\n    println!();\n    println!(\"=== All tests passed ===\");\n}",
  "timeout": 90
}
```

```json title="Response"
{
  "trace_id": "rust-complex-001",
  "stdout": "=== Rust Complex Test ===\n\n1. Fibonacci sequence:\n   First 15: [0, 1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377]\n   Fib(50) = 12586269025\n\n2. QuickSort:\n   Input:  [64, 34, 25, 12, 22, 11, 90]\n   Output: [11, 12, 22, 25, 34, 64, 90]\n\n3. Iterator operations on 1..10:\n   Squares: [1, 4, 9, 16, 25, 36, 49, 64, 81, 100]\n   Evens: [2, 4, 6, 8, 10]\n   Sum: 55\n   Product: 3628800\n\n4. People data processing:\n   NYC: [\"Alice\", \"Charlie\"]\n   LA: [\"Bob\", \"Diana\"]\n   Chicago: [\"Eve\"]\n   Average age: 30.0\n\n5. Error handling:\n   10 / 2 = Ok(5.0)\n   10 / 0 = Err(\"Division by zero\")\n\n6. Pattern matching:\n   Shape 1 area: 78.54\n   Shape 2 area: 12.00\n   Shape 3 area: 12.00\n\n7. Closures:\n   multiply(6, 7) = 42\n   add(5)(3) = 8\n\n=== All tests passed ===\n",
  "stderr": "",
  "exit_code": 0
}
```

### Variables and Types

```json title="Request"
{
  "trace_id": "rust-vars-001",
  "lang": "rust",
  "code": "fn main() {\n    let name: &str = \"Alice\";\n    let age: i32 = 30;\n    let pi: f64 = 3.14159;\n    let active: bool = true;\n\n    println!(\"Name: {}\", name);\n    println!(\"Age: {}\", age);\n    println!(\"Pi: {:.2}\", pi);\n    println!(\"Active: {}\", active);\n}",
  "timeout": 60
}
```

```json title="Response"
{
  "trace_id": "rust-vars-001",
  "stdout": "Name: Alice\nAge: 30\nPi: 3.14\nActive: true\n",
  "stderr": "",
  "exit_code": 0
}
```

### Functions

```json title="Request"
{
  "trace_id": "rust-func-001",
  "lang": "rust",
  "code": "fn add(a: i32, b: i32) -> i32 {\n    a + b\n}\n\nfn factorial(n: u64) -> u64 {\n    if n <= 1 { 1 } else { n * factorial(n - 1) }\n}\n\nfn main() {\n    println!(\"Sum: {}\", add(10, 20));\n    println!(\"Factorial(5): {}\", factorial(5));\n    println!(\"Factorial(10): {}\", factorial(10));\n}",
  "timeout": 60
}
```

```json title="Response"
{
  "trace_id": "rust-func-001",
  "stdout": "Sum: 30\nFactorial(5): 120\nFactorial(10): 3628800\n",
  "stderr": "",
  "exit_code": 0
}
```

### Ownership and Borrowing

```json title="Request"
{
  "trace_id": "rust-ownership-001",
  "lang": "rust",
  "code": "fn main() {\n    // Ownership\n    let s1 = String::from(\"hello\");\n    let s2 = s1.clone();\n    println!(\"s1: {}, s2: {}\", s1, s2);\n\n    // Borrowing\n    let s3 = String::from(\"world\");\n    let len = calculate_length(&s3);\n    println!(\"Length of '{}': {}\", s3, len);\n}\n\nfn calculate_length(s: &String) -> usize {\n    s.len()\n}",
  "timeout": 60
}
```

```json title="Response"
{
  "trace_id": "rust-ownership-001",
  "stdout": "s1: hello, s2: hello\nLength of 'world': 5\n",
  "stderr": "",
  "exit_code": 0
}
```

### Vectors

```json title="Request"
{
  "trace_id": "rust-vec-001",
  "lang": "rust",
  "code": "fn main() {\n    let mut numbers = vec![1, 2, 3, 4, 5];\n    println!(\"Original: {:?}\", numbers);\n\n    // Push elements\n    numbers.push(6);\n    numbers.push(7);\n    println!(\"After push: {:?}\", numbers);\n\n    // Iterate\n    let sum: i32 = numbers.iter().sum();\n    println!(\"Sum: {}\", sum);\n\n    // Map\n    let doubled: Vec<i32> = numbers.iter().map(|x| x * 2).collect();\n    println!(\"Doubled: {:?}\", doubled);\n}",
  "timeout": 60
}
```

```json title="Response"
{
  "trace_id": "rust-vec-001",
  "stdout": "Original: [1, 2, 3, 4, 5]\nAfter push: [1, 2, 3, 4, 5, 6, 7]\nSum: 28\nDoubled: [2, 4, 6, 8, 10, 12, 14]\n",
  "stderr": "",
  "exit_code": 0
}
```

### Structs

```json title="Request"
{
  "trace_id": "rust-struct-001",
  "lang": "rust",
  "code": "#[derive(Debug)]\nstruct Person {\n    name: String,\n    age: u32,\n}\n\nimpl Person {\n    fn new(name: &str, age: u32) -> Self {\n        Person {\n            name: String::from(name),\n            age,\n        }\n    }\n\n    fn greet(&self) -> String {\n        format!(\"Hello, I'm {} and I'm {} years old\", self.name, self.age)\n    }\n}\n\nfn main() {\n    let person = Person::new(\"Alice\", 30);\n    println!(\"{:?}\", person);\n    println!(\"{}\", person.greet());\n}",
  "timeout": 60
}
```

```json title="Response"
{
  "trace_id": "rust-struct-001",
  "stdout": "Person { name: \"Alice\", age: 30 }\nHello, I'm Alice and I'm 30 years old\n",
  "stderr": "",
  "exit_code": 0
}
```

### Enums and Pattern Matching

```json title="Request"
{
  "trace_id": "rust-enum-001",
  "lang": "rust",
  "code": "enum Color {\n    Red,\n    Green,\n    Blue,\n    Rgb(u8, u8, u8),\n}\n\nfn describe_color(color: Color) -> String {\n    match color {\n        Color::Red => String::from(\"Pure red\"),\n        Color::Green => String::from(\"Pure green\"),\n        Color::Blue => String::from(\"Pure blue\"),\n        Color::Rgb(r, g, b) => format!(\"RGB({}, {}, {})\", r, g, b),\n    }\n}\n\nfn main() {\n    println!(\"{}\", describe_color(Color::Red));\n    println!(\"{}\", describe_color(Color::Rgb(255, 128, 0)));\n}",
  "timeout": 60
}
```

```json title="Response"
{
  "trace_id": "rust-enum-001",
  "stdout": "Pure red\nRGB(255, 128, 0)\n",
  "stderr": "",
  "exit_code": 0
}
```

### Option and Result

```json title="Request"
{
  "trace_id": "rust-option-001",
  "lang": "rust",
  "code": "fn divide(a: f64, b: f64) -> Option<f64> {\n    if b == 0.0 {\n        None\n    } else {\n        Some(a / b)\n    }\n}\n\nfn parse_number(s: &str) -> Result<i32, String> {\n    s.parse::<i32>()\n        .map_err(|_| format!(\"'{}' is not a valid number\", s))\n}\n\nfn main() {\n    // Option\n    match divide(10.0, 2.0) {\n        Some(result) => println!(\"10 / 2 = {}\", result),\n        None => println!(\"Cannot divide by zero\"),\n    }\n\n    match divide(10.0, 0.0) {\n        Some(result) => println!(\"10 / 0 = {}\", result),\n        None => println!(\"Cannot divide by zero\"),\n    }\n\n    // Result\n    match parse_number(\"42\") {\n        Ok(n) => println!(\"Parsed: {}\", n),\n        Err(e) => println!(\"Error: {}\", e),\n    }\n\n    match parse_number(\"abc\") {\n        Ok(n) => println!(\"Parsed: {}\", n),\n        Err(e) => println!(\"Error: {}\", e),\n    }\n}",
  "timeout": 60
}
```

```json title="Response"
{
  "trace_id": "rust-option-001",
  "stdout": "10 / 2 = 5\nCannot divide by zero\nParsed: 42\nError: 'abc' is not a valid number\n",
  "stderr": "",
  "exit_code": 0
}
```

### Iterators and Closures

```json title="Request"
{
  "trace_id": "rust-iter-001",
  "lang": "rust",
  "code": "fn main() {\n    let numbers = vec![1, 2, 3, 4, 5, 6, 7, 8, 9, 10];\n\n    // Filter and map\n    let result: Vec<i32> = numbers\n        .iter()\n        .filter(|&x| x % 2 == 0)\n        .map(|x| x * x)\n        .collect();\n\n    println!(\"Even numbers squared: {:?}\", result);\n\n    // Fold/reduce\n    let sum: i32 = numbers.iter().fold(0, |acc, x| acc + x);\n    println!(\"Sum: {}\", sum);\n\n    // Find\n    let first_gt_5 = numbers.iter().find(|&&x| x > 5);\n    println!(\"First > 5: {:?}\", first_gt_5);\n}",
  "timeout": 60
}
```

```json title="Response"
{
  "trace_id": "rust-iter-001",
  "stdout": "Even numbers squared: [4, 16, 36, 64, 100]\nSum: 55\nFirst > 5: Some(6)\n",
  "stderr": "",
  "exit_code": 0
}
```

### Traits

```json title="Request"
{
  "trace_id": "rust-trait-001",
  "lang": "rust",
  "code": "trait Shape {\n    fn area(&self) -> f64;\n    fn name(&self) -> &str;\n}\n\nstruct Circle {\n    radius: f64,\n}\n\nstruct Rectangle {\n    width: f64,\n    height: f64,\n}\n\nimpl Shape for Circle {\n    fn area(&self) -> f64 {\n        std::f64::consts::PI * self.radius * self.radius\n    }\n    fn name(&self) -> &str { \"Circle\" }\n}\n\nimpl Shape for Rectangle {\n    fn area(&self) -> f64 {\n        self.width * self.height\n    }\n    fn name(&self) -> &str { \"Rectangle\" }\n}\n\nfn print_area(shape: &dyn Shape) {\n    println!(\"{} area: {:.2}\", shape.name(), shape.area());\n}\n\nfn main() {\n    let circle = Circle { radius: 5.0 };\n    let rect = Rectangle { width: 4.0, height: 3.0 };\n\n    print_area(&circle);\n    print_area(&rect);\n}",
  "timeout": 60
}
```

```json title="Response"
{
  "trace_id": "rust-trait-001",
  "stdout": "Circle area: 78.54\nRectangle area: 12.00\n",
  "stderr": "",
  "exit_code": 0
}
```

### File Operations

```json title="Request"
{
  "trace_id": "rust-file-001",
  "lang": "rust",
  "code": "use std::fs;\n\nfn main() {\n    // Write file\n    fs::write(\"/tmp/test.txt\", \"Hello from Rust!\\nLine 2\")\n        .expect(\"Failed to write file\");\n\n    // Read file\n    let content = fs::read_to_string(\"/tmp/test.txt\")\n        .expect(\"Failed to read file\");\n\n    println!(\"Content:\");\n    println!(\"{}\", content);\n}",
  "timeout": 60
}
```

```json title="Response"
{
  "trace_id": "rust-file-001",
  "stdout": "Content:\nHello from Rust!\nLine 2\n",
  "stderr": "",
  "exit_code": 0
}
```

## Available Standard Library Modules

| Module | Description |
|--------|-------------|
| `std::collections` | HashMap, HashSet, BTreeMap, VecDeque |
| `std::fmt` | Formatting and printing |
| `std::fs` | File system operations |
| `std::io` | Input/output operations |
| `std::iter` | Iterator utilities |
| `std::str` | String operations |
| `std::vec` | Vector type |
| `std::thread` | Threading (limited use) |
| `std::time` | Time operations |

:::note

  Network modules (`std::net`) are available but won't work since the VM has no network access.

:::

## Error Examples

### Compile Error

```json title="Request"
{
  "trace_id": "rust-err-compile",
  "lang": "rust",
  "code": "fn main() {\n    let x = 10\n    println!(\"{}\", x);\n}",
  "timeout": 60
}
```

```json title="Response"
{
  "trace_id": "rust-err-compile",
  "stdout": "",
  "stderr": "error: expected `;`, found `println`\n --> main.rs:3:5\n  |\n2 |     let x = 10\n  |               - help: add `;` here\n3 |     println!(\"{}\", x);\n  |     ^^^^^^^ expected `;`\n\nerror: aborting due to previous error\n",
  "exit_code": 1
}
```

### Borrow Checker Error

```json title="Request"
{
  "trace_id": "rust-err-borrow",
  "lang": "rust",
  "code": "fn main() {\n    let s = String::from(\"hello\");\n    let r = &s;\n    drop(s);\n    println!(\"{}\", r);\n}",
  "timeout": 60
}
```

```json title="Response"
{
  "trace_id": "rust-err-borrow",
  "stdout": "",
  "stderr": "error[E0505]: cannot move out of `s` because it is borrowed\n --> main.rs:4:10\n  |\n3 |     let r = &s;\n  |             -- borrow of `s` occurs here\n4 |     drop(s);\n  |          ^ move out of `s` occurs here\n5 |     println!(\"{}\", r);\n  |                    - borrow later used here\n\nerror: aborting due to previous error\n",
  "exit_code": 1
}
```

### Runtime Panic

```json title="Request"
{
  "trace_id": "rust-err-panic",
  "lang": "rust",
  "code": "fn main() {\n    let v = vec![1, 2, 3];\n    println!(\"{}\", v[10]);\n}",
  "timeout": 60
}
```

```json title="Response"
{
  "trace_id": "rust-err-panic",
  "stdout": "",
  "stderr": "thread 'main' panicked at 'index out of bounds: the len is 3 but the index is 10', main.rs:3:20\nnote: run with `RUST_BACKTRACE=1` environment variable to display a backtrace\n",
  "exit_code": 101
}
```

## Performance

| Operation | Time |
|-----------|------|
| Compile + Hello World | ~2s |
| Fibonacci(30) | ~5ms |
| Vector operations (100k items) | ~5ms |
| String operations | ~1ms |

:::warning

  Rust compilation takes 1-2 seconds. Total execution time is compilation + runtime.

:::

## Limitations

:::warning

  The Rust environment has the following limitations:

:::

1. **No Cargo**: External crates cannot be used
2. **rustc only**: Direct compiler invocation
3. **Single file**: Code must be in a single file
4. **No network**: Network operations will fail
5. **Long compilation**: Adds ~2s to execution
6. **Large rootfs**: 2 GB image size

## Environment Variables

Rust execution uses these environment variables:

```bash title="Terminal"
HOME=/tmp
CARGO_HOME=/opt/cargo
RUSTUP_HOME=/opt/rustup
PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:/opt/cargo/bin
```

## Best Practices

**Write code using only `std` crate. No external dependencies via Cargo:**

Use `Result` and `Option` types. Avoid `unwrap()` unless you're certain.

**Add `#[derive(Debug)]` to structs for easy printing with `{:?}`.:**

Set timeout to at least 60 seconds to account for compilation time.
