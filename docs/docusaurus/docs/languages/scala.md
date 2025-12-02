---
title: 'Scala'
description: 'Scala code execution on JVM'
---

## Overview

Scala is a powerful language that combines object-oriented and functional programming. Runner Codes provides Scala with OpenJDK runtime.

## Specifications

| Property | Value |
|----------|-------|
| Base OS | Debian Bookworm |
| Version | Scala 2.11 |
| Rootfs Size | 900 MB |
| Execution | Compiled (scalac + scala) |
| File Extension | `.scala` |
| Compile Command | `scalac Main.scala` |
| Run Command | `scala Main` |
| Execution Time | ~3.9s |

```bash title="1. Create Rootfs with infra.operator"
sudo infra.operator rootfs create --name scala --size 900 --base debian --packages "scala"
```

```bash title="2. Create Snapshot"
sudo infra.operator snapshot create --lang scala --mem 512 --vcpus 1
```

```bash title="3. Upload rootfs to S3"
sudo infra.operator rootfs upload --lang scala --bucket runner-codes
```

```bash title="3. Upload snapshot to S3"
sudo infra.operator snapshot upload --lang scala --bucket runner-codes
```

```bash title="4. Test Execution"
sudo infra.operator host --lang scala --code "object Main { def main(args: Array[String]): Unit = { println(\"Hello from Scala\") } }" --mem 512 --vcpus 1 --snapshot
```



## Examples

### Hello World

```json title="Request"
{
  "trace_id": "scala-hello-001",
  "lang": "scala",
  "code": "object Main {\n  def main(args: Array[String]): Unit = {\n    println(\"Hello from Scala!\")\n  }\n}",
  "timeout": 45
}
```

```json title="Response"
{
  "trace_id": "scala-hello-001",
  "stdout": "Hello from Scala!\n",
  "stderr": "",
  "exit_code": 0
}
```

### Case Classes

```json title="Request"
{
  "trace_id": "scala-case-001",
  "lang": "scala",
  "code": "case class Person(name: String, age: Int)\n\nobject Main {\n  def main(args: Array[String]): Unit = {\n    val alice = Person(\"Alice\", 30)\n    println(s\"Name: ${alice.name}\")\n    println(s\"Age: ${alice.age}\")\n    println(alice)\n  }\n}",
  "timeout": 45
}
```

```json title="Response"
{
  "trace_id": "scala-case-001",
  "stdout": "Name: Alice\nAge: 30\nPerson(Alice,30)\n",
  "stderr": "",
  "exit_code": 0
}
```

### Pattern Matching

```json title="Request"
{
  "trace_id": "scala-match-001",
  "lang": "scala",
  "code": "object Main {\n  def main(args: Array[String]): Unit = {\n    val x: Any = 42\n    x match {\n      case i: Int => println(s\"Integer: $i\")\n      case s: String => println(s\"String: $s\")\n      case _ => println(\"Unknown\")\n    }\n  }\n}",
  "timeout": 45
}
```

```json title="Response"
{
  "trace_id": "scala-match-001",
  "stdout": "Integer: 42\n",
  "stderr": "",
  "exit_code": 0
}
```

### Collections

```json title="Request"
{
  "trace_id": "scala-coll-001",
  "lang": "scala",
  "code": "object Main {\n  def main(args: Array[String]): Unit = {\n    val numbers = List(1, 2, 3, 4, 5)\n    val squares = numbers.map(x => x * x)\n    val sum = squares.sum\n    \n    println(s\"Numbers: $numbers\")\n    println(s\"Squares: $squares\")\n    println(s\"Sum: $sum\")\n  }\n}",
  "timeout": 45
}
```

```json title="Response"
{
  "trace_id": "scala-coll-001",
  "stdout": "Numbers: List(1, 2, 3, 4, 5)\nSquares: List(1, 4, 9, 16, 25)\nSum: 55\n",
  "stderr": "",
  "exit_code": 0
}
```

### Higher-Order Functions

```json title="Request"
{
  "trace_id": "scala-hof-001",
  "lang": "scala",
  "code": "object Main {\n  def main(args: Array[String]): Unit = {\n    val numbers = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)\n    \n    val evens = numbers.filter(_ % 2 == 0)\n    val doubled = numbers.map(_ * 2)\n    val sum = numbers.reduce(_ + _)\n    \n    println(s\"Evens: $evens\")\n    println(s\"Doubled: $doubled\")\n    println(s\"Sum: $sum\")\n  }\n}",
  "timeout": 45
}
```

```json title="Response"
{
  "trace_id": "scala-hof-001",
  "stdout": "Evens: List(2, 4, 6, 8, 10)\nDoubled: List(2, 4, 6, 8, 10, 12, 14, 16, 18, 20)\nSum: 55\n",
  "stderr": "",
  "exit_code": 0
}
```

## Limitations

:::warning

  The Scala environment has the following limitations:

:::

1. **Compilation overhead**: scalac is slow (~3-4s)
2. **No external libraries**: Only Scala standard library
3. **Memory limit**: 512 MiB
4. **Increase timeout**: Use 45+ seconds
5. **Main object required**: Must have `object Main` with `def main(args: Array[String])`

## Best Practices


    Always name your main object `Main` for proper execution.



    Case classes provide equals, hashCode, toString automatically.



    Scala compilation is slow; use 45+ second timeouts.


