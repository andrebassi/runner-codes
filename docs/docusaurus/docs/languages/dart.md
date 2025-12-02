---
title: 'Dart'
description: 'Dart code execution with Dart SDK'
---

## Overview

Dart is a programming language developed by Google, optimized for building user interfaces. It's the official language for the Flutter framework.

## Specifications

| Property | Value |
|----------|-------|
| Docker Image | `dart:stable` |
| Version | Dart 3.10.2 |
| Rootfs Size | 1000 MB |
| Execution | JIT compiled |
| File Extension | `.dart` |
| Run Command | `dart run {file}` |
| Execution Time | ~433ms |

```bash title="1. Create Rootfs from Docker"
sudo infra.operator rootfs from-docker --name dart --image dart:stable --size 1000
```

```bash title="2. Create Snapshot"
sudo infra.operator snapshot create --lang dart --mem 512 --vcpus 1
```

```bash title="3. Upload rootfs to S3"
sudo infra.operator rootfs upload --lang dart --bucket runner-codes
```

```bash title="3. Upload snapshot to S3"
sudo infra.operator snapshot upload --lang dart --bucket runner-codes
```

```bash title="4. Test Execution"
sudo infra.operator host --lang dart --code 'import "dart:io"; void main() { print("Hello from Dart ${Platform.version}"); }' --mem 512 --vcpus 1 --snapshot
```



## Examples

### Hello World
```dart title="Simple main function with print"
void main() {
  print("Hello World");
}
```

### Variaveis
```dart title="Variable types and const declarations"
void main() {
  var name = "Dart";  // Tipo inferido
  String language = "Dart";  // Tipo explicito
  final version = 3.2;  // Constante runtime
  const pi = 3.14159;  // Constante compile-time

  print("$name version $version");
}
```

### Funcoes
```dart title="Functions with named and optional parameters"
int add(int a, int b) => a + b;

String greet({required String name, int age = 0}) {
  return "Hello $name, age $age";
}

void main() {
  print(add(2, 3));
  print(greet(name: "Alice", age: 30));
}
```

### Classes
```dart title="Class with constructor and methods"
class Person {
  String name;
  int age;

  Person(this.name, this.age);

  void introduce() {
    print("Hi, I'm $name and I'm $age years old");
  }
}

void main() {
  var person = Person("Bob", 25);
  person.introduce();
}
```

### Async/Await
```dart title="Async function with Future"
Future<String> fetchData() async {
  await Future.delayed(Duration(milliseconds: 100));
  return "Data loaded!";
}

void main() async {
  var data = await fetchData();
  print(data);
}
```

## Limitacoes

- Sem Flutter/UI frameworks
- Sem acesso a rede (http package bloqueado)
- Timeout padrao de 10 segundos
- Memoria limitada a 512MB
