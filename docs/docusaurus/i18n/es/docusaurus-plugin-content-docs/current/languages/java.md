---
title: 'Java'
description: 'Java en Runner Codes: imagen, recursos, warmup y ejemplo'
---

## Resumen

| Elemento | Valor |
|------|-------|
| nombre en la API | `java` |
| imagen OCI de origen | `eclipse-temurin:21-jdk-alpine` |
| límite de recursos por job | 2 vCPU, 1024 MiB |

El límite de vCPU y RAM existe contra el abuso; la memoria de libkrun es elástica y el host solo paga lo que el guest toca.

## Warmup

Ninguno: la imagen oficial ya trae lo necesario.

## Cómo se ejecuta el código

El archivo principal (el primero de `files`) se compila con `javac` y la clase `Main` se ejecuta con `java Main`. Por eso:

- la clase con `main` debe llamarse `Main`;
- el archivo debe llamarse `Main.java`.

Con `code` suelto en `/api/v1/execute`, el guest graba `Main.java` por sí solo. Un error de compilación devuelve `exception: "Compilation failed"`, con la salida de `javac` en `stderr`. Todo se ejecuta en un directorio en `/tmp` (tmpfs), en una VM nueva que se destruye al final del job, sin red.

## Construir la imagen

```bash
rc image build --stack java
```

## Ejemplo

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "java",
    "files": [
      {"name": "Main.java", "content": "public class Main {\n    public static void main(String[] args) {\n        System.out.println(6 * 7);\n    }\n}\n"}
    ]
  }'
```

`stdout` esperado: `42`.
