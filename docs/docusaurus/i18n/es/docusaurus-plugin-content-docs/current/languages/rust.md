---
title: 'Rust'
description: 'Rust en Runner Codes: imagen, recursos, warmup y ejemplo'
---

## Resumen

| Elemento | Valor |
|------|-------|
| nombre en la API | `rust` |
| imagen OCI de origen | `rust:1-alpine` |
| límite de recursos por job | 2 vCPU, 1024 MiB |

El límite de vCPU y RAM existe contra el abuso; la memoria de libkrun es elástica y el host solo paga lo que el guest toca.

## Warmup

Ninguno: la imagen oficial ya trae lo necesario.

## Cómo se ejecuta el código

El archivo principal (el primero de `files`) se compila con `rustc -o <binario> <archivo>` y el binario se ejecuta con el stdin del job. Un error de compilación devuelve `exception: "Compilation failed"`, con la salida de `rustc` en `stderr`. Sin red, los crates externos no están disponibles. Todo se ejecuta en un directorio en `/tmp` (tmpfs), en una VM nueva que se destruye al final del job.

## Construir la imagen

```bash
rc image build --stack rust
```

## Ejemplo

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "rust",
    "files": [
      {"name": "main.rs", "content": "fn main() {\n    println!(\"{}\", 6 * 7);\n}\n"}
    ]
  }'
```

`stdout` esperado: `42`.
