---
title: 'PHP'
description: 'PHP en Runner Codes: imagen, recursos, warmup y ejemplo'
---

## Resumen

| Elemento | Valor |
|------|-------|
| nombre en la API | `php` |
| imagen OCI de origen | `php:8.3-cli-alpine` |
| límite de recursos por job | 1 vCPU, 512 MiB |

El límite de vCPU y RAM existe contra el abuso; la memoria de libkrun es elástica y el host solo paga lo que el guest toca.

## Warmup

Ninguno: la imagen oficial ya trae lo necesario.

## Cómo se ejecuta el código

El archivo principal (el primero de `files`) se ejecuta con `php <archivo>` (CLI). Los demás archivos quedan en el mismo directorio y pueden incluirse con `require`. Todo se ejecuta en un directorio en `/tmp` (tmpfs), en una VM nueva que se destruye al final del job, sin red.

## Construir la imagen

```bash
rc image build --stack php
```

## Ejemplo

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "php",
    "files": [
      {"name": "main.php", "content": "<?php\necho strtoupper(trim(fgets(STDIN))), PHP_EOL;\n"}
    ],
    "stdin": "abc"
  }'
```

`stdout` esperado: `ABC`.
