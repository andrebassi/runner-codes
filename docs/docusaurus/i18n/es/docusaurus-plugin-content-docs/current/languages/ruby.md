---
title: 'Ruby'
description: 'Ruby en Runner Codes: imagen, recursos, warmup y ejemplo'
---

## Resumen

| Elemento | Valor |
|------|-------|
| nombre en la API | `ruby` |
| imagen OCI de origen | `ruby:3.3-alpine` |
| límite de recursos por job | 1 vCPU, 512 MiB |

El límite de vCPU y RAM existe contra el abuso; la memoria de libkrun es elástica y el host solo paga lo que el guest toca.

## Warmup

Ninguno: la imagen oficial ya trae lo necesario.

## Cómo se ejecuta el código

El archivo principal (el primero de `files`) se ejecuta con `ruby <archivo>`. Los demás archivos quedan en el mismo directorio y pueden cargarse con `require_relative`. Sin red, las gems externas no se instalan. Todo se ejecuta en un directorio en `/tmp` (tmpfs), en una VM nueva que se destruye al final del job.

## Construir la imagen

```bash
rc image build --stack ruby
```

## Ejemplo

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "ruby",
    "files": [
      {"name": "main.rb", "content": "puts gets.strip.reverse\n"}
    ],
    "stdin": "abc"
  }'
```

`stdout` esperado: `cba`.
