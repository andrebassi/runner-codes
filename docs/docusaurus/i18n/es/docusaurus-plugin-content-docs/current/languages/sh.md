---
title: 'sh'
description: 'Shell POSIX (sh) en Runner Codes: imagen, recursos, warmup y ejemplo'
---

## Resumen

| Elemento | Valor |
|------|-------|
| nombre en la API | `sh` |
| imagen OCI de origen | `alpine:3.22` |
| límite de recursos por job | 1 vCPU, 256 MiB |

El límite de vCPU y RAM existe contra el abuso; la memoria de libkrun es elástica y el host solo paga lo que el guest toca.

## Warmup

Ninguno: la imagen base ya trae `sh`.

## Cómo se ejecuta el código

El archivo principal (el primero de `files`) se ejecuta con `sh <archivo>`: el shell de la imagen Alpine (BusyBox). Para funciones propias de Bash (arrays, `[[ ]]`), use el lenguaje [`bash`](/languages/bash). Todo se ejecuta en un directorio en `/tmp` (tmpfs), en una VM nueva que se destruye al final del job, sin red.

## Construir la imagen

```bash
rc image build --stack sh
```

## Ejemplo

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "sh",
    "files": [
      {"name": "main.sh", "content": "read n\necho $((n * 2))\n"}
    ],
    "stdin": "21"
  }'
```

`stdout` esperado: `42`.
