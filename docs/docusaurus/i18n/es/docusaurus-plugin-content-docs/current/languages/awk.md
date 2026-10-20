---
title: 'AWK'
description: 'AWK en Runner Codes: imagen, recursos, warmup y ejemplo'
---

## Resumen

| Elemento | Valor |
|------|-------|
| nombre en la API | `awk` |
| imagen OCI de origen | `alpine:3.22` |
| límite de recursos por job | 1 vCPU, 256 MiB |

El límite de vCPU y RAM existe contra el abuso; la memoria de libkrun es elástica y el host solo paga lo que el guest toca.

## Warmup

Ninguno: la imagen base ya trae `awk`.

## Cómo se ejecuta el código

El archivo principal (el primero de `files`) es el programa AWK, ejecutado con `awk -f <archivo>`. El stdin del job es la entrada que el programa procesa, línea por línea. El `awk` es el de la imagen Alpine (BusyBox). Todo se ejecuta en un directorio en `/tmp` (tmpfs), en una VM nueva que se destruye al final del job, sin red.

## Construir la imagen

```bash
rc image build --stack awk
```

## Ejemplo

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "awk",
    "files": [
      {"name": "main.awk", "content": "{ soma += $1 } END { print soma }\n"}
    ],
    "stdin": "10\n20\n12\n"
  }'
```

`stdout` esperado: `42`.
