---
title: 'Bash'
description: 'Bash en Runner Codes: imagen, recursos, warmup y ejemplo'
---

## Resumen

| Elemento | Valor |
|------|-------|
| nombre en la API | `bash` |
| imagen OCI de origen | `bash:5.2` |
| límite de recursos por job | 1 vCPU, 256 MiB |

El límite de vCPU y RAM existe contra el abuso; la memoria de libkrun es elástica y el host solo paga lo que el guest toca.

## Warmup

Ninguno: la imagen oficial ya trae lo necesario.

## Cómo se ejecuta el código

El archivo principal (el primero de `files`) se ejecuta con `bash <archivo>`. Las utilidades disponibles son las de la imagen oficial; no hay red, así que `curl`, `wget` y los gestores de paquetes no alcanzan nada. Todo se ejecuta en un directorio en `/tmp` (tmpfs), en una VM nueva que se destruye al final del job.

## Construir la imagen

```bash
rc image build --stack bash
```

## Ejemplo

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "bash",
    "files": [
      {"name": "main.sh", "content": "read nome\necho \"olá, $nome\"\n"}
    ],
    "stdin": "mundo"
  }'
```

`stdout` esperado: `olá, mundo`.
