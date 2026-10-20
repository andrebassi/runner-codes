---
title: 'Node.js'
description: 'Node.js en Runner Codes: imagen, recursos, warmup y ejemplo'
---

## Resumen

| Elemento | Valor |
|------|-------|
| nombre en la API | `nodejs` |
| imagen OCI de origen | `node:22-alpine` |
| límite de recursos por job | 1 vCPU, 512 MiB |

El límite de vCPU y RAM existe contra el abuso; la memoria de libkrun es elástica y el host solo paga lo que el guest toca.

## Warmup

Ninguno: la imagen oficial ya trae lo necesario.

## Cómo se ejecuta el código

El archivo principal (el primero de `files`) se ejecuta con `node <archivo>`. Los demás archivos quedan en el mismo directorio y pueden cargarse con `require`. Todo se ejecuta en un directorio en `/tmp` (tmpfs), en una VM nueva que se destruye al final del job, sin red: `npm install` no funciona dentro del job.

## Construir la imagen

```bash
rc image build --stack nodejs
```

## Ejemplo

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "nodejs",
    "files": [
      {"name": "main.js", "content": "let d = \"\";\nprocess.stdin.on(\"data\", c => d += c);\nprocess.stdin.on(\"end\", () => console.log(d.trim().toUpperCase()));\n"}
    ],
    "stdin": "abc"
  }'
```

`stdout` esperado: `ABC`.
