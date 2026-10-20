---
title: 'jq'
description: 'jq en Runner Codes: imagen, recursos, warmup y ejemplo'
---

## Resumen

| Elemento | Valor |
|------|-------|
| nombre en la API | `jq` |
| imagen OCI de origen | `alpine:3.22` + `jq` |
| límite de recursos por job | 1 vCPU, 256 MiB |

El límite de vCPU y RAM existe contra el abuso; la memoria de libkrun es elástica y el host solo paga lo que el guest toca.

## Warmup

`apk add jq`: la imagen base no trae `jq`. El paquete se instala en la VM de build y queda grabado en el ext4. Consulte [Imágenes](/concepts/images).

## Cómo se ejecuta el código

El archivo principal (el primero de `files`) es el filtro, ejecutado con `jq --null-input -f <archivo>`. Con `--null-input`, la entrada inicial del filtro es `null`; para leer el JSON del stdin del job, use `input` (un valor) o `inputs` (todos). Todo se ejecuta en un directorio en `/tmp` (tmpfs), en una VM nueva que se destruye al final del job, sin red.

## Construir la imagen

```bash
rc image build --stack jq
```

## Ejemplo

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "jq",
    "files": [
      {"name": "main.jq", "content": "input | .a + .b\n"}
    ],
    "stdin": "{\"a\": 40, \"b\": 2}"
  }'
```

`stdout` esperado: `42`.
