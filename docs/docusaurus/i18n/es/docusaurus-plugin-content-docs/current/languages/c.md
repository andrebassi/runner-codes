---
title: 'C'
description: 'C en Runner Codes: imagen, recursos, warmup y ejemplo'
---

## Resumen

| Elemento | Valor |
|------|-------|
| nombre en la API | `c` |
| imagen OCI de origen | `alpine:3.22` + `build-base` |
| límite de recursos por job | 1 vCPU, 512 MiB |

El límite de vCPU y RAM existe contra el abuso; la memoria de libkrun es elástica y el host solo paga lo que el guest toca.

## Warmup

`apk add build-base`: la imagen base no trae compilador. El paquete se instala en la VM de build y queda grabado en el ext4; el job no necesita red. Consulte [Imágenes](/concepts/images).

## Cómo se ejecuta el código

El archivo principal (el primero de `files`) se compila con `gcc -o <binario> <archivo>` y el binario se ejecuta con el stdin del job. Un error de compilación devuelve `exception: "Compilation failed"`, con la salida de `gcc` en `stderr`. Todo se ejecuta en un directorio en `/tmp` (tmpfs), en una VM nueva que se destruye al final del job, sin red.

## Construir la imagen

```bash
rc image build --stack c
```

## Ejemplo

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "c",
    "files": [
      {"name": "main.c", "content": "#include <stdio.h>\n\nint main(void) {\n    int n;\n    scanf(\"%d\", &n);\n    printf(\"%d\\n\", n * 2);\n    return 0;\n}\n"}
    ],
    "stdin": "21"
  }'
```

`stdout` esperado: `42`.
