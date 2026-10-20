---
title: 'SQLite'
description: 'SQLite en Runner Codes: imagen, recursos, warmup y ejemplo'
---

## Resumen

| Elemento | Valor |
|------|-------|
| nombre en la API | `sqlite` |
| imagen OCI de origen | `alpine:3.22` + `sqlite` |
| límite de recursos por job | 1 vCPU, 256 MiB |

El límite de vCPU y RAM existe contra el abuso; la memoria de libkrun es elástica y el host solo paga lo que el guest toca.

## Warmup

`apk add sqlite`: la imagen base no trae `sqlite3`. El paquete se instala en la VM de build y queda grabado en el ext4. Consulte [Imágenes](/concepts/images).

## Cómo se ejecuta el código

El contenido del archivo principal (el primero de `files`) va por el stdin de `sqlite3 :memory:`: una base de datos en memoria, que desaparece al final del job. Si el job tiene stdin, se envía **antes** del código, seguido de un salto de línea. La salida usa el formato predeterminado de `sqlite3` (columnas separadas por `|`). Todo se ejecuta en una VM nueva que se destruye al final del job, sin red.

Este es el SQLite ejecutable. Los validadores de sintaxis de bases de datos no están en el catálogo; consulte [Parsers de bases de datos](/guides/database-parsers-setup).

## Construir la imagen

```bash
rc image build --stack sqlite
```

## Ejemplo

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "sqlite",
    "files": [
      {"name": "main.sql", "content": "CREATE TABLE t (n INTEGER);\nINSERT INTO t VALUES (40), (2);\nSELECT SUM(n) FROM t;\n"}
    ]
  }'
```

`stdout` esperado: `42`.
