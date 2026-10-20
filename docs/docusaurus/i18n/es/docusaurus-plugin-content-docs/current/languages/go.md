---
title: 'Go'
description: 'Go en Runner Codes: imagen, recursos, warmup y ejemplo'
---

## Resumen

| Elemento | Valor |
|------|-------|
| nombre en la API | `go` |
| imagen OCI de origen | `golang:1.25-alpine` |
| límite de recursos por job | 2 vCPU, 1024 MiB |

El límite de vCPU y RAM existe contra el abuso; la memoria de libkrun es elástica y el host solo paga lo que el guest toca.

## Warmup

El build compila un programa con `GOCACHE=/opt/runner/gocache`, lo que llena la caché de build de la biblioteca estándar dentro de la imagen. Como el root es de solo lectura, `start.sh` copia esa caché a `/tmp/go-cache` en el arranque, la ruta que el ejecutor del guest fuerza en `GOCACHE`. La copia ocurre antes de que la VM entre al pool y no pesa en el job. Medido: `go run` en 247 ms con la caché caliente, frente a 3,4 s sin ella.

Consulte [Imágenes](/concepts/images).

## Cómo se ejecuta el código

El archivo principal (el primero de `files`) se ejecuta con `go run <archivo>`. Sin red, solo la biblioteca estándar está disponible: los módulos externos no se descargan. Todo se ejecuta en un directorio en `/tmp` (tmpfs), en una VM nueva que se destruye al final del job.

## Construir la imagen

```bash
rc image build --stack go
```

## Ejemplo

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "go",
    "files": [
      {"name": "main.go", "content": "package main\n\nimport \"fmt\"\n\nfunc main() {\n\tfmt.Println(6 * 7)\n}\n"}
    ]
  }'
```

`stdout` esperado: `42`.
