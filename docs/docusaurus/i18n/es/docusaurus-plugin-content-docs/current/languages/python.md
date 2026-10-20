---
title: 'Python'
description: 'Python en Runner Codes: imagen, recursos, warmup y ejemplo'
---

## Resumen

| Elemento | Valor |
|------|-------|
| nombre en la API | `python` |
| imagen OCI de origen | `python:3.12-alpine` |
| límite de recursos por job | 1 vCPU, 512 MiB |

El límite de vCPU y RAM existe contra el abuso; la memoria de libkrun es elástica y el host solo paga lo que el guest toca.

## Warmup

`python3 -m compileall` sobre la stdlib. La imagen oficial no trae `.pyc`; compilar en la imagen elimina ese costo del primer import. La imagen pasó de 66 a 85 MiB y el warmup tardó ~1 s.

El warmup se ejecuta una vez, en la VM de build, y el resultado queda grabado en el ext4. Consulte [Imágenes](/concepts/images).

## Cómo se ejecuta el código

El archivo principal (el primero de `files`) se ejecuta con `python3 <archivo>`. Los demás archivos quedan en el mismo directorio y pueden importarse. Todo se ejecuta en un directorio en `/tmp` (tmpfs), en una VM nueva que se destruye al final del job, sin red.

## Construir la imagen

```bash
rc image build --stack python
```

## Ejemplo

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "python",
    "files": [
      {"name": "main.py", "content": "from util import dobro\nprint(dobro(int(input())))\n"},
      {"name": "util.py", "content": "def dobro(x):\n    return 2 * x\n"}
    ],
    "stdin": "21"
  }'
```

`stdout` esperado: `42`.

## Números medidos

En macOS 27.0.1, Apple Silicon: arranque hasta la 1.ª respuesta con p50 260 ms y p95 266 ms; job por la CLI con arranque en frío en ~240 ms en total (ejecución 31–38 ms); por la API con el pool lleno, 26–43 ms. Build de la imagen: ~8–10 s. Consulte [Rendimiento](/advanced/performance).
