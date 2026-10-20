---
title: 'Perl'
description: 'Perl en Runner Codes: imagen, recursos, warmup y ejemplo'
---

## Resumen

| Elemento | Valor |
|------|-------|
| nombre en la API | `perl` |
| imagen OCI de origen | `alpine:3.22` + `perl` |
| límite de recursos por job | 1 vCPU, 512 MiB |

El límite de vCPU y RAM existe contra el abuso; la memoria de libkrun es elástica y el host solo paga lo que el guest toca.

## Warmup

`apk add perl`: la imagen base no trae Perl. El paquete se instala en la VM de build y queda grabado en el ext4. Consulte [Imágenes](/concepts/images).

## Cómo se ejecuta el código

El archivo principal (el primero de `files`) se ejecuta con `perl <archivo>`. Sin red, los módulos de CPAN no se instalan; solo lo que viene en el paquete `perl` de Alpine. Todo se ejecuta en un directorio en `/tmp` (tmpfs), en una VM nueva que se destruye al final del job.

## Construir la imagen

```bash
rc image build --stack perl
```

## Ejemplo

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "perl",
    "files": [
      {"name": "main.pl", "content": "my $l = <STDIN>;\nchomp $l;\nprint scalar reverse($l), \"\\n\";\n"}
    ],
    "stdin": "abc"
  }'
```

`stdout` esperado: `cba`.
