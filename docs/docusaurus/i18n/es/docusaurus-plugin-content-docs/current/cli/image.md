---
title: 'image'
description: 'Construir el ext4 de solo lectura de un lenguaje a partir de la imagen OCI oficial'
---

## Uso

```bash
rc image build --stack python
rc image build --stack python --stack go
rc image build --stack python,go
rc image build --all
```

| Flag | Descripción |
|------|-----------|
| `--stack` | lenguaje del catálogo; repetible o separado por comas |
| `--all` | todos los lenguajes del catálogo |

Sin `--stack` ni `--all`, el comando falla con `informe --stack ou --all`. Un lenguaje fuera del catálogo falla con E006.

Mediante el Taskfile (que compila antes):

```bash
task image:build -- --stack python
task image:build -- --all
```

## image prune

```bash
rc image prune          # builds antiguos a los que ya no apunta ningún nombre (el guest o start.sh cambiaron, la clave cambió)
rc image prune --all    # también las imágenes y los packs que ninguna máquina usa
```

Las imágenes fuera del catálogo (`rc machine create --image ...`) usan el mismo build, sin warmup.

## Salida

Una línea por lenguaje:

```
python <caminho do ext4> cached=false elapsed=<tempo> <métricas do build>
```

- `cached=true` cuando la clave ya existía en la caché y no se reconstruyó nada.
- Las métricas vienen de la VM de build: tiempo de extracción, de warmup y de `mkfs`, y el tamaño usado y el del disco.

## Qué hace el build

1. Descarga la imagen OCI con go-containerregistry (sin Docker) y aplana las capas en un tar.
2. Arranca una VM de build Alpine 3.22.6 (tarball base oficial de Alpine, con sha256 verificado; e2fsprogs y GNU tar vía `apk`).
3. Dentro de ella, como root: extrae el tar en un tmpfs, inyecta el guest y `start.sh` en `/opt/runner/`, ejecuta el warmup en chroot y genera el ext4 con `mkfs.ext4 -d`.
4. Graba `<lang>-<chave>.ext4` en la caché y apunta el symlink `<lang>.ext4` hacia él.

El porqué de cada etapa, la clave de la caché y el warmup de cada lenguaje están en [Imágenes](/concepts/images).

## Cuándo reconstruir

La clave incluye el digest de la imagen, el warmup, `build.sh`, `start.sh` y el guest. Después de cambiar cualquiera de ellos (incluso recompilar el guest con otro código), vuelva a ejecutar el build: la clave cambia sola y se genera una imagen nueva.

## Tiempo

En la máquina medida (macOS 27.0.1, Apple Silicon), la imagen de Python tardó ~8–10 s, de los cuales 2,7 s fueron de pull y aplanado. La de Go
tardó 35 s, 28 de ellos en `mkfs.ext4`. El build necesita red (pull y warmup); la VM del job no la tiene.
