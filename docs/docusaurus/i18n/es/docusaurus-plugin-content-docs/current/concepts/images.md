---
title: 'Imágenes'
description: 'Cómo la imagen OCI oficial se convierte en un ext4 de solo lectura: build, caché y warmup'
---

## Qué es una imagen aquí

Cada lenguaje del catálogo se ejecuta sobre un archivo ext4 de solo lectura, que se convierte en el root de la VM. Se genera a partir de la imagen OCI **oficial** del lenguaje (por ejemplo `python:3.12-alpine`), con tres elementos añadidos:

- el guest estático en `/opt/runner/guest`;
- el `/opt/runner/start.sh`, primer proceso de la VM;
- el resultado del warmup del lenguaje, cuando lo hay.

## Cómo funciona el build

```bash
rc image build --stack python
rc image build --all
```

1. **Pull sin Docker.** El host descarga la imagen con go-containerregistry, para la arquitectura del host.
2. **Aplanado en el host.** Las capas se convierten en un único tar.
3. **VM de build.** Una VM Alpine 3.22.6 (el tarball base oficial de Alpine, con sha256 verificado) recibe el tar, con e2fsprogs y GNU tar instalados vía `apk`. Dentro de ella, como root:
   - extrae el tar en un tmpfs preservando propietario y permisos (`--numeric-owner`);
   - copia el guest y el `start.sh` a `/opt/runner/`;
   - ejecuta el warmup en chroot, con red, solo en esta etapa;
   - genera el ext4 con `mkfs.ext4 -d`.
4. El ext4 va a la caché y un symlink `<lang>.ext4` pasa a apuntar a él.

### Por qué una VM de build

- El APFS de macOS no distingue mayúsculas de minúsculas: `Case` y `case` se convierten en el mismo archivo. Dentro de la VM el sistema de archivos es Linux, sensible a mayúsculas.
- El e2fsprogs de Homebrew no lee tar, así que no es posible generar el ext4 directamente desde el tar en el Mac.
- Extraer como root preserva uid, gid y permisos de la imagen.

## Caché

La clave de la imagen es:

```
sha256(digest da imagem + warmup + build.sh + start.sh + guest)[:12]
```

| Sistema | Ruta |
|---------|---------|
| macOS | `~/Library/Caches/runner-codes/images/<lang>-<chave>.ext4` |
| Linux | `~/.cache/runner-codes/images/<lang>-<chave>.ext4` |

con el symlink `<lang>.ext4` en el mismo directorio. La raíz de la caché cambia con `RUNNER_CACHE_DIR`.

Consecuencia: cambiar el guest, el `start.sh`, el script de build, el warmup o la imagen de origen genera una clave nueva y fuerza el rebuild en la siguiente ejecución de `image build`. No hay que invalidar nada a mano. Si la imagen existente se construyó con un guest antiguo, basta con ejecutar el build de nuevo.

## Warmup

El warmup es una línea de `sh` declarada en `languages.yaml`. Se ejecuta **en la VM de build**, con red, y el resultado queda grabado en la imagen. Nada del warmup se ejecuta en la VM del job.

| Lenguaje | Warmup | Por qué |
|-----------|--------|---------|
| python | `python3 -m compileall` en la stdlib | la imagen oficial no trae `.pyc`; compilar elimina ese costo del primer import. La imagen pasó de 66 a 85 MiB y el warmup tardó ~1 s |
| go | compila un programa con `GOCACHE=/opt/runner/gocache` | llena la caché de build; como el root es de solo lectura, el `start.sh` copia la caché a `/tmp/go-cache` en el arranque (la ruta que el executor impone), antes de que la VM entre al pool |
| c, cpp | `apk add build-base` | la imagen base es `alpine:3.22`, sin compilador |
| perl | `apk add perl` | ídem |
| jq | `apk add jq` | ídem |
| sqlite | `apk add sqlite` | ídem |

Los demás lenguajes no tienen warmup.

## Imágenes fuera del catálogo

Las máquinas aceptan cualquier imagen OCI (`rc machine create --image alpine:3.20`, o `image = "..."` en el
[runner.code](/cli/runner-code)). El build es el mismo, con el warmup vacío, y la imagen entra en la caché como las demás. Un
[pack](/cli/pack) extraído se convierte en la imagen `pack:<chave>`.

## Limpiar la caché

```bash
rc image prune          # builds antiguos, a los que ya no apunta ningún nombre
rc image prune --all    # también las imágenes y packs que ninguna máquina usa
```

## Tiempos medidos

En la máquina medida (macOS 27.0.1, Apple Silicon), la imagen de Python tardó ~8–10 s en construirse, de los cuales 2,7 s fueron de
pull y aplanado. En las imágenes grandes domina el `mkfs.ext4`: 28 s de los 35 s de Go, porque el ext4 se escribe en el host vía
virtiofs. La tabla de cada stack está en el [Informe de pruebas](/advanced/test-report).
