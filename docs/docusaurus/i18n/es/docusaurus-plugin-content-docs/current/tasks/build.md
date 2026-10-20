---
title: 'Build'
description: 'Compilación del host y del guest, firma en macOS y build de las imágenes'
---

## task build

```bash
task build
```

Ejecuta `scripts/build.sh`, que:

1. compila el host `bin/rc` para el sistema actual, con `CGO_ENABLED=0` (la libkrun se carga mediante purego en tiempo de ejecución);
2. compila el guest `bin/rc-guest-linux-<arch>` a partir de `./cmd/runner-guest`: siempre Linux, estático, de la misma arquitectura que el host (la libkrun no emula otra arquitectura);
3. **falla** si el guest sale dinámico, porque un binario dinámico no se ejecuta en la imagen musl (`not found`);
4. en macOS, firma el host con `codesign` y el entitlement `com.apple.security.hypervisor` (`build/hv.entitlements`). Sin él, `krun_start_enter` falla.

La versión grabada en los binarios viene de `VERSION` o de `git describe`; sin git, `dev`. Es la que aparece en `GET /health`.

### Por qué dos binarios

purego obliga a un enlace dinámico contra la glibc en Linux. Las imágenes de los lenguajes son Alpine (musl). Por eso el guest es un `main` separado, sin purego, compilado de forma estática.

## task image:build

```bash
task image:build -- --stack python
task image:build -- --all
```

Depende de `task build` (el guest debe existir para poder inyectarse). Consulte [image](/cli/image) e [Imágenes](/concepts/images).

## Instalación de la libkrun

```bash
task krun:install
```

| Sistema | Cómo |
|---------|------|
| macOS | `task krun:install`: libkrun 1.19.6 / libkrunfw 5.6.2 de las derivaciones en `nix/` |
| Linux | `task krun:install`: libkrun / libkrunfw de nixpkgs, con virtio-blk activado |

Ambos instalan `.#krun` en el perfil de Nix (`~/.nix-profile/lib`), donde `rc` busca por defecto.

## Windows

No hay build para Windows. Compilar para `windows/amd64` falla a propósito, con un mensaje que indica el motivo (la libkrun upstream no tiene backend WHP). El gate `task test:targets` verifica que ese rechazo siga ocurriendo.
