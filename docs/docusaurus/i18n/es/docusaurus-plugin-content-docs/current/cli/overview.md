---
title: 'Visión general de la CLI'
description: 'Los subcomandos de rc'
---

## El binario

`rc` es el binario del host. Se ejecuta en macOS Apple Silicon y en Linux x86_64/aarch64 con KVM.

```bash
rc --help
```

| Subcomando | Qué hace | Página |
|------------|-----------|--------|
| `run` | ejecuta código en una VM nueva (arranque en frío), con la salida en streaming | [run y shell](/cli/run) |
| `shell` | shell interactivo en una VM nueva de una stack | [run y shell](/cli/run#shell) |
| `machine ...` | máquinas persistentes: `create`, `start`, `exec`, `shell`, `cp`, `logs`, `stop`, `delete`, `run`, `branch`, `pause`, `resume`, `sync`, `update`, `status`, `ls`, `data-dir`, `prune` | [machine](/cli/machine), [runner.code](/cli/runner-code), [Red y volúmenes](/cli/network) |
| `pack ...` | `create`, `inspect`, `prune`: imagen o máquina en un artefacto portátil | [pack](/cli/pack) |
| `serve` | API HTTP con pool de VMs pre-arrancadas y rutas de máquina | [API HTTP](/cli/http-api) |
| `image build`, `image prune` | genera el ext4 de una o más stacks a partir de la imagen OCI oficial; limpia la caché | [image](/cli/image) |
| `languages` | lista el catálogo y si la imagen de cada stack ya está construida | más abajo |
| `doctor` | verifica si el host puede ejecutar microVMs y qué ofrece | más abajo |
| `vmm` | proceso de una VM; uso interno, invocado por el launcher | [libkrun](/concepts/libkrun) |

El guest (`runner-guest`) es otro binario y se ejecuta solo dentro de la VM; no se invoca a mano.

## languages

```bash
rc languages
```

Una línea por stack del catálogo, con `ready=true` cuando el ext4 ya existe en la caché, y la imagen OCI de origen:

```
awk      ready=true  alpine:3.22
...
llama    ready=true  fedora:43
nodejs   ready=false node:22-alpine
python   ready=true  python:3.12-alpine
```

## doctor

```bash
rc doctor
```

Una línea por elemento, con `ok`, `info`, `warn` o `FAIL`. Salida real en un Mac M1 Pro:

```
ok    platform
ok    libkrun
ok    gpu: libkrun built with virtio-gpu (Venus); use --gpu on a stack with gpu
info  cuda: not supported (no CUDA remoting; --gpu is Vulkan)
info  nested: not supported on this host; --nested is refused
ok    guest
info  sandbox: macOS has no Landlock/seccomp; the VM process runs unconfined
ok    image awk
...
```

| Elemento | Qué verifica |
|------|----------------|
| `platform` | macOS en Apple Silicon, o Linux con `/dev/kvm` abierto para lectura y escritura |
| `libkrun` | si la biblioteca carga desde la ruta configurada |
| `gpu`, `cuda` | si libkrun tiene virtio-gpu (Venus); CUDA no está soportado |
| `nested` | si el host anida virtualización (`--nested`) |
| `guest` | si el binario del guest existe junto al host |
| `sandbox` | en Linux, la ABI de Landlock y seccomp; avisa cuando la ABI es < 9 |
| `image <stack>` | una línea por imagen ya construida |

Una falla aparece como `FAIL <item> <motivo>`, y el comando termina con error cuando hay alguna.

## Variables de entorno

| Variable | Valor por defecto | Uso |
|----------|--------|-----|
| `RUNNER_LISTEN` | `127.0.0.1:8080` | dirección de `serve` |
| `RUNNER_CACHE_DIR` | `~/Library/Caches/runner-codes` (macOS), `~/.cache/runner-codes` (Linux) | imágenes, VM de build, máquinas, packs, sockets |
| `RUNNER_KRUN_LIB` | `~/.nix-profile/lib/libkrun.dylib` (macOS); en Linux `~/.nix-profile/lib/libkrun.so.1` o el soname `libkrun.so.1` | ruta de libkrun |
| `RUNNER_GUEST_BIN` | `rc-guest-linux-<arch>` junto al host | guest inyectado en las imágenes |
| `RUNNER_SANDBOX` | activado | `off` desactiva Landlock + seccomp en Linux, solo para diagnóstico |

## Mediante el Taskfile

| Task | Equivale a |
|------|------------|
| `task run -- <flags>` | `./bin/rc run <flags>` |
| `task shell -- <flags>` | `./bin/rc shell <flags>` |
| `task serve -- <flags>` | `./bin/rc serve <flags>` |
| `task image:build -- <flags>` | `./bin/rc image build <flags>` |
| `task doctor` | `./bin/rc doctor` |
