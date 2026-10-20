---
title: 'Solución de problemas'
description: 'Errores conocidos, causa y corrección'
---

## Primer paso

```bash
rc doctor
```

Verifica plataforma, libkrun, GPU, CUDA, virtualización anidada, guest, confinamiento e imágenes. Cada línea empieza con
`ok`, `info`, `warn` o `FAIL`, y el fallo trae el motivo.

## Couldn't find or load libkrunfw.5.dylib

**Causa:** la libkrun upstream abre la libkrunfw solo por el nombre (`dlopen("libkrunfw.5.dylib")`), y el loader dinámico no la encuentra fuera de las rutas estándar.

**Corrección:** la libkrun de `.#krun` (macOS) ya abre la libkrunfw por la ruta absoluta en el store de Nix, así que el error solo aparece con una libkrun de otro origen. En ese caso el launcher todavía define `DYLD_LIBRARY_PATH` con el directorio de `RUNNER_KRUN_LIB`: verifique que sea una ruta completa y que la libkrunfw esté en el mismo directorio.

## krun_start_enter rc=-22

**Causa:** en macOS, el binario no tiene el entitlement `com.apple.security.hypervisor`.

**Corrección:** compile con `task build`, que firma el host. Un `go build` directo genera un binario sin firma.

## /opt/runner/guest: not found

**Causa:** el guest se compiló de forma dinámica. Las imágenes son Alpine (musl) y no tienen el loader de la glibc; el kernel encuentra el archivo, pero no el intérprete, y reporta `not found`.

**Corrección:** compile con `task build`, que genera el guest estático y falla si sale dinámico. Después reconstruya las imágenes.

## Comportamiento antiguo después de cambiar el guest

**Causa:** la imagen en uso se construyó con el guest anterior.

**Corrección:** ejecute `rc image build --stack <l>` de nuevo. La clave de la caché incluye el guest, así que se genera una imagen nueva automáticamente; no hace falta borrar nada.

## Un argumento con espacios llega partido al guest

**Causa:** la libkrun pasa el argv del primer proceso por la cmdline del kernel, que separa por espacios.

**Corrección:** no pase un comando compuesto como argumento. Escríbalo en un archivo y ejecute el archivo; es lo que hacen el `start.sh` y el warmup del build.

## VM for X did not become ready ... console: ...

**Causa:** el guest no respondió al job `sh true` de la espera de disponibilidad. El mensaje trae el final de la consola del guest, que normalmente indica el motivo (guest dinámico, `start.sh` fallando, imagen corrupta).

**Corrección:** lea el fragmento de consola del mensaje. Los casos más comunes son los de esta página: guest dinámico o imagen desactualizada.

## machine X did not become ready / failed to start

**Causa:** el proceso `vmm` de la máquina murió en el arranque o el guest no respondió. El mensaje cita la última línea del
`vmm.log` escrita **en este** start (el log se escribe en modo append), por ejemplo `nested virtualization is not supported on this
host`.

**Corrección:** lea la línea citada. El log completo es el `vmm.log` del directorio que imprime `rc machine data-dir --name X`;
la consola sale en `rc machine logs --name X`.

## INVALID_STATE: machine X is running; stop it first or use force

**Causa:** `delete` (y otras operaciones de máquina detenida) en una máquina encendida.

**Corrección:** `rc machine stop --name X` antes, o `--force` para matarla.

## machine X is paused; resume it (rc machine resume) or stop it first

**Causa:** `start` o `exec` en una máquina pausada.

**Corrección:** `rc machine resume --name X`, o `stop` para apagarla.

## resolve secret 'NOME': env_unset

**Causa:** un `--secret-env` (o `[secrets]` con `from_env`) apunta a una variable que no existe en el entorno de
quien ejecutó el `start`.

**Corrección:** exporte la variable antes del `start`; el valor se lee en cada start y nunca queda grabado.

## El programa en la máquina no resuelve un nombre (`bad address`)

**Causa:** la política de salida no permite el nombre; el DNS del gateway responde NXDOMAIN a propósito.

**Corrección:** revise el `egress-denials.log` del directorio de la máquina (`rc machine data-dir`) y permítalo con
`rc machine update --name X --allow-host <nome>`, con la máquina encendida. Consulte [Red y volúmenes](/cli/network).

## E007

**Causa:** `--gpu` (o `"gpu": true`) en un stack sin `gpu` en el catálogo.

**Corrección:** use un stack con GPU, como `llama`. CUDA no existe: la GPU es Vulkan (Venus). Consulte [GPU](/advanced/gpu).

## E006: unsupported language

**Causa:** el lenguaje no está en el catálogo **o** su imagen todavía no se ha construido en este host.

**Corrección:** `rc languages` muestra el catálogo y `ready`. Constrúyala con `rc image build --stack <l>`.

## E005: stdin too long

**Causa:** stdin mayor de 1.000.000 bytes.

**Corrección:** reduzca la entrada o envíe los datos como archivo en `files` (hasta 4 MiB sumando todos los archivos).

## Sin acceso a /dev/kvm (Linux)

**Causa:** el usuario no tiene permiso de lectura y escritura en `/dev/kvm`, o KVM no está habilitado.

**Corrección:** habilite KVM en el host y dé al usuario acceso al dispositivo (en general, el grupo `kvm`).

## Windows

No está soportado: la libkrun upstream no tiene backend WHP. El build para Windows falla a propósito.

## La API respondió "status": "success", pero el programa falló

No es un error. `status` indica que la API ejecutó el job. El resultado del programa está en `stderr` y `exception` (`"Timeout"`, `"Compilation failed"`).
