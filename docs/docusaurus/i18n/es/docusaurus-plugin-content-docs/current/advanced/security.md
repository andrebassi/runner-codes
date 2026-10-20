---
title: 'Seguridad'
description: 'Qué aísla el código no confiable, qué se demuestra con pruebas y qué falta todavía'
---

## Modelo

El código del job se trata como hostil. La barrera principal es la VM; la segunda es el confinamiento del proceso que ejecuta la VM.

La documentación de la libkrun es explícita: **el guest y el VMM están en el mismo contexto de seguridad.** Un escape de la VM cae dentro del proceso `rc vmm`, con sus privilegios. Por eso el aislamiento es la VM **más** el confinamiento de ese proceso, y ese confinamiento hoy es distinto según la plataforma.

## Capas de un job (en ambas plataformas)

| Capa | Cómo |
|--------|------|
| VM por job | cada job se ejecuta en una VM nueva, destruida al final; nada pasa de un job al siguiente |
| root de solo lectura | el root es un ext4 montado de solo lectura (`/dev/vda`); solo se escribe en el tmpfs de `/tmp` (256 MiB) |
| estado efímero | el tmpfs desaparece con la VM |
| sin red | `krun_disable_implicit_vsock` + `krun_add_vsock(ctx, 0)`: sin TSI, sin interfaz de red |
| sin virtiofs | el root es un disco, no un directorio compartido del host; virtiofs no impediría que el guest alcanzara otros directorios del host |
| límites por lenguaje | vCPU y RAM limitados en el catálogo; timeout por defecto 10 s, máximo 300 s, con SIGKILL al grupo del proceso |
| límites de entrada | stdin ≤ 1.000.000 bytes, hasta 32 archivos, código ≤ 4 MiB, nombres sin `/`, sin `..`, sin empezar con `.` |
| si el padre muere, la VM muere | ninguna VM queda huérfana si el host se cae |

## Por plataforma

| Elemento | macOS | Linux |
|------|-------|-------|
| hipervisor | Hypervisor.framework | KVM |
| permiso para crear una VM | entitlement `com.apple.security.hypervisor` en el binario | acceso a `/dev/kvm` |
| la VM muere con el padre | pipe de vigilancia en el fd 3 | `Pdeathsig` |
| timeout | SIGKILL al grupo del proceso | SIGKILL al grupo del proceso |
| uid/gid sin privilegios en el proceso de la VM | — | soportado por la spec (`krun_setuid`/`krun_setgid`) |
| Landlock | no existe en macOS | aplicado al proceso `vmm` de cada VM de job |
| seccomp | no existe en macOS | denylist aplicada a todos los hilos (TSYNC) |

En macOS el confinamiento del proceso `vmm` se reduce al entitlement y al grupo de procesos.

### Confinamiento en Linux

Antes de entregar el proceso a la libkrun, el `vmm` de una VM de job (root en disco, sin red) aplica:

| Mecanismo | Lo que queda permitido | Lo que queda denegado |
|---|---|---|
| Landlock (archivos) | lectura del disco root y de las bibliotecas ya cargadas, `/proc`, `/sys`; escritura en la consola y en el directorio propio de la VM (`run/<id>/`); `/dev/kvm` con ioctl | todo el resto del sistema de archivos |
| Landlock (red, ABI ≥ 4) | nada | todo `bind` y `connect` TCP (y UDP en la ABI 10) |
| Landlock (alcance, ABI ≥ 6) | — | señales y sockets abstractos fuera del propio proceso |
| Landlock (socket Unix, ABI ≥ 9) | el socket de la propia VM | `connect` al socket de otra VM |
| seccomp | el resto | `execve`, `ptrace`, `mount`, `unshare`/`setns`, módulos del kernel, `kexec`, `bpf`, `perf_event_open`, llavero, reloj, swap, reboot, `userfaultfd` y similares (`EPERM`) |

La libkrunfw se carga antes de Landlock, porque la libkrun solo la abre dentro de `krun_start_enter` y ese `open`
fallaría después del confinamiento. El seccomp es una denylist, y no una allowlist, a propósito: la allowlist exigiría
medir cada syscall de la libkrun en un Linux con KVM, y una que faltara tumbaría todas las VMs.

**Medido** en Ubuntu 26.04 (kernel 7.0, aarch64, Landlock ABI 8), con el propio proceso confinado: lectura de
archivos fuera de la política, escritura junto al disco, TCP, `execve` y `personality` denegados; lectura del disco y
creación del propio socket permitidas. Sin el confinamiento, la misma sonda lo consigue todo (prueba de fallo).

**No medido:** una microVM completa bajo el confinamiento en un Linux con KVM (la VM de prueba no tiene `/dev/kvm`).

**Brecha conocida:** con Landlock ABI < 9 el kernel no controla `connect` a un socket Unix por ruta; el proceso
de una VM todavía alcanza el socket de otra. `rc doctor` lo advierte. `RUNNER_SANDBOX=off` desactiva el confinamiento para
diagnóstico, con un aviso en stderr.

## Máquinas persistentes

Una máquina renuncia a parte del aislamiento de un job (guarda estado y puede tener red), y cada apertura está
declarada y tiene un mínimo:

| Capa | Cómo |
|--------|------|
| red desactivada por defecto | sin `--net` (o `net`, o `-p` solo), la máquina no tiene tarjeta de red; con `-p` sin `--net`, el host alcanza el puerto, pero el guest no sale |
| política de salida | gateway en espacio de usuario en el propio proceso `vmm`, sin TSI: allow-host por nombre, CIDR, `--block-private`; un nombre fuera de la lista recibe NXDOMAIN; una IP solo vale si vino de un nombre permitido. Consulte [Red y volúmenes](/cli/network) |
| mínimo que ninguna regla abre | metadata de la nube y todo el link-local; el loopback del host solo con `--allow-cidr 127.0.0.1`, nunca por una IP aprendida en DNS (DNS rebinding) |
| rechazos registrados | toda salida denegada va a `egress-denials.log` en el directorio de la máquina |
| credencial sin lectura | el guest ve un placeholder; el gateway lo cambia por el valor real solo en el TLS del host del binding, con una CA propia de la máquina (name constraints). Un placeholder en la URL se rechaza con 403. Consulte [Credenciales](/cli/credentials) |
| secretos | `--secret-env`/`--secret-file` se leen en el start y se envían al guest solo en memoria; no quedan en el registro ni en el disco de la máquina |
| agente SSH | el guest usa las claves del host mediante un socket reenviado; la clave privada no entra en la VM |
| volumen `:ro` | la protección está en el dispositivo, no solo en la opción de montaje: `mount -o remount,rw` por el root del guest no abre el volumen (probado) |
| virtualización anidada | `--nested` se rechaza donde el host no admite anidamiento; por la API, solo con `rc serve --allow-nested-virt` |
| pantalla | el visor escucha solo en `127.0.0.1` del host, sin contraseña |

Lo que una máquina **no** protege: el root del guest lee el entorno de sus propios procesos, así que un secreto entregado
a la máquina está al alcance del código que se ejecuta en ella. La sustitución de credenciales tampoco impide que el servidor
devuelva el valor en el cuerpo de la respuesta.

## Lo que demuestra la prueba de extremo a extremo

| Propiedad | Prueba |
|-------------|-------|
| sin red | la conexión a `1.1.1.1` falla dentro de la VM; con la red activada a propósito, la prueba falla |
| root de solo lectura | escribir en `/etc` falla |
| sin estado entre jobs | el archivo escrito en `/tmp` por un job no existe en el job siguiente |
| límite de memoria | asignar 2 GiB en una VM de 512 MiB falla, sin afectar al host |
| timeout | el job en bucle infinito se mata y ninguna VM sobrevive |

## La API

La API HTTP **no tiene autenticación**. El valor por defecto es escuchar solo en `127.0.0.1`. No la exponga fuera de loopback sin un proxy autenticado delante.

## Build de imágenes

El pull de la imagen ocurre en el host. El resto del build se ejecuta en una VM propia, con red, porque la instalación de e2fsprogs y GNU tar y el warmup (`apk add`, por ejemplo) la necesitan. La red de la VM de build no llega a la VM del job. El tarball base de Alpine usado por la VM de build tiene el sha256 verificado antes de usarse. Las imágenes de lenguajes vienen de las imágenes OCI oficiales.

## Pendientes

- validar una microVM completa bajo Landlock + seccomp en un Linux con KVM;
- autenticación en la API (hoy delegada a un proxy);
- aislamiento de sockets Unix entre VMs en kernels con Landlock ABI < 9.
