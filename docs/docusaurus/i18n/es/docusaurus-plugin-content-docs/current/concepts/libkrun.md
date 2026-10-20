---
title: 'libkrun'
description: 'Cómo usa Runner Codes la libkrun: arranque, un proceso por VM, red desactivada y root de solo lectura'
---

## Qué es

La [libkrun](https://github.com/containers/libkrun) es un VMM en forma de biblioteca: el proceso que la invoca se convierte en la VM. El kernel viene de la libkrunfw, también como biblioteca. Versiones en uso:

| Componente | Versión |
|------------|--------|
| libkrun | 1.19.6 (macOS, derivación propia en `nix/`); 1.19.5 en nixpkgs (Linux) |
| libkrunfw | 5.6.2, kernel Linux 6.12.109 (macOS, `nix/`); 5.5.0 en nixpkgs (Linux) |

Runner Codes invoca la libkrun directamente, sin capa intermedia, y la carga en tiempo de ejecución con purego: no hay cgo en el host.

| Plataforma | Hipervisor |
|------------|-----------|
| macOS Apple Silicon | Hypervisor.framework (HVF) |
| Linux x86_64 / aarch64 | KVM (`/dev/kvm`) |
| Windows | no soportado: la libkrun upstream no tiene backend WHP |

## Un proceso por VM

`krun_start_enter` no retorna mientras la VM está en ejecución: toma el control del proceso. Por eso cada VM es un proceso propio, `rc vmm --spec <arquivo>`, creado por el launcher.

- **Spawn, nunca fork.** El HVF mata un fork de un proceso Go (multihilo) unos 2 s después.
- **Timeout:** SIGKILL al grupo del proceso de la VM.
- **Si el padre muere, la VM muere** (VMs de job): en Linux, `Pdeathsig`; en macOS, que no tiene equivalente, el hijo vigila un pipe heredado en el fd 3 y termina cuando se cierra. La VM de una máquina persistente es la excepción: se ejecuta desacoplada, en su propia sesión, y sobrevive al `rc` que la encendió.
- **Biblioteca en macOS:** el proceso `vmm` necesita `DYLD_LIBRARY_PATH` apuntando al directorio de la libkrun para encontrar `libkrunfw.5.dylib`; el launcher lo define.
- **Entitlement en macOS:** el binario necesita `com.apple.security.hypervisor`. Sin él, `krun_start_enter` devuelve `-22`.

## Root: disco ext4 de solo lectura

La VM monta como root un disco ext4 de solo lectura:

- `krun_add_disk2` adjunta el ext4 del lenguaje;
- `krun_set_root_disk_remount` monta `/dev/vda` como root, de solo lectura.

El primer proceso de una VM de job es `/opt/runner/start.sh`, que:

1. monta un tmpfs de 256 MiB en `/tmp`;
2. exporta el entorno de la config OCI (`/opt/runner/env`: `PATH`, `JAVA_HOME`...) y define `HOME=/tmp/home`;
3. para Go, copia la caché de build precalentada de `/opt/runner/gocache` a `/tmp/go-cache`;
4. ejecuta el guest en el puerto 5000.

Toda escritura va al tmpfs, que desaparece con la VM. En una máquina, el primer proceso es `/opt/runner/machine.sh`, que
monta el disco de la máquina y un overlayfs sobre la imagen; consulte [machine](/cli/machine).

**Por qué disco y no virtiofs:** con virtiofs, el uso compartido no impide que el guest alcance otros directorios del host (documentación de la libkrun), y no resultó más rápido.

## Red: desactivada en los jobs, por política en las máquinas

Por defecto la libkrun activa por sí sola el TSI (transparent socket impersonation), que da red al guest a través del host. Runner
Codes lo desactiva:

- `krun_disable_implicit_vsock` impide el vsock implícito;
- `krun_add_vsock(ctx, 0)` añade un vsock sin ninguna flag de TSI.

Resultado: la VM de job no tiene red. La prueba de extremo a extremo lo confirma intentando conectarse a `1.1.1.1`, y se valida a
sí misma fallando cuando la red se activa a propósito.

Una máquina con `--net` tampoco usa TSI: la libkrun conecta una virtio-net a un socketpair
(`krun_add_net_unixstream`), y del lado del host queda un gateway en espacio de usuario, dentro del propio proceso `vmm`, que
aplica la política de salida. Consulte [Red y volúmenes](/cli/network).

## Canal con el guest

`krun_add_vsock_port2` con `listen=true` conecta el puerto vsock 5000 del guest a un socket Unix en el host. El protocolo está en [Comunicación vsock](/concepts/vsock-communication).

## Argumentos del proceso del guest

La libkrun pasa el argv del primer proceso por la cmdline del kernel. Un argumento con espacios llega partido al guest. Por eso todo comando compuesto va en un archivo (es el caso del `start.sh` y del warmup en el build), nunca como argumento.

## Memoria y CPU

La memoria de la libkrun es elástica: el host solo paga las páginas que el guest toca. Configurar entre 256 y 2048 MiB no cambió el tiempo de arranque en la medición. Los valores de vCPU y RAM por lenguaje del catálogo son un límite contra abusos, no una reserva.
