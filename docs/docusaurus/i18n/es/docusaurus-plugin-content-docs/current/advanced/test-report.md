---
title: 'Informe de pruebas por plataforma'
description: 'Pruebas de extremo a extremo de rc en macOS, Linux y Windows: entorno, método, números medidos, defectos encontrados y lo que no se probó'
---

# Informe de pruebas por plataforma

Última ejecución completa: **20/10/2026**, binario `rc` sobre libkrun + libkrunfw. Las mediciones base (arranque, memoria,
build de imágenes, ejecución por stack) son del **19/10/2026**, en el mismo host. Todo número de esta página fue medido; lo que
no se probó se indica como no probado.

## Ejecución del 20/10/2026 (macOS 27, Apple M1 Pro)

| Gate | Resultado |
|---|---|
| `task test` | 20 paquetes, todos en verde (unitarias con `-race`) |
| `task test:cov` | **95,2%** total, dominio **100%**, guest **75%** (catraca en 75), SDK de Python **100%** en línea y branch |
| `task test:e2e` | **42 de 42** pruebas en verde, en dos ejecuciones seguidas |
| `task runbook` | **126** comprobaciones, **0** fallos |
| `task guides` | **9** carpetas, **175** comprobaciones, **0** fallos |
| `task test:targets` | darwin/arm64, linux/amd64 y linux/arm64 compilan; windows/amd64 rechazado con el motivo |

En Linux, Landlock + seccomp se probaron con el proceso realmente confinado en Ubuntu 26.04, en una VM sin KVM; una
microVM completa en Linux **no se midió**. De `--nested` solo se midió el **rechazo** (este Mac es M1); el camino que
lo activa no se midió.

> **Actualización de la misma fecha:** una microVM completa **se midió** en Linux x86_64 con KVM (un droplet de
> DigitalOcean, KVM anidado), con el job bajo Landlock (por hilo, ABI 4) + seccomp, y el runtime de Kubernetes pasó
> 19/19 en k3s y microk8s. Detalles en [Linux con KVM y Kubernetes](#linux-com-kvm-e-kubernetes).

## Resumen por plataforma

| Plataforma | Entorno | microVM real | Resultado |
|---|---|---|---|
| **macOS** | MacBook M1 Pro, macOS 27.0.1, Hypervisor.framework | **sí** | e2e 42/42 en dos ejecuciones seguidas; runbook y guías sin fallos |
| **Linux** | Ubuntu 26.04.1 LTS en VMPal, kernel 7.0.0-30, aarch64 | **no** (sin `/dev/kvm`) | confinamiento Landlock + seccomp aplicado y demostrado en el proceso real; 30 pruebas en verde en dos ejecuciones |
| **Linux con KVM** | droplet de DigitalOcean `s-4vcpu-8gb`, Ubuntu 24.04, kernel 6.8, x86_64, KVM anidado | **sí** | job Python bajo Landlock (ABI 4, por hilo) + seccomp; Kubernetes 19/19 en k3s 1.36 y microk8s 1.35, dos ejecuciones |
| **Windows** | Windows 11 Pro 24H2 (build 26100.9457) ARM64 en VMPal | **no** | fuera de alcance: build rechazado a propósito; el hipervisor no arranca en la VM (`EL2 not present`) |

El motivo común del "no" en Linux y Windows: ambas son VMs dentro de un **M1 Pro**. Virtualization.framework
solo entrega el nivel de excepción EL2 (necesario para un hipervisor dentro de la VM) a partir del **M3**. Sin EL2, ninguna
VM anidada arranca, ya sea KVM o Hyper-V.

## macOS (destino medido de extremo a extremo)

### Entorno

| Elemento | Valor |
|---|---|
| hardware | MacBook Pro M1 Pro (arm64) |
| sistema | macOS 27.0.1 |
| VMM | libkrun 1.19.6 (derivación Nix del repo, `nix/`) |
| kernel del guest | libkrunfw 5.6.2 → Linux 6.12.109 aarch64 |
| binario | `bin/rc`, firmado ad-hoc con `com.apple.security.hypervisor` |
| guest | `bin/rc-guest-linux-arm64`, estático, 2,6 MB |

### Arranque y recursos (spike, 30 arranques por medida)

Medido con un harness que lanza el proceso de la VM y cronometra hasta la primera respuesta de un job por vsock.

| Medida | Valor |
|---|---|
| arranque hasta la 1.ª respuesta Python, root ext4 de solo lectura | p50 **260 ms**, p95 **266 ms** (mín. 254, máx. 279) |
| arranque hasta la 1.ª respuesta `sh`, root por virtiofs | p50 245 ms, p95 264 ms |
| ciclo completo spawn → `uname` → apagar | 221–252 ms con caché caliente; 869 ms en el primero |
| footprint de VM detenida (512 MiB configurados) | **44 MB** (`footprint`); el RSS de 172 MB incluye páginas limpias compartidas |
| RAM configurada 256 / 1024 / 2048 MiB | arranque 260 / 260 / 261 ms; RSS +40 MB en total |
| `SIGKILL` al grupo del proceso | ningún proceso sobrevive (todos los arranques medidos) |

Decisión tomada a partir de esto: root en disco ext4 de solo lectura. virtiofs no fue más rápido de forma relevante y expone un
directorio del host al guest.

### Red

| Configuración | `wget http://1.1.1.1` | `nc 1.1.1.1 80` |
|---|---|---|
| vsock sin TSI (por defecto en las VMs de job) | `Network unreachable` | rc=1 |
| vsock con TSI (solo la VM de build) | conecta | rc=0 |

Prueba en ambos sentidos: la prueba usa una IP, porque con un nombre el fallo de DNS ocultaría la diferencia.

### Build de las imágenes (15 stacks, `rc image build --all`)

| Stack | Imagen OCI | Build | ext4 |
|---|---|---|---|
| awk | alpine:3.22 | 2,6 s | 45 MiB |
| bash | bash:5.2 | 2,9 s | 57 MiB |
| c | alpine:3.22 + build-base | 19,8 s | 332 MiB |
| cpp | alpine:3.22 + build-base | 19,5 s | 332 MiB |
| go | golang:1.25-alpine | 35,3 s | 377 MiB |
| java | eclipse-temurin:21-jdk-alpine | 34,0 s | 478 MiB |
| jq | alpine:3.22 + jq | 2,7 s | 47 MiB |
| nodejs | node:22-alpine | 14,1 s | 234 MiB |
| perl | alpine:3.22 + perl | 8,4 s | 98 MiB |
| php | php:8.3-cli-alpine | 9,4 s | 152 MiB |
| python | python:3.12-alpine | 8,2 s | 128 MiB |
| ruby | ruby:3.3-alpine | 8,2 s | 134 MiB |
| rust | rust:1-alpine | 37,4 s | 1,2 GiB |
| sh | alpine:3.22 | 2,2 s | 45 MiB |
| sqlite | alpine:3.22 + sqlite | 2,8 s | 49 MiB |

El `mkfs.ext4` domina el tiempo de las imágenes grandes (27,8 s de los 35 s de Go): el ext4 se escribe en el host vía virtiofs.
Pull y aplanado de `python:3.12-alpine`: 2,7 s, 55 MB.

### Ejecución por stack (`rc run`, arranque en frío)

| Stack | Ejecución | Total (con arranque) |
|---|---|---|
| python | 31–44 ms | ~240 ms |
| nodejs | 128–148 ms | 353–371 ms |
| go | **247 ms** con la caché precalentada (3.429 ms sin ella) | 510 ms |
| java (JDK 21.0.12) | 522 ms | 739 ms |
| c | 71–72 ms | 282 ms |
| sh | 2 ms | 199 ms |

### API con pool pre-arrancado

| Situación | Solicitud HTTP completa |
|---|---|
| 1 stack con imagen, pool de 2 | 26–43 ms |
| 15 stacks, otras todavía reponiendo el pool | 73–78 ms |
| 15 stacks, todos los pools llenos | 49–175 ms |
| antes del límite global de arranques (30 VMs arrancando a la vez) | 1,7–2,3 s (regresión encontrada y corregida) |

### Suite e2e (`task test:e2e`, tag `e2e`)

42 pruebas, ejecutadas dos veces seguidas y en verde en ambas. Cada prueba fija un comportamiento medido. La tabla siguiente
describe las que fijan el contrato de los jobs y de las máquinas; las demás están en la tabla posterior.

| Prueba | Qué demuestra |
|---|---|
| `TestStdoutAndExitCode` | salida correcta; el `exit 3` del programa se convierte en rc≠0 de la CLI |
| `TestTimeoutKillsVM` | un bucle infinito excede el tiempo en ~2,2 s y no queda ningún proceso de VM |
| `TestNoNetwork` | conexión a 1.1.1.1:80 bloqueada. **Prueba de fallo:** con `Network: true` plantado en el pool la prueba falló (la sonda Python imprimió que se conectó; el mensaje estaba en portugués en ese momento y hoy es `guest reached the network: "CONNECTED\n"`); restaurado, pasó |
| `TestRootReadOnlyAndIsolation` | `/etc` no acepta escritura; un archivo escrito en `/tmp` por un job no existe en el siguiente |
| `TestMemoryCeiling` | asignar 2 GiB en una VM de 512 MiB falla dentro de la VM sin afectar al host |
| `TestLanguageFamilies` | 13 stacks se ejecutan (nodejs, go, java, c, cpp, rust, ruby, php, perl, bash, awk, jq, sqlite); un fallo de compilación Java sale con rc≠0 |
| `TestShell` | `rc shell` con stdin por pipe arranca con root de solo lectura, `/tmp` escribible y sin red; en el stack bash es bash 5.2 de verdad |
| `TestShellTTY` | `rc shell` bajo un pseudoterminal real (`creack/pty`, 40×120): el stdin del guest es una terminal, `stty size` devuelve `40 120`, y Ctrl+C interrumpe un `sleep 30` sin tumbar la VM (la prueba tarda ~1,4 s; sin la interrupción tardaría 30 s). **Prueba de fallo:** sin el modo raw en el host la prueba falló (`VM survived the shell exit`); restaurado, pasó |
| `TestServePool` | `serve` llena los pools, atiende `/run` y, ante SIGTERM, no deja VMs vivas |
| `TestRunStreams` | `rc run` con Python que escribe, duerme 1,5 s y vuelve a escribir: la 1.ª línea llega al stdout del host ≥ 1 s antes que la 2.ª (medido: 0,25 s y 1,75 s después del inicio). **Prueba de fallo:** con `rc run` acumulando la salida, las dos líneas llegaron con 3,8 µs de diferencia y la prueba falló |
| `TestServeStream` | `POST /api/v1/execute/stream` con la VM real: `text/event-stream`, eventos `stdout` con la 1.ª línea ≥ 1 s antes que la 2.ª, `result` final con `exitCode` 0; un job rechazado responde 400 antes del stream |
| `TestMachineLifecycle` | `rc machine`: create, CONFLICT con un nombre repetido, INVALID_STATE en el exec con la máquina detenida, start (198–244 ms), escritura en `/etc`, `/opt` y `/workspace`, rc del exec reenviado (7), Python del stack ejecutándose dentro, stop, logs con la máquina detenida, start de nuevo: todo persistió y `/tmp` volvió vacío; delete no deja VMs. **Prueba de fallo:** con la máquina arrancando sin `machine.sh`, la escritura en `/etc` falló y la prueba falló |
| `TestMachineShellTTY` | `rc machine shell` bajo un pseudoterminal real (40×120) en una máquina encendida: el shell ve una TTY y `40 120`, Ctrl+C interrumpe un `sleep 30` sin tumbar el shell, lo que el shell escribe en `/workspace` aparece para el exec, `exit 4` se convierte en rc 4 y la máquina sigue encendida. **Prueba de fallo:** sin devpts en `machine.sh` (el defecto real encontrado: `open /dev/ptmx: no such device`), el shell no se abrió y la prueba falló |
| `TestRunnerCode` | máquina de un `runner.code` con `--cpus 1` por encima del archivo (la flag prevalece); el workdir `/app` se crea en el start; el workload arranca con env y workdir y aparece en `logs --source workload`; el init se ejecuta una vez (contador en 1 después de stop+start); el stop envía `SIGINT` y el trap del workload escribe antes del sync. **Prueba de fallo (dos defectos plantados):** stop enviando SIGTERM en lugar del `stop_signal` → el archivo del trap no existe; init ignorando el marcador → contador en 2. Ambos fallaron |
| `TestRunnerCodeStopOnExit` | con `stop_on_exit`, el fin del workload apaga la máquina por sí solo (~0,5 s) y el log muestra la salida y `exited with code 0` |
| `TestRunnerCodeRefusals` | `net = true` rechazado con el motivo y nada creado; sin stack ni image es un error |
| `TestRunnerCodeImage` | `--image alpine:3.20` (imagen fuera del catálogo): construida, arranca y ejecuta `cat /etc/alpine-release` |
| `TestMachineNoNetwork` | una máquina sin `--net` no tiene `eth0` y no sale |
| `TestMachineNetwork` | con `--net`: `eth0` `100.96.0.2/30`, DNS en el gateway, `example.com` responde; metadata `169.254.169.254` rechazada al instante incluso con la red abierta |
| `TestMachineEgressPolicy` | `--allow-host example.com`: el nombre pasa, `iana.org` da `bad address` en menos de 1 s y va a `egress-denials.log`; `machine update` permite y quita con la máquina encendida. **Prueba de fallo:** con el gateway ignorando la política, la metadata y el nombre bloqueado pasaron y la prueba falló |
| `TestMachinePortsAndVolumes` | `runner.code` con volumen rw y ro, puerto publicado y sin `--net`: el workload sirve el archivo del host por el puerto, la escritura del guest aparece en el host, el `:ro` rechaza la escritura **incluso después de `mount -o remount,rw` por el root del guest**, el guest no sale hacia afuera, y los volúmenes vuelven en cada start. **Prueba de fallo:** con el `:ro` solo en la opción de montaje (sin la flag en el dispositivo), el remount abrió el volumen y la prueba falló. Antes de esta comprobación, ese defecto pasaba desapercibido |
| `TestMachineCopy` | `rc machine cp` de 5 MiB (varios fragmentos) hacia la máquina y de vuelta: sha256 igual dentro y fuera |
| `TestMachineAPI` | rutas `/api/v1/machines`: 201/409/200/204/404, exec con rc 4, exec/stream con la 1.ª línea ≥ 1 s antes que la 2.ª y `exit` al final, PUT/GET de 2,6 MB idénticos, INVALID_STATE (409) con la máquina detenida, stop en menos de 5 s. **Prueba de fallo:** con la VM desacoplada sin Wait (zombi en `rc serve`), el stop tardó 10 s y la prueba falló |
| `TestGPUVulkan` | `--gpu` en el stack `llama`: `vulkaninfo` muestra `Virtio-GPU Venus (Apple M1 Pro)`; sin `--gpu` no existe `/dev/dri`; `--gpu` en un stack sin GPU se rechaza con E007. **Prueba de fallo:** con `Configure` sin la llamada a `krun_set_gpu_options2`, la prueba falló (`no Venus device with --gpu`) |
| `TestGPULlama` | `llama-bench` con el mismo modelo en la GPU (`-ngl 99 -dev Vulkan0`) y en la CPU: el dispositivo usado es Venus y el prompt sale ≥ 2× más rápido (medido 893–1.365 t/s frente a 193–264 t/s, 4 ejecuciones) |

Pruebas que entraron con las stories 7.4 a 7.6 y con las guías:

| Prueba | Qué demuestra |
|---|---|
| `TestMachineRun` | `machine run`: el comando se ejecuta en una máquina que desaparece al final, el rc es el del comando, `-i` reenvía el stdin, sin nada que ejecutar es un error antes de crear |
| `TestMachineRunDetached` | `-d` deja la máquina encendida con el workload bajo el nombre pedido |
| `TestMachineRunTTY` | `-it` bajo un pseudoterminal real abre el shell con TTY; su `exit` se convierte en el rc y la máquina desaparece |
| `TestMachineRunCallerKilled` | un `machine run` cuya CLI recibe SIGKILL no deja la VM viva sin dueño (antes de la corrección, quedaba) |
| `TestMachineBranchAndPause` | pausada, la máquina rechaza exec con el motivo; el branch lleva lo que la madre acaba de escribir; madre e hija siguen independientes; `resume` reactiva la madre |
| `TestMachineEgressCIDROnly` | con solo `allow_cidrs`, un nombre resuelto no abre conexión (brecha cerrada el 19/10/2026); el CIDR permitido sigue alcanzable |
| `TestMachinePing` | ICMP pasa por el gateway bajo la misma política que TCP; un destino fuera de la allowlist no responde y queda en el log de denegaciones |
| `TestNamedNetworkMachines` | dos máquinas de la misma red se alcanzan por IP y por `<máquina>.<rede>.internal`, sin `--net`; otra red no las alcanza; una máquina detenida desaparece del DNS |
| `TestMachineCredential` | el guest solo ve el placeholder y el servicio HTTPS recibe el valor real; un placeholder en la URL se rechaza con 403; un host fuera del binding recibe el placeholder literal |
| `TestSecretsAndSSHAgent` | `--secret-env`/`--secret-file` llegan al exec y no quedan en el registro; una variable ausente hace fallar el start; `--ssh-agent` lista la clave del host sin que la clave privada entre en la VM |
| `TestExposeAndMountSockets` | `--expose-socket` y `--mount-socket`, en ambas direcciones |
| `TestDockerSocketAndDiskVolume` | `docker_socket` responde en el directorio de la máquina; un volumen de disco es un ext4 de verdad, fuera del overlay, y persiste |
| `TestStagedVolume` | `:staged`: el host no cambia hasta el sync; el sync replica; el stop ordenado sincroniza; la hija de un branch no devuelve nada |
| `TestMachineS3Volume` | volumen `s3://` contra un servidor S3 real: lectura, escritura byte a byte (8 MiB), `mv` y `rm`, `:ro`, remontaje tras stop/start, una credencial incorrecta hace fallar el start con el motivo |
| `TestMachineDisplay` | el fotograma del guest llega al visor del host con los colores correctos; tecla y clic por `POST /input` llegan al evdev del guest |
| `TestPack` | stack en stub + payload; máquina en archivo único leída con el origen borrado; modo persistente; `create --from` y `pack inspect`; rechazos |
| `TestDevStdinLinks` | `/dev/stdin`, `/dev/stdout`, `/dev/stderr` y `/dev/fd` existen en la VM de job y en la máquina |

### `rc shell`

| Verificación | Resultado |
|---|---|
| python: `python3`, `/tmp` escribible, `/etc` de solo lectura, sin red | ok |
| go: `go version` con el PATH de la imagen | `go1.25.14 linux/arm64` |
| bash: prompt y `[[ ]]` | `bash-5.2#`, ok |
| bajo pseudoterminal | stdin del guest = `/dev/console` sobre `hvc0`; `test -t 0` verdadero; `stty size` = tamaño de la terminal del host |
| Ctrl+C | interrumpe el comando en el guest; la VM sigue viva |

Cómo funciona: con una terminal en el stdin, `rc shell` activa `tty` en la spec (`krun_disable_implicit_console` +
`krun_add_virtio_console_default(0, 1, 2)`; con los tres fds siendo TTY la libkrun crea un único puerto de consola) y
pone la terminal del host en modo raw durante la sesión, restaurándola antes de salir. Con stdin por pipe el comportamiento
sigue siendo el de antes (consola implícita, sin TTY). El comando `tty` todavía imprime `not a tty`, porque no encuentra
el nombre del dispositivo; `test -t 0` y `stty` muestran que es una terminal.

**Evento de carga:** una ejecución de la suite con `-count=2` falló en 6 de 9 pruebas con un load average de ~24 en el host
(VMs de VMPal al 88% de CPU y `nix-daemon` al 79%); `TestServePool` tardó 40 s frente a los 7–8 s normales. Aisladas y
en dos ejecuciones siguientes, las mismas suites pasaron completas.

### Números del lote del 20/10/2026

| Medida | Valor |
|---|---|
| pack | imagen python 134 MiB → payload de 31 MiB; máquina con paquete instalado, archivo único: 46 MiB |
| volumen S3 | 8 MiB escritos en el guest llegan al bucket con el mismo md5 (servidor S3 local) |
| defecto de seguridad encontrado | `allow_cidrs` por sí solo permitía salir hacia cualquier nombre; corregido y fijado por `TestMachineEgressCIDROnly` |

## Linux (confinamiento probado; microVM no)

### Entorno

| Elemento | Valor |
|---|---|
| VM | "Ubuntu VM 2" en VMPal, 4 vCPU, 7,9 GB |
| sistema | Ubuntu 26.04.1 LTS, kernel 7.0.0-30-generic, aarch64 |
| LSMs activos | `lockdown,capability,landlock,yama,apparmor,ima,evm` |
| Landlock | **ABI 8** (`landlock_create_ruleset(NULL, 0, LANDLOCK_CREATE_RULESET_VERSION)`) |
| `/dev/kvm` | **ausente** (sin EL2 anidado en el M1 Pro) |

### Método

El binario de pruebas del paquete `internal/adapters/krun` se compila en el Mac para `linux/arm64` y se ejecuta en la VM:

```bash
CGO_ENABLED=0 GOOS=linux GOARCH=arm64 go test -c -cover -o krun.test ./internal/adapters/krun/
vmpal cp krun.test "Ubuntu VM 2:rc-sandbox/krun.test"
# en la VM:
./krun.test -test.v -test.count=1 -test.coverprofile=cover.out
```

La prueba del confinamiento relanza el propio binario como **sonda**. La sonda aplica la política real
(`ApplySandbox`, la misma función que llama el proceso `vmm`) e intenta cada operación. El proceso de prueba en sí
nunca se confina.

### Resultado del confinamiento (sonda confinada)

| Operación | Esperado | Medido |
|---|---|---|
| leer el disco root | permitido | permitido |
| crear el socket de la propia VM (`bind`) | permitido | permitido |
| leer un archivo fuera de la política | denegado | **denegado** (Landlock) |
| escribir junto al disco root | denegado | **denegado** (Landlock) |
| `connect` TCP | denegado | **denegado** (Landlock, red) |
| `execve("/bin/true")` | denegado | **denegado** (seccomp, `EPERM`) |
| `personality(0xffffffff)` | denegado | **denegado** (seccomp, `EPERM`) |
| `connect` al socket Unix de otra VM | denegado a partir de la ABI 9 | **permitido** (kernel con ABI 8) |

**Prueba de fallo** (`TestProbeWithoutSandbox`): la misma sonda sin confinamiento lee el archivo prohibido, escribe junto
al disco, se conecta al socket de otra VM, abre TCP y llama a `personality`. Es decir, la prueba distingue.

`ptrace` no se usa como prueba de seccomp: en Ubuntu, Yama ya niega `ptrace` al padre sin ningún confinamiento.

### Resultado de la suite en Linux

30 pruebas, en verde en dos ejecuciones seguidas (`TestLoadRealLibrary` omitida: sin libkrun en la VM). Cobertura del paquete:
81,9%. El código ejecutado dentro del subproceso confinado no entra en la medición.

### `rc doctor` en la VM

```
FAIL  platform     no access to /dev/kvm: open /dev/kvm: no such file or directory
ok    sandbox landlock ABI 8 + seccomp denylist
warn  landlock ABI 8 < 9: a VM process can still connect to another VM's unix socket
```

### No probado en Linux

- una microVM completa (arranque, job, timeout) bajo Landlock + seccomp: exige KVM;
- la lista de rutas que la libkrun abre en `krun_start_enter` en Linux. La política permite las bibliotecas
  cargadas, `/proc`, `/sys`, `/dev/kvm` y los nodos de dispositivo comunes, y precarga la libkrunfw. Si la libkrun
  necesita otra ruta, la VM no arranca; `RUNNER_SANDBOX=off` desactiva el confinamiento para diagnóstico;
- `linux/amd64` (solo compilado).

## Linux con KVM y Kubernetes {#linux-com-kvm-e-kubernetes}

### Entorno

| Ítem | Valor |
|---|---|
| máquina | droplet de DigitalOcean `s-4vcpu-8gb` (4 vCPU, 8 GB), nyc3; `scripts/do-kvm-probe.sh` midió `/dev/kvm` y las flags `vmx` antes de crear el nodo de prueba |
| sistema | Ubuntu 24.04, kernel 6.8.0-142, x86_64; Landlock **ABI 4** |
| libkrun | 1.19.5 y libkrunfw 5.5.0 de nixpkgs (`task krun:install`) |
| Kubernetes | k3s v1.36.5 (containerd 2.3.4, flannel) y microk8s v1.35.6 (containerd 2.1.6, Calico), un droplet cada uno |

### Resultado

| Qué | Medido |
|---|---|
| `rc run --stack python`, arranque en frío | `status=success`, ~1,4 s de extremo a extremo (KVM anidado; en el Mac, ~240 ms) |
| `rc image build --stack python` | 60 s (el `mkfs` tardó 45 s) |
| pod `runtimeClassName: rc` hasta `Running` | ~2 s, imagen ya en el nodo |
| `deploy/k8s/e2e.sh` | **19/19** en k3s y microk8s, dos ejecuciones seguidas en cada uno |
| prueba de fallo del e2e | código de salida fijo en 0 en el shim: las 2 comprobaciones de código de salida fallaron |
| `task k8s:defects` | 20 defectos plantados; 19 atrapados en la 1.ª ejecución, el que se escapó se volvió test, 20 de 20 después |
| `TestRestrictThreadBlocks` (root, kernel real) | Landlock por hilo bloquea el archivo de fuera; sin `landlock_restrict_self` el test falla |
| `TestAttach` (root, netns real) | tap0 nace en el netns del pod y no en el del host, con un filtro mirred en cada sentido |

### Defectos encontrados solo en Linux real

| Síntoma | Causa | Corrección |
|---|---|---|
| `nix profile add .#krun` falla con `File exists` | en la libkrun de nixpkgs, `lib/` y `lib64/` traen los mismos archivos | `ln -sf` en `flake.nix` |
| todo job falla: `prctl(PR_SET_NO_NEW_PRIVS): operation not supported` | por debajo de la ABI 8 go-landlock usa psx, y sin cgo `AllThreadsSyscall` devuelve ENOTSUP (purego hace que el runtime se vea "con cgo") | ruleset en el hilo de `krun_start_enter` (`sandbox_rules.go`); los hilos de libkrun lo heredan |
| el test del launcher no compilaba en Linux | `syscall.Getsid` solo existe en BSD/macOS | `unix.Getsid` |
| la copia del rootfs "falla" con `waitid: no child processes` | pkg/shim recoge todo hijo en el SIGCHLD | los hijos del shim esperan por `reaper.Default` |
| el resumen del kubelet se rompe en todo el nodo; namespace atascado en Terminating | `Stats` vacío del shim (`failed to decode container metrics`) | métricas cgroup v2 con el uso real del vmm |
| microk8s: `unsupported protocol: Yunix` | containerd 2.1 leía la respuesta protobuf del start como dirección | JSON antiguo para quien llama por el protocolo antiguo |
| microk8s: pod sin red (`Network is unreachable`) | Calico da un `/32` con gateway `169.254.1.1` fuera de la subred | ruta de enlace al gateway antes de la predeterminada |

### No probado en Linux con KVM

- `linux/arm64` con KVM (solo x86_64);
- Landlock con ABI ≥ 8 (camino TSYNC) con una microVM real: el droplet tiene ABI 4;
- `--nested` activado (el `rc doctor` del droplet dice `supported`, sin prueba);
- el vmm de la VM de pod corre **sin** Landlock/seccomp (deuda declarada).

## Windows (fuera de alcance, medido)

### Entorno

| Elemento | Valor |
|---|---|
| VM | "Windows VM 1" en VMPal |
| sistema | Windows 11 Pro 24H2, build 26100.9457, ARM64, CPU "Apple Silicon" |

### Build de `rc`

```
GOOS=windows GOARCH=arm64|amd64 go build ./cmd/rc
undefined: runnerCodesDoesNotSupportWindows_libkrunUpstreamHasNoWHPBackend
```

El rechazo es deliberado: la libkrun upstream solo tiene backend KVM e Hypervisor.framework, sin Windows Hypervisor
Platform.

### Hipervisor dentro de la VM

| Paso | Medido |
|---|---|
| estado inicial | `HypervisorPresent=False`; `VirtualizationFirmwareEnabled`, `VMMonitorModeExtensions` y `SLAT` todos `False`; WSL no instalado |
| `vmpal set --nested on` | Windows: `nested virtualization is for Linux VMs`; Ubuntu: `Nested virtualization needs a Mac with M3 or later` |
| punto de restauración de VMPal `antes-hypervisor-2026-10-19` | creado |
| `Enable-WindowsOptionalFeature` HypervisorPlatform, VirtualMachinePlatform, Microsoft-Hyper-V-All | habilitados, `RestartNeeded=True` |
| tras reiniciar | `bcdedit hypervisorlaunchtype Auto`; `vmms` y `vmcompute` en ejecución; **`HypervisorPresent=False`** |
| log del sistema | `Microsoft-Windows-Hyper-V-Hypervisor id=43: Hypervisor launch failed; EL2 not present.` |
| revertido al punto de restauración | los tres componentes `Disabled`; `vmms` y `vmcompute` ausentes |

Conclusión: sin EL2 la VM no ejecuta ningún hipervisor, y no hay configuración de Windows que lo evite. Incluso con
soporte de `rc`, esa VM no tendría dónde crear microVMs.

## Lo que falta para completar la matriz

| Brecha | Qué la desbloquea |
|---|---|
| microVM bajo Landlock con TSYNC (ABI ≥ 8) | Linux con KVM y Landlock ABI ≥ 8 (Ubuntu 26.04, kernel 7.0, tiene la 8) (la ABI 4 del droplet ya se midió, por el camino por hilo) |
| `linux/arm64` con KVM en ejecución | Linux arm64 con KVM (`linux/amd64` se midió en el droplet) |
| aislamiento de sockets Unix entre VMs | kernel con Landlock ABI ≥ 9 |
| `--nested` activándose de verdad | Mac M3+ o Linux con KVM anidable |
| Windows nativo | cambiar la libkrun upstream por el fork con WHP (decisión de arquitectura) y una máquina Windows con virtualización |
