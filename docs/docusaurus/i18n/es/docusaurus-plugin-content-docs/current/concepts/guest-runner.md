---
title: 'Guest runner'
description: 'El binario estático que se ejecuta dentro de la VM y ejecuta el código del job'
---

## Qué es

El `runner-guest` es el proceso que ejecuta el código dentro de la microVM. Se compila de forma estática para Linux y se inyecta en cada imagen como `/opt/runner/guest`. En el host, se encuentra en `bin/rc-guest-linux-<arch>`.

**Por qué estático y separado del host:** el host carga la libkrun mediante purego, lo que obliga a un enlace dinámico contra la glibc en Linux. Las imágenes de los lenguajes son Alpine (musl); un binario dinámico allí falla con `/opt/runner/guest: not found`.

## Cómo arranca

El primer proceso de una VM de job es `/opt/runner/start.sh`:

```sh
mount -t tmpfs -o size=256m,mode=1777 tmpfs /tmp
# /dev/stdin, /dev/stdout, /dev/stderr y /dev/fd como enlaces a /proc/self/fd
...
# entorno de la config OCI de la imagen (PATH, JAVA_HOME...), una variable por línea
while IFS= read -r l; do [ -n "$l" ] && export "$l"; done < /opt/runner/env
mkdir -p /tmp/home
export HOME=/tmp/home
# caché de build de Go precalentada en la imagen, copiada a la ruta que el executor impone
if [ -d /opt/runner/gocache ]; then
  cp -a /opt/runner/gocache /tmp/go-cache
fi
exec /opt/runner/guest --port 5000
```

(Resumen de `internal/adapters/oci/scripts/start.sh`.) En una máquina persistente, el primer proceso es
`/opt/runner/machine.sh`, que monta el disco y el overlay y ejecuta el mismo guest en un `chroot`.

El guest escucha en el puerto vsock 5000. La libkrun conecta ese puerto a un socket Unix en el host.

## Qué hace con un job

1. Lee un frame (4 bytes big-endian de tamaño + JSON) con el [job](/api-reference/job-request).
2. Verifica el lenguaje y el tamaño del stdin.
3. Crea un directorio temporal en `/tmp` y escribe los archivos:
   - con `files`, escribe cada uno por su nombre; el **primero** es el archivo principal;
   - solo con `code`, escribe `script.<ext>` (Java: `Main.java`).
4. Ejecuta según el tipo de lenguaje:
   - **interpretado** (Python, Node.js, Ruby, PHP, Perl, Bash, sh, AWK, jq): `<intérprete> <archivo>`;
   - **compilado** (C, C++, Rust): compila a un binario y ejecuta el binario;
   - **JVM** (Java): `javac` y luego `java Main`;
   - **Go:** `go run`;
   - **SQLite:** el código entra por el stdin de `sqlite3 :memory:`.
5. Aplica el timeout del job (por defecto 10 s).
6. Responde con un frame de [resultado](/api-reference/job-response).

Un fallo de compilación devuelve `exception: "Compilation failed"` con el error del compilador en `stderr`. Exceder el tiempo devuelve `exception: "Timeout"`.

## Otras operaciones

Además de ejecutar código, el guest atiende las operaciones de las máquinas, todas por el mismo protocolo: `exec` (argv sin shell
implícito, con env, directorio y usuario), `put`/`get` (copia de archivos en fragmentos de 1 MiB), `pty` (terminal
interactiva), `workload` (el proceso que la máquina mantiene en ejecución), `shutdown` (señal al workload, `sync` y salida),
`dial` (puerto publicado, conectando al `127.0.0.1` del guest), `set-env` (secretos solo en memoria), `ssh-agent` (socket del
agente SSH del host) y `mount-s3` (volumen `s3://` por FUSE). Detalles en [Protocolo vsock](/api-reference/vsock-protocol).

## Aislamiento entre jobs

El guest no necesita limpiar nada entre jobs: cada VM atiende un único job y se destruye. El tmpfs desaparece con ella.

## Lenguajes que conoce el guest

El executor del guest (`internal/guest/executor.go`) tiene entradas para más lenguajes de los que publica el catálogo. Solo los 15 de [`languages.yaml`](/languages/overview) tienen imagen y son aceptados por la API.
