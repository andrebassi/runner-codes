---
title: 'Guest runner'
description: 'The static binary that runs inside the VM and executes the job code'
---

## What it is

`runner-guest` is the process that executes code inside the microVM. It is compiled statically for Linux and injected into every image as `/opt/runner/guest`. On the host, it lives at `bin/rc-guest-linux-<arch>`.

**Why static and separate from the host:** the host loads libkrun through purego, which forces dynamic linking against glibc on Linux. The language images are Alpine (musl); a dynamic binary there fails with `/opt/runner/guest: not found`.

## How it starts

The first process of a job VM is `/opt/runner/start.sh`:

```sh
mount -t tmpfs -o size=256m,mode=1777 tmpfs /tmp
# /dev/stdin, /dev/stdout, /dev/stderr and /dev/fd as links to /proc/self/fd
...
# environment from the image's OCI config (PATH, JAVA_HOME...), one variable per line
while IFS= read -r l; do [ -n "$l" ] && export "$l"; done < /opt/runner/env
mkdir -p /tmp/home
export HOME=/tmp/home
# Go build cache warmed in the image, copied to the path the executor enforces
if [ -d /opt/runner/gocache ]; then
  cp -a /opt/runner/gocache /tmp/go-cache
fi
exec /opt/runner/guest --port 5000
```

(Summary of `internal/adapters/oci/scripts/start.sh`.) In a persistent machine, the first process is
`/opt/runner/machine.sh`, which mounts the disk and the overlay and runs the same guest in a `chroot`.

The guest listens on vsock port 5000. libkrun binds that port to a Unix socket on the host.

## What it does with a job

1. Reads a frame (4 big-endian bytes of length + JSON) with the [job](/api-reference/job-request).
2. Checks the language and the stdin size.
3. Creates a temporary directory under `/tmp` and writes the files:
   - with `files`, it writes each one by name; the **first** is the main file;
   - with only `code`, it writes `script.<ext>` (Java: `Main.java`).
4. Runs it according to the language type:
   - **interpreted** (Python, Node.js, Ruby, PHP, Perl, Bash, sh, AWK, jq): `<interpretador> <arquivo>`;
   - **compiled** (C, C++, Rust): compiles to a binary and runs the binary;
   - **JVM** (Java): `javac` and then `java Main`;
   - **Go:** `go run`;
   - **SQLite:** the code goes through the stdin of `sqlite3 :memory:`.
5. Applies the job timeout (default 10 s).
6. Responds with a [result](/api-reference/job-response) frame.

A compilation failure returns `exception: "Compilation failed"` with the compiler error in `stderr`. A timeout returns `exception: "Timeout"`.

## Other operations

Besides running code, the guest serves the machine operations, all over the same protocol: `exec` (argv with no
implicit shell, with env, directory and user), `put`/`get` (file copy in 1 MiB chunks), `pty` (interactive
terminal), `workload` (the process the machine keeps running), `shutdown` (signal to the workload, `sync` and exit),
`dial` (published port, dialing the guest's `127.0.0.1`), `set-env` (in-memory-only secrets), `ssh-agent` (socket for
the host's SSH agent) and `mount-s3` (`s3://` volume via FUSE). Details in [vsock protocol](/api-reference/vsock-protocol).

## Isolation between jobs

The guest does not need to clean anything up between jobs: each VM serves a single job and is destroyed. The tmpfs goes away with it.

## Languages the guest knows

The guest executor (`internal/guest/executor.go`) has entries for more languages than the catalog publishes. Only the 15 in [`languages.yaml`](/languages/overview) have an image and are accepted by the API.
