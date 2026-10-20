---
title: 'Troubleshooting'
description: 'Known errors, their cause and the fix'
---

## First step

```bash
rc doctor
```

It checks the platform, libkrun, GPU, CUDA, nested virtualization, guest, confinement and images. Each line starts with
`ok`, `info`, `warn` or `FAIL`, and a failure includes the reason.

## Couldn't find or load libkrunfw.5.dylib

**Cause:** upstream libkrun opens libkrunfw by name only (`dlopen("libkrunfw.5.dylib")`), and the dynamic loader cannot find it outside the standard paths.

**Fix:** the libkrun from `.#krun` (macOS) already opens libkrunfw by its absolute path in the Nix store, so the error only shows up with a libkrun from some other source. In that case the launcher still sets `DYLD_LIBRARY_PATH` to the directory of `RUNNER_KRUN_LIB`: check that it is a full path and that libkrunfw is in the same directory.

## krun_start_enter rc=-22

**Cause:** on macOS, the binary lacks the `com.apple.security.hypervisor` entitlement.

**Fix:** build with `task build`, which signs the host. A plain `go build` produces an unsigned binary.

## /opt/runner/guest: not found

**Cause:** the guest was built dynamically. The images are Alpine (musl) and do not have the glibc loader; the kernel finds the file, but not the interpreter, and reports `not found`.

**Fix:** build with `task build`, which produces a static guest and fails if it comes out dynamic. Then rebuild the images.

## Old behavior after changing the guest

**Cause:** the image in use was built with the previous guest.

**Fix:** run `rc image build --stack <l>` again. The cache key includes the guest, so a new image is produced on its own; there is no need to delete anything.

## An argument with a space arrives split in the guest

**Cause:** libkrun passes the first process's argv through the kernel command line, which splits on spaces.

**Fix:** do not pass a compound command as an argument. Write it to a file and run the file — that is what `start.sh` and the build warmup do.

## VM for X did not become ready ... console: ...

**Cause:** the guest did not answer the `sh true` job from the readiness wait. The message includes the tail of the guest console, which usually states the reason (dynamic guest, `start.sh` failing, corrupted image).

**Fix:** read the console excerpt in the message. The most common cases are the ones on this page: a dynamic guest or an outdated image.

## machine X did not become ready / failed to start

**Cause:** the machine's `vmm` process died during boot or the guest did not respond. The message quotes the last line
of `vmm.log` written during **this** start (the log is append-only), for example `nested virtualization is not
supported on this host`.

**Fix:** read the quoted line. The full log is the `vmm.log` in the directory that `rc machine data-dir --name X`
prints; the console is shown by `rc machine logs --name X`.

## INVALID_STATE: machine X is running; stop it first or use force

**Cause:** `delete` (and other stopped-machine operations) on a running machine.

**Fix:** `rc machine stop --name X` first, or `--force` to kill it.

## machine X is paused; resume it (rc machine resume) or stop it first

**Cause:** `start` or `exec` on a paused machine.

**Fix:** `rc machine resume --name X`, or `stop` to shut it down.

## resolve secret 'NOME': env_unset

**Cause:** a `--secret-env` (or `[secrets]` with `from_env`) points to a variable that does not exist in the
environment of whoever ran `start`.

**Fix:** export the variable before `start`; the value is read on every start and is never stored.

## The program in the machine cannot resolve a name (`bad address`)

**Cause:** the egress policy does not allow the name; the gateway's DNS answers NXDOMAIN on purpose.

**Fix:** check `egress-denials.log` in the machine directory (`rc machine data-dir`) and allow it with
`rc machine update --name X --allow-host <nome>`, with the machine running. See [Network and volumes](/cli/network).

## E007

**Cause:** `--gpu` (or `"gpu": true`) on a stack without `gpu` in the catalog.

**Fix:** use a stack with GPU, such as `llama`. CUDA does not exist: the GPU is Vulkan (Venus). See [GPU](/advanced/gpu).

## E006: unsupported language

**Cause:** the language is not in the catalog **or** its image has not been built on this host yet.

**Fix:** `rc languages` shows the catalog and `ready`. Build it with `rc image build --stack <l>`.

## E005: stdin too long

**Cause:** stdin above 1,000,000 bytes.

**Fix:** reduce the input or send the data as a file in `files` (up to 4 MiB across all files).

## No access to /dev/kvm (Linux)

**Cause:** the user lacks read and write permission on `/dev/kvm`, or KVM is not enabled.

**Fix:** enable KVM on the host and give the user access to the device (usually via the `kvm` group).

## Windows

Not supported: upstream libkrun has no WHP backend. The Windows build fails on purpose.

## The API returned "status": "success", but the program failed

This is not an error. `status` says the API ran the job. The program's result is in `stderr` and `exception` (`"Timeout"`, `"Compilation failed"`).
