---
title: 'Build'
description: 'Compiling the host and the guest, signing on macOS and building images'
---

## task build

```bash
task build
```

Runs `scripts/build.sh`, which:

1. compiles the host `bin/rc` for the current system, with `CGO_ENABLED=0` (libkrun is loaded through purego at runtime);
2. compiles the guest `bin/rc-guest-linux-<arch>` from `./cmd/runner-guest`: always Linux, static, same architecture as the host — libkrun does not emulate another architecture;
3. **fails** if the guest comes out dynamic, because a dynamic binary does not run in the musl image (`not found`);
4. on macOS, signs the host with `codesign` and the `com.apple.security.hypervisor` entitlement (`build/hv.entitlements`). Without it, `krun_start_enter` fails.

The version stamped into the binaries comes from `VERSION` or from `git describe`; without git, `dev`. It is what shows up in `GET /health`.

### Why two binaries

purego forces dynamic linking against glibc on Linux. The language images are Alpine (musl). That is why the guest is a separate `main`, without purego, compiled statically.

## task image:build

```bash
task image:build -- --stack python
task image:build -- --all
```

Depends on `task build` (the guest must exist to be injected). See [image](/cli/image) and [Images](/concepts/images).

## Installing libkrun

```bash
task krun:install
```

| System | How |
|--------|-----|
| macOS | `task krun:install`: libkrun 1.19.6 / libkrunfw 5.6.2 from the derivations in `nix/` |
| Linux | `task krun:install`: libkrun / libkrunfw from nixpkgs, with virtio-blk enabled |

Both install `.#krun` into the Nix profile (`~/.nix-profile/lib`), where `rc` looks by default.

## Windows

There is no Windows build. Compiling for `windows/amd64` fails on purpose, with a message stating the reason (upstream libkrun has no WHP backend). The `task test:targets` gate checks that this rejection keeps happening.
