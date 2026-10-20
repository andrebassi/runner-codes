---
title: 'image'
description: 'Build a language''s read-only ext4 from the official OCI image'
---

## Usage

```bash
rc image build --stack python
rc image build --stack python --stack go
rc image build --stack python,go
rc image build --all
```

| Flag | Description |
|------|-----------|
| `--stack` | catalog language; repeatable or comma-separated |
| `--all` | every language in the catalog |

Without `--stack` or `--all`, the command fails with `informe --stack ou --all`. A language outside the catalog fails with E006.

Through the Taskfile (which compiles first):

```bash
task image:build -- --stack python
task image:build -- --all
```

## image prune

```bash
rc image prune          # old builds no name points to anymore (the guest or start.sh changed, the key changed)
rc image prune --all    # also the images and packs no machine uses
```

Images outside the catalog (`rc machine create --image ...`) use the same build, without warmup.

## Output

One line per language:

```
python <caminho do ext4> cached=false elapsed=<tempo> <métricas do build>
```

- `cached=true` when the key already existed in the cache and nothing was rebuilt.
- The metrics come from the build VM: extraction, warmup and `mkfs` times, plus the used size and the disk size.

## What the build does

1. Pulls the OCI image with go-containerregistry (no Docker) and flattens the layers into a tar.
2. Boots an Alpine 3.22.6 build VM (the official Alpine base tarball, with its sha256 checked; e2fsprogs and GNU tar via `apk`).
3. Inside it, as root: extracts the tar into a tmpfs, injects the guest and `start.sh` into `/opt/runner/`, runs the warmup in a chroot and generates the ext4 with `mkfs.ext4 -d`.
4. Writes `<lang>-<chave>.ext4` to the cache and points the `<lang>.ext4` symlink at it.

The reason for each step, the cache key and each language's warmup are in [Images](/concepts/images).

## When to rebuild

The key includes the image digest, the warmup, `build.sh`, `start.sh` and the guest. After changing any of them — including recompiling the guest with different code — run the build again: the key changes on its own and a new image is generated.

## Timing

On the measured machine (macOS 27.0.1, Apple Silicon), the Python image took ~8–10 s, 2.7 s of which were pull and flattening. The Go one
took 35 s, 28 of them in `mkfs.ext4`. The build needs network (pull and warmup); the job VM has none.
