---
title: 'Images'
description: 'How the official OCI image becomes a read-only ext4: build, cache and warmup'
---

## What an image is here

Each language in the catalog runs on top of a read-only ext4 file, which becomes the VM's root. It is generated from the language's **official** OCI image (for example `python:3.12-alpine`), with three things added:

- the static guest at `/opt/runner/guest`;
- `/opt/runner/start.sh`, the VM's first process;
- the result of the language warmup, when there is one.

## How the build works

```bash
rc image build --stack python
rc image build --all
```

1. **Pull without Docker.** The host downloads the image with go-containerregistry, for the host's architecture.
2. **Flattening on the host.** The layers become a single tar.
3. **Build VM.** An Alpine 3.22.6 VM (the official Alpine base tarball, with a verified sha256) receives the tar, with e2fsprogs and GNU tar installed via `apk`. Inside it, as root, it:
   - extracts the tar into a tmpfs, preserving owner and permissions (`--numeric-owner`);
   - copies the guest and `start.sh` to `/opt/runner/`;
   - runs the warmup in a chroot, with network access, only at this stage;
   - generates the ext4 with `mkfs.ext4 -d`.
4. The ext4 goes into the cache and a `<lang>.ext4` symlink now points to it.

### Why a build VM

- macOS APFS is case-insensitive: `Case` and `case` become the same file. Inside the VM the filesystem is Linux, which is case-sensitive.
- Homebrew's e2fsprogs cannot read tar, so the ext4 cannot be generated straight from the tar on the Mac.
- Extracting as root preserves the image's uid, gid and permissions.

## Cache

The image key is:

```
sha256(digest da imagem + warmup + build.sh + start.sh + guest)[:12]
```

| System | Path |
|--------|------|
| macOS | `~/Library/Caches/runner-codes/images/<lang>-<chave>.ext4` |
| Linux | `~/.cache/runner-codes/images/<lang>-<chave>.ext4` |

with the `<lang>.ext4` symlink in the same directory. The cache root can be changed with `RUNNER_CACHE_DIR`.

Consequence: changing the guest, `start.sh`, the build script, the warmup or the source image produces a new key and forces a rebuild on the next `image build`. Nothing needs to be invalidated by hand. If the existing image was built with an old guest, just run the build again.

## Warmup

The warmup is a line of `sh` declared in `languages.yaml`. It runs **in the build VM**, with network access, and its result is baked into the image. None of the warmup runs in the job VM.

| Language | Warmup | Why |
|----------|--------|-----|
| python | `python3 -m compileall` on the stdlib | the official image ships no `.pyc`; compiling removes that cost from the first import. The image went from 66 to 85 MiB and the warmup took ~1 s |
| go | compiles a program with `GOCACHE=/opt/runner/gocache` | populates the build cache; since the root is read-only, `start.sh` copies the cache to `/tmp/go-cache` at boot (the path the executor enforces), before the VM enters the pool |
| c, cpp | `apk add build-base` | the base image is `alpine:3.22`, with no compiler |
| perl | `apk add perl` | same |
| jq | `apk add jq` | same |
| sqlite | `apk add sqlite` | same |

The other languages have no warmup.

## Images outside the catalog

Machines accept any OCI image (`rc machine create --image alpine:3.20`, or `image = "..."` in
[runner.code](/cli/runner-code)). The build is the same, with an empty warmup, and the image goes into the cache like
the others. An extracted [pack](/cli/pack) becomes the image `pack:<chave>`.

## Cleaning the cache

```bash
rc image prune          # old builds that no name points to anymore
rc image prune --all    # also the images and packs no machine uses
```

## Measured times

On the measured machine (macOS 27.0.1, Apple Silicon), the Python image took ~8–10 s to build, of which 2.7 s was pull
and flattening. For large images `mkfs.ext4` dominates: 28 s of Go's 35 s, because the ext4 is written on the host via
virtiofs. The table for each stack is in the [Test report](/advanced/test-report).
