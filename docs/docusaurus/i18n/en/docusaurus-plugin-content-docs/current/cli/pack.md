---
sidebar_position: 8
title: Pack (portable artifact)
---

# Pack: a machine in a file

`rc pack create` turns an image, or a machine together with what was installed in it, into an artifact you copy to
another host and run with no pull, no build and no download.

```bash
rc pack create --stack python --output ./py312         # stub ./py312 + payload ./py312.rcmachine
./py312 run -- python3 --version

rc pack create --from-vm dev --output ./dev --single-file   # a single executable
./dev run -- python3 -c 'import meu_pacote'
```

| Output | What it is |
|-------|---------|
| `py312` | the stub: this `rc`, signed with the hypervisor entitlement on macOS |
| `py312.rcmachine` | the payload: the ext4 image (and, for a machine, its disk) and the manifest |
| `--single-file` | the payload appended to the stub, with a 16-byte trailer at the end |

The two files travel together. The stub on its own prints its usage and exits.

## Sources

| Flag | What goes into the payload |
|------|----------------------|
| `--stack python` | the stack image (built first if it does not exist yet) |
| `--image alpine:3.20` | the built OCI image |
| `--from-vm NOME` | the machine's image **and its disk**: whatever was installed and written travels along. The machine must be stopped (a running disk is not consistent) |

Machine flags (`--cpus`, `--mem`, `-e`, `--workdir`, `--net`, `--allow-host`, `-p`, `--` workload...) go into the
manifest and become the artifact's defaults. These do not travel:
- host directory volumes (the path belongs to another machine), which are rejected with the reason; `s3://` volumes do travel;
- credentials, because the CA and placeholders belong to the source machine;
- named networks.

## Artifact subcommands

```bash
./dev run [-- COMMAND...]    # ephemeral machine, deleted at the end (the exit code is returned)
./dev start                  # persistent machine named after the file (created the first time)
./dev exec -- pip install x  # on it
./dev shell | logs | status | stop
./dev inspect                # the manifest, without extracting
```

Also: `rc machine create --name x --from dev.rcmachine` (or `--from ./dev`), `rc machine run --from ...` and
`rc pack inspect ARQUIVO`.

## How it works inside

- **Its own sparse format.** A machine disk has 10 GiB logical and is almost all holes, and Go's `archive/tar` does not
  write sparse files. The payload stores only the extents with data (found via `SEEK_DATA`/`SEEK_HOLE`) in a gzip stream; the
  extraction recreates the holes. Measured: the python image (134 MiB) becomes 31 MiB; a python machine with a package installed,
  as a single file with the stub, comes to 46 MiB.
- **Extracted once** into `<cache>/packs/<chave>`, where the key is the manifest's fingerprint, and registered as the image
  `pack:<chave>`. A second `run` does not extract again.
- **The payload comes from outside**: file names with `/` or `..`, extents outside the file, truncated data, a manifest over
  1 MiB and a future format version are all rejected (tested).

## Proof: check a value, not a boot

A pack that lost its disk still boots and exits with 0. The e2e test (`TestPack`) writes a marker and a Python module to a
machine, packs it, **deletes the source machine** and reads the marker through the artifact. The planted defect (a pack without the
disk) is caught.

## What the artifact does not carry

The stub does not embed the VM runtime: it uses the **destination host's libkrun** (`task krun:install`; `rc doctor`
checks it): the Nix libkrun on macOS links virglrenderer and MoltenVK by absolute `/nix/store` path, and
carrying that closure would require rewriting the library paths. What travels is the image and the machine state,
which is what cannot be reproduced on the destination. The stub is tied to the OS and architecture it was created on
(`host_platform` in the manifest).
