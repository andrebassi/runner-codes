---
title: 'Examples and guides'
description: 'Ready-to-run runner.code examples and guides that prove each rc flow in a real VM'
---

## Examples

One example per folder in [`examples/`](https://github.com/andrebassi/runner-codes/tree/main/examples), each tested in a
real VM on macOS 27 (Apple M1 Pro). The machine examples are a [`runner.code`](/cli/runner-code) file; `rc` accepts
`-s` as a synonym for `-f` to point to it.

| Example | What it shows | Status |
|---------|---------------|--------|
| [`python-app`](https://github.com/andrebassi/runner-codes/tree/main/examples/python-app) | public image, `init` with `pip` over the network | works |
| [`node-app`](https://github.com/andrebassi/runner-codes/tree/main/examples/node-app) | `init` with `npm -g`, app on a published port | works |
| [`openclaw-app`](https://github.com/andrebassi/runner-codes/tree/main/examples/openclaw-app) | LLM gateway as the workload, `/health` through the port | works |
| [`doom-web`](https://github.com/andrebassi/runner-codes/tree/main/examples/doom-web) | static server from a volume, game in the browser | works |
| [`docker-in-vm`](https://github.com/andrebassi/runner-codes/tree/main/examples/docker-in-vm) | full Docker inside the VM | works |
| [`headless-browser`](https://github.com/andrebassi/runner-codes/tree/main/examples/headless-browser) | headless Chromium and a screenshot | works, without GPU |
| [`gpu-chrome`](https://github.com/andrebassi/runner-codes/tree/main/examples/gpu-chrome) | Fedora with Vulkan/Venus and Chromium | Vulkan works; Chromium without GPU |
| [`local-llm`](https://github.com/andrebassi/runner-codes/tree/main/examples/local-llm) | llama.cpp with Vulkan on the host GPU | works: 56.7 tokens/s on the GPU versus 10.3 on the CPU (Qwen2.5 0.5B Q4_K_M) |
| [`desktop`](https://github.com/andrebassi/runner-codes/tree/main/examples/desktop) | Wayland desktop (Weston) on the [browser display](/cli/display), with keyboard and mouse | works |
| [`sdk-go`](https://github.com/andrebassi/runner-codes/tree/main/examples/sdk-go) | the [Go SDK](/api-reference/sdks): job and machine lifecycle against an `rc serve` | works |
| [`sdk-python`](https://github.com/andrebassi/runner-codes/tree/main/examples/sdk-python) | the same with the [Python SDK](/api-reference/sdks) | works |

```bash
rc machine run -s examples/python-app/runner.code -- python3 --version
rc machine run -d --name doom -s examples/doom-web/runner.code && open http://localhost:8080
rc machine run -d --name openclaw -s examples/openclaw-app/runner.code && curl localhost:18789/health
```

### First start time (measured)

The first start runs `init` (package installation); the following ones reuse the machine disk.

| Example | 1st start | Subsequent |
|---------|-----------|------------|
| python-app | 6 s | < 1 s |
| node-app | 2 s | < 1 s |
| openclaw-app | ~73 s (includes converting the image) | < 1 s |
| docker-in-vm | 4 s | 4 s (dockerd starts on every start) |
| headless-browser | 15 s | < 1 s |
| gpu-chrome | 581 s (Fedora metadata and packages) | < 1 s |
| local-llm | 488 s (Fedora + llama.cpp build) | < 1 s |
| desktop | 12 s | 5 s (udev, seatd and Weston start on every start) |

The notes for each example (why Chromium renders without GPU, the pitfalls of Docker inside the VM, the desktop's 2D
display) are in [`examples/README.md`](https://github.com/andrebassi/runner-codes/tree/main/examples).

## Guides

Each folder in [`examples/guides/`](https://github.com/andrebassi/runner-codes/tree/main/examples/guides) is an `rc`
usage guide, with a `README.md` that explains the flow and a `run.sh` that proves what the guide says in a real VM and
exits with the number of failures.

```bash
task build
task guides                        # all folders
task guides DIRS="pack teardown"   # some of them
```

In the 10/20/2026 run (macOS 27, Apple M1 Pro): 12 folders, 215 checks, 0 failures.

| Guide | What it proves |
|-------|----------------|
| [install](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/install) | a real boot: guest marker and a kernel different from the host's |
| [throwaway-machine](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/throwaway-machine) | `:ro` repo, writable output, no network; network one host at a time; `--block-private`, host loopback |
| [pack](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/pack) | image and machine in one artifact; the marker comes along with the origin deleted; cache reused |
| [teardown](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/teardown) | stop, delete, prove it is clean; Ctrl-C and `kill -9` on the CLI leave no VM alive |
| [credentials](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/credentials) | the value never enters the VM (search with counter-proof); interception only on the binding's host; 403 in the URL |
| [dev-env](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/dev-env) | persistence of packages, `$HOME` and disk; `/tmp` resets; init once, as root; `user` and `--user` |
| [docker-in-machine](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/docker-in-machine) | Docker on a real ext4, nested container, `docker.sock` on the host, data that survives |
| [coding-agent](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/coding-agent) | coding agents installed by init; git in the mounted repo; host SSH agent without the key in the VM; API key as a secret |
| [branch-and-checkpoint](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/branch-and-checkpoint) | disk checkpoint with retention and lock, restore of any generation, pause/resume, children from a warm source, checkpoint exported as a pack |
| [gpu-cuda](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/gpu-cuda) | there is no CUDA: `doctor`, CLI, `runner.code` and API say no, with the reason, without creating anything |
| [local-api](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/local-api) | `rc serve` with lifecycle JSON bodies; `exitCode` in the body; SSE one line per event; unknown field rejected |
| [reference](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/reference) | one step per topic with proof (`runner.code`, named networks, nested, limitations, sockets, display, file ownership, S3) and the out-of-scope topics, with the reason |

The folder's `lib.sh` gathers what every `run.sh` uses: the `rc machine ls` format, a VM's process
(`rc vmm --spec <dir>/spec.json`), the machine directory (`rc machine data-dir`) and the empty list (`No machines found`).

## Runbook

Besides the guides, [`docs/runbook.md`](https://github.com/andrebassi/runner-codes/blob/main/docs/runbook.md) is the
manual test sequence, step by step, with the expected result of each step; `task runbook` runs the same steps and
fails if any of them comes out different (116 checks, 0 failures on 10/20/2026).
