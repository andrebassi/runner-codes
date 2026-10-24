---
title: 'Examples and guides'
description: 'SDK examples and guides that prove each rc flow in a real VM'
---

## Examples

[`examples/`](https://github.com/andrebassi/runner-codes/tree/main/examples) holds the SDK examples, each tested
against a local `rc serve` on macOS 27 (Apple M1 Pro). To declare a machine in a versioned file, see the
[`runner.code`](/cli/runner-code) format.

| Example | What it shows | Status |
|---------|---------------|--------|
| [`sdk-go`](https://github.com/andrebassi/runner-codes/tree/main/examples/sdk-go) | the [Go SDK](/api-reference/sdks): job and machine lifecycle against an `rc serve` | works |
| [`sdk-python`](https://github.com/andrebassi/runner-codes/tree/main/examples/sdk-python) | the same with the [Python SDK](/api-reference/sdks) | works |
| [`desktop-claude`](https://github.com/andrebassi/runner-codes/tree/main/examples/desktop-claude) | XFCE desktop with Claude Code and Firefox in a `runner.code`, created and checked by the Go SDK | works |
| [`desktop-claude-agent`](https://github.com/andrebassi/runner-codes/tree/main/examples/desktop-claude-agent) | tasks for that VM's Claude in three ways: `claude -p`, typed into the window, through the [screen API](/cli/screen) | works |

## Guides

Each folder in [`examples/guides/`](https://github.com/andrebassi/runner-codes/tree/main/examples/guides) is an `rc`
usage guide, with a `README.md` that explains the flow and a `run.sh` that proves what the guide says in a real VM and
exits with the number of failures.

```bash
task build
task guides                        # all folders
task guides DIRS="pack teardown"   # some of them
```

In the 10/21/2026 run (macOS 27, Apple M1 Pro): 9 folders, 175 checks, 0 failures.

| Guide | What it proves |
|-------|----------------|
| [install](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/install) | a real boot: guest marker and a kernel different from the host's |
| [throwaway-machine](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/throwaway-machine) | `:ro` repo, writable output, no network; network one host at a time; `--block-private`, host loopback |
| [pack](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/pack) | image and machine in one artifact; the marker comes along with the origin deleted; cache reused |
| [teardown](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/teardown) | stop, delete, prove it is clean; Ctrl-C and `kill -9` on the CLI leave no VM alive |
| [credentials](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/credentials) | the value never enters the VM (search with counter-proof); interception only on the binding's host; 403 in the URL |
| [branch-and-checkpoint](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/branch-and-checkpoint) | disk checkpoint with retention and lock, restore of any generation, pause/resume, children from a warm source, checkpoint exported as a pack |
| [gpu-cuda](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/gpu-cuda) | there is no CUDA: `doctor`, CLI, `runner.code` and API say no, with the reason, without creating anything |
| [local-api](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/local-api) | `rc serve` with lifecycle JSON bodies; `exitCode` in the body; SSE one line per event; unknown field rejected |
| [reference](https://github.com/andrebassi/runner-codes/tree/main/examples/guides/reference) | one step per topic with proof (`runner.code`, named networks, nested, limitations, sockets, display, file ownership, S3) and the out-of-scope topics, with the reason |

The folder's `lib.sh` gathers what every `run.sh` uses: the `rc machine ls` format, a VM's process
(`rc vmm --spec <dir>/spec.json`), the machine directory (`rc machine data-dir`) and the empty list (`No machines found`).

## Runbook

Besides the guides, [`docs/runbook.md`](https://github.com/andrebassi/runner-codes/blob/main/docs/runbook.md) is the
manual test sequence, step by step, with the expected result of each step; `task runbook` runs the same steps and
fails if any of them comes out different (146 checks, 0 failures on 10/24/2026).
