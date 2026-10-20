---
title: 'Tasks overview'
description: 'The tasks in the Runner Codes Taskfile.yaml'
---

## List

The project uses [Task](https://taskfile.dev). `task` with no arguments lists everything.

| Task | What it does |
|------|--------------|
| `task krun:install` | installs libkrun + libkrunfw through Nix (`.#krun`) |
| `task build` | compiles the host (`bin/rc`) and the guest into `bin/` and signs the host on macOS |
| `task image:build -- --stack <l>` | generates the stack's ext4 (`-- --all` for all of them); compiles first |
| `task run -- <flags>` | runs code in a fresh VM |
| `task shell -- --stack <l>` | interactive shell in a fresh VM |
| `task serve -- <flags>` | starts the API on `127.0.0.1:8080` with a pool; compiles first |
| `task doctor` | checks whether the host can run microVMs |
| `task test` | unit tests with the race detector |
| `task test:cov` | coverage gate: fails below 90% total, 100% in the domain, or below the guest ratchet; includes the Python SDK |
| `task sdk:py:cov` | Python SDK only: line and branch, each ≥ 90% |
| `task test:targets` | multi-target gate: darwin/arm64, linux/amd64 and linux/arm64 compile; windows is rejected |
| `task test:e2e` | end to end against the real libkrun; needs the images built |
| `task runbook` | runs `docs/runbook.md` and checks every expected result |
| `task guides` | runs the guides in `examples/guides`, each in a real VM (`DIRS="pack teardown"` for some of them) |
| `task site:demos` | the real runs behind the site's demos (`SCENES="network pack"` for some of them) |
| `task gates` | `test:targets`, `test:cov`, `test:e2e` and `runbook`, in that order |
| `task ship -- "<título>" <arquivo>` | publishes the current branch, opens the PR and merges it |
| `task clean` | deletes `bin/`; the image cache stays where it is |

## Typical flow

```bash
task krun:install
task build
task doctor
task image:build -- --stack python
task run -- --stack python --code 'print(1)'
./bin/rc machine run --stack python -- python3 -V
task serve
task runbook
```

Details in [Build](/tasks/build) and [Testing](/tasks/testing).
