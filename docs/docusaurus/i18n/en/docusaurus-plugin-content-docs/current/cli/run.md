---
title: 'run and shell'
description: 'Run code or open a shell in a fresh microVM from the command line'
---

## Usage

```bash
rc run --stack <linguagem> (--code '<código>' | --file <arquivo>) [--stdin '<texto>'] [--timeout 5s]
```

| Flag | Default | Description |
|------|--------|-----------|
| `--stack` | — | catalog language |
| `--code` | — | code of the main file |
| `--file` | — | reads the code from a local file (replaces `--code`) |
| `--stdin` | empty | the program's standard input |
| `--timeout` | `10s` | duration in Go format (`500ms`, `5s`, `2m`); maximum 300 s |
| `--gpu` | off | VM with the host GPU over Vulkan (virtio-gpu/Venus); only on a stack with `gpu` in the catalog, otherwise E007. See [GPU](/advanced/gpu) |

## What happens

`run` does not use the pool: it boots a fresh VM (cold boot), runs the job and destroys the VM. On the measured machine, a Python job took ~240 ms total, with 31–38 ms of execution.

Output is **streamed**: each chunk appears in the terminal while the program runs, not only at the end.

- The program's stdout goes to stdout.
- The program's stderr goes to stderr, followed by a summary line:

```
[status=success exit=0 exec=33ms total=241ms timeout=false]
```

(illustrative values)

### Language buffering

Streaming delivers what the program **writes** to the pipe. When the output is not a terminal, several languages
accumulate stdout in a buffer and only write it at the end (or block by block):

| Language | Streaming behavior |
|-----------|----------------------------|
| Python | line by line: the runner sets `PYTHONUNBUFFERED=1` in streaming mode |
| Node.js, Go, Bash, sh | line by line, no configuration |
| C, C++ (`printf`), Ruby, Perl | in blocks: call `fflush(stdout)`, `$stdout.sync = true` or `$\| = 1` in the program |
| stderr | unbuffered in all of them |

## Exit code

| Situation | rc |
|----------|----|
| program exited with 0 | 0 |
| program exited with a non-zero code, failed to compile or hit the timeout | 1 |
| job rejected (language, limits) or infrastructure failure | 1, with the error message |

## Examples

```bash
# Python with stdin
rc run --stack python --code 'print(input().upper())' --stdin 'olá'

# Local file
rc run --stack go --file ./main.go

# Short timeout
rc run --stack bash --code 'sleep 10' --timeout 2s
```

Through the Taskfile:

```bash
task run -- --stack python --code 'print(1)'
```

## shell

```bash
rc shell --stack python [--gpu]
task shell -- --stack python
```

Opens an interactive shell in a fresh VM of the stack, with the same image the jobs use: read-only root, no network and a writable `/tmp`.
The VM disappears on `exit`.

- **In a terminal**, the guest gets a real TTY (a virtio console in place of the implicit console) and the host
  terminal stays in raw mode during the session: Ctrl+C interrupts only the command in the guest, and the window size reaches the
  guest (`stty size`).
- **With stdin from a pipe**, the console is the regular one, without a TTY: `echo 'python3 -V' | rc shell --stack python` works.
- It is `bash` if the image has it; on Alpine images, `sh`.

For a shell that keeps what you install, use a machine: `rc machine run -it --stack python` (gone at the end) or
`rc machine shell` on a running machine. See [machine](/cli/machine).

## Prerequisite

The language image must be built (`rc image build --stack <l>`); without it, the job is rejected with E006.
