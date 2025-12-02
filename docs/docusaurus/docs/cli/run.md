---
title: 'Run Commands'
description: 'Execute code in isolated Firecracker microVMs'
---

## Overview

The `run` command executes code in an isolated Firecracker microVM. It automatically loads the appropriate snapshot (downloading from S3 if necessary) and sends the code to the guest mode (`infra.operator guest`) for execution.

## Commands

### run

Execute code in a microVM.

```bash title="Execute code in microVM"
infra.operator run --lang <language> [--code <code> | --file <path>] [flags]
```

**Flags:**

| Flag | Short | Description | Default |
|------|-------|-------------|---------|
| `--lang` | `-l` | Language runtime (required) | - |
| `--code` | `-c` | Inline code to execute | - |
| `--file` | `-f` | File to execute | - |
| `--timeout` | `-t` | Execution timeout in seconds | `30` |

:::note

  Either `--code` or `--file` must be provided, but not both.

:::

## Examples

### Inline Code Execution

```bash title="Execute Python code"
infra.operator run --lang python --code "print('Hello from Firecracker!')"
```

```bash title="Execute Node.js code"
infra.operator run --lang nodejs --code "console.log('Hello from Firecracker!')"
```

```bash title="Execute Go code"
infra.operator run --lang go --code 'package main; func main() { println("Hello from Firecracker!") }'
```

```bash title="Execute Rust code"
infra.operator run --lang rust --code 'fn main() { println!("Hello from Firecracker!"); }'
```

```bash title="Execute Ruby code"
infra.operator run --lang ruby --code "puts 'Hello from Firecracker!'"
```

### File Execution

```bash title="Execute Python file"
infra.operator run --lang python --file script.py
```

```bash title="Execute Node.js file"
infra.operator run --lang nodejs --file app.js
```

```bash title="Execute Go file"
infra.operator run --lang go --file main.go
```

### Custom Timeout

```bash title="Long-running computation (5 minutes)"
infra.operator run --lang python --file compute.py --timeout 300
```

```bash title="Quick script (10 seconds)"
infra.operator run --lang bash --code "echo 'fast'" --timeout 10
```

## Output Format

The run command outputs JSON with execution results:

```json title="Execution result"
{
  "stdout": "Hello from Firecracker!\n",
  "stderr": "",
  "exit_code": 0,
  "load_time": "209ms",
  "exec_time": "45ms"
}
```

**Fields:**

| Field | Description |
|-------|-------------|
| `stdout` | Standard output from the code |
| `stderr` | Standard error output |
| `exit_code` | Process exit code (0 = success) |
| `load_time` | Time to load the snapshot |
| `exec_time` | Time to execute the code |

## Execution Flow

![Code Execution Flow](/img/execution-flow.svg)

## Supported Languages

Execute code in any of the 45 supported languages:

**Popular Languages:**

```bash title="Execute Python code"
infra.operator run -l python -c "print('Hello')"
```

```bash title="Execute Node.js code"
infra.operator run -l nodejs -c "console.log('Hello')"
```

```bash title="Execute TypeScript code"
infra.operator run -l typescript -c "console.log('Hello' as string)"
```

```bash title="Execute Go code"
infra.operator run -l go -c 'package main; func main() { println("Hello") }'
```

```bash title="Execute Rust code"
infra.operator run -l rust -c 'fn main() { println!("Hello"); }'
```

**JVM Languages:**

```bash title="Execute Java code"
infra.operator run -l java -c 'public class Main { public static void main(String[] args) { System.out.println("Hello"); } }'
```

```bash title="Execute Kotlin code"
infra.operator run -l kotlin -c 'fun main() { println("Hello") }'
```

```bash title="Execute Scala code"
infra.operator run -l scala -c 'object Main extends App { println("Hello") }'
```

```bash title="Execute Groovy code"
infra.operator run -l groovy -c 'println "Hello"'
```

**Scripting Languages:**

```bash title="Execute Ruby code"
infra.operator run -l ruby -c "puts 'Hello'"
```

```bash title="Execute PHP code"
infra.operator run -l php -c "<?php echo 'Hello';"
```

```bash title="Execute Perl code"
infra.operator run -l perl -c 'print "Hello\n";'
```

```bash title="Execute Lua code"
infra.operator run -l lua -c "print('Hello')"
```

```bash title="Execute Bash code"
infra.operator run -l bash -c "echo 'Hello'"
```

**Functional Languages:**

```bash title="Execute Haskell code"
infra.operator run -l haskell -c 'main = putStrLn "Hello"'
```

```bash title="Execute Elixir code"
infra.operator run -l elixir -c 'IO.puts "Hello"'
```

```bash title="Execute Erlang code"
infra.operator run -l erlang -c 'main() -> io:format("Hello~n").'
```

```bash title="Execute Clojure code"
infra.operator run -l clojure -c '(println "Hello")'
```

```bash title="Execute Common Lisp code"
infra.operator run -l lisp -c '(format t "Hello~%")'
```

## Error Handling

### Exit Codes

| Exit Code | Meaning |
|-----------|---------|
| 0 | Success |
| 1 | General error |
| 124 | Timeout exceeded |
| 127 | Language not supported |

### Error Examples

```bash title="Syntax error example"
infra.operator run -l python -c "print('Hello"
```

```json title="Syntax error output"
{
  "stdout": "",
  "stderr": "SyntaxError: EOL while scanning string literal",
  "exit_code": 1
}
```

```bash title="Timeout example"
infra.operator run -l python -c "import time; time.sleep(60)" --timeout 5
```

```json title="Timeout output"
{
  "stdout": "",
  "stderr": "Execution timeout exceeded (5s)",
  "exit_code": 124
}
```

```bash title="Unknown language example"
infra.operator run -l unknown -c "print('Hello')"
```

```text title="Unknown language error"
Error: unknown language: unknown
Available: python, nodejs, go, rust, ...
```

## Performance

### Typical Execution Times

| Phase | Time |
|-------|------|
| Snapshot load (cached) | ~200ms |
| Snapshot download (S3) | ~2.4s |
| Code execution | ~50ms-5s |
| Total (cached) | ~250ms+ |
| Total (first run) | ~2.7s+ |

### Optimization Tips

- Pre-download snapshots to `/dev/shm/snapshots/` for fastest execution
- Use smaller memory allocations when possible (512MB default)
- Keep code execution simple and focused


## Taskfile Integration

```bash title="Execute inline code"
task -t Taskfile.cli.yaml run LANG=python CODE='print(1)'
```

```bash title="Execute file"
task -t Taskfile.cli.yaml run:file LANG=python FILE=script.py
```

```bash title="Remote execution on EC2"
task -t Taskfile.cli.yaml remote:run LANG=python CODE='print(1)'
```

## Benchmark Command

Test execution performance across all languages:

```bash title="Benchmark all 45 languages"
infra.operator benchmark --all
```

```text title="Benchmark output"
==============================================
  BENCHMARK: Snapshot Load Time
==============================================
Testing 45 languages...

[1/45] python: LOAD=209ms [OK]
[2/45] nodejs: LOAD=215ms [OK]
[3/45] go: LOAD=198ms [OK]
...
[45/45] cobol: LOAD=195ms [OK]

==============================================
  BENCHMARK REPORT
==============================================

Results: 45/45 PASSED, 0 FAILED

Average Load Time: 220ms
Total Load Time: 9.9s
```

```bash title="Benchmark specific languages"
infra.operator benchmark --langs python,nodejs,go,rust
```

## Security

Each execution is fully isolated:

- **Process isolation**: Runs in separate microVM
- **Memory isolation**: Dedicated memory space
- **No network**: No network access by default
- **Timeout enforcement**: Hard timeout via Firecracker API
- **Read-write rootfs**: Changes are ephemeral

:::warning

  Code is executed with full guest privileges. Do not execute untrusted code without proper sandboxing.

:::

## Troubleshooting

**Snapshot not found:**

```bash title="Download snapshot"
infra.operator snapshot download --lang python
```

**Execution timeout:**

```bash title="Execute with extended timeout"
infra.operator run --lang python --file long_script.py --timeout 300
```

**Out of memory:**

```bash title="Create snapshot with more memory"
infra.operator snapshot create --lang java --mem 1024
```

**Permission denied on /dev/kvm:**

```bash title="Add user to kvm group"
sudo usermod -aG kvm $USER
```

Then logout and login again.

## Next Steps



