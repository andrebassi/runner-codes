---
title: 'Testing Tasks'
description: 'Run unit tests and verify code execution across all supported languages'
---

## Overview

Testing tasks include unit tests for all packages and integration tests for code execution in Firecracker microVMs.

---

## Unit Tests

### test:unit

Run unit tests for all Go packages.

```bash title="Run all unit tests"
task test:unit
```

```yaml title="Task definition for test:unit"
test:unit:
  desc: Run unit tests
  cmds:
    - go test -v ./...
```

```text title="Example unit test output"
=== RUN   TestExecutor_Execute_Python
--- PASS: TestExecutor_Execute_Python (0.05s)
=== RUN   TestExecutor_Execute_Bash
--- PASS: TestExecutor_Execute_Bash (0.01s)
=== RUN   TestServer_HandleRequest
--- PASS: TestServer_HandleRequest (0.00s)
PASS
ok      infra-operator/pkg/guest    0.150s

=== RUN   TestRunner_NewRunner
--- PASS: TestRunner_NewRunner (0.00s)
=== RUN   TestFirecracker_BuildConfig
--- PASS: TestFirecracker_BuildConfig (0.00s)
=== RUN   TestVsock_Protocol
--- PASS: TestVsock_Protocol (0.02s)
PASS
ok      infra-operator/pkg/host     0.080s
```

### test:coverage

Run tests with coverage report.

```bash title="Run tests with coverage report"
task test:coverage
```

```yaml title="Task definition for test:coverage"
test:coverage:
  desc: Run tests with coverage
  cmds:
    - go test -coverprofile=coverage.out ./...
    - go tool cover -func=coverage.out
```

```text title="Example coverage report output"
infra-operator/pkg/guest/executor.go:24:    Execute         75.0%
infra-operator/pkg/guest/executor.go:89:    getLanguageConfig 100.0%
infra-operator/pkg/guest/server.go:35:      Serve           42.9%
infra-operator/pkg/guest/types.go:18:       Validate        100.0%
infra-operator/pkg/host/runner.go:45:       Run             25.0%
infra-operator/pkg/host/firecracker.go:78:  BuildConfig     100.0%
infra-operator/pkg/host/vsock.go:92:        Connect         33.3%
total:                                      (statements)    31.2%
```

### test:coverage-html

Generate HTML coverage report.

```bash title="Generate and open HTML coverage report"
task test:coverage-html
```

Opens an interactive coverage report in your browser.

### Test Coverage Summary

| Package | Coverage | Tests |
|---------|----------|-------|
| `pkg/guest` | 39.4% | 35+ tests |
| `pkg/host` | 21.4% | 45+ tests |
| **Total** | 19.8% | 80+ tests |

---

## Integration Tests

### aws:test

Run a basic Python test to verify the setup on EC2.

```bash title="Run basic Python test on EC2"
task aws:test 2>&1 | tee /tmp/log.txt
```

```yaml title="Task definition for aws:test"
aws:test:
  desc: Run Firecracker test on EC2 instance
  cmds:
    - |
      ssh ubuntu@$PUBLIC_IP 'bash -s' << 'ENDSSH'
      cd /opt/llm-infra-operator

      echo "=== Checking prerequisites ==="
      echo "KVM: $(ls -la /dev/kvm)"
      echo "Vsock: $(ls -la /dev/vhost-vsock)"
      echo "Firecracker: $(firecracker --version)"

      echo "=== Running infra.operator test ==="
      sudo infra.operator host \
        --lang python \
        --code "print('Hello from Firecracker microVM!')" \
        --timeout 30
      ENDSSH
```

```text title="Example output from Firecracker test"
Running Firecracker test on 54.159.149.186...
=== Checking prerequisites ===
KVM: crw-rw-rw- 1 root kvm 10, 232 Nov 29 04:25 /dev/kvm
Vsock: crw-rw-rw- 1 root kvm 10, 241 Nov 29 04:25 /dev/vhost-vsock
Firecracker: Firecracker v1.7.0

=== Running infra.operator test ===
2025/11/29 04:30:00 Starting Firecracker microVM...
2025/11/29 04:30:00 Language: python
2025/11/29 04:30:03 VM booted in 2.8s
2025/11/29 04:30:03 Connecting to guest via vsock...
2025/11/29 04:30:03 Connected to guest
2025/11/29 04:30:03 Sending job request...

Execution Result:
{
  "trace_id": "tr-1764388648079963068",
  "stdout": "Hello from Firecracker microVM!\n",
  "stderr": "",
  "exit_code": 0
}

2025/11/29 04:30:03 VM shutdown complete.
```

---

## aws:test-languages

Quick test to verify language versions are installed.

```bash title="Test multiple language installations"
task aws:test-languages 2>&1 | tee /tmp/log.txt
```

```yaml title="Task definition for aws:test-languages"
aws:test-languages:
  desc: Test all supported languages on EC2
  cmds:
    - |
      ssh ubuntu@$PUBLIC_IP 'bash -s' << 'ENDSSH'
      cd /opt/llm-infra-operator

      echo "=== Testing Python ==="
      sudo infra.operator host --lang python \
        --code "import sys; print(f'Python {sys.version}')" --timeout 30

      echo "=== Testing Bash ==="
      sudo infra.operator host --lang bash \
        --code "echo 'Bash version:' && bash --version | head -1" --timeout 30

      echo "=== Testing Node.js ==="
      sudo infra.operator host --lang node \
        --code "console.log('Node.js ' + process.version)" --timeout 30
      ENDSSH
```

```text title="Example output from language tests"
Testing all languages on 54.159.149.186...
=== Testing Python ===
{
  "stdout": "Python 3.10.12 (main, Nov 20 2024, 15:14:05) [GCC 11.4.0]\n",
  "exit_code": 0
}

=== Testing Bash ===
{
  "stdout": "Bash version:\nGNU bash, version 5.1.16(1)-release (x86_64-pc-linux-gnu)\n",
  "exit_code": 0
}

=== Testing Node.js ===
{
  "stdout": "Node.js v20.19.6\n",
  "exit_code": 0
}
```

---

## aws:test-all-languages

Comprehensive test of all supported languages with version output.

```bash title="Test all supported languages"
task aws:test-all-languages 2>&1 | tee /tmp/log.txt
```

```yaml title="Task definition for aws:test-all-languages"
aws:test-all-languages:
  desc: Test all languages execute correctly in VM
  cmds:
    - |
      ssh ubuntu@$PUBLIC_IP 'bash -s' << 'ENDSSH'
      cd /opt/llm-infra-operator

      echo "============================================="
      echo "  Testing All Languages in Firecracker VM"
      echo "============================================="

      echo "=== 1. Python 3 ==="
      sudo infra.operator host --lang python \
        --code "import sys; print(f'Python {sys.version_info.major}.{sys.version_info.minor}.{sys.version_info.micro}')" \
        --timeout 30

      echo "=== 2. Node.js ==="
      sudo infra.operator host --lang node \
        --code "console.log('Node.js ' + process.version)" \
        --timeout 30

      echo "=== 3. Go ==="
      sudo infra.operator host --lang go \
        --code 'package main; import ("fmt"; "runtime"); func main() { fmt.Println("Go", runtime.Version()) }' \
        --timeout 30

      echo "=== 4. Rust ==="
      sudo infra.operator host --lang rust \
        --code 'fn main() { println!("Rust compiled successfully!"); }' \
        --timeout 60

      echo "=== 5. Bash ==="
      sudo infra.operator host --lang bash \
        --code "echo 'Bash '\$BASH_VERSION" \
        --timeout 30

      echo "============================================="
      echo "  All language tests completed!"
      echo "============================================="
      ENDSSH
```

```text title="Example output from comprehensive language tests"
Testing all languages...
=============================================
  Testing All Languages in Firecracker VM
=============================================

=== 1. Python 3 ===
Execution Result:
{
  "trace_id": "tr-1764388648079963068",
  "stdout": "Python 3.10.12\n",
  "stderr": "",
  "exit_code": 0
}

=== 2. Node.js ===
Execution Result:
{
  "trace_id": "tr-1764388664848286562",
  "stdout": "Node.js v20.19.6\n",
  "stderr": "",
  "exit_code": 0
}

=== 3. Go ===
Execution Result:
{
  "trace_id": "tr-1764388681617609956",
  "stdout": "Go go1.22.3\n",
  "stderr": "",
  "exit_code": 0
}

=== 4. Rust ===
Execution Result:
{
  "trace_id": "tr-1764388698386933350",
  "stdout": "Rust compiled successfully!\n",
  "stderr": "",
  "exit_code": 0
}

=== 5. Bash ===
Execution Result:
{
  "trace_id": "tr-1764388715156256744",
  "stdout": "Bash 5.1.16(1)-release\n",
  "stderr": "",
  "exit_code": 0
}

=============================================
  All language tests completed!
=============================================
```

---

---

## Test Examples by Language

### Python: Fibonacci

```bash title="Execute Python Fibonacci calculation"
ssh -i aws/keys/infra-operator-key.pem ubuntu@$(cat aws/.public-ip) 'cd /opt/llm-infra-operator && sudo infra.operator host \
  --lang python \
  --code "def fib(n):
    if n <= 1: return n
    return fib(n-1) + fib(n-2)
print(f\"Fibonacci(30) = {fib(30)}\")" \
  --timeout 30'
```

```json title="Python Fibonacci execution result"
{
  "trace_id": "tr-1764388732925580138",
  "stdout": "Fibonacci(30) = 832040\n",
  "stderr": "",
  "exit_code": 0
}
```

### Node.js: Async/Await

```bash title="Execute Node.js async/await code"
ssh -i aws/keys/infra-operator-key.pem ubuntu@$(cat aws/.public-ip) 'cd /opt/llm-infra-operator && sudo infra.operator host \
  --lang node \
  --code "async function main() {
  const result = await Promise.resolve(42);
  console.log(\`The answer is: \${result}\`);
}
main();" \
  --timeout 30'
```

```json title="Node.js async execution result"
{
  "trace_id": "tr-1764388749694903532",
  "stdout": "The answer is: 42\n",
  "stderr": "",
  "exit_code": 0
}
```

### Go: Goroutines

```bash title="Execute Go goroutines example"
ssh -i aws/keys/infra-operator-key.pem ubuntu@$(cat aws/.public-ip) 'cd /opt/llm-infra-operator && sudo infra.operator host \
  --lang go \
  --code "package main
import (\"fmt\"; \"sync\")
func main() {
    var wg sync.WaitGroup
    for i := 0; i < 3; i++ {
        wg.Add(1)
        go func(n int) {
            defer wg.Done()
            fmt.Printf(\"Goroutine %d\n\", n)
        }(i)
    }
    wg.Wait()
    fmt.Println(\"All goroutines completed\")
}" \
  --timeout 30'
```

```json title="Go goroutines execution result"
{
  "trace_id": "tr-1764388766464226926",
  "stdout": "Goroutine 2\nGoroutine 0\nGoroutine 1\nAll goroutines completed\n",
  "stderr": "",
  "exit_code": 0
}
```

### Rust: Pattern Matching

```bash title="Execute Rust pattern matching code"
ssh -i aws/keys/infra-operator-key.pem ubuntu@$(cat aws/.public-ip) 'cd /opt/llm-infra-operator && sudo infra.operator host \
  --lang rust \
  --code "fn main() {
    let numbers = vec![1, 2, 3, 4, 5];
    let sum: i32 = numbers.iter().sum();
    println!(\"Sum: {}\", sum);
}" \
  --timeout 60'
```

```json title="Rust execution result"
{
  "trace_id": "tr-1764388783233550320",
  "stdout": "Sum: 15\n",
  "stderr": "",
  "exit_code": 0
}
```

### Bash: System Information

```bash title="Execute Bash system information script"
ssh -i aws/keys/infra-operator-key.pem ubuntu@$(cat aws/.public-ip) 'cd /opt/llm-infra-operator && sudo infra.operator host \
  --lang bash \
  --code "echo \"Hostname: \$(hostname)\"
echo \"Kernel: \$(uname -r)\"
echo \"Architecture: \$(uname -m)\"
echo \"Memory: \$(free -h | grep Mem | awk '\''{print \$2}'\'')\""
  --timeout 30'
```

```json title="Bash execution result"
{
  "trace_id": "tr-1764388800002873714",
  "stdout": "Hostname: ubuntu-fc-uvm\nKernel: 4.14.174\nArchitecture: x86_64\nMemory: 489M\n",
  "stderr": "",
  "exit_code": 0
}
```

---

## Error Handling Tests

### Timeout Test

```bash title="Test execution timeout handling"
# This should timeout after 5 seconds
sudo infra.operator host \
  --lang python \
  --code "import time; time.sleep(100)" \
  --timeout 5
```

```json title="Timeout error response"
{
  "trace_id": "tr-1764388816772197108",
  "stdout": "",
  "stderr": "\nExecution timed out",
  "exit_code": 124
}
```

### Syntax Error Test

```bash title="Test syntax error handling"
sudo infra.operator host \
  --lang python \
  --code "print('Hello" \
  --timeout 10
```

```json title="Syntax error response"
{
  "trace_id": "tr-1764388833541520502",
  "stdout": "",
  "stderr": "  File \"/tmp/job-xxx/script.py\", line 1\n    print('Hello\n               ^\nSyntaxError: EOL while scanning string literal\n",
  "exit_code": 1
}
```

### Unsupported Language Test

```bash title="Test unsupported language handling"
sudo infra.operator host \
  --lang unsupported_lang \
  --code "print('test')" \
  --timeout 10
```

```json title="Unsupported language error response"
{
  "trace_id": "tr-1764388850310843896",
  "stdout": "",
  "stderr": "unsupported language: unsupported_lang",
  "exit_code": 127,
  "error": "unsupported language: unsupported_lang"
}
```

---

## Performance Benchmarks

| Operation | Duration |
|-----------|----------|
| VM cold boot | ~3s |
| Python execution | ~50ms |
| Node.js execution | ~100ms |
| Go compile + run | ~500ms |
| Rust compile + run | ~2-5s |
| Bash execution | ~10ms |
| VM shutdown | ~100ms |

:::note

Rust compilation is significantly slower due to the compiler's complexity. Consider using snapshots for faster warm starts.

:::
