---
title: 'Language Support Overview'
description: 'Supported programming languages and execution details'
---

## Supported Languages

Runner Codes supports **45 programming languages** across multiple categories, each with its own optimized rootfs image.

### Main Languages

| Language | Version | Rootfs | Boot Time |
|----------|---------|--------|-----------|
| [Python](/languages/python) | 3.10 | 600 MB | ~3.1s |
| [Node.js](/languages/nodejs) | 20 LTS | 700 MB | ~3.2s |
| [TypeScript](/languages/typescript) | 5.x | 800 MB | ~3.5s |
| [Go](/languages/go) | 1.22 | 700 MB | ~3.5s |
| [Rust](/languages/rust) | stable | 2 GB | ~5-8s |

### Systems Languages

| Language | Version | Rootfs | Boot Time |
|----------|---------|--------|-----------|
| C | GCC 11 | 500 MB | ~3s |
| C++ | G++ 11 | 600 MB | ~3.5s |
| Zig | 0.11 | 600 MB | ~4s |
| D | LDC 1.28 | 800 MB | ~4s |

### JVM Languages

| Language | Version | Rootfs | Boot Time |
|----------|---------|--------|-----------|
| Java | 17 | 900 MB | ~4s |
| Kotlin | 1.9 | 1 GB | ~4.5s |
| Scala | 3.x | 1.2 GB | ~5s |
| Groovy | 4.x | 1 GB | ~4.5s |
| Clojure | 1.11 | 1 GB | ~5s |

### Scripting Languages

| Language | Version | Rootfs | Boot Time |
|----------|---------|--------|-----------|
| Ruby | 3.0 | 600 MB | ~3s |
| PHP | 8.1 | 600 MB | ~3s |
| Perl | 5.34 | 500 MB | ~3s |
| Lua | 5.4 | 400 MB | ~2.8s |
| Tcl | 8.6 | 400 MB | ~2.8s |
| Bash | 5.1 | 512 MB | ~3s |
| AWK | GNU | 400 MB | ~2.5s |

### Functional Languages

| Language | Version | Rootfs | Boot Time |
|----------|---------|--------|-----------|
| Haskell | GHC 9 | 1.5 GB | ~6s |
| OCaml | 4.13 | 700 MB | ~4s |
| F# | .NET 8 | 1.5 GB | ~5s |
| Elixir | 1.12 | 800 MB | ~4s |
| Erlang | OTP 24 | 700 MB | ~4s |
| Scheme | Guile | 500 MB | ~3s |
| Common Lisp | SBCL | 600 MB | ~3.5s |

### Modern Compiled Languages

| Language | Version | Rootfs | Boot Time |
|----------|---------|--------|-----------|
| Crystal | 1.x | 800 MB | ~4s |
| Nim | 2.0 | 600 MB | ~4s |
| Swift | 5.x | 1.5 GB | ~5s |
| V | 0.4 | 500 MB | ~3.5s |

### Scientific Computing

| Language | Version | Rootfs | Boot Time |
|----------|---------|--------|-----------|
| Julia | 1.10 | 1.5 GB | ~8s |
| R | 4.1 | 1 GB | ~5s |
| Octave | 6.1 | 1.2 GB | ~6s |
| Fortran | GFortran | 500 MB | ~3.5s |

### Database & Query Languages

| Language | Version | Rootfs | Boot Time |
|----------|---------|--------|-----------|
| SQLite | 3.x | 400 MB | ~2.5s |
| jq | 1.6 | 400 MB | ~2.5s |

### Legacy Languages

| Language | Version | Rootfs | Boot Time |
|----------|---------|--------|-----------|
| COBOL | GnuCOBOL | 600 MB | ~4s |
| Pascal | FreePascal | 500 MB | ~3.5s |
| Prolog | SWI-Prolog | 600 MB | ~3.5s |
| NASM | x86 Assembly | 400 MB | ~3s |

## Language Comparison

### Performance Tiers

| Tier | Languages | Boot + Execute | Use Cases |
|------|-----------|----------------|-----------|
| **Fast** (~2-3s) | Python, Node.js, Bash, Ruby, PHP, Perl, Lua, Tcl, AWK, SQLite, jq | ~2.5-3.2s | Scripting, rapid prototyping |
| **Medium** (~3-5s) | Go, Java, Kotlin, Scala, Groovy, Clojure, Elixir, Erlang | ~3.5-5s | Enterprise, concurrent apps |
| **Compiled** (~4-8s) | C, C++, Rust, D, Zig, Crystal, Nim, Haskell, OCaml, F#, Swift | ~4-8s | Performance-critical code |
| **Heavy** (~5-10s) | Julia, R, Octave, .NET (C#, F#), COBOL | ~5-10s | Scientific computing, legacy |

### Detailed Comparison

| Language | Type | Rootfs Size | Boot + Execute | Best For |
|----------|------|-------------|----------------|----------|
| Python | Interpreted | 600 MB | ~3.1s | Data science, scripting |
| Node.js | Interpreted | 700 MB | ~3.2s | Web, async operations |
| TypeScript | Transpiled | 800 MB | ~3.5s | Type-safe JavaScript |
| Go | Compiled | 700 MB | ~3.5s | Systems, concurrency |
| Rust | Compiled | 2 GB | ~5-8s | Performance, safety |
| Java | JVM | 900 MB | ~4s | Enterprise applications |
| Kotlin | JVM | 1 GB | ~4.5s | Modern JVM development |
| C | Compiled | 500 MB | ~3s | Systems programming |
| C++ | Compiled | 600 MB | ~3.5s | High-performance apps |
| Ruby | Interpreted | 600 MB | ~3s | Scripting, web |
| PHP | Interpreted | 600 MB | ~3s | Web scripting |
| Bash | Interpreted | 512 MB | ~3.0s | Shell scripting |
| Haskell | Compiled | 1.5 GB | ~6s | Functional programming |
| Elixir | BEAM | 800 MB | ~4s | Concurrent systems |
| Julia | JIT | 1.5 GB | ~8s | Scientific computing |
| SQLite | Query | 400 MB | ~2.5s | Database operations |
| COBOL | Compiled | 600 MB | ~4s | Business logic |

:::note

  Times include VM boot (~3s) + code execution. Actual code execution times vary based on complexity.

:::

## Execution Models

### Interpreted Languages

Python, Node.js, Ruby, PHP, Perl, Lua, Tcl, Bash, R are executed directly by their interpreters:

![Interpreted Languages Execution Flow](/img/execution-interpreted.svg)

### Compiled Languages

C, C++, Go, Rust, D, Zig, Haskell, Crystal, Nim, COBOL require compilation:

![Compiled Languages Execution Flow](/img/execution-compiled.svg)

### JVM Languages

Java, Kotlin, Scala, Groovy, Clojure use the JVM:

![JVM Languages Execution Flow](/img/execution-jvm.svg)

### BEAM Languages

Elixir and Erlang run on the BEAM virtual machine:

![BEAM Languages Execution Flow](/img/execution-beam.svg)

## Language Configuration

Each language is configured in `pkg/guest/executor.go`:

```go title="Type definition"
type LanguageConfig struct {
    Name       string   // Language identifier ("python", "node", etc.)
    Extension  string   // File extension (".py", ".js", etc.)
    Command    string   // Interpreter/compiler command
    Args       []string // Additional arguments
    NeedsBuild bool     // Whether compilation is required
    BuildCmd   string   // Compiler command (if compiled)
    BuildArgs  []string // Compiler arguments
}

// Example configurations
languages := map[string]LanguageConfig{
    "python": {
        Name:      "python",
        Extension: ".py",
        Command:   "python3",
    },
    "rust": {
        Name:       "rust",
        Extension:  ".rs",
        NeedsBuild: true,
        BuildCmd:   "rustc",
        BuildArgs:  []string{"-o", "runbin"},
    },
    "kotlin": {
        Name:       "kotlin",
        Extension:  ".kt",
        NeedsBuild: true,
        BuildCmd:   "kotlinc",
        BuildArgs:  []string{"-include-runtime", "-d", "Main.jar"},
        Command:    "java",
        Args:       []string{"-jar", "Main.jar"},
    },
    // ... 42+ more languages
}
```

## Environment Variables

The executor sets these environment variables for all languages:

```go title="Go code"
cmd.Env = append(os.Environ(),
    "HOME=/tmp",
    "TMPDIR=/tmp",
    // Go
    "GOCACHE=/tmp/go-cache",
    "GOPATH=/tmp/go",
    "GOROOT=/usr/local/go",
    // Rust
    "CARGO_HOME=/opt/cargo",
    "RUSTUP_HOME=/opt/rustup",
    // JVM
    "JAVA_HOME=/usr/lib/jvm/java-17-openjdk-amd64",
    // .NET
    "DOTNET_ROOT=/usr/share/dotnet",
    // Julia
    "JULIA_DEPOT_PATH=/tmp/julia",
    // PATH
    "PATH=/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:...",
)
```

## Choosing the Right Language


    **Python, Node.js, Ruby, PHP, Perl, Lua**

    - Fast startup times (~3s)
    - Rich standard libraries
    - Great for quick tasks
    - Easy to read and maintain



    **Rust, C, C++, Go, Zig**

    - Compiled to native code
    - Zero-cost abstractions
    - Memory safety (Rust, Go)
    - Best for compute-intensive tasks



    **Java, Kotlin, Scala, C#, F#**

    - Strong typing
    - Rich ecosystems
    - Good for complex business logic
    - Cross-platform support



    **Haskell, Elixir, Erlang, OCaml, Clojure, Scheme**

    - Immutable data structures
    - Pattern matching
    - Concurrent programming
    - Mathematical elegance



    **Julia, R, Octave, Python, Fortran**

    - Numerical computing
    - Statistical analysis
    - Linear algebra
    - Data visualization



    **SQLite, jq, AWK, Python**

    - Query languages
    - Text processing
    - JSON manipulation
    - Data transformation



    **COBOL, Fortran, Pascal**

    - Business applications
    - Scientific computing
    - Educational purposes
    - Migration projects


## Performance Benchmarks

### Fibonacci(30) Benchmark

| Language | Execution Time | Notes |
|----------|----------------|-------|
| C | ~0.003s | Direct compilation |
| Rust | ~0.005s | Zero-cost abstractions |
| Go | ~0.01s | Compiled, optimized |
| Java | ~0.02s | JIT optimization |
| Node.js | ~0.1s | V8 JIT optimization |
| Python | ~0.3s | Recursive implementation |
| Ruby | ~0.5s | Interpreted |
| Bash | ~8s | Recursive, very slow |

:::note

  Times exclude VM boot. Includes compilation time for compiled languages.

:::

### Hello World (Total Time Including Boot)

| Language | Total Time | Compile | Execute |
|----------|------------|---------|---------|
| Bash | 3010ms | - | 10ms |
| Python | 3050ms | - | 50ms |
| SQLite | 3020ms | - | 20ms |
| Node.js | 3100ms | - | 100ms |
| Go | 3500ms | 450ms | 50ms |
| Java | 4000ms | 800ms | 200ms |
| Rust | 5000ms | 1900ms | 100ms |
| Haskell | 6000ms | 2800ms | 200ms |

## Adding Custom Languages

To add support for a new language:

**1. Create rootfs:**

```bash title="Create new rootfs with language runtime"
task aws:create-rootfs-{lang}
```

**2. Add configuration:**

Add language configuration to `pkg/guest/executor.go`:

```go title="Language configuration"
"newlang": {
    Name:       "newlang",
    Extension:  ".nlg",
    Command:    "newlang",
    Args:       []string{"run"},
    NeedsBuild: false,
},
```

**3. Build and deploy:**

```bash title="Build infra.operator for Linux"
task build:infra-operator-linux
```

```bash title="Deploy to AWS"
task aws:deploy
```

**4. Test:**

```bash title="Test new language"
task aws:test LANG=newlang CODE="print('hello')"
```

## Limitations

### Common Limitations

- **No network access**: VMs don't have network connectivity
- **No persistent storage**: Files are lost after execution
- **Memory limit**: 512 MiB by default
- **Timeout**: Configurable, default 10 seconds

### Language-Specific Limitations

| Language | Limitations |
|----------|-------------|
| Python | No external packages (unless in rootfs) |
| Node.js | No npm install during execution |
| Go | go mod not supported inline |
| Rust | No cargo, only rustc |
| Java | No external JARs |
| Kotlin | No Gradle/Maven |
| Haskell | No cabal packages |
| Julia | No Pkg packages |
| R | No CRAN packages |
| .NET | No NuGet packages |
| Elixir | No Mix dependencies |

:::warning

  All languages run in isolated environments without network access. External dependencies must be pre-installed in the rootfs image.

:::
