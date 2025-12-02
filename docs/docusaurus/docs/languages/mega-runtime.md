---
title: 'Mega Runtime (All-in-One)'
description: 'Single rootfs with 47 programming languages - Debian-based'
---

## Overview

The **Mega Runtime** is a consolidated Debian-based rootfs containing **47 programming languages** in a single image. Instead of managing 40+ separate rootfs images, you can use one unified runtime that supports all languages.

## Specifications

| Property | Value |
|----------|-------|
| Base Image | `debian:bookworm-slim` |
| Total Languages | 47 |
| Docker Image Size | ~9.5 GB |
| Rootfs Size | 10 GB |
| Snapshot Size | ~600 MB |
| Boot Time (snapshot) | ~100ms |

## Included Languages

### Scripting Languages
| Language | Binary | Version |
|----------|--------|---------|
| Python | `python3` | 3.11.x |
| Node.js | `node` | 18.x |
| Ruby | `ruby` | 3.1.x |
| PHP | `php` | 8.2.x |
| Perl | `perl` | 5.36.x |
| Lua | `lua5.4` | 5.4.x |
| Tcl | `tclsh` | 8.6.x |
| AWK | `gawk` | 5.x |
| jq | `jq` | 1.6 |
| Bash | `bash` | 5.2.x |

### Compiled Languages
| Language | Binary | Version |
|----------|--------|---------|
| Go | `go` | 1.19.x |
| Rust | `rustc` | 1.63.x |
| C | `gcc` | 12.x |
| C++ | `g++` | 12.x |
| Fortran | `gfortran` | 12.x |
| Pascal | `fpc` | 3.2.2 |
| COBOL | `cobc` | 3.1.2 |
| D | `ldc2` | 1.30.x |
| Nim | `nim` | 2.0.2 |
| Zig | `zig` | 0.11.0 |
| NASM | `nasm` | 2.16.x |
| Crystal | `crystal` | 1.10.1 |
| V | `v` | 0.4.12 |

### JVM Languages
| Language | Binary | Version |
|----------|--------|---------|
| Java | `java`/`javac` | 17 (OpenJDK) |
| Kotlin | `kotlin`/`kotlinc` | 1.9.22 |
| Scala | `scala`/`scalac` | 3.3.1 |
| Groovy | `groovy` | 4.0.15 |
| Clojure | `clojure` | 1.11.x |

### Functional Languages
| Language | Binary | Version |
|----------|--------|---------|
| Haskell | `ghc`/`runghc` | 9.0.x |
| Elixir | `elixir` | 1.14.x |
| Erlang | `erl` | OTP 25.x |
| OCaml | `ocaml` | 4.13.x |
| Scheme | `guile` | 3.0.x |
| Common Lisp | `sbcl` | 2.2.x |

### Scientific Computing
| Language | Binary | Version |
|----------|--------|---------|
| R | `Rscript` | 4.2.x |
| Julia | `julia` | 1.10.0 |
| Octave | `octave` | 7.x |

### JavaScript/TypeScript Runtimes
| Runtime | Binary | Version |
|---------|--------|---------|
| TypeScript | `npx tsx` | 5.x |
| Deno | `deno` | latest |
| Bun | `bun` | latest |

### .NET Languages
| Language | Binary | Version |
|----------|--------|---------|
| C# | `dotnet` | 8.0.x |

### Modern Languages
| Language | Binary | Version |
|----------|--------|---------|
| Swift | `swift` | 5.9.x |
| Dart | `dart` | 3.2.x |

### Logic & Other
| Language | Binary | Version |
|----------|--------|---------|
| Prolog | `swipl` | 9.0.4 |
| SQLite | `sqlite3` | 3.40.x |

### Database Parsers (node-sql-parser)
| Parser | Script |
|--------|--------|
| MySQL | `/usr/local/lib/mysql-run.js` |
| PostgreSQL | `/usr/local/lib/psql-run.js` |
| MariaDB | `/usr/local/lib/mariadb-run.js` |
| SQLite | `/usr/local/lib/sqlite-run.js` |
| MongoDB | `/usr/local/lib/mongodb-run.js` |
| Redis | `/usr/local/lib/redis-run.js` |

## Building the Docker Image

### 1. Create the Dockerfile

Create a directory and the Dockerfile:

```bash title="Create build directory"
mkdir -p /tmp/docker/mega-runtime-debian
cd /tmp/docker/mega-runtime-debian
```

Create the `Dockerfile`:

```dockerfile title="Dockerfile code"
FROM debian:bookworm-slim

ENV DEBIAN_FRONTEND=noninteractive

# ===== BASE SYSTEM =====
RUN apt-get update && apt-get install -y --no-install-recommends \
    bash coreutils curl wget git unzip ca-certificates xz-utils \
    build-essential gcc g++ make cmake \
    && rm -rf /var/lib/apt/lists/*

# ===== SCRIPTING LANGUAGES =====
RUN apt-get update && apt-get install -y --no-install-recommends \
    python3 python3-pip nodejs npm ruby php-cli perl lua5.4 tcl gawk jq \
    && rm -rf /var/lib/apt/lists/*

# ===== TypeScript & SQL Parser (global install) =====
RUN npm install -g typescript ts-node tsx node-sql-parser

# ===== GO & RUST =====
RUN apt-get update && apt-get install -y --no-install-recommends \
    golang rustc cargo \
    && rm -rf /var/lib/apt/lists/*

# ===== JVM LANGUAGES =====
RUN apt-get update && apt-get install -y --no-install-recommends \
    default-jdk clojure \
    && rm -rf /var/lib/apt/lists/*

# Kotlin
RUN curl -sLO https://github.com/JetBrains/kotlin/releases/download/v1.9.22/kotlin-compiler-1.9.22.zip && \
    unzip -q kotlin-compiler-1.9.22.zip -d /opt && rm kotlin-compiler-1.9.22.zip && \
    ln -s /opt/kotlinc/bin/kotlin /usr/local/bin/ && ln -s /opt/kotlinc/bin/kotlinc /usr/local/bin/

# Scala
RUN curl -sLO https://github.com/lampepfl/dotty/releases/download/3.3.1/scala3-3.3.1.tar.gz && \
    tar -xzf scala3-3.3.1.tar.gz -C /opt && rm scala3-3.3.1.tar.gz && \
    ln -s /opt/scala3-3.3.1/bin/scala /usr/local/bin/ && ln -s /opt/scala3-3.3.1/bin/scalac /usr/local/bin/

# Groovy
RUN curl -sLO https://groovy.jfrog.io/artifactory/dist-release-local/groovy-zips/apache-groovy-binary-4.0.15.zip && \
    unzip -q apache-groovy-binary-4.0.15.zip -d /opt && rm apache-groovy-binary-4.0.15.zip && \
    ln -s /opt/groovy-4.0.15/bin/groovy /usr/local/bin/

# ===== FUNCTIONAL LANGUAGES =====
RUN apt-get update && apt-get install -y --no-install-recommends \
    ghc erlang elixir ocaml guile-3.0 sbcl \
    && rm -rf /var/lib/apt/lists/*

# ===== SCIENTIFIC COMPUTING =====
RUN apt-get update && apt-get install -y --no-install-recommends \
    r-base octave \
    && rm -rf /var/lib/apt/lists/*

# Julia
RUN curl -sLO https://julialang-s3.julialang.org/bin/linux/x64/1.10/julia-1.10.0-linux-x86_64.tar.gz && \
    tar -xzf julia-1.10.0-linux-x86_64.tar.gz -C /opt && rm julia-1.10.0-linux-x86_64.tar.gz && \
    ln -s /opt/julia-1.10.0/bin/julia /usr/local/bin/

# ===== SYSTEMS LANGUAGES =====
RUN apt-get update && apt-get install -y --no-install-recommends nasm ldc && rm -rf /var/lib/apt/lists/*

# Nim
RUN curl -sLO https://nim-lang.org/download/nim-2.0.2-linux_x64.tar.xz && \
    tar -xJf nim-2.0.2-linux_x64.tar.xz -C /opt && rm nim-2.0.2-linux_x64.tar.xz && \
    ln -s /opt/nim-2.0.2/bin/nim /usr/local/bin/

# Zig
RUN curl -sLO https://ziglang.org/download/0.11.0/zig-linux-x86_64-0.11.0.tar.xz && \
    tar -xJf zig-linux-x86_64-0.11.0.tar.xz -C /opt && rm zig-linux-x86_64-0.11.0.tar.xz && \
    ln -s /opt/zig-linux-x86_64-0.11.0/zig /usr/local/bin/

# ===== DATABASE & OTHER =====
RUN apt-get update && apt-get install -y --no-install-recommends sqlite3 && rm -rf /var/lib/apt/lists/*

# Deno
RUN curl -fsSL https://deno.land/install.sh | sh && ln -s /root/.deno/bin/deno /usr/local/bin/ || true

# Bun
RUN curl -fsSL https://bun.sh/install | bash && ln -s /root/.bun/bin/bun /usr/local/bin/ || true

# ===== LANGUAGES MISSING FROM ALPINE (NOW AVAILABLE IN DEBIAN) =====
# Prolog (SWI-Prolog)
RUN apt-get update && apt-get install -y --no-install-recommends swi-prolog && rm -rf /var/lib/apt/lists/*

# Pascal (Free Pascal Compiler)
RUN apt-get update && apt-get install -y --no-install-recommends fp-compiler && rm -rf /var/lib/apt/lists/*

# COBOL (GnuCOBOL)
RUN apt-get update && apt-get install -y --no-install-recommends gnucobol && rm -rf /var/lib/apt/lists/*

# Fortran
RUN apt-get update && apt-get install -y --no-install-recommends gfortran && rm -rf /var/lib/apt/lists/*

# ===== DATABASE PARSER SCRIPTS =====
COPY mysql-run.js psql-run.js mariadb-run.js sqlite-run.js mongodb-run.js redis-run.js /usr/local/lib/

# ===== CLEANUP =====
RUN apt-get clean && rm -rf /var/lib/apt/lists/* /tmp/* /var/tmp/*

CMD ["/bin/bash"]
```

### 2. Create Database Parser Scripts

Create `mysql-run.js`:
```javascript title="MySQL parser wrapper script"
const { Parser } = require('node-sql-parser');
const parser = new Parser();
const sql = process.argv[2] || '';
try {
    const ast = parser.astify(sql, { database: 'MySQL' });
    console.log(JSON.stringify(ast, null, 2));
} catch (e) {
    console.error('Parse error:', e.message);
    process.exit(1);
}
```

Create `psql-run.js`:
```javascript title="PostgreSQL parser wrapper script"
const { Parser } = require('node-sql-parser');
const parser = new Parser();
const sql = process.argv[2] || '';
try {
    const ast = parser.astify(sql, { database: 'PostgreSQL' });
    console.log(JSON.stringify(ast, null, 2));
} catch (e) {
    console.error('Parse error:', e.message);
    process.exit(1);
}
```

Create similar files for `mariadb-run.js`, `sqlite-run.js`, `mongodb-run.js`, and `redis-run.js`.

### 3. Build the Docker Image

```bash title="Build mega runtime Docker image"
cd /tmp/docker/mega-runtime-debian
docker build -t ttl.sh/llm-fc-mega-runtime-debian:24h .
```

### 4. Push to Registry

```bash title="Push image to ttl.sh registry"
docker push ttl.sh/llm-fc-mega-runtime-debian:24h
```

## Creating the Rootfs

### 1. Create Rootfs from Docker Image

```bash title="Create 8GB rootfs from Docker image"
# On EC2 instance with infra.operator
sudo /usr/local/bin/infra.operator rootfs from-docker \
    --name mega-runtime \
    --image ttl.sh/llm-fc-mega-runtime-debian:24h \
    --size 8000
```

This creates `/srv/firecracker/images/rootfs-mega-runtime.ext4` (8GB).

### 2. Verify Rootfs

```bash title="Check file size"
ls -lh /srv/firecracker/images/rootfs-mega-runtime.ext4
```

```bash title="Mount and verify binaries"
sudo mkdir -p /mnt/mega
sudo mount -o loop /srv/firecracker/images/rootfs-mega-runtime.ext4 /mnt/mega
ls /mnt/mega/usr/bin/ | head -20
sudo umount /mnt/mega
```

## Creating the Snapshot

### 1. Create Snapshot

```bash title="Create VM snapshot with 512MB memory"
sudo /usr/local/bin/infra.operator snapshot create \
    --lang mega-runtime \
    --mem 512 \
    --vcpus 1
```

This creates:
- `/srv/firecracker/snapshots/mega-runtime/snapshot.mem`
- `/srv/firecracker/snapshots/mega-runtime/snapshot.vmstate`

### 2. Verify Snapshot

```bash title="List snapshot files"
ls -lh /srv/firecracker/snapshots/mega-runtime/
```

## Uploading to S3

### 1. Set AWS Credentials

```bash title="Configure AWS credentials"
export AWS_ACCESS_KEY_ID="your-key"
export AWS_SECRET_ACCESS_KEY="your-secret"
export AWS_DEFAULT_REGION="us-east-1"
```

### 2. Upload Rootfs (Large File - Multipart)

```bash title="Upload 8GB rootfs to S3 bucket"
aws s3 cp /srv/firecracker/images/rootfs-mega-runtime.ext4 \
    s3://runner-codes/rootfs-mega-runtime.ext4 \
    --no-progress
```

### 3. Upload Snapshot

```bash title="Upload snapshot files to S3"
aws s3 cp /srv/firecracker/snapshots/mega-runtime/ \
    s3://runner-codes/snapshots/mega-runtime/ \
    --recursive --no-progress
```

## Test Examples for Each Language

### Python
```bash title="Test Python execution"
sudo infra.operator host --lang mega-runtime \
    --code "print('Hello from Python!')" \
    --run-cmd "python3" \
    --mem 512 --vcpus 1 --snapshot
```

### Node.js
```bash title="Test Node.js execution"
sudo infra.operator host --lang mega-runtime \
    --code "console.log('Hello from Node.js!')" \
    --run-cmd "node" \
    --mem 512 --vcpus 1 --snapshot
```

### TypeScript (with tsx)
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code "const msg: string = 'Hello TypeScript!'; console.log(msg)" \
    --run-cmd "npx tsx" \
    --mem 512 --vcpus 1 --snapshot
```

### Go
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code 'package main; import "fmt"; func main() { fmt.Println("Hello Go!") }' \
    --run-cmd "go run" \
    --mem 512 --vcpus 1 --snapshot
```

### Rust
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code 'fn main() { println!("Hello Rust!"); }' \
    --run-cmd "rustc -o /tmp/out && /tmp/out" \
    --mem 512 --vcpus 1 --snapshot
```

### Java
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code 'public class Main { public static void main(String[] args) { System.out.println("Hello Java!"); } }' \
    --run-cmd "java" \
    --mem 512 --vcpus 1 --snapshot
```

### Kotlin
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code 'fun main() { println("Hello Kotlin!") }' \
    --run-cmd "kotlin" \
    --mem 512 --vcpus 1 --snapshot
```

### Scala
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code 'object Main extends App { println("Hello Scala!") }' \
    --run-cmd "scala" \
    --mem 512 --vcpus 1 --snapshot
```

### Ruby
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code "puts 'Hello Ruby!'" \
    --run-cmd "ruby" \
    --mem 512 --vcpus 1 --snapshot
```

### PHP
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code "<?php echo 'Hello PHP!' . PHP_EOL; ?>" \
    --run-cmd "php" \
    --mem 512 --vcpus 1 --snapshot
```

### Perl
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code "print 'Hello Perl!\n';" \
    --run-cmd "perl" \
    --mem 512 --vcpus 1 --snapshot
```

### Lua
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code "print('Hello Lua!')" \
    --run-cmd "lua5.4" \
    --mem 512 --vcpus 1 --snapshot
```

### Bash
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code "echo 'Hello Bash!'" \
    --run-cmd "bash" \
    --mem 512 --vcpus 1 --snapshot
```

### C
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code '#include <stdio.h>\nint main() { printf("Hello C!\\n"); return 0; }' \
    --run-cmd "gcc -o /tmp/out && /tmp/out" \
    --mem 512 --vcpus 1 --snapshot
```

### C++
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code '#include <iostream>\nint main() { std::cout << "Hello C++!" << std::endl; return 0; }' \
    --run-cmd "g++ -o /tmp/out && /tmp/out" \
    --mem 512 --vcpus 1 --snapshot
```

### Haskell
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code 'main = putStrLn "Hello Haskell!"' \
    --run-cmd "runghc" \
    --mem 512 --vcpus 1 --snapshot
```

### Elixir
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code 'IO.puts("Hello Elixir!")' \
    --run-cmd "elixir" \
    --mem 512 --vcpus 1 --snapshot
```

### Erlang
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code '-module(main). -export([start/0]). start() -> io:format("Hello Erlang!~n").' \
    --run-cmd "erlc && erl -noshell -s main start -s init stop" \
    --mem 512 --vcpus 1 --snapshot
```

### OCaml
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code 'print_endline "Hello OCaml!"' \
    --run-cmd "ocaml" \
    --mem 512 --vcpus 1 --snapshot
```

### Scheme (Guile)
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code '(display "Hello Scheme!") (newline)' \
    --run-cmd "guile" \
    --mem 512 --vcpus 1 --snapshot
```

### Common Lisp (SBCL)
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code '(format t "Hello Lisp!~%")' \
    --run-cmd "sbcl --script" \
    --mem 512 --vcpus 1 --snapshot
```

### Clojure
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code '(println "Hello Clojure!")' \
    --run-cmd "clojure -e" \
    --mem 512 --vcpus 1 --snapshot
```

### R
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code 'cat("Hello R!\n")' \
    --run-cmd "Rscript" \
    --mem 512 --vcpus 1 --snapshot
```

### Julia
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code 'println("Hello Julia!")' \
    --run-cmd "julia" \
    --mem 512 --vcpus 1 --snapshot
```

### Octave
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code "disp('Hello Octave!')" \
    --run-cmd "octave --no-gui --quiet" \
    --mem 512 --vcpus 1 --snapshot
```

### Fortran
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code "program hello\n  print *, 'Hello Fortran!'\nend program hello" \
    --run-cmd "gfortran -o /tmp/out && /tmp/out" \
    --mem 512 --vcpus 1 --snapshot
```

### Pascal
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code "program Hello; begin writeln('Hello Pascal!'); end." \
    --run-cmd "fpc -o/tmp/out && /tmp/out" \
    --mem 512 --vcpus 1 --snapshot
```

### COBOL
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code "       IDENTIFICATION DIVISION.\n       PROGRAM-ID. HELLO.\n       PROCEDURE DIVISION.\n           DISPLAY 'Hello COBOL!'.\n           STOP RUN." \
    --run-cmd "cobc -x -o /tmp/out && /tmp/out" \
    --mem 512 --vcpus 1 --snapshot
```

### Prolog (SWI-Prolog)
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code ":- initialization(main). main :- write('Hello Prolog!'), nl, halt." \
    --run-cmd "swipl" \
    --mem 512 --vcpus 1 --snapshot
```

### D
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code 'import std.stdio; void main() { writeln("Hello D!"); }' \
    --run-cmd "ldc2 -run" \
    --mem 512 --vcpus 1 --snapshot
```

### Nim
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code 'echo "Hello Nim!"' \
    --run-cmd "nim c -r" \
    --mem 512 --vcpus 1 --snapshot
```

### Zig
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code 'const std = @import("std"); pub fn main() void { std.debug.print("Hello Zig!\n", .{}); }' \
    --run-cmd "zig run" \
    --mem 512 --vcpus 1 --snapshot
```

### NASM (Assembly)
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code 'section .data\n    msg db "Hello NASM!",10\n    len equ $-msg\nsection .text\n    global _start\n_start:\n    mov rax, 1\n    mov rdi, 1\n    mov rsi, msg\n    mov rdx, len\n    syscall\n    mov rax, 60\n    xor rdi, rdi\n    syscall' \
    --run-cmd "nasm -f elf64 -o /tmp/out.o && ld -o /tmp/out /tmp/out.o && /tmp/out" \
    --mem 512 --vcpus 1 --snapshot
```

### SQLite
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code "SELECT 'Hello SQLite!' as message;" \
    --run-cmd "sqlite3 :memory:" \
    --mem 512 --vcpus 1 --snapshot
```

### Tcl
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code "puts {Hello Tcl!}" \
    --run-cmd "tclsh" \
    --mem 512 --vcpus 1 --snapshot
```

### AWK
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code 'BEGIN { print "Hello AWK!" }' \
    --run-cmd "gawk" \
    --mem 512 --vcpus 1 --snapshot
```

### jq
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code '.message' \
    --stdin '{"message": "Hello jq!"}' \
    --run-cmd "jq" \
    --mem 512 --vcpus 1 --snapshot
```

### Groovy
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code 'println "Hello Groovy!"' \
    --run-cmd "groovy" \
    --mem 512 --vcpus 1 --snapshot
```

### Crystal
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code 'puts "Hello Crystal!"' \
    --run-cmd "crystal run" \
    --mem 512 --vcpus 1 --snapshot
```

### V Language
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code 'fn main() { println("Hello V!") }' \
    --run-cmd "v run" \
    --mem 512 --vcpus 1 --snapshot
```

### C# (.NET)
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code 'Console.WriteLine("Hello C#!");' \
    --run-cmd "dotnet script" \
    --mem 512 --vcpus 1 --snapshot
```

### Swift
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code 'print("Hello Swift!")' \
    --run-cmd "swift" \
    --mem 512 --vcpus 1 --snapshot
```

### Dart
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code "void main() { print('Hello Dart!'); }" \
    --run-cmd "dart run" \
    --mem 512 --vcpus 1 --snapshot
```

### Deno
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code 'console.log("Hello Deno!")' \
    --run-cmd "deno run" \
    --mem 512 --vcpus 1 --snapshot
```

### Bun
```bash title="Run"
sudo infra.operator host --lang mega-runtime \
    --code 'console.log("Hello Bun!")' \
    --run-cmd "bun run" \
    --mem 512 --vcpus 1 --snapshot
```

### MySQL Parser
```bash title="Environment"
export NODE_PATH=/usr/local/lib/node_modules
sudo infra.operator host --lang mega-runtime \
    --code "SELECT * FROM users WHERE id = 1" \
    --run-cmd "node /usr/local/lib/mysql-run.js" \
    --mem 512 --vcpus 1 --snapshot
```

### PostgreSQL Parser
```bash title="Environment"
export NODE_PATH=/usr/local/lib/node_modules
sudo infra.operator host --lang mega-runtime \
    --code "SELECT * FROM products ORDER BY price DESC LIMIT 10" \
    --run-cmd "node /usr/local/lib/psql-run.js" \
    --mem 512 --vcpus 1 --snapshot
```

## Environment Variables

:::warning

  For database parsers (node-sql-parser), you must set NODE_PATH:

:::

```bash title="Environment"
export NODE_PATH=/usr/local/lib/node_modules
```

## Advantages






## Limitations

:::warning

  The Mega Runtime has some trade-offs:

:::

1. **Larger Image Size**: 8GB vs ~200MB for single-language rootfs
2. **Longer Initial Download**: First pull takes longer
3. **Memory Usage**: May use slightly more memory

## When to Use

| Use Case | Recommendation |
|----------|----------------|
| Development/Testing | **Mega Runtime** - Simplicity wins |
| Production (single language) | Individual rootfs - Minimal attack surface |
| Multi-language service | **Mega Runtime** - Easier ops |
| Resource-constrained | Individual rootfs - Smaller is better |

## Performance

| Operation | Time |
|-----------|------|
| Snapshot Restore | ~100ms |
| Python Hello World | ~50ms |
| Node.js Hello World | ~40ms |
| Go Hello World | ~80ms (compile + run) |
| Java Hello World | ~200ms (JVM startup) |

## Troubleshooting

### TypeScript errors with ts-node
Use `tsx` instead of `ts-node`:
```bash title="Terminal"
npx tsx script.ts  # Works!
ts-node script.ts  # May fail with ES module errors
```

### node-sql-parser not found
Set NODE_PATH before running:
```bash title="Run"
export NODE_PATH=/usr/local/lib/node_modules
node /usr/local/lib/mysql-run.js "SELECT 1"
```

### Pascal quote escaping
Use single quotes in Pascal source code:
```pascal title="Pascal code"
writeln('Hello');  // Correct
writeln("Hello");  // Error: illegal character
```

### Clojure version check
Don't use `--version`, use `-e`:
```bash title="Terminal"
clojure -e "(println \"Hello\")"  # Works!
clojure --version                  # Error
```
