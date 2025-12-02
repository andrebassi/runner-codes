# Linguagens Suportadas - Runner Codes

> **Status**: 41/41 linguagens funcionando (100%) - Completo
> **Atualizado**: 2025-11-30
> **Testado em**: AWS EC2 m5zn.metal (bare metal)

## Resumo

O Runner Codes suporta **41 linguagens de programação** (incluindo databases) com execução real em microVMs Firecracker isoladas. Cada linguagem possui seu próprio rootfs (sistema de arquivos raiz) com todas as dependências necessárias.

---

## Tabela de Linguagens

| # | Linguagem | Versao | Rootfs Size | Tipo | Comando | Extensao |
|---|-----------|--------|-------------|------|---------|----------|
| 1 | Python | 3.10.12 | 600 MB | Interpretada | `python3` | `.py` |
| 2 | Node.js | 20.19.6 | 700 MB | Interpretada | `node` | `.js` |
| 3 | TypeScript | 5.x (via ts-node) | 800 MB | Transpilada | `ts-node` | `.ts` |
| 4 | Go | 1.21.5 | 700 MB | Compilada | `go run` | `.go` |
| 5 | Rust | 1.75+ | 2.0 GB | Compilada | `rustc` | `.rs` |
| 6 | Bash | 5.1.16 | 512 MB | Shell | `bash` | `.sh` |
| 7 | C | GCC 11.4.0 | 600 MB | Compilada | `gcc` | `.c` |
| 8 | C++ | G++ 11.2.0 | 700 MB | Compilada | `g++` | `.cpp` |
| 9 | Java | OpenJDK 11.0.29 | 800 MB | Compilada/JVM | `java` | `.java` |
| 10 | Kotlin | 1.9.x | 900 MB | Compilada/JVM | `kotlin` | `.kt` |
| 11 | Scala | 3.x | 900 MB | Compilada/JVM | `scala` | `.scala` |
| 12 | Ruby | 3.0.2 | 600 MB | Interpretada | `ruby` | `.rb` |
| 13 | PHP | 8.1.2 | 600 MB | Interpretada | `php` | `.php` |
| 14 | Perl | 5.34.0 | 500 MB | Interpretada | `perl` | `.pl` |
| 15 | Lua | 5.4.4 | 400 MB | Interpretada | `lua` | `.lua` |
| 16 | R | 4.1.2 | 1.2 GB | Interpretada | `Rscript` | `.r` |
| 17 | Haskell | GHC 8.8.4 | 1.2 GB | Compilada | `runghc` | `.hs` |
| 18 | Elixir | 1.12.2 | 900 MB | Interpretada | `elixir` | `.ex` |
| 19 | Erlang | OTP 24 | 800 MB | Interpretada | `escript` | `.erl` |
| 20 | Julia | 1.10.0 | 1.5 GB | JIT | `julia` | `.jl` |
| 21 | Fortran | GFortran 11.2.0 | 600 MB | Compilada | `gfortran` | `.f90` |
| 22 | Common Lisp | SBCL 2.1.11 | 700 MB | Interpretada | `sbcl --script` | `.lisp` |
| 23 | Scheme | Guile 3.0 | 600 MB | Interpretada | `guile` | `.scm` |
| 24 | Prolog | SWI-Prolog 8.4.2 | 600 MB | Logica | `swipl` | `.pl` |
| 25 | Pascal | FPC 3.2.2 | 600 MB | Compilada | `fpc` | `.pas` |
| 26 | OCaml | 4.13.1 | 1.5 GB | Compilada | `ocaml` | `.ml` |
| 27 | Nim | 2.0.0 | 800 MB | Compilada | `nim` | `.nim` |
| 28 | Crystal | 1.x | 1.0 GB | Compilada | `crystal` | `.cr` |
| 29 | D | LDC 1.28.0 | 800 MB | Compilada | `ldc2` | `.d` |
| 30 | Zig | 0.12.0 | 600 MB | Compilada | `zig run` | `.zig` |
| 31 | COBOL | GnuCOBOL 3.1.2 | 700 MB | Compilada | `cobc` | `.cob` |
| 32 | Groovy | 4.x | 800 MB | JVM | `groovy` | `.groovy` |
| 33 | Clojure | 1.12.3 (Temurin 21) | 600 MB | JVM | `clojure` | `.clj` |
| 34 | SQLite | 3.37.2 | 400 MB | Query | `sqlite3` | `.sql` |
| 35 | Tcl | 8.6.12 | 400 MB | Interpretada | `tclsh` | `.tcl` |
| 36 | AWK | BusyBox 1.36.1 | 200 MB | Interpretada | `awk` | `.awk` |
| 37 | jq | 1.7.1 | 200 MB | Query | `jq --null-input` | `.jq` |
| 38 | NASM | 2.16.03 | 250 MB | Assembly | `nasm-run` | `.asm` |
| 39 | Octave | 6.1.0 | 1.0 GB | Interpretada | `octave` | `.m` |
| 40 | .NET (C#) | 8.0.404 | 2.0 GB | Compilada | `csharp-run` | `.cs` |
| 41 | MySQL | MariaDB 10.11.14 | 700 MB | Database | `mysql-run` | `.sql` |

---

## Categorizacao por Tipo

### Linguagens Compiladas (14)
C, C++, Go, Rust, Java, Kotlin, Scala, Fortran, Pascal, OCaml, Nim, Crystal, D, Zig, COBOL, .NET

### Linguagens Interpretadas (18)
Python, Node.js, Ruby, PHP, Perl, Lua, R, Elixir, Erlang, Common Lisp, Scheme, Prolog, Tcl, AWK, Octave

### Linguagens JVM (5)
Java, Kotlin, Scala, Groovy, Clojure

### Linguagens Transpiladas (1)
TypeScript (para JavaScript)

### Query/DSL (3)
SQLite, jq, AWK

### Database (1)
MySQL (MariaDB)

### Shell (1)
Bash

### Assembly (1)
NASM (x86_64)

---

## Performance e Otimizacao

### Por que rootfs grandes (3-4GB) NAO ficam lentos?

O Firecracker foi projetado para inicializacao ultra-rapida. Ter um rootfs de 4GB (como .NET) nao impacta significativamente a performance porque:

#### 1. Lazy Loading (Carregamento sob demanda)
O microVM **nao carrega todo o rootfs na memoria**. O Firecracker usa o arquivo `.ext4` como block device e so le os blocos necessarios conforme sao acessados.

```
Rootfs .NET: 4GB no disco
Carregado na memoria durante execucao: ~50-100MB (apenas binarios necessarios)
```

#### 2. Linux Page Cache
Arquivos acessados frequentemente ficam em cache na RAM do **host**, tornando acessos subsequentes extremamente rapidos (microssegundos ao inves de milissegundos).

#### 3. Copy-on-Write
Firecracker suporta overlays, onde apenas as mudancas sao escritas, nao o rootfs inteiro.

#### 4. Boot Time Fixo
O tempo de boot do Firecracker e consistente (~125ms) independente do tamanho do rootfs, porque:
- O kernel e carregado diretamente na memoria
- Apenas o init process e iniciado
- Nao ha servicos de sistema para inicializar

### Metricas de Performance

| Metrica | Valor Tipico | Notas |
|---------|--------------|-------|
| **Boot do Firecracker** | ~125ms | Independente do tamanho do rootfs |
| **Primeira execucao Python** | ~200ms | Interpretador ja carregado |
| **Primeira execucao .NET** | ~2-3s | JIT compilation inicial |
| **Execucoes subsequentes** | <500ms | Page cache ativo |
| **Memoria por microVM** | 512MB | Configuravel |

### O que realmente impacta performance?

| Fator | Impacto | Descricao |
|-------|---------|-----------|
| **RAM alocada ao microVM** | Alto | Mais RAM = compilacao mais rapida |
| **CPU do host** | Alto | m5zn.metal tem CPUs de alta frequencia |
| **Storage (NVMe vs HDD)** | Medio | EC2 metal tem NVMe local rapido |
| **Tamanho do rootfs** | Baixo | So afeta tempo de copia/deploy |

### Quando o tamanho importa?

O tamanho do rootfs so e relevante durante:
1. **Deploy/Upload**: Copiar 4GB vs 300MB para S3
2. **Primeira criacao**: Gerar rootfs de 4GB leva mais tempo
3. **Backup/Snapshot**: Arquivos maiores demoram mais para comprimir

Durante **execucao de codigo**, a performance e praticamente identica entre linguagens.

---

## Alteracoes e Ajustes Realizados (2025-11-29)

### Problemas Resolvidos

#### 1. OCaml - "Unbound module Stdlib"
**Problema**: O pacote OCaml instalado via `debootstrap --include` nao incluia a biblioteca padrao completa.

**Solucao**: Recriar rootfs com instalacao via `apt-get` dentro do chroot:
```bash
sudo debootstrap --variant=minbase --include=ca-certificates,wget bookworm /mnt/rootfs-ocaml http://deb.debian.org/debian/
sudo mount --bind /proc /mnt/rootfs-ocaml/proc
sudo chroot /mnt/rootfs-ocaml /bin/bash -c 'apt-get update && apt-get install -y --no-install-recommends ocaml-nox'
sudo umount /mnt/rootfs-ocaml/proc
```

#### 2. .NET - "No such file or directory" / Rootfs vazio
**Problema**: Uso de `umount -l` (lazy unmount) enquanto o mount estava ocupado causou perda de dados.

**Solucao**: Processo correto de unmount:
```bash
# 1. Matar processos usando o mount
sudo fuser -km /mnt/dotnet

# 2. Sync para garantir escrita
sync

# 3. Unmount normal (sem -l)
sudo umount /mnt/dotnet

# 4. Verificar rootfs apos unmount
ls -la /srv/firecracker/images/rootfs-dotnet.ext4
```

#### 3. .NET - Teste incorreto
**Problema**: O comando `dotnet new console --force` sobrescrevia o arquivo de teste com template que imprime "Hello, World!" (com virgula) ao inves de "Hello World".

**Solucao**: Usar arquivo .csproj explicito ao inves de `dotnet new`:
```bash
# Criar Program.cs e test.csproj manualmente
echo 'Console.WriteLine("Hello World");' > Program.cs
cat > test.csproj << 'EOF'
<Project Sdk="Microsoft.NET.Sdk">
  <PropertyGroup>
    <OutputType>Exe</OutputType>
    <TargetFramework>net8.0</TargetFramework>
  </PropertyGroup>
</Project>
EOF
dotnet run --project test.csproj
```

### Scripts de Correcao Criados

| Script | Proposito |
|--------|-----------|
| `/tmp/test-all-final.sh` | Script de teste para todas 40 linguagens |
| `/tmp/fix-ocaml-dotnet.sh` | Recriacao de rootfs OCaml e .NET |
| `/tmp/final-fix.sh` | Fix final com sync correto |

---

## Estrutura de Arquivos

```
/srv/firecracker/images/
├── rootfs-python.ext4      # 600 MB
├── rootfs-nodejs.ext4      # 700 MB
├── rootfs-typescript.ext4  # 800 MB
├── rootfs-go.ext4          # 700 MB
├── rootfs-rust.ext4        # 2.0 GB
├── rootfs-bash.ext4        # 512 MB
├── rootfs-c.ext4           # 600 MB
├── rootfs-cpp.ext4         # 700 MB
├── rootfs-java.ext4        # 800 MB
├── rootfs-kotlin.ext4      # 900 MB
├── rootfs-scala.ext4       # 900 MB
├── rootfs-ruby.ext4        # 600 MB
├── rootfs-php.ext4         # 600 MB
├── rootfs-perl.ext4        # 500 MB
├── rootfs-lua.ext4         # 400 MB
├── rootfs-r.ext4           # 1.2 GB
├── rootfs-haskell.ext4     # 1.2 GB
├── rootfs-elixir.ext4      # 900 MB
├── rootfs-erlang.ext4      # 800 MB
├── rootfs-julia.ext4       # 1.5 GB
├── rootfs-fortran.ext4     # 600 MB
├── rootfs-lisp.ext4        # 700 MB
├── rootfs-scheme.ext4      # 600 MB
├── rootfs-prolog.ext4      # 600 MB
├── rootfs-pascal.ext4      # 600 MB
├── rootfs-ocaml.ext4       # 1.5 GB
├── rootfs-nim.ext4         # 800 MB
├── rootfs-crystal.ext4     # 1.0 GB
├── rootfs-d.ext4           # 800 MB
├── rootfs-zig.ext4         # 800 MB
├── rootfs-cobol.ext4       # 700 MB
├── rootfs-groovy.ext4      # 800 MB
├── rootfs-clojure.ext4     # 800 MB
├── rootfs-sqlite.ext4      # 400 MB
├── rootfs-tcl.ext4         # 400 MB
├── rootfs-awk.ext4         # 400 MB
├── rootfs-jq.ext4          # 400 MB
├── rootfs-nasm.ext4        # 500 MB
├── rootfs-octave.ext4      # 1.0 GB
└── rootfs-dotnet.ext4      # 4.0 GB
```

**Total**: ~32 GB de rootfs images

---

## Resultado do Teste Final

```
=== Testing all languages ===
Started: Sat Nov 29 16:45:20 UTC 2025
[python] OK
[nodejs] OK
[typescript] OK
[go] OK
[rust] OK
[bash] OK
[c] OK
[cpp] OK
[java] OK
[kotlin] OK
[scala] OK
[ruby] OK
[php] OK
[perl] OK
[lua] OK
[r] OK
[haskell] OK
[elixir] OK
[erlang] OK
[julia] OK
[fortran] OK
[lisp] OK
[scheme] OK
[prolog] OK
[pascal] OK
[ocaml] OK
[nim] OK
[crystal] OK
[d] OK
[zig] OK
[cobol] OK
[groovy] OK
[clojure] OK
[sqlite] OK
[tcl] OK
[awk] OK
[jq] OK
[nasm] OK
[octave] OK
[dotnet] OK

=== Summary ===
PASSED: 40
FAILED: 0

Completed: Sat Nov 29 16:45:41 UTC 2025
```

---

## Linguagens com Teste Real em Firecracker (2025-11-30)

| Linguagem | Base Image | Versao | Rootfs | Snapshot | S3 | Teste |
|-----------|------------|--------|--------|----------|-----|-------|
| Clojure | clojure:temurin-21-alpine | 1.12.3 | ✅ | ✅ | ✅ | ✅ `(println "Hello from Clojure")` |
| OCaml | frolvlad/alpine-ocaml | 4.14.2 | ✅ | ✅ | ✅ | ✅ `print_string "Hello from OCaml"` (~26ms) |
| Groovy | groovy:jdk21-alpine | 4.0.27 | ✅ | ✅ | ✅ | ✅ `println "Hello from Groovy"` (~2.1s) |
| Deno | denoland/deno:debian | 2.5.6 | ✅ | ✅ | ✅ | ✅ `console.log("Hello from Deno")` (~2.6s) |
| Bun | oven/bun:alpine | 1.3.3 | ✅ | ✅ | ✅ | ✅ `console.log("Hello from Bun")` (~56ms) |
| D | dlang2/ldc-ubuntu | 1.26.0 | ✅ | ✅ | ✅ | ✅ `writeln("Hello from D")` (~287ms) |
| C#/.NET | mcr.microsoft.com/dotnet/sdk:8.0-alpine | 8.0.404 | ✅ | ✅ | ✅ | ✅ `Console.WriteLine("Hello from C#")` (~2.8s) |
| AWK | alpine:3.20 (BusyBox) | 5.1.16 | ✅ | ✅ | ✅ | ✅ `BEGIN {print "Hello from AWK"}` (~7ms) |
| jq | alpine:3.20 | 1.7.1 | ✅ | ✅ | ✅ | ✅ `{"message": "Hello from jq"}` (~8ms) |
| NASM | alpine:3.20 | 2.16.03 | ✅ | ✅ | ✅ | ✅ x86_64 syscall write (~15ms) |
| Zig | alpine:3.20 | 0.12.0 | ✅ | ✅ | ✅ | ✅ `std.debug.print("Hello from Zig")` (~7.2s) |
| MySQL | alpine:3.20 + MariaDB | 10.11.14 | ✅ | ✅ | ✅ | ✅ `SELECT * FROM EMPLOYEE` (~1.7s) |

---

## Proximos Passos

1. [x] Clojure - rootfs → snapshot → upload → teste → doc ✅
2. [x] OCaml - rootfs → snapshot → upload → teste → doc ✅
3. [x] Groovy - rootfs → snapshot → upload → teste → doc ✅
4. [x] Deno - rootfs → snapshot → upload → teste → doc ✅
5. [x] Bun - rootfs → snapshot → upload → teste → doc ✅
6. [x] D - rootfs → snapshot → upload → teste → doc ✅
7. [x] C#/.NET - rootfs → snapshot → upload → teste → doc ✅
8. [x] AWK - rootfs → snapshot → upload → teste → doc ✅
9. [x] jq - rootfs → snapshot → upload → teste → doc ✅
10. [x] NASM - rootfs → snapshot → upload → teste → doc ✅
11. [x] Zig - rootfs → snapshot → upload → teste → doc ✅
12. [x] MySQL - rootfs → snapshot → upload → teste → doc ✅

---

## Detalhes de Implementacao - MySQL (MariaDB)

### Configuracao

| Item | Valor |
|------|-------|
| **Base Image** | alpine:3.20 |
| **Database** | MariaDB 10.11.14 |
| **Rootfs Size** | 700 MB |
| **Memory** | 512 MB |
| **Wrapper Script** | `/usr/local/bin/mysql-run` |

### Comandos de Criacao

```bash
# 1. Criar rootfs base
infra.operator rootfs from-docker --image alpine:3.20 --name mysql --size 700

# 2. Montar rootfs
sudo mkdir -p /tmp/mnt-mysql
sudo mount /srv/firecracker/images/rootfs-mysql.ext4 /tmp/mnt-mysql

# 3. Instalar MariaDB
sudo chroot /tmp/mnt-mysql apk add --no-cache mariadb mariadb-client

# 4. Configurar diretorios
sudo chroot /tmp/mnt-mysql mkdir -p /var/lib/mysql /run/mysqld
sudo chroot /tmp/mnt-mysql chown -R mysql:mysql /var/lib/mysql /run/mysqld

# 5. Inicializar banco de dados
sudo chroot /tmp/mnt-mysql mysql_install_db --user=mysql --datadir=/var/lib/mysql

# 6. Criar wrapper script
cat << 'SCRIPT' | sudo tee /tmp/mnt-mysql/usr/local/bin/mysql-run > /dev/null
#!/bin/sh
# mysql-run: start MySQL, execute SQL file, return results

if [ -z "$1" ]; then
    echo "Usage: mysql-run <file.sql>"
    exit 1
fi

SQL_FILE="$1"

# Start MariaDB in background
mkdir -p /run/mysqld
chown mysql:mysql /run/mysqld
mysqld_safe --user=mysql --datadir=/var/lib/mysql &>/dev/null &

# Wait for MySQL to be ready (max 5 seconds)
for i in $(seq 1 50); do
    if mysqladmin ping &>/dev/null; then
        break
    fi
    sleep 0.1
done

# Create default database and execute SQL file
mysql -t -e "CREATE DATABASE IF NOT EXISTS testdb; USE testdb; SOURCE $SQL_FILE;"
EXIT_CODE=$?

# Shutdown MySQL
mysqladmin shutdown &>/dev/null
sleep 0.5

exit $EXIT_CODE
SCRIPT

sudo chmod +x /tmp/mnt-mysql/usr/local/bin/mysql-run

# 7. Sync e desmontar
sync
sudo umount /tmp/mnt-mysql

# 8. Criar snapshot
infra.operator snapshot create --lang mysql --mem 512 --vcpus 1

# 9. Upload para S3
infra.operator rootfs upload --lang mysql --bucket runner-codes
infra.operator snapshot upload --lang mysql --bucket runner-codes
```

### Executor Config (pkg/guest/executor.go)

```go
"mysql": {
    Name:      "mysql",
    Extension: ".sql",
    Command:   "/usr/local/bin/mysql-run",
    Args:      []string{},
},
```

### Exemplo de Teste

**Input SQL:**
```sql
CREATE TABLE EMPLOYEE (
  empId INTEGER PRIMARY KEY,
  name TEXT NOT NULL,
  dept TEXT NOT NULL
);

INSERT INTO EMPLOYEE VALUES (1, "Clark", "Sales");
INSERT INTO EMPLOYEE VALUES (2, "Dave", "Accounting");
INSERT INTO EMPLOYEE VALUES (3, "Ava", "Sales");

SELECT * FROM EMPLOYEE WHERE dept = "Sales";
```

**Output:**
```
+-------+-------+-------+
| empId | name  | dept  |
+-------+-------+-------+
|     1 | Clark | Sales |
|     3 | Ava   | Sales |
+-------+-------+-------+
```

### Notas Importantes

1. **MariaDB vs MySQL**: Alpine usa MariaDB como drop-in replacement para MySQL
2. **Database padrao**: O wrapper cria automaticamente um banco `testdb` para executar as queries
3. **Formatacao**: O flag `-t` no mysql client formata output como tabela ASCII
4. **Startup time**: ~1.5s para iniciar MariaDB + executar query
5. **Cleanup**: O wrapper faz shutdown do MySQL apos executar para liberar recursos

**Todas as 41 linguagens estao completas e testadas em Firecracker!**
