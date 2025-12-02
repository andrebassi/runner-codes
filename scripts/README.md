# LLM-FireSandbox Scripts

Scripts para criação e manutenção das imagens rootfs para as 40 linguagens de programação suportadas.

## Linguagens Suportadas (40 total)

| Categoria | Linguagens |
|-----------|------------|
| **Main Languages** | Python, Node.js, TypeScript, Go, Rust, Bash |
| **Systems Languages** | C, C++, Fortran |
| **JVM Languages** | Java, Kotlin, Scala, Groovy, Clojure |
| **Scripting Languages** | Ruby, PHP, Perl, Lua, R, TCL, AWK, JQ |
| **Functional Languages** | Haskell, Elixir, Erlang, OCaml, Scheme |
| **Modern Languages** | Julia, Nim, Crystal, D, Zig |
| **Legacy/Specialty** | COBOL, Pascal, Lisp, Prolog |
| **Database/Tools** | SQLite, Octave, NASM, .NET |

## Scripts Disponíveis

### Scripts de Criação

| Script | Descrição |
|--------|-----------|
| `create-rootfs.sh` | Script base para criar uma única rootfs |
| `create-all-rootfs.sh` | Cria todas as rootfs básicas |
| `create-all-languages.sh` | Cria rootfs para todas as linguagens principais |
| `create-remaining-rootfs.sh` | Cria rootfs para linguagens restantes |
| `create-additional-rootfs.sh` | Cria rootfs adicionais (scala, kotlin, etc.) |

### Scripts de Correção

| Script | Descrição |
|--------|-----------|
| `fix-rootfs.sh` | Corrige rootfs com pacotes faltantes |
| `fix-all-10.sh` | Corrige as 10 linguagens com instalação manual |
| `fix-manual-installs.sh` | Instala binários manuais (Kotlin, Julia, Nim, Crystal, Zig, Clojure, .NET) |
| `recreate-failed.sh` | Recria rootfs que falharam nos testes |

### Scripts de Teste

| Script | Descrição |
|--------|-----------|
| `test-all-languages.sh` | Testa todas as 40 linguagens |

## Uso

### Pré-requisitos

- Ubuntu 22.04 (bare metal ou VM com KVM)
- Pacotes: `debootstrap`, `e2fsprogs`, `wget`, `curl`
- AWS CLI configurado (para upload S3)
- Guest-runner compilado em `/opt/llm-firesandbox/rootfs/guest-runner`

### Configuração

```bash
# Diretório das imagens
IMAGES_DIR="/srv/firecracker/images"

# Bucket S3
S3_BUCKET="llm-firesandbox-rootfs"

# Guest Runner
GUEST_RUNNER="/opt/llm-firesandbox/rootfs/guest-runner"
```

### Criar Todas as Rootfs

```bash
# 1. Criar rootfs básicas
sudo ./create-all-languages.sh

# 2. Criar linguagens restantes
sudo ./create-remaining-rootfs.sh

# 3. Criar linguagens adicionais
sudo ./create-additional-rootfs.sh

# 4. Corrigir instalações manuais
sudo ./fix-manual-installs.sh

# 5. Testar todas
sudo ./test-all-languages.sh
```

### Testar Todas as Linguagens

```bash
sudo ./test-all-languages.sh
```

Saída esperada:
```
Total: 40 passed, 0 failed
```

## Tamanhos das Imagens

| Linguagem | Tamanho | Motivo |
|-----------|---------|--------|
| Haskell | 2500MB | GHC é muito grande |
| .NET | 2000MB | SDK completo |
| Java/Kotlin/Scala | 1500MB | JDK completo |
| Clojure/Groovy | 1200MB | JDK + runtime |
| Crystal | 1000MB | Compilador + libs |
| Outras | 400-800MB | Compactas |

## Linguagens com Instalação Manual

Estas linguagens não estão disponíveis via apt e requerem download manual:

| Linguagem | Fonte | Versão |
|-----------|-------|--------|
| Kotlin | JetBrains GitHub | 1.9.22 |
| Julia | JuliaLang S3 | 1.10.0 |
| Nim | nim-lang.org | 2.0.0 |
| Crystal | Crystal GitHub | 1.10.1 |
| Zig | ziglang.org | 0.11.0 |
| Clojure | Clojure GitHub | 1.11.1.1435 |
| .NET | Microsoft | 8.0 |

## S3 Storage

- **Bucket**: `s3://llm-firesandbox-rootfs/`
- **Region**: `us-east-1`
- **Total**: ~37 GiB (42 objetos)

### Upload para S3

```bash
aws s3 cp /srv/firecracker/images/rootfs-LANG.ext4 s3://llm-firesandbox-rootfs/
```

### Download do S3

```bash
aws s3 cp s3://llm-firesandbox-rootfs/rootfs-LANG.ext4 /srv/firecracker/images/
```

## Troubleshooting

### "No space left on device" durante instalação

Aumentar o tamanho da imagem rootfs:
```bash
# Exemplo: Haskell precisa de 2500MB
SIZE_MB=2500
dd if=/dev/zero of=/tmp/rootfs-haskell.ext4 bs=1M count=${SIZE_MB}
```

### Pacotes JVM falhando

Montar `/proc` antes de instalar:
```bash
sudo mount -t proc proc /mnt/LANG/proc
sudo chroot /mnt/LANG apt-get install -y default-jdk
sudo umount /mnt/LANG/proc
```

### Runtime não encontrado após criação

Verificar se o binário está no PATH:
```bash
sudo mount -o loop /srv/firecracker/images/rootfs-LANG.ext4 /mnt/LANG
sudo chroot /mnt/LANG which COMANDO
sudo umount /mnt/LANG
```

## Estrutura de uma Rootfs

```
/
├── bin/                    # Binários essenciais
├── etc/
│   ├── apt/sources.list   # Repos Ubuntu
│   └── systemd/system/
│       └── guest-runner.service
├── opt/                    # Runtimes manuais (Kotlin, Julia, etc.)
├── usr/
│   ├── bin/               # Binários de linguagens
│   └── local/bin/
│       └── guest-runner   # Agente de execução
└── var/                    # Cache apt (limpo)
```

## Última Atualização

- **Data**: 2025-11-29
- **Status**: 40/40 linguagens funcionando
- **Testado em**: EC2 m5zn.metal (Ubuntu 22.04)
