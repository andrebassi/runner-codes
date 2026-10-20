---
title: 'Linguagens'
description: 'O catálogo de linguagens, recursos por linguagem e como adicionar uma nova'
---

## Catálogo

O catálogo vem do `languages.yaml`, embutido no binário (`internal/adapters/catalog/languages.yaml`). São 15 linguagens (mais a stack `llama`, abaixo):

| Linguagem | Nome na API | Imagem OCI de origem | vCPU | RAM | Warmup |
|-----------|-------------|----------------------|------|-----|--------|
| [Python](/languages/python) | `python` | `python:3.12-alpine` | 1 | 512 MiB | `compileall` da stdlib |
| [Node.js](/languages/nodejs) | `nodejs` | `node:22-alpine` | 1 | 512 MiB | — |
| [Go](/languages/go) | `go` | `golang:1.25-alpine` | 2 | 1024 MiB | `GOCACHE` populado |
| [Rust](/languages/rust) | `rust` | `rust:1-alpine` | 2 | 1024 MiB | — |
| [Java](/languages/java) | `java` | `eclipse-temurin:21-jdk-alpine` | 2 | 1024 MiB | — |
| [C](/languages/c) | `c` | `alpine:3.22` | 1 | 512 MiB | `apk add build-base` |
| [C++](/languages/cpp) | `cpp` | `alpine:3.22` | 1 | 512 MiB | `apk add build-base` |
| [Ruby](/languages/ruby) | `ruby` | `ruby:3.3-alpine` | 1 | 512 MiB | — |
| [PHP](/languages/php) | `php` | `php:8.3-cli-alpine` | 1 | 512 MiB | — |
| [Perl](/languages/perl) | `perl` | `alpine:3.22` | 1 | 512 MiB | `apk add perl` |
| [Bash](/languages/bash) | `bash` | `bash:5.2` | 1 | 256 MiB | — |
| [sh](/languages/sh) | `sh` | `alpine:3.22` | 1 | 256 MiB | — |
| [AWK](/languages/awk) | `awk` | `alpine:3.22` | 1 | 256 MiB | — |
| [jq](/languages/jq) | `jq` | `alpine:3.22` | 1 | 256 MiB | `apk add jq` |
| [SQLite](/languages/sqlite) | `sqlite` | `alpine:3.22` | 1 | 256 MiB | `apk add sqlite` |

`rc languages` (ou `GET /api/v1/languages`) mostra a mesma lista com `ready`, que diz se a imagem já foi construída neste host. Linguagem sem imagem construída é recusada com E006.

## Recursos

vCPU e RAM são **teto contra abuso**, não reserva. A memória da libkrun é elástica: o host só paga as páginas que o guest toca, e uma VM parada mediu 44 MB de footprint independentemente do teto. vCPU ociosa dorme no hypervisor.

## Como o código roda

| Família | Linguagens | Execução |
|---------|------------|----------|
| interpretada | Python, Node.js, Ruby, PHP, Perl, Bash, sh, AWK, jq | `<interpretador> <arquivo principal>` |
| compilada | C, C++, Rust | compila para um binário e executa o binário |
| JVM | Java | `javac` e `java Main` |
| Go | Go | `go run` |
| SQL | SQLite | o código vai pelo stdin do `sqlite3 :memory:` |

Em todas: o primeiro item de `files` é o arquivo principal, o diretório de trabalho fica no tmpfs de `/tmp`, a VM é nova e destruída no fim, e não há rede.

## Adicionar uma linguagem

São duas condições, e as duas precisam valer:

1. **A linguagem existe no executor do guest** (`internal/guest/executor.go`), com extensão, comando e, se for compilada, o comando de build. O executor tem entradas para mais linguagens do que o catálogo publica; se a que você quer não estiver lá, ela precisa ser acrescentada ao executor (e o guest recompilado).
2. **Uma entrada nova no `languages.yaml`**:

```yaml
  - name: lua
    image: <imagem OCI oficial>
    cpus: 1
    memory_mib: 256
    warmup: ""
```

| Campo | Regra |
|-------|-------|
| `name` | exatamente o nome da entrada no executor do guest |
| `image` | imagem OCI pública, da arquitetura do host; o build a baixa sem Docker |
| `cpus`, `memory_mib` | teto contra abuso |
| `warmup` | uma linha de `sh`, rodada em chroot na VM de build, com rede; o resultado fica gravado na imagem. Nada do warmup roda na VM do job |

Depois:

```bash
task build
task image:build -- --stack <nome>
rc run --stack <nome> --code '<programa mínimo>'
```

O binário do executor precisa estar no `PATH` da imagem ou no caminho absoluto que o executor usa. Uma imagem baseada em Alpine (musl) é o caso testado; o guest é estático e roda em qualquer uma.

## A stack `llama` (GPU)

Além das 15 linguagens, o catálogo traz a stack `llama` (`fedora:43`, 4 vCPU, 4096 MiB, `gpu: true`): llama.cpp com
backend Vulkan e um modelo Qwen2.5 0.5B gravado na imagem. Ela existe para `--gpu`; veja [GPU](/advanced/gpu).

## Fora do catálogo

- **Máquinas com imagem qualquer:** `rc machine create --image <ref>` (ou `image = "..."` no
  [runner.code](/cli/runner-code)) constrói e liga uma imagem OCI que não está no catálogo. Ela serve a máquinas, não
  aos jobs da API.
- **Parsers de banco:** o executor do guest ainda tem entradas para validadores de sintaxe SQL e NoSQL, mas eles não
  têm imagem no catálogo. Veja [Parsers de banco de dados](/guides/database-parsers-setup).
