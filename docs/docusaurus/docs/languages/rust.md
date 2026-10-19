---
title: 'Rust'
description: 'Rust no Runner Codes: imagem, recursos, warmup e exemplo'
---

## Resumo

| Item | Valor |
|------|-------|
| nome na API | `rust` |
| imagem OCI de origem | `rust:1-alpine` |
| teto de recursos por job | 2 vCPU, 1024 MiB |

O teto de vCPU e RAM existe contra abuso; a memória da libkrun é elástica e o host só paga o que o guest toca.

## Warmup

Nenhum: a imagem oficial já traz o necessário.

## Como o código é executado

O arquivo principal (o primeiro de `files`) é compilado com `rustc -o <binário> <arquivo>` e o binário é executado com o stdin do job. Erro de compilação devolve `exception: "Compilation failed"`, com a saída do `rustc` em `stderr`. Sem rede, crates externos não estão disponíveis. Tudo roda num diretório em `/tmp` (tmpfs), numa VM nova destruída no fim do job.

## Construir a imagem

```bash
rc image build --stack rust
```

## Exemplo

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "rust",
    "files": [
      {"name": "main.rs", "content": "fn main() {\n    println!(\"{}\", 6 * 7);\n}\n"}
    ]
  }'
```

`stdout` esperado: `42`.
