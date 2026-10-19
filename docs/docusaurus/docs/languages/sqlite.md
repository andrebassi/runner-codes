---
title: 'SQLite'
description: 'SQLite no Runner Codes: imagem, recursos, warmup e exemplo'
---

## Resumo

| Item | Valor |
|------|-------|
| nome na API | `sqlite` |
| imagem OCI de origem | `alpine:3.22` + `sqlite` |
| teto de recursos por job | 1 vCPU, 256 MiB |

O teto de vCPU e RAM existe contra abuso; a memória da libkrun é elástica e o host só paga o que o guest toca.

## Warmup

`apk add sqlite`: a imagem base não traz o `sqlite3`. O pacote é instalado na VM de build e fica gravado no ext4. Veja [Imagens](/concepts/images).

## Como o código é executado

O conteúdo do arquivo principal (o primeiro de `files`) vai pelo stdin do `sqlite3 :memory:` — um banco em memória, que some no fim do job. Se o job tiver stdin, ele é enviado **antes** do código, seguido de uma quebra de linha. A saída usa o formato padrão do `sqlite3` (colunas separadas por `|`). Tudo roda numa VM nova destruída no fim do job, sem rede.

Esta é a SQLite executável. Os validadores de sintaxe de banco não estão no catálogo; veja [Parsers de banco de dados](/guides/database-parsers-setup).

## Construir a imagem

```bash
runner.code image build --lang sqlite
```

## Exemplo

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "sqlite",
    "files": [
      {"name": "main.sql", "content": "CREATE TABLE t (n INTEGER);\nINSERT INTO t VALUES (40), (2);\nSELECT SUM(n) FROM t;\n"}
    ]
  }'
```

`stdout` esperado: `42`.
