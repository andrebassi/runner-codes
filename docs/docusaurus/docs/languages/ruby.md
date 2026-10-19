---
title: 'Ruby'
description: 'Ruby no Runner Codes: imagem, recursos, warmup e exemplo'
---

## Resumo

| Item | Valor |
|------|-------|
| nome na API | `ruby` |
| imagem OCI de origem | `ruby:3.3-alpine` |
| teto de recursos por job | 1 vCPU, 512 MiB |

O teto de vCPU e RAM existe contra abuso; a memória da libkrun é elástica e o host só paga o que o guest toca.

## Warmup

Nenhum: a imagem oficial já traz o necessário.

## Como o código é executado

O arquivo principal (o primeiro de `files`) é executado com `ruby <arquivo>`. Os demais arquivos ficam no mesmo diretório e podem ser carregados com `require_relative`. Sem rede, gems externas não são instaladas. Tudo roda num diretório em `/tmp` (tmpfs), numa VM nova destruída no fim do job.

## Construir a imagem

```bash
rc image build --stack ruby
```

## Exemplo

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "ruby",
    "files": [
      {"name": "main.rb", "content": "puts gets.strip.reverse\n"}
    ],
    "stdin": "abc"
  }'
```

`stdout` esperado: `cba`.
