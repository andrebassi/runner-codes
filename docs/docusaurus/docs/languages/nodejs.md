---
title: 'Node.js'
description: 'Node.js no Runner Codes: imagem, recursos, warmup e exemplo'
---

## Resumo

| Item | Valor |
|------|-------|
| nome na API | `nodejs` |
| imagem OCI de origem | `node:22-alpine` |
| teto de recursos por job | 1 vCPU, 512 MiB |

O teto de vCPU e RAM existe contra abuso; a memória da libkrun é elástica e o host só paga o que o guest toca.

## Warmup

Nenhum: a imagem oficial já traz o necessário.

## Como o código é executado

O arquivo principal (o primeiro de `files`) é executado com `node <arquivo>`. Os demais arquivos ficam no mesmo diretório e podem ser carregados com `require`. Tudo roda num diretório em `/tmp` (tmpfs), numa VM nova destruída no fim do job, sem rede — `npm install` não funciona dentro do job.

## Construir a imagem

```bash
runner.code image build --lang nodejs
```

## Exemplo

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "nodejs",
    "files": [
      {"name": "main.js", "content": "let d = \"\";\nprocess.stdin.on(\"data\", c => d += c);\nprocess.stdin.on(\"end\", () => console.log(d.trim().toUpperCase()));\n"}
    ],
    "stdin": "abc"
  }'
```

`stdout` esperado: `ABC`.
