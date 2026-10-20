---
title: 'Go'
description: 'Go no Runner Codes: imagem, recursos, warmup e exemplo'
---

## Resumo

| Item | Valor |
|------|-------|
| nome na API | `go` |
| imagem OCI de origem | `golang:1.25-alpine` |
| teto de recursos por job | 2 vCPU, 1024 MiB |

O teto de vCPU e RAM existe contra abuso; a memória da libkrun é elástica e o host só paga o que o guest toca.

## Warmup

O build compila um programa com `GOCACHE=/opt/runner/gocache`, o que popula o cache de build da biblioteca padrão dentro da imagem. Como o root é read-only, o `start.sh` copia esse cache para `/tmp/go-cache` no boot, o caminho que o executor do guest força em `GOCACHE`. A cópia acontece antes de a VM entrar no pool e não pesa no job. Medido: `go run` em 247 ms com o cache aquecido, contra 3,4 s sem ele.

Veja [Imagens](/concepts/images).

## Como o código é executado

O arquivo principal (o primeiro de `files`) é executado com `go run <arquivo>`. Sem rede, só a biblioteca padrão está disponível: módulos externos não são baixados. Tudo roda num diretório em `/tmp` (tmpfs), numa VM nova destruída no fim do job.

## Construir a imagem

```bash
rc image build --stack go
```

## Exemplo

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "go",
    "files": [
      {"name": "main.go", "content": "package main\n\nimport \"fmt\"\n\nfunc main() {\n\tfmt.Println(6 * 7)\n}\n"}
    ]
  }'
```

`stdout` esperado: `42`.
