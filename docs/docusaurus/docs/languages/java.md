---
title: 'Java'
description: 'Java no Runner Codes: imagem, recursos, warmup e exemplo'
---

## Resumo

| Item | Valor |
|------|-------|
| nome na API | `java` |
| imagem OCI de origem | `eclipse-temurin:21-jdk-alpine` |
| teto de recursos por job | 2 vCPU, 1024 MiB |

O teto de vCPU e RAM existe contra abuso; a memória da libkrun é elástica e o host só paga o que o guest toca.

## Warmup

Nenhum: a imagem oficial já traz o necessário.

## Como o código é executado

O arquivo principal (o primeiro de `files`) é compilado com `javac` e a classe `Main` é executada com `java Main`. Por isso:

- a classe com `main` precisa se chamar `Main`;
- o arquivo precisa se chamar `Main.java`.

Com `code` solto em `/api/v1/execute`, o guest grava `Main.java` sozinho. Erro de compilação devolve `exception: "Compilation failed"`, com a saída do `javac` em `stderr`. Tudo roda num diretório em `/tmp` (tmpfs), numa VM nova destruída no fim do job, sem rede.

## Construir a imagem

```bash
rc image build --stack java
```

## Exemplo

```bash
curl -s http://127.0.0.1:8080/api/v1/run \
  -H 'Content-Type: application/json' \
  -d '{
    "language": "java",
    "files": [
      {"name": "Main.java", "content": "public class Main {\n    public static void main(String[] args) {\n        System.out.println(6 * 7);\n    }\n}\n"}
    ]
  }'
```

`stdout` esperado: `42`.
