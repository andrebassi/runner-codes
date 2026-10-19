---
title: 'Parsers de banco de dados'
description: 'Como trazer de volta os validadores de sintaxe SQL e NoSQL, que hoje não estão no catálogo'
---

## Estado atual

Os validadores de sintaxe de banco — `mysql-parser`, `postgresql-parser`, `mariadb-parser`, `sqlite-parser`, `mongodb-parser` e `redis-parser` — **não estão no catálogo**. A API recusa essas linguagens com E006.

Eles existiam na versão com Firecracker. O que sobrou:

- o executor do guest (`internal/guest/executor.go`) ainda tem as entradas, que chamam `/usr/local/bin/node` com um script de validação (`/usr/local/bin/mysql-run`, `psql-run`, `mariadb-run`, `sqlite-run`, `mongodb-run`, `redis-run`), e, nos parsers SQL, `NODE_PATH=/opt/sql-parser/node_modules`;
- os scripts e Dockerfiles de cada parser estão em `docker/<parser>/` na raiz do repositório; os SQL usam `node-sql-parser`.

A SQLite **executável** (`sqlite`) continua no catálogo; ver [SQLite](/languages/sqlite).

## O que é preciso para reativar um parser

Cada parser precisa de duas coisas: uma imagem própria e uma entrada nova no `languages.yaml`.

### 1. Imagem própria

O build de imagens baixa a imagem OCI de um registry com go-containerregistry; não usa Docker local. Portanto a imagem do parser precisa:

- conter o Node.js em `/usr/local/bin/node`;
- conter o script do parser no caminho que o executor espera (por exemplo `/usr/local/bin/mysql-run`);
- para os parsers SQL, conter `node-sql-parser` em `/opt/sql-parser/node_modules`;
- ser publicada num registry acessível pelo host que roda o build.

O Dockerfile em `docker/<parser>/` descreve esse conteúdo e pode servir de ponto de partida para gerar e publicar a imagem.

### 2. Entrada no languages.yaml

O catálogo fica embutido no binário, em `internal/adapters/catalog/languages.yaml`. Uma entrada nova tem o formato:

```yaml
  - name: mysql-parser
    image: <registry>/<imagem do parser>:<tag>
    cpus: 1
    memory_mib: 256
    warmup: ""
```

- `name` precisa ser exatamente o nome usado no executor do guest.
- `cpus` e `memory_mib` são teto contra abuso; escolha-os medindo o parser.
- `warmup` pode ficar vazio se a imagem já traz tudo.

Depois:

```bash
task build
task image:build -- --stack mysql-parser
rc run --stack mysql-parser --code 'SELECT 1;'
```

### 3. Teste

Cada parser reativado precisa entrar no teste ponta a ponta com uma consulta válida e uma inválida, para provar que a validação reprova o que deve.

## Não verificado

Nenhum parser foi reconstruído sobre a libkrun até a data desta documentação. Os passos acima descrevem o que o código atual exige; não foram executados de ponta a ponta.
