---
title: '数据库解析器'
description: '如何恢复目前不在目录中的 SQL 和 NoSQL 语法校验器'
---

## 当前状态 {#estado-atual}

数据库语法校验器（`mysql-parser`、`postgresql-parser`、`mariadb-parser`、`sqlite-parser`、`mongodb-parser` 和 `redis-parser`）**不在目录中**。API 会以 E006 拒绝这些语言。

它们来自项目的早期版本，尚未基于 libkrun 重建。仓库中现有的内容：

- guest 的执行器（`internal/guest/executor.go`）仍保留这些条目，它们用一个校验脚本调用 `/usr/local/bin/node`（`/usr/local/bin/mysql-run`、`psql-run`、`mariadb-run`、`sqlite-run`、`mongodb-run`、`redis-run`），SQL 解析器还会设置 `NODE_PATH=/opt/sql-parser/node_modules`；
- 每个解析器的脚本和 Dockerfile 位于仓库根目录的 `docker/<parser>/` 中；SQL 解析器使用 `node-sql-parser`。

**可执行的** SQLite（`sqlite`）仍在目录中；见 [SQLite](/languages/sqlite)。

## 重新启用一个解析器需要什么 {#o-que-é-preciso-para-reativar-um-parser}

每个解析器需要两样东西：一个专用镜像，以及 `languages.yaml` 中的一个新条目。

### 1. 专用镜像 {#1-imagem-própria}

镜像构建通过 go-containerregistry 从 registry 下载 OCI 镜像；不使用本地 Docker。因此解析器的镜像需要：

- 在 `/usr/local/bin/node` 包含 Node.js；
- 在执行器预期的路径上包含解析器脚本（例如 `/usr/local/bin/mysql-run`）；
- 对于 SQL 解析器，在 `/opt/sql-parser/node_modules` 中包含 `node-sql-parser`；
- 发布到运行构建的 host 能访问的 registry。

`docker/<parser>/` 中的 Dockerfile 描述了这些内容，可以作为生成和发布镜像的起点。

### 2. languages.yaml 中的条目 {#2-entrada-no-languagesyaml}

目录嵌入在二进制中，位于 `internal/adapters/catalog/languages.yaml`。新条目的格式如下：

```yaml
  - name: mysql-parser
    image: <registry>/<imagem do parser>:<tag>
    cpus: 1
    memory_mib: 256
    warmup: ""
```

- `name` 必须与 guest 执行器中使用的名字完全一致。
- `cpus` 和 `memory_mib` 是防止滥用的上限；请通过实测解析器来选定。
- 如果镜像已经包含一切，`warmup` 可以留空。

之后：

```bash
task build
task image:build -- --stack mysql-parser
rc run --stack mysql-parser --code 'SELECT 1;'
```

### 3. 测试 {#3-teste}

每个重新启用的解析器都必须加入端到端测试，带一条有效查询和一条无效查询，以证明校验能拒绝应当拒绝的内容。

## 未验证 {#não-verificado}

截至本文档编写时，还没有任何解析器在 libkrun 上重建。上述步骤描述的是当前代码的要求；尚未端到端执行过。
