---
title: 'Database parsers'
description: 'How to bring back the SQL and NoSQL syntax validators, which are currently not in the catalog'
---

## Current state

The database syntax validators (`mysql-parser`, `postgresql-parser`, `mariadb-parser`, `sqlite-parser`, `mongodb-parser` and `redis-parser`) **are not in the catalog**. The API rejects these languages with E006.

They come from an earlier version of the project and were not rebuilt on top of libkrun. What exists in the repository:

- the guest executor (`internal/guest/executor.go`) still has the entries, which call `/usr/local/bin/node` with a validation script (`/usr/local/bin/mysql-run`, `psql-run`, `mariadb-run`, `sqlite-run`, `mongodb-run`, `redis-run`), and, for the SQL parsers, `NODE_PATH=/opt/sql-parser/node_modules`;
- the scripts and Dockerfiles for each parser are in `docker/<parser>/` at the repository root; the SQL ones use `node-sql-parser`.

The **executable** SQLite (`sqlite`) is still in the catalog; see [SQLite](/languages/sqlite).

## What it takes to re-enable a parser

Each parser needs two things: its own image and a new entry in `languages.yaml`.

### 1. Its own image

The image build pulls the OCI image from a registry with go-containerregistry; it does not use local Docker. Therefore the parser image must:

- contain Node.js at `/usr/local/bin/node`;
- contain the parser script at the path the executor expects (for example `/usr/local/bin/mysql-run`);
- for the SQL parsers, contain `node-sql-parser` in `/opt/sql-parser/node_modules`;
- be published to a registry reachable by the host running the build.

The Dockerfile in `docker/<parser>/` describes that content and can serve as a starting point for building and publishing the image.

### 2. Entry in languages.yaml

The catalog is embedded in the binary, in `internal/adapters/catalog/languages.yaml`. A new entry has this format:

```yaml
  - name: mysql-parser
    image: <registry>/<imagem do parser>:<tag>
    cpus: 1
    memory_mib: 256
    warmup: ""
```

- `name` must be exactly the name used in the guest executor.
- `cpus` and `memory_mib` are a ceiling against abuse; choose them by measuring the parser.
- `warmup` can stay empty if the image already ships everything.

Then:

```bash
task build
task image:build -- --stack mysql-parser
rc run --stack mysql-parser --code 'SELECT 1;'
```

### 3. Test

Every re-enabled parser must join the end-to-end test with one valid query and one invalid query, to prove the validation rejects what it should.

## Not verified

No parser had been rebuilt on top of libkrun as of the date of this documentation. The steps above describe what the current code requires; they have not been run end to end.
