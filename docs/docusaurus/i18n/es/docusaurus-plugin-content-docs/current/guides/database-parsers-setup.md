---
title: 'Parsers de bases de datos'
description: 'Cómo recuperar los validadores de sintaxis SQL y NoSQL, que hoy no están en el catálogo'
---

## Estado actual

Los validadores de sintaxis de bases de datos (`mysql-parser`, `postgresql-parser`, `mariadb-parser`, `sqlite-parser`, `mongodb-parser` y `redis-parser`) **no están en el catálogo**. La API rechaza esos lenguajes con E006.

Provienen de una versión anterior del proyecto y no se reconstruyeron sobre la libkrun. Lo que existe en el repositorio:

- el executor del guest (`internal/guest/executor.go`) aún tiene las entradas, que llaman a `/usr/local/bin/node` con un script de validación (`/usr/local/bin/mysql-run`, `psql-run`, `mariadb-run`, `sqlite-run`, `mongodb-run`, `redis-run`) y, en los parsers SQL, `NODE_PATH=/opt/sql-parser/node_modules`;
- los scripts y Dockerfiles de cada parser están en `docker/<parser>/` en la raíz del repositorio; los SQL usan `node-sql-parser`.

SQLite **ejecutable** (`sqlite`) sigue en el catálogo; consulte [SQLite](/languages/sqlite).

## Qué se necesita para reactivar un parser

Cada parser necesita dos cosas: una imagen propia y una entrada nueva en `languages.yaml`.

### 1. Imagen propia

El build de imágenes descarga la imagen OCI de un registry con go-containerregistry; no usa Docker local. Por lo tanto, la imagen del parser debe:

- contener Node.js en `/usr/local/bin/node`;
- contener el script del parser en la ruta que espera el executor (por ejemplo `/usr/local/bin/mysql-run`);
- para los parsers SQL, contener `node-sql-parser` en `/opt/sql-parser/node_modules`;
- estar publicada en un registry accesible desde el host que ejecuta el build.

El Dockerfile en `docker/<parser>/` describe ese contenido y puede servir como punto de partida para generar y publicar la imagen.

### 2. Entrada en languages.yaml

El catálogo está embebido en el binario, en `internal/adapters/catalog/languages.yaml`. Una entrada nueva tiene el formato:

```yaml
  - name: mysql-parser
    image: <registry>/<imagem do parser>:<tag>
    cpus: 1
    memory_mib: 256
    warmup: ""
```

- `name` debe ser exactamente el nombre usado en el executor del guest.
- `cpus` y `memory_mib` son un límite contra abusos; elíjalos midiendo el parser.
- `warmup` puede quedar vacío si la imagen ya trae todo.

Después:

```bash
task build
task image:build -- --stack mysql-parser
rc run --stack mysql-parser --code 'SELECT 1;'
```

### 3. Prueba

Cada parser reactivado debe entrar en la prueba de extremo a extremo con una consulta válida y una inválida, para demostrar que la validación rechaza lo que debe.

## No verificado

Ningún parser se había reconstruido sobre la libkrun a la fecha de esta documentación. Los pasos anteriores describen lo que exige el código actual; no se ejecutaron de extremo a extremo.
