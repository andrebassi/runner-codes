---
title: 'Guest runner'
description: 'O binário estático que roda dentro da VM e executa o código do job'
---

## O que é

O `runner-guest` é o processo que executa o código dentro da microVM. Ele é compilado estático para Linux e injetado em toda imagem como `/opt/runner/guest`. No host, fica em `bin/runner.code-guest-linux-<arch>`.

**Por que estático e separado do host:** o host carrega a libkrun por purego, o que força link dinâmico contra a glibc no Linux. As imagens das linguagens são Alpine (musl); um binário dinâmico ali falha com `/opt/runner/guest: not found`.

## Como ele sobe

O primeiro processo da VM é `/opt/runner/start.sh`:

```sh
mount -t tmpfs -o size=256m,mode=1777 tmpfs /tmp
mkdir -p /tmp/home
export HOME=/tmp/home
if [ -d /opt/runner/gocache ]; then
  cp -a /opt/runner/gocache /tmp/gocache
  export GOCACHE=/tmp/gocache
fi
exec /opt/runner/guest --port 5000
```

O guest escuta na porta vsock 5000. A libkrun liga essa porta a um socket Unix no host.

## O que ele faz com um job

1. Lê um frame (4 bytes big-endian de tamanho + JSON) com o [job](/api-reference/job-request).
2. Confere a linguagem e o tamanho do stdin.
3. Cria um diretório temporário em `/tmp` e grava os arquivos:
   - com `files`, grava cada um pelo nome; o **primeiro** é o arquivo principal;
   - com só `code`, grava `script.<ext>` (Java: `Main.java`).
4. Executa conforme o tipo da linguagem:
   - **interpretada** (Python, Node.js, Ruby, PHP, Perl, Bash, sh, AWK, jq): `<interpretador> <arquivo>`;
   - **compilada** (C, C++, Rust): compila para um binário e executa o binário;
   - **JVM** (Java): `javac` e depois `java Main`;
   - **Go:** `go run`;
   - **SQLite:** o código vai pelo stdin do `sqlite3 :memory:`.
5. Aplica o timeout do job (padrão 10 s).
6. Responde com um frame de [resultado](/api-reference/job-response).

Falha de compilação devolve `exception: "Compilation failed"` com o erro do compilador em `stderr`. Estouro de tempo devolve `exception: "Timeout"`.

## Isolamento entre jobs

O guest não precisa limpar nada entre jobs: cada VM atende um único job e é destruída. O tmpfs some com ela.

## Linguagens que o guest conhece

O executor do guest (`internal/guest/executor.go`) tem entradas para mais linguagens do que o catálogo publica. Só as 15 do [`languages.yaml`](/languages/overview) têm imagem e são aceitas pela API.
