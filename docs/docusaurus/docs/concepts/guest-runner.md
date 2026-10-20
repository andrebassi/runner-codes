---
title: 'Guest runner'
description: 'O binário estático que roda dentro da VM e executa o código do job'
---

## O que é

O `runner-guest` é o processo que executa o código dentro da microVM. Ele é compilado estático para Linux e injetado em toda imagem como `/opt/runner/guest`. No host, fica em `bin/rc-guest-linux-<arch>`.

**Por que estático e separado do host:** o host carrega a libkrun por purego, o que força link dinâmico contra a glibc no Linux. As imagens das linguagens são Alpine (musl); um binário dinâmico ali falha com `/opt/runner/guest: not found`.

## Como ele sobe

O primeiro processo de uma VM de job é `/opt/runner/start.sh`:

```sh
mount -t tmpfs -o size=256m,mode=1777 tmpfs /tmp
# /dev/stdin, /dev/stdout, /dev/stderr e /dev/fd como links para /proc/self/fd
...
# ambiente da config OCI da imagem (PATH, JAVA_HOME...), uma variável por linha
while IFS= read -r l; do [ -n "$l" ] && export "$l"; done < /opt/runner/env
mkdir -p /tmp/home
export HOME=/tmp/home
# cache de build do Go aquecido na imagem, copiado para o caminho que o executor força
if [ -d /opt/runner/gocache ]; then
  cp -a /opt/runner/gocache /tmp/go-cache
fi
exec /opt/runner/guest --port 5000
```

(Resumo do `internal/adapters/oci/scripts/start.sh`.) Numa máquina persistente, o primeiro processo é o
`/opt/runner/machine.sh`, que monta o disco e o overlay e roda o mesmo guest num `chroot`.

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

## Outras operações

Além de rodar código, o guest atende as operações das máquinas, todas pelo mesmo protocolo: `exec` (argv sem shell
implícito, com env, diretório e usuário), `put`/`get` (cópia de arquivo em pedaços de 1 MiB), `pty` (terminal
interativo), `workload` (o processo que a máquina mantém rodando), `shutdown` (sinal ao workload, `sync` e saída),
`dial` (porta publicada, discando o `127.0.0.1` do guest), `set-env` (segredos só em memória), `ssh-agent` (socket do
agente SSH do host) e `mount-s3` (volume `s3://` por FUSE). Detalhes em [Protocolo vsock](/api-reference/vsock-protocol).

## Isolamento entre jobs

O guest não precisa limpar nada entre jobs: cada VM atende um único job e é destruída. O tmpfs some com ela.

## Linguagens que o guest conhece

O executor do guest (`internal/guest/executor.go`) tem entradas para mais linguagens do que o catálogo publica. Só as 15 do [`languages.yaml`](/languages/overview) têm imagem e são aceitas pela API.
