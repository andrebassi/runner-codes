---
title: 'Protocolo vsock'
description: 'Frame de tamanho + JSON entre host e guest, byte a byte'
---

## Transporte

| Item | Valor |
|------|-------|
| lado do guest | escuta na porta vsock 5000 |
| lado do host | socket Unix, ligado à porta 5000 por `krun_add_vsock_port2` com `listen=true` |
| handshake | nenhum: a conexão no socket Unix chega direto ao guest |
| mensagens por conexão | um job e um resultado |

## Frame

![Message framing](/img/message-framing.svg)

```
offset 0      4                          4 + N
       ┌──────┬──────────────────────────┐
       │  N   │  JSON (N bytes, UTF-8)   │
       └──────┴──────────────────────────┘
       uint32 big-endian
```

- `N` conta apenas o payload.
- O frame máximo é 64 MiB. O leitor do host recusa um tamanho acima disso antes de alocar memória: sem o limite, um prefixo corrompido (até 4 GiB) faria o leitor alocar tudo antes de ler o corpo.
- O host envia prefixo e corpo numa única escrita, para o guest nunca ler um prefixo sem o corpo correspondente.

## Exemplo

Job `{"trace_id":"t1","lang":"python","code":"print(1)","timeout":10}` tem 64 bytes de JSON:

```
00 00 00 40  7b 22 74 72 61 63 65 5f 69 64 22 3a ...
└─ 64 ─────┘ └─ {"trace_id": ...
```

## Payloads

- Host → guest: [Job request](/api-reference/job-request).
- Guest → host: [Job response](/api-reference/job-response).

## Streaming

Com `"stream": true` no job, o guest responde com vários frames em vez de um, todos no mesmo formato
(tamanho + JSON), em `internal/protocol`:

```json
{"type":"stdout","data":"early\n"}
{"type":"stderr","data":"warn\n"}
{"type":"result","result":{"status":"success","stdout":"early\n","stderr":"warn\n","exception":null,"executionTime":12}}
```

- Um frame por escrita do processo no pipe (até 32 KiB no Linux), na ordem em que o guest leu.
- O `result` é sempre o último e traz o resultado completo, igual ao do modo sem streaming.
- Sem `stream`, o fio é idêntico ao anterior: um frame de resultado. A prova de prontidão (`sh true`) usa esse modo.
- Se o host recebe um frame sem `type`, o guest da imagem é anterior ao streaming: `rc image build` refaz a imagem.

## Operações

O campo `op` do job escolhe a operação. Vazio é o job de código de sempre, para o fio dos jobs efêmeros não mudar; as
outras servem às máquinas persistentes (`internal/protocol`).

| `op` | Campos usados | O que faz |
|------|---------------|-----------|
| (vazio) | `lang`, `code`, `files`, `stdin`, `timeout`, `stream` | roda código de uma linguagem |
| `exec` | `argv`, `env`, `cwd`, `user`, `timeout`, `stream` | roda um programa sem shell implícito |
| `put` / `get` | `path`, `offset`, `data`, `final`, `mode` | cópia de arquivo em pedaços de 1 MiB (até 4 GiB por arquivo) |
| `pty` | `argv`, `rows`, `cols`, `user` | terminal interativo: a conexão vira um canal de eventos `input`/`resize` (host) e `output`/`exit` (guest) |
| `workload` | `argv`, `env`, `cwd`, `user`, `stop_on_exit` | liga o processo que a máquina mantém rodando |
| `shutdown` | `signal`, `timeout` | manda o sinal ao workload, espera, faz `sync` e desliga |
| `dial` | `port` | depois do resultado de sucesso, a conexão vira um tubo de bytes até `127.0.0.1:port` dentro do guest (porta publicada) |
| `set-env` | `env` | guarda os segredos na memória do guest e junta a todo comando seguinte; nunca vão para o disco |
| `ssh-agent` | `path`, `port` | abre no guest um socket Unix que repassa cada conexão à porta vsock do host (agente SSH) |
| `mount-s3` | `path`, `s3`, `env` | monta um bucket por FUSE no guest, depois de conferir que ele responde |

## Outras portas vsock

| Porta | Sentido | Uso |
|-------|---------|-----|
| 5000 | host → guest | todas as operações acima |
| 5001 | guest → host | agente SSH (`--ssh-agent`) |
| 5100 + i | guest → host | o i-ésimo `--mount-socket` |
