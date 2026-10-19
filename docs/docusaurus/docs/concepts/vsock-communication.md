---
title: 'Comunicação vsock'
description: 'Como o host fala com o guest: socket Unix, porta vsock 5000 e frames de tamanho + JSON'
---

## O canal

A VM não tem rede. O único canal entre host e guest é um vsock:

- o guest escuta na porta vsock **5000**;
- no host, `krun_add_vsock_port2` com `listen=true` liga essa porta a um **socket Unix** num diretório de trabalho do cache;
- o host conecta nesse socket Unix como em qualquer socket local.

Não existe handshake textual: a conexão no socket Unix já chega ao guest.

## Formato da mensagem

![Message framing](/img/message-framing.svg)

Toda mensagem, nos dois sentidos, é:

```
+------------------------+----------------------+
| tamanho (4 bytes, BE)  | payload JSON (UTF-8) |
+------------------------+----------------------+
```

- O tamanho é um inteiro sem sinal de 4 bytes, big-endian, e conta só o payload.
- O frame máximo é **64 MiB**; o host recusa ler um frame acima disso.

O formato é o mesmo de antes da migração para a libkrun.

## Sequência

```
host                                guest
 │ connect(socket Unix) ─────────────▶ accept (vsock 5000)
 │ [tamanho][Job JSON] ──────────────▶ executa
 │ ◀────────────── [tamanho][Result JSON]
 │ fecha; a VM é destruída
```

## Prontidão

A libkrun cria o socket Unix antes de o guest começar a escutar, então uma conexão bem-sucedida não prova que o guest está pronto. A espera de prontidão manda um job `sh true` a cada 1 ms até receber uma resposta. Só depois disso a VM recebe jobs reais.

## Payloads

- Job: `{trace_id, lang, code, files[{name, content}], stdin, timeout}` — veja [Job request](/api-reference/job-request).
- Resultado: `{trace_id, status, stdout, stderr, exception, executionTime, exit_code, error}` — veja [Job response](/api-reference/job-response).

Detalhes byte a byte em [Protocolo vsock](/api-reference/vsock-protocol).
