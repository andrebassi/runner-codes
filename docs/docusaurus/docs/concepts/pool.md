---
title: 'Pool de VMs'
description: 'VMs pré-bootadas por linguagem, usadas uma única vez e repostas em background'
---

## Para que serve

Bootar uma VM e esperar o guest responder custa da ordem de 260 ms (p50 medido). Com o pool, esse custo sai do caminho da requisição: a API pega uma VM que já está pronta.

```bash
rc serve --listen 127.0.0.1:8080 --pool-size 2
```

## Regras

- **N VMs por linguagem.** `--pool-size` (padrão 2) define quantas VMs ficam prontas para cada linguagem que tem imagem construída. Linguagem sem imagem não entra no pool.
- **Uso único.** Cada VM atende um job e é destruída. Nenhum estado — arquivo em `/tmp`, processo, variável — passa de um job para o seguinte.
- **Reposição em background.** Quando uma VM sai do pool, outra é bootada em seguida, com no máximo 2 boots simultâneos por linguagem.
- **Nunca recusa.** Se o pool daquela linguagem estiver vazio, o job sobe uma VM na hora (boot frio) e espera por ela.
- **Encerramento.** SIGINT ou SIGTERM fecham o servidor e matam todas as VMs do pool.

## O que é "pronta"

Conectar no socket Unix não basta: a libkrun cria o socket antes de o guest começar a escutar. A espera de prontidão manda um job `sh true` a cada 1 ms até receber resposta. Só então a VM entra no pool.

Para Go, a cópia do `GOCACHE` aquecido para o tmpfs também acontece no boot, antes da VM entrar no pool, e não pesa no job.

## Custo em memória

Uma VM parada mediu 44 MB de footprint. O RSS reportado (172 MB) inclui páginas limpas compartilhadas. A memória da libkrun é elástica — o host só paga o que o guest toca —, então o teto de RAM da linguagem no catálogo não é reservado por VM do pool.

## Números

| Caminho | Medido |
|---------|--------|
| API com o pool cheio | 26–43 ms de requisição HTTP completa |
| CLI, boot frio | ~240 ms no total |

macOS 27.0.1, Apple Silicon, 19/10/2026. Linux ainda não medido.

## A CLI não usa pool

`rc run` sempre sobe uma VM nova (pool de tamanho zero) e a destrói no fim.
