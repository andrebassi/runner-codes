# local-api

A API HTTP local do rc (`rc serve`), exercitada de ponta a ponta: ciclo de vida de uma máquina, exec com código de
saída, streaming e arquivos.

| Tema | No rc |
|---|---|
| fluxo | `run.sh`: `rc serve` de verdade, ciclo de vida com corpos JSON nos nomes aceitos (inclusive os aliases) |
| subir o servidor | `rc serve --listen 127.0.0.1:<porta>` (só TCP de loopback; não há socket Unix) |
| ciclo de vida | `run.sh`: create, start, exec (exitCode 0 e 3), streaming, arquivo ida e volta, delete, `NOT_FOUND` |
| preflight | `rc doctor` |
| limpeza | `cleanup_prefix apidoc-` (`lib.sh`) no `trap` do `run.sh` |
| campos do corpo | campo desconhecido recusado (400); aliases `network: true`, `memoryMb`, `cmd`, `allowedHosts`, `allowedCidrs`; `exitCode` lido do corpo |
| armadilhas | `files/%2F...` e `files/<caminho>` aceitos; `code` no topo do erro |
| Windows | fora do escopo: o rc não roda no Windows |

Comportamento a saber:

- o estado depois do create é `stopped`;
- o erro vem em `{"error":{"code","message"},"code":...}`;
- não há `serve openapi`: os campos estão em `docs/docusaurus/docs/api-reference/overview.md`;
- a API não tem autenticação, por isso escuta só em loopback por padrão.

O streaming (`exec/stream`) é um evento SSE por linha, com o texto cru (`event: stdout` / `data: line1`).
