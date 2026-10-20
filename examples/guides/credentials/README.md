# credentials

Uma credencial que a máquina usa e nunca lê: o guest só vê um placeholder, e o gateway do host troca o placeholder
pelo valor na requisição ao host da binding.

| Tema | No rc |
|---|---|
| fluxo | `run.sh`: binding, contenção provada e uso real (o eco recebe o valor) |
| plataforma de rede | o rc é sempre virtio-net; TSI é recusado |
| armadilhas | 403 na URL provado; os textos são `runner.code credentials: ...`; não há opção de sub-rede do guest |
| criar a máquina com credencial | `run.sh` |
| prova de contenção | `run.sh`, com contraprova da busca (sem `/dev/stdin` no guest, a busca dava 0 por estar cega) |
| preflight | `rc machine create --help` lista `--credential` |

Placeholder `RC_PLACEHOLDER_*`; CA "runner.code credentials CA"; o valor vem do ambiente do start ou de um
`--secret-file`/`[secrets]` de mesmo nome (lido a cada requisição).
