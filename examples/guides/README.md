# Guias, testados numa VM

Cada pasta é um guia de uso do `rc`, com um `run.sh` que prova o que o guia diz numa VM de verdade e sai com o
número de falhas. O `README.md` de cada pasta explica o fluxo; o topo de cada `run.sh` diz o que ele confere.

```bash
task build
task guides                        # todas as pastas
task guides DIRS="pack teardown"   # algumas
```

`lib.sh` reúne o que todo `run.sh` precisa, uma vez só:
- `rc machine ls` tem 1 linha de cabeçalho e o estado na 3ª coluna;
- o processo de uma VM é `rc vmm --spec <dir>/spec.json`;
- o diretório da máquina sai de `rc machine data-dir`;
- a lista vazia diz `No machines found`.

## Guias

| Guia | O que prova |
|---|---|
| [install](install/) | boot de verdade: marcador do guest e kernel diferente do host |
| [throwaway-machine](throwaway-machine/) | repo `:ro`, saída gravável, sem rede; rede um host por vez; `--block-private`, loopback do host |
| [pack](pack/) | imagem e máquina num artefato; o marcador vem junto com a origem apagada; cache reaproveitado |
| [teardown](teardown/) | parar, apagar, provar limpo; Ctrl-C e `kill -9` no CLI não deixam VM viva |
| [credentials](credentials/) | o valor nunca entra na VM (busca com contraprova); interceptação só no host da binding; 403 na URL |
| [dev-env](dev-env/) | persistência de pacote, `$HOME` e disco; `/tmp` zera; init uma vez, como root; `user` e `--user` |
| [docker-in-machine](docker-in-machine/) | Docker num ext4 de verdade, contêiner aninhado, `docker.sock` no host, dados que sobrevivem |
| [coding-agent](coding-agent/) | claude e opencode instalados pelo init; git no repo montado; agente SSH do host sem a chave na VM; chave da API como segredo |
| [branch-and-checkpoint](branch-and-checkpoint/) | checkpoint de disco com retenção e trava, restauração de qualquer geração, pause/resume, filhas de uma fonte aquecida, checkpoint exportado como pack |
| [gpu-cuda](gpu-cuda/) | não há CUDA: `doctor`, CLI, `runner.code` e API dizem não, com o motivo, sem criar nada |
| [local-api](local-api/) | `rc serve` com corpos JSON de ciclo de vida; exitCode no corpo; SSE uma linha por evento; campo desconhecido recusado |
| [reference](reference/) | uma etapa por tema com prova (arquivo `runner.code`, redes nomeadas, nested, limitações, sockets, display, posse de arquivo, S3); os temas fora do escopo, com o motivo |

## O que estes guias trouxeram ao rc

Coisas que os guias usam e o rc não tinha, ou tinha errado. Cada uma tem teste de unidade, prova no binário real
(e2e numa VM, ou o `run.sh` da pasta; o `--cuda` não liga VM) e defeito plantado de propósito que o teste pega:

| O que | Por que |
|---|---|
| `machine run` preso ao CLI | um `kill -9` no `rc` deixava a VM rodando, com o código não confiável sem prazo (trap do teardown) |
| `/dev/stdin`, `/dev/fd` no guest | `grep -f /dev/stdin` falhava, e a busca de contenção do guia credentials dava 0 por estar cega |
| vsock sem meio-fechamento | cliente que fazia `SHUT_WR` perdia a resposta (porta publicada e `docker.sock`) |
| `--user` / `user` | dev-env: workload, exec e shell como o usuário, init como root, `uid:gid` para volume do host |
| volume de disco (`-v /var/lib/docker`) | o overlay2 do Docker precisa de ext4 de verdade, persistente |
| `docker_socket` | o `docker` do host fala com o daemon da VM sem porta TCP aberta |
| `--secret-env`, `--secret-file`, `[secrets]` | o valor só em memória no guest; com o nome de uma credencial, alimenta a binding |
| `--ssh-agent`, `[auth]` | git e ssh com as chaves do host, que não entram na VM |
| `machine list`, `data-dir`, `prune`, `pack prune`, `delete --cascade` | os scripts de limpeza dos guias |
| API: aliases de corpo (`network`, `memoryMb`, `cmd`), `files/*path`, `code` no topo, SSE por linha | um cliente de ciclo de vida escrito com esses nomes roda contra o rc sem adaptar o corpo |
| API: campo desconhecido recusado | ignorado calado, um `allowedHost` errado criava a máquina com a saída aberta |
| `start` numa máquina pausada | dizia `is already running`; agora `is paused; resume it ... or stop it first` |
| `--cuda` e `rc doctor` | `unknown flag` não dizia que o recurso não existe nem qual é a alternativa (`--gpu`, Vulkan) |
| `ping` (ICMP) pelo gateway | não havia ICMP: o ping falhava com a rede ligada; agora sai pela mesma política do TCP (negado vai para o log), e na rede nomeada responde quem está ligado nela |
| `--nested` | não existia; onde o host não aninha, a recusa vem na hora, com o motivo |
| vmm que morre no boot | o start esperava 30 s e dizia `did not become ready`; agora termina na hora com a última linha do log do vmm (deste start, não de um anterior) |
| `--expose-socket`, `--mount-socket` | só existia o `docker.sock`; agora qualquer socket, nas duas direções |
| faixa de portas `A-B:C-D` | recusada; agora uma porta por número, até 64 |
| `stop_signal` | só 7 nomes; agora a tabela do Linux inteira e `SIGRTMIN+N` (a numeração da glibc) |
| `gpu_vram` sem `gpu` | recusado; agora ignorado, para o mesmo arquivo ligar e desligar a GPU sem apagar a linha |
| `-v HOST:GUEST:staged`, `rc machine sync` | não existia; a cópia mora no disco da máquina e volta ao host no sync, no stop gracioso e no fim do `run` (a filha de um branch não devolve) |
