# reference

Os temas de referência do rc. Cada um tem um destino: uma etapa do [`run.sh`](run.sh) desta pasta, uma pasta
irmã, ou o motivo de estar fora do escopo.

```bash
task build
bash examples/guides/reference/run.sh                         # todas as etapas
bash examples/guides/reference/run.sh runner-code limitations # algumas
```

## Com prova numa VM (etapas do `run.sh`)

| Tema | Etapa | O que prova |
|---|---|---|
| arquivo da máquina (`runner.code`) | `runner-code` | as chaves da referência aceitas: faixa de portas `A-B:A-B` (a do meio responde no host), `stop_signal = "SIGRTMIN+3"` (37), `gpu_vram` sem `gpu` ignorado, `[dev].env` somando e `[dev].workdir`, chave desconhecida recusada com o nome |
| redes nomeadas | `named-networks` | membros se alcançam pelo IP da rede, inclusive `ping`; o nome `<máquina>.<rede>.internal` resolve; outra rede não resolve nem alcança |
| virtualização aninhada | `nested-virtualization` | `--nested` liga onde o host aninha; onde não aninha (este Mac é M1), `rc doctor` diz, e o start recusa **na hora** com o motivo; o `runner.code` não liga nested |
| limitações | `limitations` | `eth0` sempre (não há TSI); `ping` funciona; volume de arquivo único recusado; `-v DIR:/workspace` vence o disco |
| modelo de segurança | `security-model` | `--expose-socket` (o host fala com o servidor do guest) e `--mount-socket` (o guest fala com o do host); as flags que o rc não tem são recusadas sem criar nada |
| tela no navegador | `browser-display` | `--gpu --display WxH` serve o visualizador em `127.0.0.1:<porta>` e o guest tem `/dev/dri/card0` |
| dono de arquivo no pack | `pack-layer-ownership` | dono de arquivo preservado: `postgres:16-alpine` mantém `70:70` na máquina e no pack, e o alpine mantém `/etc/shadow` `0:42` |
| volumes | `volumes` | bucket S3 (rclone serve s3 local) montado no guest com as `AWS_*` do `-e`; `-v HOST:GUEST:staged`: cópia no ext4 da máquina, o host só muda no `rc machine sync` (espelho: o que sumiu da cópia some do host), no stop gracioso e no fim do `run`, e o script novo chega executável |

## Em outra pasta

| Tema | Onde |
|---|---|
| pause/resume, branch, checkpoints | [branch-and-checkpoint](../branch-and-checkpoint/): o checkpoint do rc é de disco (a libkrun não tem snapshot de RAM) |
| substituição de credencial | [credentials](../credentials/) |
| GPU | `--gpu` é Vulkan (virtio-gpu Venus) e roda no macOS; a stack `llama` e o e2e de GPU usam; CUDA: [gpu-cuda](../gpu-cuda/) |
| instalação | [install](../install/): o rc instala pelo Nix do repositório, `task krun:install` e `task build` |
| desenvolvimento do rc | `CLAUDE.md` e o `Taskfile.yaml` do repositório |

## Fora do escopo

| Tema | Por quê |
|---|---|
| decisor externo de saída | o rc decide a saída por política (hosts, padrões, CIDRs, `block_private`) e intercepta credencial por SNI; não há decisor externo por fluxo |
| vídeo por hardware | o visualizador do rc manda quadros por long-poll; não há H.264 nem `ffmpeg` |
| checkpoint de RAM entre SOs | não há checkpoint de RAM; o que atravessa é o disco (`pack create --from-vm`), e o Linux não foi medido |
| shim de Kubernetes | não há shim do containerd |
| telemetria | o rc não grava nem envia telemetria |

Recusadas com mensagem, sem criar nada (conferido no `run.sh`): `--allow-system-mounts`, `--use-host-proxy`,
`--trust-host-certs`, `--cuda`.

Nos volumes do host, o virtio-fs da libkrun no macOS cria o arquivo novo `0600` (diretório `0700`) e guarda o
modo do guest num xattr (`user.containers.override_stat`), medido. O `rc machine sync` aplica esse modo no host;
um `-v` comum não passa por ele, e o arquivo que o guest cria ali fica `0600` no host.
