# throwaway-machine

Código não confiável numa VM descartável: o repo só de leitura, uma pasta de saída e a rede fechada (ou aberta um
host por vez). A prova tem duas metades: de dentro (não escreve no repo, não sai para a rede) e de fora (o artefato
saiu, o repo está intacto).

| Tema | No rc |
|---|---|
| fluxo e prova | `run.sh`: as duas metades da prova, rota offline e rede um host por vez |
| macOS | `--image -` (docker save) existe; o `machine update` mexe só na política de saída (não há `--no-net`/`--volume` ali) |
| armadilhas | Ctrl-C leva a VM junto (provado no teardown); recusa de rede aparece como erro de DNS (`bad address`) |
| Windows | fora do escopo |
| imagem pré-baixada | desnecessária: a imagem é baixada no host e fica em cache; a VM nunca usa rede para isso |
| preflight | o rc limita 16 montagens por máquina (`MaxMounts`) |
| limpeza | `run.sh` confere que nenhuma VM efêmera sobrou |

O loopback do host abre com `--allow-cidr 127.0.0.1/32`; não há opção de sub-rede do guest.
