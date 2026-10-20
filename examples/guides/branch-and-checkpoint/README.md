# branch-and-checkpoint

A libkrun não tem snapshot de memória, então o checkpoint do rc é de **disco**: `rc machine branch` numa
máquina ligada dá `sync` no guest, congela a VM, clona o ext4 (copy-on-write) e retoma. O checkpoint é uma
máquina parada; restaurar é fazer `branch` dele, o que deixa o checkpoint intacto.

| Tema | No rc |
|---|---|
| preflight | `rc doctor`; não há opção de máquina "capturável" nem de store a conferir |
| fonte com estado | `run.sh` passo 2: contador em `/tmp` (RAM) e marcador em `/root` (disco) |
| uma captura | [`checkpoint.sh`](checkpoint.sh): uma captura, retenção `--keep`, trava por fonte (exit 3, `result=skipped`), trava de processo morto retomada |
| capturas em série | `run.sh` passo 3: quatro capturas com `--keep 2` |
| restaurar | `rc machine branch --from <checkpoint> --name <nova>` + `start` |
| ponto de partida | `run.sh` passo 6: o ponto de partida é um checkpoint; as filhas saem dele |
| limpeza | `cleanup_prefix` (`lib.sh`); não há cache de restauração para limpar |
| agendamento | `checkpoint.sh` serve igual em cron, systemd ou launchd (abaixo) |
| armadilhas | as que valem aqui estão no `run.sh` (abaixo) |
| plataformas | macOS arm64 testado; Linux não medido; Windows fora |

## Como o checkpoint do rc se comporta

| Tema | rc |
|---|---|
| restauração | **só disco**: os processos recomeçam (o `run.sh` prova que o contador recomeçou) |
| levar para outro host | um checkpoint é uma máquina parada: `rc pack create --from-vm <ck> --output ./ck` vira artefato que roda em outro host (testado) |
| o que dá para capturar | toda máquina ligada; não há opção a ligar no start |
| gerações | cada geração é um checkpoint (uma máquina); `--keep` é o único limite |
| `stop` numa máquina pausada | o `stop` retoma e para limpo (o `run.sh` prova que a escrita de antes da pausa ficou); o `start` recusa com `is paused; resume it (rc machine resume) or stop it first` |
| `pause` | congela o processo da VM no lugar (`SIGSTOP`): memória e conexões ficam como estão, e não sobrevive a um reboot do host |

Pause e resume: o contador continua de onde parou e não anda enquanto pausado.

## Num timer

```bash
# cron, a cada 10 minutos, guardando 6
*/10 * * * * RC=/caminho/bin/rc /caminho/checkpoint.sh --name dev --keep 6 >>/var/log/rc-ckpt.log 2>&1
```

A saída é `chave=valor`; a última linha é `result=captured`, `result=skipped` ou `result=failed`, com exit 0, 3
ou 1. O intervalo vem do `took_s=`, e não do tempo em que a fonte fica congelada (milissegundos).

## Rodar

```bash
task build
bash examples/guides/branch-and-checkpoint/run.sh   # 43 conferências
```
