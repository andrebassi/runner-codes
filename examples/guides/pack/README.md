# pack

Imagem ou máquina provisionada num artefato que roda em outro host sem pull. A prova é ler de volta um marcador
gravado na máquina de origem (um pack que perdeu o disco ainda liga e sai 0), e o segundo run reaproveitar o cache.

| Tema | No rc |
|---|---|
| fluxo | `run.sh`: pack de imagem (stub + sidecar), pack de máquina com marcador lido com a origem apagada, cache reaproveitado |
| armadilhas | `--output` nomeia o stub: provado; `rc pack inspect` mostra o conteúdo; `machine prune` e `pack prune` existem |
| Windows | fora do escopo |
| empacotar e conferir | `run.sh` |
| limpeza | `rc pack prune` (mantém as 5 mais recentes; `--all` só as em uso) |

O payload é `<stub>.rcmachine`; a libkrun é a do host de destino (o pack não embute o runtime); não há `pack push`
para registry.
