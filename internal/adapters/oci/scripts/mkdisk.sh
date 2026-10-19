#!/bin/sh
# Roda DENTRO da VM de build. Cria o modelo vazio do disco de uma máquina persistente:
# arquivo esparso de $2 GiB formatado em ext4. Esparso: no host o arquivo só ocupa o que o
# ext4 escreveu (metadados), e cada máquina recebe um clone (clonefile/FICLONE) dele.
#   $1 ext4 de saída   $2 tamanho em GiB
set -eu
out=$1; gib=$2
rm -f "$out"
truncate -s "${gib}G" "$out"
# lazy_*_init: não zera tabela de inode nem journal agora (o kernel faz depois, em segundo
# plano); sem isso o mkfs escreveria GBs e o arquivo deixaria de ser esparso.
mkfs.ext4 -q -F -m 0 -E lazy_itable_init=1,lazy_journal_init=1 -L rc-machine "$out"
echo "RESULT ok gib=$gib"
