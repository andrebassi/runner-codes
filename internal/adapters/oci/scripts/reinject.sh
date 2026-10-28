#!/bin/sh
# Roda DENTRO da VM de build (Alpine, root). Troca, num ext4 já pronto, só os arquivos que o host injeta em
# /opt/runner (guest, start.sh, shell.sh, machine.sh, env) — o resto da imagem (camadas OCI e warmup) é o mesmo.
# É o caminho barato de um rc novo: o guest muda a cada versão, e refazer a imagem inteira levava 8 min num worker
# de 1 vCPU (medido em 28/10/2026); trocar cinco arquivos leva segundos. Argumento: $1 o ext4 (sem espaço).
#
# O debugfs sai com código 0 mesmo quando um comando dele falha. Por isso cada arquivo é lido de volta e comparado
# byte a byte com o original: diferença é falha (código 1), e o host volta a construir a imagem inteira.
set -eu
img=$1
# O debugfs mora no e2fsprogs-extra no Alpine; o bootstrap antigo só tinha o e2fsprogs.
command -v debugfs >/dev/null 2>&1 || apk add --no-progress -q e2fsprogs-extra
t0=$(awk '{printf "%d", $1*1000}' /proc/uptime)
for f in guest start.sh shell.sh machine.sh env; do
  debugfs -w -R "rm /opt/runner/$f" "$img" >/dev/null 2>&1 || true
  debugfs -w -R "write /work/inject/$f /opt/runner/$f" "$img" >/dev/null 2>&1
  # Dono root e modo 0755, como o build.sh deixa (cp + chmod 755 dentro da VM).
  debugfs -w -R "sif /opt/runner/$f uid 0" "$img" >/dev/null 2>&1
  debugfs -w -R "sif /opt/runner/$f gid 0" "$img" >/dev/null 2>&1
  debugfs -w -R "sif /opt/runner/$f mode 0100755" "$img" >/dev/null 2>&1
  rm -f /tmp/check
  debugfs -R "dump /opt/runner/$f /tmp/check" "$img" >/dev/null 2>&1
  cmp -s "/work/inject/$f" /tmp/check || { echo "reinject: /opt/runner/$f não confere"; exit 1; }
done
# Sistema de arquivos íntegro depois das trocas (-n: só confere, não conserta).
e2fsck -fn "$img" >/dev/null 2>&1 || { echo "reinject: e2fsck acusou erro"; exit 1; }
t1=$(awk '{printf "%d", $1*1000}' /proc/uptime)
echo "RESULT reinject_ms=$((t1-t0))"
