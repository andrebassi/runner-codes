#!/bin/sh
# Primeiro processo de uma MÁQUINA persistente (krun_set_exec), no lugar do start.sh dos jobs.
# A imagem continua read-only em /dev/vda; o disco da máquina (/dev/vdb, ext4) guarda:
#   upper/      o que for gravado por cima da imagem (pacote instalado, /etc, /root...)
#   work/       área de trabalho do overlayfs (precisa estar no mesmo disco do upper)
#   workspace/  o /workspace da máquina
# Tudo vira um overlay montado em /persist/merged, e o guest roda num chroot ali: para quem
# usa exec e cp, a raiz da máquina é gravável e sobrevive a stop/start. /tmp segue tmpfs.
set -eu
mount -t ext4 /dev/vdb /persist
mkdir -p /persist/upper /persist/work /persist/workspace /persist/merged
# lowerdir=/ não atravessa montagens: o /persist da imagem aparece vazio no merged, sem laço.
mount -t overlay overlay -o lowerdir=/,upperdir=/persist/upper,workdir=/persist/work /persist/merged
m=/persist/merged
mkdir -p $m/proc $m/sys $m/dev $m/tmp $m/workspace
mount -t proc proc $m/proc
mount -t sysfs sysfs $m/sys
mount --bind /dev $m/dev
mount --bind /persist/workspace $m/workspace
# RUNNER_MACHINE faz o start.sh deixar o HOME em /root (persistente), não no tmpfs.
export RUNNER_MACHINE=1
exec chroot $m /opt/runner/start.sh
