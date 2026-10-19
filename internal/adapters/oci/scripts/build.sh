#!/bin/sh
# Roda DENTRO da VM de build (Alpine, root, Linux case-sensitive). Monta o ext4 de uma
# linguagem a partir do tar OCI achatado. Argumentos sem espaço (a libkrun parte o argv):
#   $1 tar de entrada   $2 ext4 de saída   $3 arquivo com o warmup (pode não existir)
# O guest e o start.sh já foram copiados para /work/inject/ pelo host.
set -eu
in=$1; out=$2; warm=$3
ms() { awk '{printf "%d", $1*1000}' /proc/uptime; }

mkdir -p /build
mount -t tmpfs -o size=6g tmpfs /build
t0=$(ms)
# --numeric-owner preserva uid/gid da imagem; extrair no host perderia dono e caixa das letras.
tar -xpf "$in" -C /build --numeric-owner --xattrs
mkdir -p /build/opt/runner /build/proc /build/sys /build/dev /build/tmp
cp /work/inject/guest /work/inject/start.sh /work/inject/shell.sh /work/inject/env /build/opt/runner/
chmod 755 /build/opt/runner/guest /build/opt/runner/start.sh /build/opt/runner/shell.sh
t1=$(ms)

if [ -s "$warm" ]; then
  # Warmup com rede (apk add, por exemplo): resolv.conf da VM de build entra só durante o chroot.
  cp /build/etc/resolv.conf /work/resolv.orig 2>/dev/null || true
  cp /etc/resolv.conf /build/etc/resolv.conf
  cp "$warm" /build/tmp/warmup.sh
  mount -t proc proc /build/proc
  mount --bind /dev /build/dev
  chroot /build /bin/sh /tmp/warmup.sh
  # -l: algum processo do warmup ainda segura /dev ao sair (medido: "Resource busy");
  # o tmpfs inteiro é descartado depois do mkfs, então desmontar lazy é seguro.
  umount -l /build/dev /build/proc
  rm -f /build/tmp/warmup.sh
  if [ -f /work/resolv.orig ]; then cp /work/resolv.orig /build/etc/resolv.conf; else rm -f /build/etc/resolv.conf; fi
fi
rm -rf /build/tmp/* /build/var/cache/apk/*
t2=$(ms)

used=$(du -sm /build | cut -f1)
# 25% de folga + 32 MiB: o ext4 gasta inodes e journal além do conteúdo.
size=$(( used + used / 4 + 32 ))
rm -f "$out"
mkfs.ext4 -q -F -L root -d /build "$out" "${size}M"
t3=$(ms)
echo "RESULT extract_ms=$((t1-t0)) warmup_ms=$((t2-t1)) mkfs_ms=$((t3-t2)) used_mib=$used disk_mib=$size"
