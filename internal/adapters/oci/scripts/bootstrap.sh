#!/bin/sh
# Roda uma vez na VM de build recém-extraída (com rede por TSI) para instalar as
# ferramentas que o Alpine mínimo não traz: mkfs.ext4 com suporte a -d e GNU tar.
set -eu
apk add --no-progress -q e2fsprogs tar
mkfs.ext4 -V 2>&1 | head -1
tar --version | head -1
