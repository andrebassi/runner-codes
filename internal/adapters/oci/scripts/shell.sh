#!/bin/sh
# Shell interativo de `rc shell`: mesmo ambiente do job (root read-only, sem rede,
# tmpfs em /tmp, ambiente da imagem OCI), mas entrega o terminal em vez do guest-runner.
mount -t tmpfs -o size=256m,mode=1777 tmpfs /tmp
mkdir -p /tmp/home
export HOME=/tmp/home
if [ -f /opt/runner/env ]; then
  while IFS= read -r l; do [ -n "$l" ] && export "$l"; done < /opt/runner/env
fi
cd /tmp
echo "rc shell: $(uname -sm) — read-only root, no network, /tmp is writable. Type 'exit' to leave."
# bash quando a imagem tem (editável com setas); senão o sh da imagem. Procura no PATH:
# a imagem bash:5.2 instala em /usr/local/bin/bash, e /bin/bash não existe nela (medido).
if b=$(command -v bash); then exec "$b" -i; fi
exec /bin/sh -i
