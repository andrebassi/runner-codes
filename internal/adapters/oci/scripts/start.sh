#!/bin/sh
# Primeiro processo do job dentro da VM (krun_set_exec). O root é ext4 read-only:
# toda escrita vai para um tmpfs, que some com a VM.
mount -t tmpfs -o size=256m,mode=1777 tmpfs /tmp
# /dev/stdin, /dev/stdout, /dev/stderr e /dev/fd: quem cria é o init de uma distro, e aqui
# ninguém cria. Sem eles, `grep -f /dev/stdin` falha com "No such file or directory" (medido
# no teste de contenção: a busca por um valor vazado dava 0 por estar cega).
[ -e /dev/fd ] || ln -s /proc/self/fd /dev/fd 2>/dev/null
for l in 0:stdin 1:stdout 2:stderr; do
  [ -e "/dev/${l#*:}" ] || ln -s "/proc/self/fd/${l%%:*}" "/dev/${l#*:}" 2>/dev/null
done
# Ambiente da config OCI (PATH, JAVA_HOME...), uma variável por linha. `export "$l"`
# não interpreta o valor, então nada da imagem vira comando aqui.
if [ -f /opt/runner/env ]; then
  while IFS= read -r l; do [ -n "$l" ] && export "$l"; done < /opt/runner/env
fi
mkdir -p /tmp/home
export HOME=/tmp/home
# Numa máquina persistente (machine.sh) a raiz é gravável e o HOME tem de sobreviver ao stop.
if [ -n "${RUNNER_MACHINE:-}" ]; then
  mkdir -p /root
  export HOME=/root
fi
# Cache de build aquecido na imagem (Go) só funciona gravável: copia para o tmpfs, no
# caminho que o executor do guest força (GOCACHE=/tmp/go-cache, internal/guest/executor.go).
# Copiar para outro nome deixava cada `go run` compilando a stdlib do zero (3,4 s medidos).
# Acontece no boot, antes de a VM entrar no pool, então não pesa no job.
if [ -d /opt/runner/gocache ]; then
  cp -a /opt/runner/gocache /tmp/go-cache
fi
exec /opt/runner/guest --port 5000
