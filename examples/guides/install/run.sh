#!/usr/bin/env bash
# Guia install: provar que a instalação (Nix, `task krun:install`, e `task build`) liga uma VM
# de verdade. A prova é um marcador impresso pelo guest e um kernel que não é o do host —
# código de saída 0 sozinho não prova nada.
# Uso: run.sh [--image REF]   (padrão alpine)
source "$(dirname "$0")/../lib.sh"
image=alpine
[ "${1:-}" = --image ] && image=$2

step "preflight (rc doctor)"
check "rc doctor sem erro" "$RC" doctor

step "verify-boot ($image)"
host_kernel="$(uname -sr)"
out=$("$RC" machine run --mem 2048 --net --image "$image" -- sh -c 'echo BOOTED_OK && uname -srm' 2>&1)
printf '%s\n' "$out" | sed 's/^/  /'
guest_kernel=$(printf '%s\n' "$out" | grep -m1 '^Linux ' | awk '{print $1" "$2}')
printf 'guest_kernel=%s host_kernel=%s\n' "${guest_kernel:-none}" "$host_kernel"
check "o guest rodou o comando (marcador)" grep -qx BOOTED_OK <<<"$out"
check "é uma VM (kernel do guest diferente do host)" test -n "$guest_kernel" -a "$guest_kernel" != "$host_kernel"

step "cleanup"
check "a máquina efêmera não deixou VM viva" test "$(vmm_count run-)" = 0
finish
