#!/usr/bin/env bash
# Guia pack: imagem ou máquina provisionada num artefato que roda em outro host sem pull. A
# prova é ler de volta um marcador gravado na máquina de origem (um pack que perdeu o disco
# ainda liga e sai 0), e o segundo run reaproveitar o cache.
#
# O payload é <stub>.rcmachine; `rc pack inspect` mostra o conteúdo; o cache dos packs fica em
# <cache>/packs; a libkrun é a do host de destino.
source "$(dirname "$0")/../lib.sh"
work=$(mktemp -d)
name=pkdoc-$$
trap 'rm -rf "$work"; cleanup_prefix pkdoc-' EXIT
packs="$HOME/Library/Caches/runner-codes/packs"
[ "$(uname -s)" = Linux ] && packs="${XDG_CACHE_HOME:-$HOME/.cache}/runner-codes/packs"
entries() { find "$packs" -mindepth 1 -maxdepth 1 -type d 2>/dev/null | wc -l | tr -d ' '; }

step "pack-image: --image alpine"
check "pack create --image alpine" "$RC" pack create --image alpine --output "$work/from-image"
check "stub e sidecar lado a lado" test -x "$work/from-image" -a -f "$work/from-image.rcmachine"
kernel=$("$work/from-image" run -- uname -sr 2>&1 | grep -m1 '^Linux ' | awk '{print $1}')
check "o artefato roda numa VM (kernel Linux)" test "$kernel" = Linux
first=$(entries)
check "segundo run funciona" test "$("$work/from-image" run -- sh -c 'echo SECOND_RUN_OK' 2>&1 | tail -1)" = SECOND_RUN_OK
check "segundo run reaproveita o cache (não extrai de novo)" test "$(entries)" = "$first"

step "pack-machine: máquina com marcador"
"$RC" machine create --name "$name" --net --image alpine >/dev/null
"$RC" machine start --name "$name" >/dev/null
"$RC" machine exec --name "$name" -- sh -c 'echo PACKED_STATE_PRESENT > /marker.txt'
check "a origem tem o marcador" test "$("$RC" machine exec --name "$name" -- cat /marker.txt)" = PACKED_STATE_PRESENT
"$RC" machine stop --name "$name" >/dev/null
check "pack create --from-vm" "$RC" pack create --from-vm "$name" --output "$work/from-vm"
"$RC" machine delete --name "$name" >/dev/null

step "verify-pack: o marcador vem no artefato, com a origem apagada"
check "o artefato leva o disco (marcador lido)" test "$("$work/from-vm" run -- cat /marker.txt 2>&1 | tail -1)" = PACKED_STATE_PRESENT
info=$("$RC" pack inspect "$work/from-vm.rcmachine")
printf '%s\n' "$info" | grep -E '"(source|platform|host_platform|rc_version)"' | sed 's/^/  /'
check "inspect diz de onde veio" grep -q "\"source\": \"machine:$name\"" <<<"$info"
check "inspect lista o disco da máquina" grep -q '"data.ext4"' <<<"$info"

step "--output nomeia o stub (traps.md): o stub sozinho mostra o uso"
check "uso com o nome do artefato" sh -c "'$work/from-vm' 2>&1 | grep -q 'usage: from-vm run'"
finish
