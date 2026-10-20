#!/usr/bin/env bash
# Guia teardown: parar, apagar e PROVAR que ficou limpo (nenhuma máquina, nenhum processo de
# VM, nenhum diretório). E as duas armadilhas: Ctrl-C num `run` leva a VM junto, e um CLI
# morto à força não deixa VM órfã.
#
# O processo de uma VM é `rc vmm --spec <dir>/spec.json`; o estado mora em
# ~/Library/Caches/runner-codes (Linux: ~/.cache/runner-codes); `--cascade` é aceito e não
# muda nada (o branch do rc é cópia independente).
source "$(dirname "$0")/../lib.sh"
p=td-$$
trap 'cleanup_prefix "$p"' EXIT

step "estado para desmontar: máquina ligada e um pack extraído"
"$RC" machine create --name "$p-m" --stack python >/dev/null
check "start" "$RC" machine start --name "$p-m"
dir=$("$RC" machine data-dir --name "$p-m")
check "data-dir existe" test -d "$dir"

step "trap: Ctrl-C (SIGINT) no run leva a VM junto"
set -m
"$RC" machine run --name "$p-int" --stack python -- sleep 300 >/dev/null 2>&1 &
cli=$!
set +m
for _ in $(seq 1 40); do [ "$(vmm_count "$p-int")" = 1 ] && break; sleep 0.25; done
check "a VM do run subiu" test "$(vmm_count "$p-int")" = 1
kill -INT "$cli"
sleep 2
check "sem VM 2 s depois do SIGINT" test "$(vmm_count "$p-int")" = 0
check "a máquina efêmera foi apagada" test "$(rc_state "$p-int")" = absent

step "trap: CLI morto com SIGKILL não deixa VM órfã (vigia no fd 3)"
"$RC" machine run --name "$p-kill" --stack python -- sleep 300 >/dev/null 2>&1 &
cli=$!
for _ in $(seq 1 40); do [ "$(vmm_count "$p-kill")" = 1 ] && break; sleep 0.25; done
kill -KILL "$cli"
sleep 2
check "sem VM 2 s depois do SIGKILL no CLI" test "$(vmm_count "$p-kill")" = 0
# O registro fica (ninguém apagou): parado, e sai com delete.
check "o registro ficou parado, não ligado" test "$(rc_state "$p-kill")" != running
check "delete --force tira o registro" "$RC" machine delete --name "$p-kill" --force

step "teardown: stop, delete --force --cascade, prune"
check "stop" "$RC" machine stop --name "$p-m"
check "delete --force --cascade" "$RC" machine delete --name "$p-m" --force --cascade
check "machine prune" "$RC" machine prune
check "pack prune" "$RC" pack prune

step "verify-clean"
check "nenhuma máquina do teste listada" test -z "$(rc_names | grep "^$p")"
check "nenhum processo de VM do teste" test "$(vmm_count "$p")" = 0
check "diretório da máquina apagado" test ! -e "$dir"
finish
