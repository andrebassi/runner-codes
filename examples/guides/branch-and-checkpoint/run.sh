#!/usr/bin/env bash
# Guia branch-and-checkpoint. A libkrun não tem snapshot de memória, então o checkpoint aqui é
# de DISCO: `rc machine branch` numa máquina ligada dá sync no guest, congela, clona o ext4
# (copy-on-write) e retoma. O que isso prova, passo a passo:
#   2. fonte com estado na RAM (/tmp, tmpfs) e no disco (/root);
#   3. capturas com retenção (--keep), publicadas antes de apagar a mais velha, e uma só por vez;
#   4. restaurar qualquer geração numa máquina nova, sem gastar o checkpoint;
#   5. pause/resume: o contador continua de onde parou e não anda enquanto pausado;
#   6. branch de uma fonte aquecida: filhas e o ponto de partida guardado;
#   +  exportar um checkpoint como artefato (pack --from-vm), que roda em outro host.
# Limite declarado: a RAM não volta numa restauração (o contador recomeça; o disco volta).
source "$(dirname "$0")/../lib.sh"
p=bcdoc-$$
work=$(mktemp -d /tmp/rcbcdocXXXX)
trap 'cleanup_prefix bcdoc-; rm -rf "$work"' EXIT
x() { "$RC" machine exec --name "$1" -- "${@:2}" 2>/dev/null | tr -d '\r'; }

# capture roda o checkpoint.sh desta pasta e devolve o nome do checkpoint (vazio se falhou).
ckpt="$(dirname "$0")/checkpoint.sh"
capture() { RC="$RC" bash "$ckpt" --name "$1" --keep "$2" | sed -n 's/^checkpoint=//p'; }

step "2. create-source (contador na RAM a cada 1 s, marcador no disco)"
src=$p-src
"$RC" machine create --name "$src" --image alpine -- sh -c \
	'echo SETUP_DONE > /root/setup.txt; i=0; while true; do i=$((i+1)); echo $i > /tmp/counter; sleep 1; done' >/dev/null
check "start" "$RC" machine start --name "$src"
sleep 3
check "marcador no disco" test "$(x "$src" cat /root/setup.txt)" = SETUP_DONE
check "contador na RAM andando" test "$(x "$src" cat /tmp/counter)" -ge 2

step "5. pause e resume (o processo continua de onde parou)"
c1=$(x "$src" cat /tmp/counter)
check "pause" "$RC" machine pause --name "$src"
check "estado paused" test "$(rc_state "$src")" = paused
check "exec recusado com a máquina pausada" sh -c "! '$RC' machine exec --name '$src' -- true"
sleep 4
check "resume" "$RC" machine resume --name "$src"
c2=$(x "$src" cat /tmp/counter)
check "não andou pausado ($c1 -> $c2, 4 s parado)" test "$((c2 - c1))" -le 2
check "não recomeçou: mesmo processo" test "$c2" -ge "$c1"
sleep 2
check "segue contando depois do resume" test "$(x "$src" cat /tmp/counter)" -gt "$c2"

step "3. capturas com retenção (--keep 2, quatro capturas, geração marcada no disco)"
kept=()
for gen in 1 2 3 4; do
	x "$src" sh -c "echo GEN$gen > /root/gen" >/dev/null
	ck=$(capture "$src" 2)
	check "captura $gen publicada ($ck)" test -n "$ck"
	kept+=("$ck")
done
# shellcheck disable=SC2207 # nomes de máquina não têm espaço (ValidateName)
left=($(rc_names | grep "^$src-ck-" | sort))
check "ficaram exatamente 2" test "${#left[@]}" = 2
check "as 2 mais novas" test "${left[*]}" = "${kept[2]} ${kept[3]}"
check "a fonte seguiu ligada" test "$(rc_state "$src")" = running
lock="${TMPDIR:-/tmp}/rc-checkpoint-$src.lock"
mkdir "$lock" && echo $$ >"$lock/pid"
RC="$RC" bash "$ckpt" --name "$src" --keep 2 >"$work/skip.out"
check "captura concorrente da mesma fonte: skipped, exit 3" test "$?:$(tail -2 "$work/skip.out" | head -1)" = "3:result=skipped"
sleep 30 & dead=$!
kill -9 $dead; wait $dead 2>/dev/null
echo $dead >"$lock/pid"
x "$src" sh -c "echo GEN5 > /root/gen" >/dev/null
check "trava de um processo morto é retomada" sh -c "RC='$RC' bash '$ckpt' --name '$src' --keep 2 | grep -qx result=captured"
check "a retenção segue em 2" test "$(rc_names | grep -c "^$src-ck-")" = 2
check "--keep 0 recusado" sh -c "! RC='$RC' bash '$ckpt' --name '$src' --keep 0"
check "fonte inexistente: result=failed, exit 1" sh -c "RC='$RC' bash '$ckpt' --name '$p-nope' | grep -qx result=failed"
# shellcheck disable=SC2207
left=($(rc_names | grep "^$src-ck-" | sort))

step "4. restaurar uma geração numa máquina nova (o checkpoint fica intacto)"
old=${left[0]}
check "restore da geração 4 ($old)" "$RC" machine branch --from "$old" --name "$p-old"
check "start" "$RC" machine start --name "$p-old"
check "disco da geração 4" test "$(x "$p-old" cat /root/gen)" = GEN4
check "marcador do setup" test "$(x "$p-old" cat /root/setup.txt)" = SETUP_DONE
sleep 2
check "RAM não volta: o contador recomeçou (checkpoint só de disco)" test "$(x "$p-old" cat /tmp/counter)" -lt "$c2"
check "o checkpoint segue parado e restaurável" test "$(rc_state "$old")" = stopped
x "$p-old" sh -c 'echo ANTES_DA_PAUSA > /root/late' >/dev/null
"$RC" machine pause --name "$p-old" >/dev/null
check "start numa pausada: recusado dizendo que está pausada" sh -c "'$RC' machine start --name '$p-old' 2>&1 | grep -q 'is paused; resume it'"
check "stop numa pausada: o rc retoma e para" "$RC" machine stop --name "$p-old"
"$RC" machine start --name "$p-old" >/dev/null
check "o stop de uma pausada foi limpo: a escrita de antes da pausa está no disco" test "$(x "$p-old" cat /root/late)" = ANTES_DA_PAUSA

step "+ exportar o checkpoint mais novo como artefato (pack --from-vm)"
check "pack create --from-vm" "$RC" pack create --from-vm "${left[1]}" --output "$work/ck"
check "o artefato traz a geração 5" sh -c "'$work/ck' run -- cat /root/gen | grep -qx GEN5"

step "6. branch de uma fonte aquecida: filhas e o ponto de partida guardado"
x "$src" sh -c 'head -c 4 /dev/urandom | od -An -tu4 | tr -d " " > /root/token' >/dev/null
token=$(x "$src" cat /root/token)
point=$(capture "$src" 5)
check "ponto de partida guardado ($point)" test -n "$point"
for w in 0 1; do
	"$RC" machine branch --from "$point" --name "$p-w-$w" >/dev/null 2>&1
	check "filha w-$w liga" "$RC" machine start --name "$p-w-$w"
	check "filha w-$w lê o token da fonte ($token)" test "$(x "$p-w-$w" cat /root/token)" = "$token"
done
x "$p-w-0" sh -c 'echo SO_W0 > /root/own' >/dev/null
check "filhas independentes (w-1 não vê o arquivo de w-0)" test -z "$(x "$p-w-1" cat /root/own)"
check "a fonte seguiu ligada" test "$(rc_state "$src")" = running
"$RC" machine delete --name "$p-w-0" --force >/dev/null
"$RC" machine delete --name "$p-w-1" --force >/dev/null
check "sem as filhas, o ponto de partida continua com o token" sh -c \
	"'$RC' machine branch --from '$point' --name '$p-again' >/dev/null && '$RC' machine start --name '$p-again' >/dev/null && '$RC' machine exec --name '$p-again' -- cat /root/token | grep -qx '$token'"

step "7. cleanup"
cleanup_prefix "$p"
check "nenhuma máquina $p-* sobrou" test -z "$(rc_names | grep "^$p")"
check "nenhum vmm de $p vivo" test "$(vmm_count "$p")" = 0
finish
