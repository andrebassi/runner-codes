#!/usr/bin/env bash
# Uma captura de disco de uma máquina do rc, com retenção, para rodar à mão ou num timer
# (cron, systemd, launchd). O rc não tem agendador nem retenção próprios: este script é
# quem faz os dois.
#
# uso: checkpoint.sh --name SRC [--keep N]
# saída: chave=valor; a última linha é result=captured | result=skipped | result=failed.
# rc: 0 capturou, 3 outra captura da mesma fonte estava rodando, 1 falhou.
#
# A captura é `rc machine branch`: sync no guest, congela, clona o disco (copy-on-write) e
# retoma. O checkpoint é uma máquina parada <SRC>-ck-<UTC>; restaurar é `rc machine branch
# --from <checkpoint> --name <nova>`, que deixa o checkpoint intacto.
set -uo pipefail
RC="${RC:-$(cd "$(dirname "$0")/../../.." && pwd)/bin/rc}"
src="" keep=6
while [ $# -gt 0 ]; do
	case "$1" in
	--name) src=$2; shift 2 ;;
	--keep) keep=$2; shift 2 ;;
	*) echo "result=failed"; echo "note=unknown argument $1" >&2; exit 1 ;;
	esac
done
[ -n "$src" ] || { echo "result=failed"; echo "note=--name is required" >&2; exit 1; }
case "$keep" in '' | *[!0-9]* | 0) echo "result=failed"; echo "note=--keep must be >= 1" >&2; exit 1 ;; esac

# Trava por fonte: duas capturas da mesma máquina ao mesmo tempo congelariam a fonte duas vezes
# e a retenção de uma apagaria o que a outra contou. O pid dentro dela deixa uma trava de um
# processo morto (kill -9, máquina que reiniciou) ser retomada em vez de travar o timer para
# sempre.
lock="${TMPDIR:-/tmp}/rc-checkpoint-$src.lock"
if ! mkdir "$lock" 2>/dev/null; then
	holder=$(cat "$lock/pid" 2>/dev/null)
	if [ -n "$holder" ] && kill -0 "$holder" 2>/dev/null; then
		echo "result=skipped"
		echo "note=a capture of $src is still running (pid $holder)"
		exit 3
	fi
	rm -rf "$lock" && mkdir "$lock" || { echo "result=failed"; exit 1; }
fi
echo $$ >"$lock/pid"
trap 'rm -rf "$lock"' EXIT

# Nome com a hora UTC até o segundo: ordena por idade na ordem alfabética, que a retenção usa.
name="$src-ck-$(date -u +%Y%m%dt%H%M%S)"
# Duas capturas no mesmo segundo dariam o mesmo nome: a segunda espera o próximo segundo.
while "$RC" machine status --name "$name" >/dev/null 2>&1; do
	sleep 1
	name="$src-ck-$(date -u +%Y%m%dt%H%M%S)"
done
start=$(date +%s)
if ! "$RC" machine branch --from "$src" --name "$name" >/dev/null 2>"$lock/err"; then
	echo "result=failed"
	echo "note=$(tr '\n' ' ' <"$lock/err")"
	exit 1
fi
echo "checkpoint=$name"
echo "took_s=$(($(date +%s) - start))"

# Retenção só depois de a captura nova existir: uma falha acima sai antes daqui e nunca deixa
# menos checkpoints do que havia.
"$RC" machine ls 2>/dev/null | awk -v p="$src-ck-" 'NR>1 && index($1, p) == 1 {print $1}' | sort |
	awk -v k="$keep" '{a[NR]=$0} END {for (i = 1; i <= NR-k; i++) print a[i]}' |
	while read -r old; do
		"$RC" machine delete --name "$old" --force >/dev/null 2>&1 && echo "deleted=$old"
	done
echo "kept=$("$RC" machine ls 2>/dev/null | awk -v p="$src-ck-" 'NR>1 && index($1, p) == 1' | wc -l | tr -d ' ')"
echo "result=captured"
