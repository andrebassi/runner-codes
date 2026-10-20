#!/usr/bin/env bash
# Biblioteca comum dos guias do rc. Cada pasta tem um run.sh que carrega este arquivo, roda o
# fluxo do guia (preflight, create, verify, cleanup) e imprime uma linha ok/FAIL por
# conferência. O rc de saída é o número de falhas.
#
# O que a lib oferece, para nenhum run.sh repetir:
#   - leitura do `rc machine ls`: 1 linha de cabeçalho e o estado na 3ª coluna;
#   - o processo de uma VM, que é `<rc> vmm --spec <dir>/spec.json`;
#   - o diretório de uma máquina, que sai de `rc machine data-dir`;
#   - a lista vazia, que o `ls` escreve como `No machines found`.

set -uo pipefail

# RC é o binário; o padrão é o bin/rc do repositório (task build).
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)"
RC="${RC:-$ROOT/bin/rc}"
fails=0

# check imprime ok/FAIL de uma conferência e conta a falha, sem abortar o resto.
check() {
	local what=$1
	shift
	if "$@" >/dev/null 2>&1; then
		printf '   ok    %s\n' "$what"
	else
		printf '   FAIL  %s\n' "$what"
		fails=$((fails + 1))
	fi
}

# step imprime o título de uma etapa.
step() { printf '\n== %s\n' "$*"; }

# rc_names lista os nomes das máquinas (vazio quando não há nenhuma).
rc_names() { "$RC" machine ls 2>/dev/null | awk 'NR>1 && $0 != "No machines found" {print $1}'; }

# rc_state devolve o estado de uma máquina (running, stopped, paused) ou "absent".
rc_state() {
	local s
	s=$("$RC" machine ls 2>/dev/null | awk -v n="$1" 'NR>1 && $1==n {print $3}')
	echo "${s:-absent}"
}

# vmm_count conta os processos de VM vivos cujo spec está no diretório dado (vazio: todos).
vmm_count() {
	local n
	n=$(ps -eo command | grep -F 'vmm --spec' | grep -v grep | grep -cF "${1:-}")
	echo "${n:-0}"
}

# cleanup_prefix para e apaga toda máquina cujo nome começa com o prefixo.
cleanup_prefix() {
	local m
	for m in $(rc_names); do
		case "$m" in
		"$1"*) "$RC" machine delete --name "$m" --force >/dev/null 2>&1 ;;
		esac
	done
}

# finish imprime o resultado e sai com o número de falhas.
finish() {
	printf '\nfalhas: %d\n' "$fails"
	exit "$fails"
}
