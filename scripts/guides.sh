#!/usr/bin/env bash
# Roda os guias (examples/guides/<pasta>/run.sh), um por um, e reprova
# se algum falhar. Uso: scripts/guides.sh [pasta ...]   (sem argumento: todas)
# Precisa de `task build` e da internet (imagens e os testes de rede).
set -uo pipefail
cd "$(dirname "$0")/.."
dirs=("$@")
if [ ${#dirs[@]} -eq 0 ]; then
	for f in examples/guides/*/run.sh; do dirs+=("$(basename "$(dirname "$f")")"); done
fi
total=0
for d in "${dirs[@]}"; do
	printf '\n######## %s\n' "$d"
	if bash "examples/guides/$d/run.sh"; then
		printf '>> %s: ok\n' "$d"
	else
		printf '>> %s: FALHOU\n' "$d"
		total=$((total + 1))
	fi
done
printf '\npastas com falha: %d de %d\n' "$total" "${#dirs[@]}"
exit "$total"
