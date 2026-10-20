#!/usr/bin/env bash
# Gate de cobertura do SDK Python (rule 15): linha E branch, cada uma >= COVERAGE_FLOOR
# (padrão 90). O coverage.py vem do nixpkgs na hora (nix shell), sem pip nem venv no projeto:
# o SDK em si não tem dependência, e a ferramenta de teste não deve virar uma.
set -euo pipefail
cd "$(dirname "$0")/../sdk/python"
floor=${COVERAGE_FLOOR:-90}
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
nix shell nixpkgs#python3Packages.coverage -c bash -c "
  set -euo pipefail
  export COVERAGE_FILE='$tmp/data'
  coverage run --branch --source=runner_code -m unittest discover -s tests -t . -v
  coverage report -m
  coverage json -q -o '$tmp/cov.json'
"
# O --fail-under do coverage.py junta linha e branch num número só: um branch metade
# testado se esconde atrás de muita linha coberta. Aqui cada um passa no piso sozinho.
python3 - "$tmp/cov.json" "$floor" <<'PY'
import json, sys
t = json.load(open(sys.argv[1]))["totals"]
floor = float(sys.argv[2])
lines = 100.0 * t["covered_lines"] / max(t["num_statements"], 1)
branches = 100.0 * t["covered_branches"] / max(t["num_branches"], 1)
print(f"sdk python: linha {lines:.1f}%  branch {branches:.1f}%  piso {floor:g}%")
sys.exit(0 if lines >= floor and branches >= floor else 1)
PY
