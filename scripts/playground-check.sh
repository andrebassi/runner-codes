#!/bin/bash
# Confere uma máquina de visitante do playground: o Claude Code instalado e rodando como root sem confirmação
# (IS_SANDBOX), nenhuma pasta do host, e a rede saindo só para o que a base libera, com a rede local fechada.
# Uso: scripts/playground-check.sh pg-<id>. Sai com o número de falhas.
set -u
cd "$(dirname "$0")/.."
# ~/.local/bin antes do perfil do Nix: lá está o roteador gpg2 do pass (zfold/YubiKey); o gpg2 do Nix na frente manda
# o pass ao agente local e o pedido de aprovação nunca chega ao celular (task check:path reprova a ordem invertida).
export PATH="$HOME/.local/bin:$HOME/.nix-profile/bin:$PATH"
RC=./bin/rc
M=${1:?uso: $0 pg-<id>}
fails=0
# check DESCRIÇÃO COMANDO...: roda e conta falha se o rc não for 0.
check() { local d=$1; shift; if "$@" >/dev/null 2>&1; then echo "   ok    $d"; else echo "   FAIL  $d"; fails=$((fails + 1)); fi; }
# in_vm CMD: roda um sh -c dentro da máquina do visitante, com prazo.
in_vm() { timeout 60s "$RC" machine exec --name "$M" -- sh -c "$1"; }
# probe URL: o status HTTP que o guest recebe; 000 quando a conexão não sai (o curl rodou e a rede recusou). Vazio
# quer dizer que nem o curl rodou, e aí nem "alcança" nem "bloqueado" podem passar.
probe() { in_vm "timeout 8 curl -s -o /dev/null -m 6 -w '%{http_code}' '$1'"; }
# reaches URL: o guest recebe qualquer resposta HTTP.
reaches() { probe "$1" | grep -qE '^[1-5][0-9][0-9]$'; }
# blocked URL: o curl rodou e a conexão não saiu (000). Sem curl na imagem, falha em vez de passar.
blocked() { test "$(probe "$1")" = 000; }

echo "== ferramentas"
check "claude --version" in_vm "claude --version | grep -qi 'claude code'"
check "roda como root com IS_SANDBOX=1" in_vm 'test "$(id -u)" = 0 && test "$IS_SANDBOX" = 1'
check "git e python3" in_vm "git --version && python3 --version"
check "/workspace é do disco da VM (nenhuma pasta do host)" in_vm "! grep -q ' /workspace virtiofs' /proc/mounts"

echo "== rede liberada"
check "api.anthropic.com" reaches https://api.anthropic.com
check "claude.ai" reaches https://claude.ai
check "registry.npmjs.org" reaches https://registry.npmjs.org
check "pypi.org" reaches https://pypi.org/simple/

echo "== rede fechada"
check "example.com não sai" blocked https://example.com
check "LAN de quem hospeda (192.168.1.1) fechada" blocked http://192.168.1.1
check "loopback do host (100.96.0.1) fechado" blocked http://100.96.0.1:8095
check "metadata de nuvem fechada" blocked http://169.254.169.254

echo "falhas: $fails"
exit "$fails"
