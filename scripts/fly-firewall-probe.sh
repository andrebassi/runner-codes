#!/bin/bash
# Mede numa Fly Machine descartável o que o playground precisa do kernel do Fly: se o firewall por usuário
# (iptables/ip6tables com --uid-owner) funciona — é ele que impede o visitante de alcançar a rede privada 6PN — e se
# um processo sem privilégio é de fato recusado ao discar a rede privada enquanto o DNS e a internet seguem abertos.
# Cria um app, roda uma máquina que testa e sai, lê o log e apaga o app. O token sai do pass e nunca é impresso.
# Uso: scripts/fly-firewall-probe.sh [imagem]   (padrão alpine:3.20)
set -u
# ~/.local/bin antes do perfil do Nix: lá está o roteador gpg2 do pass (zfold/YubiKey); o gpg2 do Nix na frente manda
# o pass ao agente local e o pedido de aprovação nunca chega ao celular (task check:path reprova a ordem invertida).
export PATH="$HOME/.local/bin:$HOME/.nix-profile/bin:$PATH"
# pass pelo roteador ~/.local/bin/gpg2 (zfold ou YubiKey), com as ferramentas do macOS na frente.
pass_cmd() { PATH="$HOME/.local/bin:/usr/bin:/bin:$PATH" pass "$@"; }
FLY_API_TOKEN=$(pass_cmd show bassi/flyio/api-token) || { echo "pass falhou"; exit 1; }
export FLY_API_TOKEN
APP=rc-fw-probe-$$
trap 'timeout 60s fly apps destroy "$APP" --yes >/dev/null 2>&1; echo "app $APP apagado"' EXIT
timeout 60s fly apps create "$APP" --org personal >/dev/null || { echo "não criou o app"; exit 1; }
# Mesmas regras do rc-visitor; cada linha de resultado começa com R: para o filtro do log abaixo.
PROBE='apk add -q iptables ip6tables curl bind-tools 2>&1 | sed "s/^/R: apk /"
adduser -D -u 1000 visitor
r() { out=$("$@" 2>&1) && echo "R: ok $*" || echo "R: FALHOU $* -> $out"; }
echo "R: owner (por usuário): $(ip6tables -A OUTPUT -m owner --uid-owner 1000 -j ACCEPT 2>&1 | head -1)"
# Sem o módulo owner, a regra vale para a máquina inteira e barra só a ABERTURA de conexão (SYN) e o UDP para a
# rede privada: as respostas do rc-visitor ao front continuam saindo.
r ip6tables -A OUTPUT -d fdaa::3 -p udp --dport 53 -j ACCEPT
r ip6tables -A OUTPUT -d fdaa::3 -p tcp --dport 53 -j ACCEPT
r ip6tables -A OUTPUT -d fc00::/7 -p tcp --syn -j REJECT --reject-with tcp-reset
r ip6tables -A OUTPUT -d fc00::/7 -p udp -j REJECT
r iptables -A OUTPUT -d 10.0.0.0/8 -p tcp --syn -j REJECT --reject-with tcp-reset
echo "R: regras: $(ip6tables -S OUTPUT | tr "\n" ";")"
echo "R: root 6PN: $(curl -s -o /dev/null -w %{http_code} -m 5 http://_api.internal:4280/ 2>&1)"
echo "R: visitor 6PN: $(su visitor -s /bin/sh -c "curl -s -o /dev/null -w %{http_code} -m 5 http://_api.internal:4280/" 2>&1)"
echo "R: visitor DNS: $(su visitor -s /bin/sh -c "dig +short api.anthropic.com | head -1" 2>&1)"
echo "R: visitor internet: $(su visitor -s /bin/sh -c "curl -s -o /dev/null -w %{http_code} -m 8 https://api.anthropic.com/" 2>&1)"
sleep 5'
timeout 240s fly machine run "${1:-alpine:3.20}" --app "$APP" --region gru --rm --restart no -- sh -c "$PROBE" >/dev/null 2>&1
sleep 8
# Só as linhas que a máquina imprimiu (R:), sem o eco do comando que o Fly registra no "Preparing to run".
timeout 60s fly logs --app "$APP" --no-tail 2>/dev/null | sed 's/\x1b\[[0-9;]*m//g' | grep -v 'Preparing to run' | grep -o 'R: .*'
