#!/bin/bash
# Reprova script que põe ~/.nix-profile/bin na frente do PATH sem ~/.local/bin antes dele. O ~/.local/bin guarda o
# roteador gpg2 do pass (zfold ou YubiKey); com o gpg2 do Nix na frente, o pass fala com o agente local, o pedido de
# aprovação nunca chega ao celular e o script fica parado esperando um pinentry (medido em 20/10/2026, três vezes).
# Uso: scripts/check-path-order.sh [ARQUIVO...]; sem argumento, varre scripts/ e examples/. Sai com o número de achados.
set -u
cd "$(dirname "$0")/.."
files=("$@")
# Na varredura padrão este arquivo fica de fora: o padrão do grep abaixo contém o texto que ele procura.
[ ${#files[@]} -gt 0 ] || while IFS= read -r f; do files+=("$f"); done < <(find scripts examples -name '*.sh' -type f ! -name check-path-order.sh | sort)
bad=0
for f in "${files[@]}"; do
  # Toda linha que monta o PATH com o perfil do Nix: o ~/.local/bin tem de aparecer antes dele na mesma linha.
  while IFS= read -r hit; do
    line=${hit#*:}
    case "$line" in
      *'.local/bin'*'.nix-profile/bin'*) ;;
      *) echo "🛑 $f:${hit%%:*}: ~/.nix-profile/bin na frente do roteador do pass (ponha \$HOME/.local/bin antes)"; bad=$((bad + 1)) ;;
    esac
  done < <(grep -n 'PATH=.*\.nix-profile/bin' "$f" 2>/dev/null)
done
echo "ordem do PATH: ${#files[@]} arquivos, $bad problema(s)"
exit "$bad"
