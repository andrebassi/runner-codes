#!/bin/bash
# Nó Linux de teste (droplet da DigitalOcean, que expõe /dev/kvm: medido por scripts/do-kvm-probe.sh) para o rc como
# runtime de Kubernetes (k3s e microk8s). O estado (id, IP, chave SSH própria) fica em .k8s/, fora do git.
# Custo: s-4vcpu-8gb ≈ US$ 0,071/h enquanto existir; "destroy" apaga droplet e chave na DigitalOcean.
#
# Uso: [K8S_NODE=k3s|microk8s] scripts/k8s-node.sh create|provision|sync|shim|ssh [cmd]|status|destroy
# Um droplet por nome (padrão k3s): os dois clusters existem juntos, e a prova de um não derruba o outro.
set -u
# ~/.local/bin antes do perfil do Nix: lá está o roteador gpg2 do pass (zfold/YubiKey); o gpg2 do Nix na frente manda
# o pass ao agente local e o pedido de aprovação nunca chega ao celular (task check:path reprova a ordem invertida).
export PATH="$HOME/.local/bin:$HOME/.nix-profile/bin:$PATH"
cd "$(dirname "$0")/.."
NODE=${K8S_NODE:-k3s}
STATE=.k8s/$NODE SIZE=${K8S_NODE_SIZE:-s-4vcpu-8gb} REGION=${K8S_NODE_REGION:-nyc3}
API=https://api.digitalocean.com/v2
mkdir -p "$STATE" && chmod 700 .k8s "$STATE"
pass_cmd() { PATH="$HOME/.local/bin:/usr/bin:/bin:$PATH" pass "$@"; }
# token só é lido do pass quando um subcomando fala com a API (ssh e sync não precisam).
token() { [ -n "${TOKEN:-}" ] || TOKEN=$(pass_cmd show bassi/digitalocean/api-token) || { echo "pass falhou" >&2; exit 1; }; }
api() { token; timeout 30s curl -s -H "Authorization: Bearer $TOKEN" -H 'Content-Type: application/json' "$@"; }
# jget lê um campo do JSON da entrada padrão por um caminho em Python ("['droplet']['id']").
jget() { python3 -c "import json,sys; print(json.load(sys.stdin)$1)"; }
ip() { cat "$STATE/ip" 2>/dev/null; }
SSH_OPTS=(-i "$STATE/id_ed25519" -o IdentitiesOnly=yes -o StrictHostKeyChecking=accept-new
  -o UserKnownHostsFile="$STATE/known_hosts" -o ConnectTimeout=10 -o ServerAliveInterval=15)
# rssh roda um comando no nó; o teto padrão de 30 min cobre o 1º build (Nix + libkrun + Go), RSSH_TIMEOUT encurta.
rssh() { timeout "${RSSH_TIMEOUT:-1800}s" ssh "${SSH_OPTS[@]}" "root@$(ip)" "$@"; }

create() {
  [ -s "$STATE/droplet" ] && [ -n "$(ip)" ] && { echo "já existe: droplet $(cat "$STATE/droplet") em $(ip)"; return; }
  [ -s "$STATE/id_ed25519" ] || ssh-keygen -q -t ed25519 -N "" -C "rc-k8s-$NODE" -f "$STATE/id_ed25519"
  # Droplet já criado numa tentativa anterior (só faltou o IP): reaproveita em vez de criar outro.
  if [ ! -s "$STATE/droplet" ]; then
    api -X POST "$API/account/keys" -d "{\"name\":\"rc-k8s-$NODE\",\"public_key\":\"$(cat "$STATE/id_ed25519.pub")\"}" \
      | jget "['ssh_key']['id']" >"$STATE/key" || { echo "não registrou a chave"; exit 1; }
    api -X POST "$API/droplets" -d "{\"name\":\"rc-k8s-$NODE\",\"region\":\"$REGION\",\"size\":\"$SIZE\",\"image\":\"ubuntu-24-04-x64\",\"ssh_keys\":[$(cat "$STATE/key")]}" \
      | jget "['droplet']['id']" >"$STATE/droplet" || { echo "não criou o droplet"; exit 1; }
    echo "droplet $(cat "$STATE/droplet") ($SIZE, $REGION) criado; esperando IP"
  fi
  # Até 3 min para ficar ativo (o boot leva ~40 s). O IP vai para variável antes do arquivo: o python imprime linha
  # vazia enquanto não há IP, e um arquivo com só "\n" passava no -s e mandava o ssh para '' (o localhost).
  local addr=""
  for _ in $(seq 36); do
    addr=$(api "$API/droplets/$(cat "$STATE/droplet")" | python3 -c 'import json,sys; d=json.load(sys.stdin)["droplet"]; print(next((n["ip_address"] for n in d["networks"]["v4"] if n["type"]=="public"),"") if d["status"]=="active" else "")')
    [ -n "$addr" ] && break; sleep 5
  done
  [ -n "$addr" ] || { echo "droplet não ficou ativo"; exit 1; }
  echo "$addr" >"$STATE/ip"
  # O sshd sobe alguns segundos depois do status active.
  for _ in $(seq 24); do rssh true 2>/dev/null && break; sleep 5; done
  echo "no ar: $(ip)"; rssh 'ls -l /dev/kvm; nproc; free -g | head -2'
}

# sync copia a árvore de trabalho (inclusive o que não foi commitado) para /root/rc no nó.
sync() {
  timeout 300s rsync -az --delete -e "ssh ${SSH_OPTS[*]}" --exclude .git --exclude bin --exclude .k8s \
    --exclude node_modules --exclude docs/docusaurus/build --exclude .playground --exclude deploy/playground/bin \
    ./ "root@$(ip):/root/rc/"
}

# provision instala Nix (daemon), libkrun do flake, Go/task do devShell e compila o rc no próprio nó.
provision() {
  rssh 'command -v nix >/dev/null || [ -x /nix/var/nix/profiles/default/bin/nix ] || (curl -fsSL https://nixos.org/nix/install | sh -s -- --daemon --yes) >/tmp/nix-install.log 2>&1; tail -2 /tmp/nix-install.log; mkdir -p /etc/nix; grep -q flakes /etc/nix/nix.conf || echo "experimental-features = nix-command flakes" >>/etc/nix/nix.conf; systemctl restart nix-daemon'
  sync
  rssh 'export PATH=/nix/var/nix/profiles/default/bin:$PATH; cd /root/rc && ./scripts/krun-install.sh && nix develop -c ./scripts/build.sh'
}

# shim recompila no nó e troca só os binários (shim, rc, guest): pod novo já usa a versão nova, sem
# reiniciar o k3s. A imagem da VM do pod não muda (mudar o guest pede deploy/k3s/install.sh de novo).
shim() {
  sync
  rssh 'export PATH=/nix/var/nix/profiles/default/bin:$PATH; cd /root/rc && nix develop -c ./scripts/build.sh >/tmp/build.log 2>&1 || { tail -20 /tmp/build.log; exit 1; }
    install -m 755 bin/containerd-shim-rc-v2-linux-amd64 /usr/local/bin/containerd-shim-rc-v2 && install -m 755 bin/rc /opt/rc/bin/rc && echo "shim trocado"'
}

case "${1:-}" in
  create) create ;;
  shim) shim ;;
  provision) provision ;;
  sync) sync ;;
  ssh) shift; rssh "$@" ;;
  status) [ -s "$STATE/droplet" ] && echo "droplet $(cat "$STATE/droplet") em $(ip)" || echo "nenhum nó" ;;
  destroy)
    [ -s "$STATE/droplet" ] && api -X DELETE "$API/droplets/$(cat "$STATE/droplet")" -o /dev/null && echo "droplet $(cat "$STATE/droplet") apagado"
    [ -s "$STATE/key" ] && api -X DELETE "$API/account/keys/$(cat "$STATE/key")" -o /dev/null && echo "chave $(cat "$STATE/key") apagada"
    rm -f "$STATE/droplet" "$STATE/key" "$STATE/ip" "$STATE/known_hosts" ;;
  *) echo "uso: $0 create|provision|sync|ssh [cmd]|status|destroy" >&2; exit 2 ;;
esac
