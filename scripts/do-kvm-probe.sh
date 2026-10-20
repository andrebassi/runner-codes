#!/bin/bash
# Mede se um droplet da DigitalOcean expõe virtualização aninhada (/dev/kvm e as flags vmx/svm): decide onde o rc roda
# como runtime de Kubernetes (k3s/microk8s), que precisa de KVM no nó. Cria uma chave SSH descartável e um droplet,
# olha, e apaga os dois na saída (trap), mesmo em erro. O token sai do pass e nunca é impresso.
# Uso: scripts/do-kvm-probe.sh [tamanho] [região]   (padrão s-2vcpu-4gb nyc3)
set -u
# ~/.local/bin antes do perfil do Nix: lá está o roteador gpg2 do pass (zfold/YubiKey); o gpg2 do Nix na frente manda
# o pass ao agente local e o pedido de aprovação nunca chega ao celular (task check:path reprova a ordem invertida).
export PATH="$HOME/.local/bin:$HOME/.nix-profile/bin:$PATH"
pass_cmd() { PATH="$HOME/.local/bin:/usr/bin:/bin:$PATH" pass "$@"; }
TOKEN=$(pass_cmd show bassi/digitalocean/api-token) || { echo "pass falhou"; exit 1; }
SIZE=${1:-s-2vcpu-4gb} REGION=${2:-nyc3}
API=https://api.digitalocean.com/v2
dir=$(mktemp -d)
# api faz uma chamada autenticada; o token vai por header, nunca na linha de comando visível em log.
api() { timeout 30s curl -s -H "Authorization: Bearer $TOKEN" -H 'Content-Type: application/json' "$@"; }
key_id="" droplet_id=""
cleanup() {
  [ -n "$droplet_id" ] && api -X DELETE "$API/droplets/$droplet_id" -o /dev/null && echo "droplet $droplet_id apagado"
  [ -n "$key_id" ] && api -X DELETE "$API/account/keys/$key_id" -o /dev/null && echo "chave $key_id apagada"
  rm -rf "$dir"
}
trap cleanup EXIT
ssh-keygen -q -t ed25519 -N "" -C rc-kvm-probe -f "$dir/k"
key_id=$(api -X POST "$API/account/keys" -d "{\"name\":\"rc-kvm-probe-$$\",\"public_key\":\"$(cat "$dir/k.pub")\"}" \
  | python3 -c 'import json,sys; print(json.load(sys.stdin)["ssh_key"]["id"])') || { echo "não registrou a chave"; exit 1; }
droplet_id=$(api -X POST "$API/droplets" -d "{\"name\":\"rc-kvm-probe\",\"region\":\"$REGION\",\"size\":\"$SIZE\",\"image\":\"ubuntu-24-04-x64\",\"ssh_keys\":[$key_id]}" \
  | python3 -c 'import json,sys; print(json.load(sys.stdin)["droplet"]["id"])') || { echo "não criou o droplet"; exit 1; }
echo "droplet $droplet_id ($SIZE, $REGION) criado; esperando IP"
ip=""
# Até 3 min para o droplet ficar ativo: o boot costuma levar ~40 s.
for _ in $(seq 36); do
  ip=$(api "$API/droplets/$droplet_id" | python3 -c 'import json,sys; d=json.load(sys.stdin)["droplet"]; print(next((n["ip_address"] for n in d["networks"]["v4"] if n["type"]=="public"),"") if d["status"]=="active" else "")')
  [ -n "$ip" ] && break; sleep 5
done
[ -n "$ip" ] || { echo "droplet não ficou ativo"; exit 1; }
# O sshd sobe alguns segundos depois do status active.
for _ in $(seq 24); do
  out=$(timeout 15s ssh -i "$dir/k" -o IdentitiesOnly=yes -o StrictHostKeyChecking=no -o UserKnownHostsFile=/dev/null \
    -o ConnectTimeout=5 "root@$ip" 'echo "kvm: $(ls -l /dev/kvm 2>&1)"; echo "flags vmx/svm: $(grep -cE "vmx|svm" /proc/cpuinfo)"; echo "cpu: $(grep -m1 "model name" /proc/cpuinfo)"' 2>/dev/null) && break
  sleep 5
done
echo "${out:-ssh não respondeu}"
