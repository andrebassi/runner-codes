#!/bin/bash
# Prova do runtime de Kubernetes a partir do Mac: leva a árvore de trabalho ao nó de teste da distribuição
# (scripts/k8s-node.sh), compila lá, reinstala o runtime (deploy/<dist>/install.sh) e roda o e2e
# (deploy/k8s/e2e.sh). O nó tem de existir: K8S_NODE=<dist> task k8s:node -- create && ... provision, e a
# distribuição instalada nele (k3s pelo get.k3s.io, microk8s pelo snap).
#
# Uso: scripts/k8s-e2e.sh k3s|microk8s   (sai com o número de falhas do e2e)
set -u
cd "$(dirname "$0")/.."
dist=${1:-}
case "$dist" in k3s | microk8s) ;; *) echo "uso: $0 k3s|microk8s" >&2; exit 2 ;; esac
export K8S_NODE=$dist
./scripts/k8s-node.sh status | grep -q droplet || { echo "nó $dist não existe (K8S_NODE=$dist task k8s:node -- create)" >&2; exit 2; }
./scripts/k8s-node.sh shim || exit 1
RSSH_TIMEOUT=900 ./scripts/k8s-node.sh ssh "cd /root/rc && deploy/$dist/install.sh >/tmp/rc-install.log 2>&1 || { tail -20 /tmp/rc-install.log; exit 1; }; deploy/k8s/e2e.sh $dist"
