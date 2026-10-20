#!/bin/bash
# Liga o runtime do rc ao containerd embutido do k3s: instala as peças (deploy/install-node.sh), põe o
# runtime "rc" num arquivo do diretório de imports do containerd do k3s, reinicia o k3s, rotula o nó e
# aplica a RuntimeClass. Reexecutar é seguro; depois de um upgrade do k3s, rodar de novo.
#
# Uso (root, no nó): deploy/k3s/install.sh
set -euo pipefail
cd "$(dirname "$0")/../.."
deploy/install-node.sh
K3S_CD=/var/lib/rancher/k3s/agent/etc/containerd
# O k3s 1.36 (containerd 2.3) gera o config-v3.toml com imports = [config-v3.toml.d/*.toml]: o runtime
# entra por ali, sem tocar no template (onde moram mirrors e auth de registry de quem administra).
gen="$K3S_CD/config-v3.toml"
[ -f "$gen" ] || gen="$K3S_CD/config.toml"
grep -q 'config-v3.toml.d' "$gen" || { echo "o config do k3s não importa config-v3.toml.d/: k3s antigo? ($gen)" >&2; exit 1; }
# O cabeçalho do plugin CRI muda entre versões do containerd: copia o do runc que o próprio k3s gerou.
hdr=$(grep -E "\.containerd\.runtimes\.runc\][[:space:]]*$" "$gen" | head -1)
[ -n "$hdr" ] || { echo "não achei o runtime runc em $gen" >&2; exit 1; }
install -d "$K3S_CD/config-v3.toml.d"
cat >"$K3S_CD/config-v3.toml.d/rc.toml" <<TOML
# Runtime do rc (microVM por pod): deploy/k3s/install.sh. Sem a tabela [options]: as do runc
# (BinaryName, SystemdCgroup) não valem para este shim.
${hdr/.runtimes.runc]/.runtimes.rc]}
  runtime_type = "$(printf 'io.containerd.rc.v2')"
  # As anotações runner.codes/* do pod (tamanho da VM) chegam à spec do sandbox.
  pod_annotations = ["runner.codes/*"]
TOML
systemctl restart k3s
# Até 2 min para o apiserver voltar depois do restart.
for _ in $(seq 60); do k3s kubectl get --raw /readyz >/dev/null 2>&1 && break; sleep 2; done
k3s kubectl label node --all runner.codes/runtime=true --overwrite
k3s kubectl apply -f deploy/k8s/runtimeclass.yaml
echo "runtime rc ligado ao k3s; prova: deploy/k3s/e2e.sh"
