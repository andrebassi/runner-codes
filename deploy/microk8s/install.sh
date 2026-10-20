#!/bin/bash
# Liga o runtime do rc ao containerd do microk8s: instala as peças (deploy/install-node.sh), acrescenta o
# runtime "rc" ao containerd-template.toml do snap (uma vez, entre marcadores), reinicia o containerd do
# microk8s, rotula o nó e aplica a RuntimeClass. Reexecutar é seguro; depois de um refresh do snap que
# reescreva o template, rodar de novo.
#
# Uso (root, no nó): deploy/microk8s/install.sh
set -euo pipefail
cd "$(dirname "$0")/../.."
command -v microk8s >/dev/null || { echo "microk8s não instalado (snap install microk8s --classic)" >&2; exit 1; }
deploy/install-node.sh
tpl=/var/snap/microk8s/current/args/containerd-template.toml
[ -f "$tpl" ] || { echo "template do containerd do microk8s não achado: $tpl" >&2; exit 1; }
# O cabeçalho do plugin CRI depende da versão do config (2 no microk8s 1.35, com containerd 2.1): copia o do
# runc que o próprio template declara.
hdr=$(grep -E "\.containerd\.runtimes\.runc\][[:space:]]*$" "$tpl" | head -1 | sed 's/^[[:space:]]*//')
[ -n "$hdr" ] || { echo "não achei o runtime runc em $tpl" >&2; exit 1; }
begin="# rc:begin (deploy/microk8s/install.sh)"
end="# rc:end"
# Tira o bloco de uma instalação anterior e põe o atual no fim: tabela TOML com o caminho completo vale
# em qualquer ponto do arquivo, e o resto do template (de quem administra o cluster) fica intocado.
# Delimitador | no sed: o marcador tem "/" (o caminho deste script) e quebrava o endereço com barra.
sed -i "\\|^$begin\$|,\\|^$end\$|d" "$tpl"
cat >>"$tpl" <<TOML
$begin
# Runtime do rc (microVM por pod). Sem a tabela [options]: as do runc não valem para este shim.
${hdr/.runtimes.runc]/.runtimes.rc]}
  runtime_type = "$(printf 'io.containerd.rc.v2')"
  # As anotações runner.codes/* do pod (tamanho da VM) chegam à spec do sandbox.
  pod_annotations = ["runner.codes/*"]
$end
TOML
# O containerd do microk8s 1.35 às vezes cai na 1ª subida com "concurrent map read and map write" no
# plugin sideload do snap (medido em 20/10/2026, nada do rc no trace); o systemd religa sozinho, mas o
# snap restart já devolveu erro. Por isso não aborta aqui: espera o serviço ficar ativo (até 60 s).
snap restart microk8s.daemon-containerd || echo "snap restart falhou; esperando o systemd religar o containerd"
for _ in $(seq 30); do systemctl is-active --quiet snap.microk8s.daemon-containerd && break; sleep 2; done
systemctl is-active --quiet snap.microk8s.daemon-containerd || { echo "containerd do microk8s não voltou" >&2; exit 1; }
timeout 300s microk8s status --wait-ready >/dev/null
microk8s kubectl label node --all runner.codes/runtime=true --overwrite
microk8s kubectl apply -f deploy/k8s/runtimeclass.yaml
echo "runtime rc ligado ao microk8s; prova: deploy/k8s/e2e.sh microk8s"
