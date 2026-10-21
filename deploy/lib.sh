#!/bin/bash
# Peças comuns aos instaladores (deploy/k3s, deploy/microk8s) e ao e2e (deploy/k8s/e2e.sh) do runtime rc no
# Kubernetes. Só define funções: quem usa faz `source deploy/lib.sh`.

# rc_runtime_class imprime a RuntimeClass `rc`, que manda o pod ao runtime do rc no containerd.
#   handler: o nome do runtime no config do containerd (a tabela runtimes.rc que rc_cri_table nomeia);
#   overhead: o que a VM custa além dos containers (processo vmm + kernel), para o scheduler reservar;
#   nodeSelector: só os nós em que o instalador pôs o rótulo runner.codes/runtime=true.
rc_runtime_class() {
  cat <<'YAML'
apiVersion: node.k8s.io/v1
kind: RuntimeClass
metadata:
  name: rc
handler: rc
overhead:
  podFixed:
    cpu: 250m
    memory: 160Mi
scheduling:
  nodeSelector:
    runner.codes/runtime: "true"
YAML
}

# rc_demo_pod imprime o pod de prova `rc-hello`: busybox numa microVM de 1 vCPU e 512 MiB que escreve no log
# um marcador, o kernel e o IPv4 da eth0, e fica de pé para o exec. O e2e confere as três linhas.
rc_demo_pod() {
  cat <<'YAML'
apiVersion: v1
kind: Pod
metadata:
  name: rc-hello
  annotations:
    runner.codes/cpus: "1"
    runner.codes/memory-mib: "512"
spec:
  runtimeClassName: rc
  restartPolicy: Never
  containers:
    - name: hello
      image: busybox:1.36
      command:
        - sh
        - -c
        - >-
          echo RC_K8S_OK;
          echo "kernel: $(uname -s) $(uname -r)";
          echo "ip: $(ip -4 -o addr show dev eth0 | awk '{print $4}')";
          exec sleep 3600
YAML
}

# rc_cri_table imprime o cabeçalho da tabela TOML do runtime rc para o config do containerd em $1. O nome do
# plugin CRI depende da versão do formato, que o próprio arquivo declara (`version = N`):
#   3 (containerd 2.x com config v3, k3s 1.36): io.containerd.cri.v1.runtime;
#   2 (config v2, o template do microk8s 1.35): io.containerd.grpc.v1.cri.
# Outra versão sai com erro: inventar um cabeçalho deixaria o runtime fora do CRI sem nada acusar.
rc_cri_table() {
  local v
  # A linha `version = 3` (espaços opcionais, comentário depois permitido); a primeira que aparecer vale.
  v=$(sed -n 's/^[[:space:]]*version[[:space:]]*=[[:space:]]*\([0-9][0-9]*\).*/\1/p' "$1" | head -1)
  case "$v" in
    3) echo "[plugins.'io.containerd.cri.v1.runtime'.containerd.runtimes.rc]" ;;
    2) echo "[plugins.'io.containerd.grpc.v1.cri'.containerd.runtimes.rc]" ;;
    *) echo "versão do config do containerd não suportada em $1: '${v:-ausente}'" >&2; return 1 ;;
  esac
}
