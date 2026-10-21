---
title: 'Kubernetes (k3s e microk8s)'
description: 'O rc como runtime do containerd: cada pod vira uma microVM da libkrun, com o IP do pod, exec, logs e Service'
---

Com o runtime `rc`, cada pod que pede `runtimeClassName: rc` roda numa **microVM da libkrun com kernel próprio**, em
vez de compartilhar o kernel do nó. Funciona no **k3s** e no **microk8s**, medido nos dois com a mesma prova de 19
checagens.

## Como funciona

```
kubelet ─CRI─▶ containerd ─▶ containerd-shim-rc-v2 (um por pod)
                                 │  sandbox (pause) → liga a VM do pod
                                 │  container da app → rootfs copiado para o share e chroot dentro da VM
                                 ▼
                         rc vmm (libkrun, KVM)  ◀── virtio-fs (share do pod) e virtio-net (TAP no netns do pod)
```

- **Uma VM por pod.** O container pause liga a VM; os containers da app rodam dentro dela, então dois containers do
  mesmo pod enxergam o mesmo kernel e o mesmo emptyDir.
- **Rede do CNI, sem NAT.** O shim cria um TAP no netns que o CNI deu ao pod e liga a `eth0` a ele com `tc mirred` nos
  dois sentidos. O guest assume o IP, o MAC e o gateway da `eth0`: para o cluster a VM é o pod (DNS, Service, pod a
  pod). Funciona com gateway na sub-rede (flannel do k3s) e fora dela (Calico do microk8s, `169.254.1.1`).
- **Rootfs copiado.** O snapshot que o containerd montou é copiado (`cp -a --reflink=auto`) para o virtio-fs do pod;
  reexportar o overlay pelo virtio-fs quebraria a escrita dentro do guest.
- **Exec, logs e TTY.** Cada processo é um stream com o guest: stdin, stdout, stderr, resize e sinais. `kubectl logs`
  sai das FIFOs do containerd como em qualquer runtime.

## Instalar

Precisa de nó Linux com `/dev/kvm` (KVM). No repositório, depois de `task krun:install` e `task build`:

```bash
sudo deploy/k3s/install.sh        # k3s: runtime em config-v3.toml.d/rc.toml, restart do k3s, RuntimeClass rc
sudo deploy/microk8s/install.sh   # microk8s: bloco no containerd-template.toml, restart do containerd do snap
```

Os dois instaladores chamam `deploy/install-node.sh` (rc e libkrun em `/opt/rc`, a imagem da VM do pod,
`/usr/local/bin/containerd-shim-rc-v2` e `/etc/rc/shim.json`), rotulam o nó com `runner.codes/runtime=true` e aplicam
`deploy/k8s/runtimeclass.yaml`. Reexecutar é seguro; depois de um upgrade da distribuição, rodar de novo.

## Usar

```yaml
apiVersion: v1
kind: Pod
metadata:
  name: rc-hello
  annotations:
    runner.codes/cpus: "1"          # opcional: vCPUs da VM (1 a 16, padrão 2)
    runner.codes/memory-mib: "512"  # opcional: teto de memória da VM (padrão 1024; a memória é elástica)
spec:
  runtimeClassName: rc
  containers:
    - name: hello
      image: busybox:1.36
      command: ["sh", "-c", "uname -sr; sleep 3600"]
```

`kubectl logs rc-hello` mostra o kernel da VM (`Linux 6.12.91`), não o do nó.

## Números medidos (droplet DigitalOcean, 4 vCPU, KVM aninhado)

| Medida | Valor |
|--------|-------|
| pod até `Running` (imagem já no nó) | ~2 s |
| prova `deploy/k8s/e2e.sh` | 19/19 no k3s 1.36 (containerd 2.3) e no microk8s 1.35 (containerd 2.1) |
| defeitos plantados no código do runtime (`task k8s:defects`) | 20, todos pegos pelos testes |

## Decisões de projeto

| Decisão | Por quê |
|---|---|
| **um shim por pod**, agrupado pela anotação `io.kubernetes.cri.sandbox-id` | sem o agrupamento, o containerd abriria um shim por container, e o container da app não acharia a VM do pod |
| **biblioteca containerd v2.3.7**, não a 2.4 | a 2.4 só aceita o protocolo novo de start (containerd ≥ 2.3); o microk8s 1.35 traz o containerd 2.1.6. A 2.3.7 lê os dois, e a resposta é traduzida para o JSON antigo só para quem chamou pelo caminho antigo |
| **rootfs copiado**, não reexportado | reexportar o overlay do snapshotter pelo virtio-fs quebra o copy-up dentro do guest; `cp -a --reflink=auto` é instantâneo em btrfs/xfs |
| **sem crun dentro da VM** | o guest do rc roda cada processo em chroot no rootfs do container; a VM inteira já é a fronteira de isolamento do pod |
| **TAP + `tc mirred`**, sem NAT nem gVisor | o guest assume IP, MAC e gateway da `eth0` do pod, então o resto do cluster (bridge, ARP, kube-proxy) não percebe diferença |
| **`Stats` com o uso real do vmm** | `Stats` vazio derruba o resumo de estatísticas do kubelet para o nó inteiro; o uso da VM vai todo para o primeiro container da app, para nada ser contado duas vezes |
| **filhos do shim esperados pelo reaper do containerd** | o pkg/shim colhe todo filho no SIGCHLD, e um `cmd.Wait` comum recebe `waitid: no child processes` |

## Como foi testado

- **Nós reais com KVM:** dois droplets da DigitalOcean (4 vCPU, 8 GB, Ubuntu 24.04), um com k3s 1.36 e outro com
  microk8s 1.35. O `/dev/kvm` foi medido antes (`task k8s:kvm-probe`).
- **Ponta a ponta:** `deploy/k8s/e2e.sh` (19 checagens) com **0 falhas** nos dois, duas rodadas seguidas em cada. Um
  defeito plantado no código de saída fez reprovar as duas checagens certas.
- **Unidade:** shim (`internal/adapters/k8sshim`) em 95,4% de cobertura e rede do pod (`internal/adapters/podnet`) em
  100%; o Attach real (netns, tap, tc) e o Landlock por thread rodam como root no Linux.
- **Defeitos plantados:** `task k8s:defects` planta 20 defeitos no código do runtime; os testes pegam todos.
- O que só apareceu no Linux real, e como foi corrigido, está no
  [relatório de testes](./test-report.md#linux-com-kvm-e-kubernetes).

## Reproduzir

```bash
K8S_NODE=k3s task k8s:node -- create         # droplet com KVM (~US$ 0,07/h enquanto existir)
K8S_NODE=k3s task k8s:node -- provision      # Nix, libkrun e build no nó
K8S_NODE=k3s task k8s:node -- ssh 'curl -sfL https://get.k3s.io | sh -'
task k8s:e2e -- k3s                          # reinstala o runtime e roda as 19 checagens
K8S_NODE=k3s task k8s:node -- destroy        # apaga o droplet e a chave
```

O mesmo com `K8S_NODE=microk8s` e `snap install microk8s --classic`. O passo a passo comentado está em
[`deploy/README.md`](https://github.com/andrebassi/runner-codes/blob/main/deploy/README.md).

## Limites

| O quê | Por quê |
|-------|---------|
| `kubectl port-forward` | o containerd disca `127.0.0.1` no netns do pod, e o programa escuta dentro da VM |
| `hostNetwork`, `hostIPC`, propagação de montagem | a VM tem kernel próprio |
| volume que muda depois (token de serviceaccount, configmap) | os volumes são copiados para o share na criação do container |
| limites por container, `kubectl top` por container | o limite vale para a VM inteira; o uso da VM vai para o primeiro container |
| Landlock/seccomp no vmm do pod | ainda não: o vmm do pod roda sem o confinamento das VMs de job |

A sequência de teste completa está no [runbook, seção 8x](https://github.com/andrebassi/runner-codes/blob/main/docs/runbook.md#8x-runtime-de-kubernetes-k3s-e-microk8s).
