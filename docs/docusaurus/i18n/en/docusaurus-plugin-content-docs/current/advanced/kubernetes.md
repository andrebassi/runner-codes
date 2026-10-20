---
title: 'Kubernetes (k3s and microk8s)'
description: 'rc as a containerd runtime: every pod becomes a libkrun microVM, with the pod IP, exec, logs and Services'
---

With the `rc` runtime, every pod that asks for `runtimeClassName: rc` runs in a **libkrun microVM with its own
kernel** instead of sharing the node kernel. It works on **k3s** and **microk8s**, measured on both with the same
19-check test.

## How it works

```
kubelet ─CRI─▶ containerd ─▶ containerd-shim-rc-v2 (one per pod)
                                 │  sandbox (pause) → boots the pod VM
                                 │  app container → rootfs copied into the share, chroot inside the VM
                                 ▼
                         rc vmm (libkrun, KVM)  ◀── virtio-fs (pod share) and virtio-net (TAP in the pod netns)
```

- **One VM per pod.** The pause container boots the VM; the app containers run inside it, so two containers in the
  same pod see the same kernel and the same emptyDir.
- **CNI networking, no NAT.** The shim creates a TAP in the netns the CNI gave the pod and links `eth0` to it with
  `tc mirred` both ways. The guest takes over the IP, MAC and gateway of `eth0`: to the cluster the VM is the pod
  (DNS, Services, pod to pod). It works with the gateway inside the subnet (k3s flannel) and outside it (microk8s
  Calico, `169.254.1.1`).
- **Copied rootfs.** The snapshot containerd mounted is copied (`cp -a --reflink=auto`) into the pod virtio-fs;
  re-exporting the overlay through virtio-fs would break writes inside the guest.
- **Exec, logs and TTY.** Every process is a stream with the guest: stdin, stdout, stderr, resize and signals.
  `kubectl logs` comes from containerd's FIFOs as with any runtime.

## Install

It needs a Linux node with `/dev/kvm` (KVM). In the repository, after `task krun:install` and `task build`:

```bash
sudo deploy/k3s/install.sh        # k3s: runtime in config-v3.toml.d/rc.toml, k3s restart, RuntimeClass rc
sudo deploy/microk8s/install.sh   # microk8s: block in containerd-template.toml, snap containerd restart
```

Both installers call `deploy/install-node.sh` (rc and libkrun in `/opt/rc`, the pod VM image,
`/usr/local/bin/containerd-shim-rc-v2` and `/etc/rc/shim.json`), label the node `runner.codes/runtime=true` and apply
`deploy/k8s/runtimeclass.yaml`. Re-running is safe; after a distribution upgrade, run it again.

## Use

```yaml
apiVersion: v1
kind: Pod
metadata:
  name: rc-hello
  annotations:
    runner.codes/cpus: "1"          # optional: VM vCPUs (1 to 16, default 2)
    runner.codes/memory-mib: "512"  # optional: VM memory ceiling (default 1024; memory is elastic)
spec:
  runtimeClassName: rc
  containers:
    - name: hello
      image: busybox:1.36
      command: ["sh", "-c", "uname -sr; sleep 3600"]
```

`kubectl logs rc-hello` shows the VM kernel (`Linux 6.12.91`), not the node's.

## Measured numbers (DigitalOcean droplet, 4 vCPU, nested KVM)

| Measure | Value |
|---------|-------|
| pod to `Running` (image already on the node) | ~2 s |
| `deploy/k8s/e2e.sh` test | 19/19 on k3s 1.36 (containerd 2.3) and microk8s 1.35 (containerd 2.1) |
| defects planted in the runtime code (`task k8s:defects`) | 20, all caught by the tests |

## Limits

| What | Why |
|------|-----|
| `kubectl port-forward` | containerd dials `127.0.0.1` in the pod netns, and the program listens inside the VM |
| `hostNetwork`, `hostIPC`, mount propagation | the VM has its own kernel |
| volumes that change later (serviceaccount token, configmap) | volumes are copied into the share when the container is created |
| per-container limits, per-container `kubectl top` | the limit applies to the whole VM; the VM usage goes to the first container |
| Landlock/seccomp on the pod vmm | not yet: the pod vmm runs without the confinement of job VMs |

The full test sequence is in the runbook, section 8x.
