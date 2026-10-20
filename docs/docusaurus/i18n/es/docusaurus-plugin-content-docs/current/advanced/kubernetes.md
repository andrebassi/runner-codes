---
title: 'Kubernetes (k3s y microk8s)'
description: 'El rc como runtime de containerd: cada pod se vuelve una microVM de libkrun, con la IP del pod, exec, logs y Services'
---

Con el runtime `rc`, cada pod que pide `runtimeClassName: rc` corre en una **microVM de libkrun con kernel propio**
en lugar de compartir el kernel del nodo. Funciona en **k3s** y **microk8s**, medido en los dos con la misma prueba
de 19 comprobaciones.

## Cómo funciona

```
kubelet ─CRI─▶ containerd ─▶ containerd-shim-rc-v2 (uno por pod)
                                 │  sandbox (pause) → arranca la VM del pod
                                 │  contenedor de la app → rootfs copiado al share y chroot dentro de la VM
                                 ▼
                         rc vmm (libkrun, KVM)  ◀── virtio-fs (share del pod) y virtio-net (TAP en el netns del pod)
```

- **Una VM por pod.** El contenedor pause arranca la VM; los contenedores de la app corren dentro de ella, así que
  dos contenedores del mismo pod ven el mismo kernel y el mismo emptyDir.
- **Red del CNI, sin NAT.** El shim crea un TAP en el netns que el CNI le dio al pod y une `eth0` a él con
  `tc mirred` en los dos sentidos. El guest asume la IP, la MAC y el gateway de `eth0`: para el clúster la VM es el
  pod (DNS, Services, pod a pod). Funciona con el gateway dentro de la subred (flannel de k3s) y fuera de ella
  (Calico de microk8s, `169.254.1.1`).
- **Rootfs copiado.** El snapshot que containerd montó se copia (`cp -a --reflink=auto`) al virtio-fs del pod;
  reexportar el overlay por virtio-fs rompería la escritura dentro del guest.
- **Exec, logs y TTY.** Cada proceso es un stream con el guest: stdin, stdout, stderr, resize y señales.
  `kubectl logs` sale de las FIFOs de containerd como en cualquier runtime.

## Instalar

Necesita un nodo Linux con `/dev/kvm` (KVM). En el repositorio, tras `task krun:install` y `task build`:

```bash
sudo deploy/k3s/install.sh        # k3s: runtime en config-v3.toml.d/rc.toml, restart de k3s, RuntimeClass rc
sudo deploy/microk8s/install.sh   # microk8s: bloque en containerd-template.toml, restart del containerd del snap
```

Los dos instaladores llaman a `deploy/install-node.sh` (rc y libkrun en `/opt/rc`, la imagen de la VM del pod,
`/usr/local/bin/containerd-shim-rc-v2` y `/etc/rc/shim.json`), etiquetan el nodo con `runner.codes/runtime=true` y
aplican `deploy/k8s/runtimeclass.yaml`. Reejecutar es seguro; tras actualizar la distribución, volver a correrlo.

## Usar

```yaml
apiVersion: v1
kind: Pod
metadata:
  name: rc-hello
  annotations:
    runner.codes/cpus: "1"          # opcional: vCPUs de la VM (1 a 16, por defecto 2)
    runner.codes/memory-mib: "512"  # opcional: techo de memoria de la VM (por defecto 1024; la memoria es elástica)
spec:
  runtimeClassName: rc
  containers:
    - name: hello
      image: busybox:1.36
      command: ["sh", "-c", "uname -sr; sleep 3600"]
```

`kubectl logs rc-hello` muestra el kernel de la VM (`Linux 6.12.91`), no el del nodo.

## Números medidos (droplet de DigitalOcean, 4 vCPU, KVM anidado)

| Medida | Valor |
|--------|-------|
| pod hasta `Running` (imagen ya en el nodo) | ~2 s |
| prueba `deploy/k8s/e2e.sh` | 19/19 en k3s 1.36 (containerd 2.3) y microk8s 1.35 (containerd 2.1) |
| defectos plantados en el código del runtime (`task k8s:defects`) | 20, todos atrapados por los tests |

## Límites

| Qué | Por qué |
|-----|---------|
| `kubectl port-forward` | containerd marca `127.0.0.1` en el netns del pod, y el programa escucha dentro de la VM |
| `hostNetwork`, `hostIPC`, propagación de montajes | la VM tiene kernel propio |
| volúmenes que cambian después (token de serviceaccount, configmap) | los volúmenes se copian al share al crear el contenedor |
| límites por contenedor, `kubectl top` por contenedor | el límite vale para toda la VM; el uso de la VM va al primer contenedor |
| Landlock/seccomp en el vmm del pod | todavía no: el vmm del pod corre sin el confinamiento de las VMs de job |

La secuencia de prueba completa está en el runbook, sección 8x.
