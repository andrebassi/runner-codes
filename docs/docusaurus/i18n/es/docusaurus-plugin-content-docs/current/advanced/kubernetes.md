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
aplican la RuntimeClass `rc` (`rc_runtime_class`, en `deploy/lib.sh`). Reejecutar es seguro; tras actualizar la distribución, volver a correrlo.

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

## Decisiones de diseño

| Decisión | Por qué |
|---|---|
| **un shim por pod**, agrupado por la anotación `io.kubernetes.cri.sandbox-id` | sin agrupar, containerd abriría un shim por contenedor, y el contenedor de la app no encontraría la VM del pod |
| **biblioteca containerd v2.3.7**, no la 2.4 | la 2.4 solo acepta el protocolo nuevo de start (containerd ≥ 2.3); microk8s 1.35 trae containerd 2.1.6. La 2.3.7 lee los dos, y la respuesta se traduce al JSON antiguo solo para quien llamó por el camino antiguo |
| **rootfs copiado**, no reexportado | reexportar el overlay del snapshotter por virtio-fs rompe el copy-up dentro del guest; `cp -a --reflink=auto` es instantáneo en btrfs/xfs |
| **sin crun dentro de la VM** | el guest del rc corre cada proceso en chroot sobre el rootfs del contenedor; la VM entera ya es la frontera de aislamiento del pod |
| **TAP + `tc mirred`**, sin NAT ni gVisor | el guest asume IP, MAC y gateway de la `eth0` del pod, así que el resto del clúster (bridge, ARP, kube-proxy) no nota diferencia |
| **`Stats` con el uso real del vmm** | un `Stats` vacío rompe el resumen de estadísticas del kubelet en todo el nodo; el uso de la VM va todo al primer contenedor de la app, para no contar nada dos veces |
| **hijos del shim esperados por el reaper de containerd** | pkg/shim recoge todo hijo en el SIGCHLD, y un `cmd.Wait` normal recibe `waitid: no child processes` |

## Cómo se probó

- **Nodos reales con KVM:** dos droplets de DigitalOcean (4 vCPU, 8 GB, Ubuntu 24.04), uno con k3s 1.36 y otro con
  microk8s 1.35. El `/dev/kvm` se midió antes (`task k8s:kvm-probe`).
- **De extremo a extremo:** `deploy/k8s/e2e.sh` (19 comprobaciones) con **0 fallos** en los dos, dos ejecuciones
  seguidas en cada uno. Un defecto plantado en el código de salida hizo fallar justo las dos comprobaciones correctas.
- **Unidad:** el shim (`internal/adapters/k8sshim`) con 95,4% de cobertura y la red del pod
  (`internal/adapters/podnet`) con 100%; el Attach real (netns, tap, tc) y el Landlock por hilo corren como root en Linux.
- **Defectos plantados:** `task k8s:defects` planta 20 defectos en el código del runtime; los tests los atrapan todos.
- Lo que solo apareció en Linux real, y cómo se corrigió, está en el
  [informe de pruebas](./test-report.md#linux-com-kvm-e-kubernetes).

## Reproducir

```bash
K8S_NODE=k3s task k8s:node -- create         # droplet con KVM (~US$ 0,07/h mientras exista)
K8S_NODE=k3s task k8s:node -- provision      # Nix, libkrun y build en el nodo
K8S_NODE=k3s task k8s:node -- ssh 'curl -sfL https://get.k3s.io | sh -'
task k8s:e2e -- k3s                          # reinstala el runtime y corre las 19 comprobaciones
K8S_NODE=k3s task k8s:node -- destroy        # borra el droplet y la clave
```

Lo mismo con `K8S_NODE=microk8s` y `snap install microk8s --classic`. El paso a paso comentado está en
[`deploy/README.md`](https://github.com/andrebassi/runner-codes/blob/main/deploy/README.md).

## Límites

| Qué | Por qué |
|-----|---------|
| `kubectl port-forward` | containerd marca `127.0.0.1` en el netns del pod, y el programa escucha dentro de la VM |
| `hostNetwork`, `hostIPC`, propagación de montajes | la VM tiene kernel propio |
| volúmenes que cambian después (token de serviceaccount, configmap) | los volúmenes se copian al share al crear el contenedor |
| límites por contenedor, `kubectl top` por contenedor | el límite vale para toda la VM; el uso de la VM va al primer contenedor |
| Landlock/seccomp en el vmm del pod | todavía no: el vmm del pod corre sin el confinamiento de las VMs de job |

La secuencia de prueba completa está en el [runbook, sección 8x](https://github.com/andrebassi/runner-codes/blob/main/docs/runbook.es.md#8x-runtime-de-kubernetes-k3s-y-microk8s).
