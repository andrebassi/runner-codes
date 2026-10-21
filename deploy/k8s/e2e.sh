#!/bin/bash
# Prova de ponta a ponta do runtime rc num cluster (k3s ou microk8s): pod numa microVM, logs, exec com
# e sem TTY, stdin, DNS do cluster, rede entre pod rc e pod runc nos dois sentidos, Service, código de
# saída, dois containers no mesmo pod e a limpeza no delete. Não aborta na primeira falha: conta.
#
# Uso (root, no nó): deploy/k8s/e2e.sh k3s   ou   deploy/k8s/e2e.sh microk8s
set -u
cd "$(dirname "$0")/../.."
# shellcheck source=deploy/lib.sh
source deploy/lib.sh
# A distribuição escolhe o kubectl embutido dela (k3s kubectl / microk8s kubectl).
dist=${1:-k3s}
case "$dist" in k3s | microk8s) K=("$dist" kubectl) ;; *) echo "uso: $0 k3s|microk8s" >&2; exit 2 ;; esac
fails=0
# kc é o kubectl com teto: um comando pendurado não trava a prova inteira.
kc() { timeout 60s "${K[@]}" "$@"; }
# expect confere uma condição e conta a falha sem abortar.
expect() {
  local what=$1; shift
  if "$@"; then echo "   ok    $what"; else echo "   FAIL  $what"; fails=$((fails + 1)); fi
}
ns=rc-e2e
kc delete namespace "$ns" --wait=true >/dev/null 2>&1
kc create namespace "$ns" >/dev/null
trap 'kc delete namespace "$ns" --wait=false >/dev/null 2>&1' EXIT

echo "== pod numa microVM"
rc_demo_pod | kc -n "$ns" apply -f - >/dev/null
expect "Ready em até 120 s" timeout 130s "${K[@]}" -n "$ns" wait --for=condition=Ready pod/rc-hello --timeout=120s
logs=$(kc -n "$ns" logs rc-hello)
expect "logs com RC_K8S_OK" grep -q RC_K8S_OK <<<"$logs"
kernel=$(sed -n 's/^kernel: Linux //p' <<<"$logs")
expect "kernel da VM ($kernel) é outro que o do nó ($(uname -r))" test -n "$kernel" -a "$kernel" != "$(uname -r)"
podip=$(kc -n "$ns" get pod rc-hello -o jsonpath='{.status.podIP}')
expect "IP do pod ($podip) é o da eth0 da VM" grep -q "ip: $podip/" <<<"$logs"

# Trava do defeito de 20/10/2026: Stats vazio do shim derrubava o resumo do kubelet do nó inteiro
# (eviction cego, metrics-server sem dado, namespace que não apagava).
node=$(kc get nodes -o jsonpath='{.items[0].metadata.name}')
expect "resumo de estatísticas do kubelet responde com pod rc rodando" sh -c "timeout 60s ${K[*]} get --raw /api/v1/nodes/$node/proxy/stats/summary | grep -q '\"rc-hello\"'"

echo "== exec"
expect "exec sem TTY (uid 0)" sh -c "timeout 60s ${K[*]} -n $ns exec rc-hello -- id | grep -q 'uid=0'"
# O kubectl só pede TTY se o stdin dele for terminal: o script (util-linux) dá um PTY ao comando.
expect "exec com TTY" sh -c "script -qec \"timeout 60s ${K[*]} -n $ns exec -it rc-hello -- sh -c 'test -t 1 && echo IS-TTY'\" /dev/null </dev/null | grep -q IS-TTY"
expect "exec com stdin" sh -c "echo from-stdin | timeout 60s ${K[*]} -n $ns exec -i rc-hello -- cat | grep -q from-stdin"
expect "código de saída do exec (7)" sh -c "timeout 60s ${K[*]} -n $ns exec rc-hello -- sh -c 'exit 7'; [ \$? = 7 ]"

echo "== rede"
expect "DNS do cluster (kubernetes.default)" sh -c "timeout 60s ${K[*]} -n $ns exec rc-hello -- nslookup kubernetes.default.svc.cluster.local 2>&1 | grep -q 'Address'"
kc -n "$ns" run web --image=busybox:1.36 --restart=Never --port=8080 -- sh -c 'mkdir -p /w && echo hello-from-runc >/w/index.html && httpd -f -p 8080 -h /w' >/dev/null
kc -n "$ns" wait --for=condition=Ready pod/web --timeout=60s >/dev/null
web=$(kc -n "$ns" get pod web -o jsonpath='{.status.podIP}')
expect "pod rc alcança pod runc ($web)" sh -c "timeout 60s ${K[*]} -n $ns exec rc-hello -- wget -qO- -T 5 http://$web:8080/ | grep -q hello-from-runc"
kc -n "$ns" exec rc-hello -- sh -c 'mkdir -p /srv && echo hello-from-rc >/srv/index.html && (httpd -p 8081 -h /srv &)' >/dev/null 2>&1
expect "pod runc alcança pod rc ($podip)" sh -c "timeout 60s ${K[*]} -n $ns exec web -- wget -qO- -T 5 http://$podip:8081/ | grep -q hello-from-rc"
kc -n "$ns" label pod rc-hello app=rc-hello >/dev/null
kc -n "$ns" expose pod rc-hello --name rc-svc --port 80 --target-port 8081 >/dev/null
# Até 30 s (15 x 2 s): o Service recém-criado só responde depois que o kube-proxy programa a regra. No k3s isso
# passa de 1 s, e uma tentativa só reprovava sempre num cluster novo (21/10/2026), com o caminho funcionando.
expect "Service ClusterIP até o pod rc" sh -c "for i in \$(seq 15); do timeout 60s ${K[*]} -n $ns exec web -- wget -qO- -T 5 http://rc-svc.$ns.svc.cluster.local/ 2>/dev/null | grep -q hello-from-rc && exit 0; sleep 2; done; exit 1"

echo "== código de saída e dois containers"
cat <<'YAML' | kc -n "$ns" apply -f - >/dev/null
apiVersion: v1
kind: Pod
metadata: {name: rc-exit}
spec:
  runtimeClassName: rc
  restartPolicy: Never
  containers: [{name: c, image: busybox:1.36, command: ["sh", "-c", "exit 3"]}]
---
apiVersion: v1
kind: Pod
metadata: {name: rc-two}
spec:
  runtimeClassName: rc
  restartPolicy: Never
  volumes: [{name: shared, emptyDir: {}}]
  containers:
    - {name: a, image: busybox:1.36, command: ["sh", "-c", "echo from-a >/data/a; sleep 3600"], volumeMounts: [{name: shared, mountPath: /data}]}
    - {name: b, image: busybox:1.36, command: ["sh", "-c", "sleep 3600"], volumeMounts: [{name: shared, mountPath: /data}]}
YAML
for _ in $(seq 60); do
  code=$(kc -n "$ns" get pod rc-exit -o jsonpath='{.status.containerStatuses[0].state.terminated.exitCode}' 2>/dev/null)
  [ -n "$code" ] && break; sleep 2
done
expect "código de saída do container (3)" test "${code:-}" = 3
kc -n "$ns" wait --for=condition=Ready pod/rc-two --timeout=120s >/dev/null
sleep 2
expect "dois containers no mesmo pod veem o mesmo emptyDir" sh -c "timeout 60s ${K[*]} -n $ns exec rc-two -c b -- cat /data/a | grep -q from-a"
ba=$(kc -n "$ns" exec rc-two -c a -- cat /proc/sys/kernel/random/boot_id 2>/dev/null)
bb=$(kc -n "$ns" exec rc-two -c b -- cat /proc/sys/kernel/random/boot_id 2>/dev/null)
expect "os dois containers estão na mesma VM (mesmo boot_id do kernel, ${ba:-vazio})" test -n "$ba" -a "$ba" = "$bb"
hb=$(cat /proc/sys/kernel/random/boot_id)
expect "e não no kernel do nó" test "$ba" != "$hb"

echo "== limpeza"
before=$(ls /var/lib/rc-shim/pods 2>/dev/null | wc -l)
kc delete namespace "$ns" --wait=true >/dev/null 2>&1
trap - EXIT
sleep 3
expect "nenhum diretório de pod sobrou (antes: $before)" sh -c "[ -z \"\$(ls /var/lib/rc-shim/pods 2>/dev/null)\" ]"
# pgrep -x no nome do binário (e não -f no texto): -f casava com a própria linha do sh deste teste.
expect "nenhum processo vmm de pod sobrou" sh -c "! pgrep -a rc | grep -q 'vmm --spec /var/lib/rc-shim'"

echo
echo "falhas: $fails"
exit "$fails"
