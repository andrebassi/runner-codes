#!/bin/bash
# Mede se uma Fly Machine expõe virtualização aninhada (/dev/kvm e as flags vmx/svm da CPU): decide se o rc roda as
# MicroVMs dos visitantes dentro do Fly ou se cada visitante vira uma Fly Machine. Cria um app descartável, roda uma
# máquina que só olha e sai, lê o log e apaga o app. O token sai do pass e nunca é impresso.
set -u
# ~/.local/bin antes do perfil do Nix: lá está o roteador gpg2 do pass (zfold/YubiKey); o gpg2 do Nix na frente manda
# o pass ao agente local e o pedido de aprovação nunca chega ao celular (task check:path reprova a ordem invertida).
export PATH="$HOME/.local/bin:$HOME/.nix-profile/bin:$PATH"
# pass pelo roteador ~/.local/bin/gpg2 (zfold ou YubiKey): o gpg2 do perfil do Nix iria direto ao agente local e o
# pedido de aprovação nunca chegaria ao celular (medido em 20/10/2026).
pass_cmd() { PATH="$HOME/.local/bin:/usr/bin:/bin:$PATH" pass "$@"; }
FLY_API_TOKEN=$(pass_cmd show bassi/flyio/api-token) || { echo "pass falhou"; exit 1; }
export FLY_API_TOKEN
APP=rc-kvm-probe-$$
trap 'timeout 60s fly apps destroy "$APP" --yes >/dev/null 2>&1; echo "app $APP apagado"' EXIT
timeout 60s fly apps create "$APP" --org personal >/dev/null || { echo "não criou o app"; exit 1; }
# A máquina imprime o que interessa e sai; --rm a apaga ao terminar.
timeout 180s fly machine run alpine:3.20 --app "$APP" --region gru --rm --restart no \
  -- sh -c 'echo "kvm: $(ls -l /dev/kvm 2>&1)"; echo "flags vmx/svm: $(grep -cE "vmx|svm" /proc/cpuinfo)"; echo "cpu: $(grep -m1 "model name" /proc/cpuinfo)"; sleep 5' >/dev/null 2>&1
sleep 8
timeout 60s fly logs --app "$APP" --no-tail 2>/dev/null | grep -E 'kvm:|flags vmx|cpu:' | sed 's/^.*app\[[^]]*\] [a-z]* \[[a-z]*\] //'
