#!/usr/bin/env bash
# Desativação da infraestrutura do runner antigo (Firecracker numa EC2 bare metal).
# Confere a instância registrada em aws/.instance-id e, com --stop, para (stop, reversível)
# o que estiver rodando e for do projeto. Nunca termina instância. Credencial vem do pass.
# Uso: scripts/legacy-ec2.sh [--stop]
set -uo pipefail
cd "$(dirname "$0")/.."
id=$(cat aws/.instance-id 2>/dev/null || true)
mode=${1:-}
export AWS_DEFAULT_REGION=us-east-1
AWS_ACCESS_KEY_ID=$(pass show bassi/runner-codes/aws-access-key) || { echo "pass failed"; exit 1; }
AWS_SECRET_ACCESS_KEY=$(pass show bassi/runner-codes/aws-secret-key) || { echo "pass failed"; exit 1; }
export AWS_ACCESS_KEY_ID AWS_SECRET_ACCESS_KEY

echo "== account"
# Sem identidade válida, tudo abaixo falha e um "(none)" seria lido como "não há
# instâncias" (aconteceu em 19/10/2026 com a chave desativada). Para aqui.
if ! timeout 30s aws sts get-caller-identity --query '[Account,Arn]' --output text; then
  echo "🛑 invalid credential; nothing was queried"
  exit 1
fi

if [ -n "$id" ]; then
  echo "== registered instance ($id)"
  timeout 30s aws ec2 describe-instances --instance-ids "$id" \
    --query 'Reservations[].Instances[].[InstanceId,State.Name,InstanceType,LaunchTime,Tags[?Key==`Name`]|[0].Value]' \
    --output text 2>&1
fi

echo "== non-terminated instances in the region"
list=$(timeout 30s aws ec2 describe-instances \
  --filters Name=instance-state-name,Values=pending,running,stopping,stopped \
  --query 'Reservations[].Instances[].[InstanceId,State.Name,InstanceType,LaunchTime,Tags[?Key==`Name`]|[0].Value]' \
  --output text)
echo "${list:-(none)}"

if [ "$mode" = "--stop" ]; then
  # Só o que está rodando e é do projeto: o id registrado ou Name com runner/firesandbox.
  ids=$(echo "$list" | awk -v id="$id" '$2=="running" && ($1==id || tolower($5) ~ /runner|firesandbox/) {print $1}')
  if [ -z "$ids" ]; then
    echo "== nothing from the project is running"
  else
    echo "== stopping: $ids"
    timeout 30s aws ec2 stop-instances --instance-ids $ids \
      --query 'StoppingInstances[].[InstanceId,PreviousState.Name,CurrentState.Name]' --output text
    # Bare metal demora para parar; consulta a cada 10 s por até 4 min.
    for _ in $(seq 1 24); do
      states=$(timeout 30s aws ec2 describe-instances --instance-ids $ids --query 'Reservations[].Instances[].State.Name' --output text)
      echo "  state: $states"
      [ -z "$(echo "$states" | tr '\t' '\n' | grep -v '^stopped$')" ] && break
      sleep 10
    done
  fi
  echo "== EBS volumes of the project instances (still billed while the instance is stopped)"
  filter=$(echo "$ids $id" | tr ' ' '\n' | grep . | sort -u | paste -sd, -)
  [ -n "$filter" ] && timeout 30s aws ec2 describe-volumes --filters "Name=attachment.instance-id,Values=$filter" \
    --query 'Volumes[].[VolumeId,Size,VolumeType,State]' --output text
fi
unset AWS_ACCESS_KEY_ID AWS_SECRET_ACCESS_KEY
