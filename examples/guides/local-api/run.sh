#!/usr/bin/env bash
# Guia local-api: o ciclo de vida de uma máquina pela API, com os aliases de corpo e rota
# (network: true, memoryMb, cmd, files/%2F...), contra um `rc serve` de verdade: create,
# start, exec (e o código de saída), exec em streaming (SSE), arquivo ida e volta, stop,
# delete e o NOT_FOUND no topo do corpo.
#
# O estado depois do create é "stopped"; o erro vem em {"error":{"code","message"},"code":...};
# o serve é `rc serve --listen`, sem socket Unix.
source "$(dirname "$0")/../lib.sh"
name=apidoc-$$
port=$((19800 + RANDOM % 150))
base="http://127.0.0.1:$port"
"$RC" serve --listen "127.0.0.1:$port" --pool-size 0 >/dev/null 2>&1 &
srv=$!
trap 'kill $srv 2>/dev/null; cleanup_prefix apidoc-' EXIT
api() { curl -s --max-time 120 "$@"; }
jget() { python3 -c 'import json,sys;d=json.load(sys.stdin);print(d.get(sys.argv[1],""))' "$1"; }
for _ in $(seq 1 50); do api -o /dev/null "$base/health" && break; sleep 0.2; done

step "serve-start"
check "GET /health" api -f "$base/health"

step "campos do corpo"
typo=$(api -o /dev/null -w '%{http_code}' -X POST "$base/api/v1/machines" -H 'content-type: application/json' -d '{"name":"apidoc-typo","image":"alpine","allowedHost":["a.com"]}')
check "campo desconhecido recusado com 400 (allowedHost)" test "$typo" = 400
aliased=$(api -X POST "$base/api/v1/machines" -H 'content-type: application/json' -d '{"name":"apidoc-al-'$$'","image":"alpine","allowedHosts":["api.example.com"]}')
check "o alias allowedHosts vira allow_hosts e liga a rede" sh -c "grep -q '\"allow_hosts\":\\[\"api.example.com\"\\]' <<<'$aliased' && grep -q '\"enabled\":true' <<<'$aliased'"

step "lifecycle-check (corpos com aliases)"
created=$(api -X POST "$base/api/v1/machines" -H 'content-type: application/json' \
	-d "{\"name\":\"$name\",\"image\":\"python:3.12-alpine\",\"network\":true,\"memoryMb\":2048,\"cmd\":[\"sh\",\"-c\",\"while true; do sleep 3600; done\"]}")
check "create (estado stopped no rc)" test "$(jget state <<<"$created")" = stopped
check "aliases memoryMb e network aceitos" sh -c "grep -q '\"memory_mib\":2048' <<<'$created' && grep -q '\"enabled\":true' <<<'$created'"
started=$(api -X POST "$base/api/v1/machines/$name/start" -H 'content-type: application/json' -d '{}')
check "start" test "$(jget state <<<"$started")" = running
execd=$(api -X POST "$base/api/v1/machines/$name/exec" -H 'content-type: application/json' -d '{"command":["sh","-c","echo API_EXEC_OK; uname -s"]}')
check "exec exitCode 0" test "$(jget exitCode <<<"$execd")" = 0
check "exec stdout" grep -q API_EXEC_OK <<<"$(jget stdout <<<"$execd")"
failing=$(api -X POST "$base/api/v1/machines/$name/exec" -H 'content-type: application/json' -d '{"command":["sh","-c","exit 3"]}')
check "exec exitCode 3 (o 200 não é sucesso do programa)" test "$(jget exitCode <<<"$failing")" = 3
stream=$(api -N -X POST "$base/api/v1/machines/$name/exec/stream" -H 'content-type: application/json' -d '{"command":["sh","-c","for i in 1 2 3; do echo line$i; sleep 1; done"]}')
check "streaming: 3 linhas data: line" test "$(grep -c '^data: line' <<<"$stream")" = 3
api -X PUT "$base/api/v1/machines/$name/files/%2Ftmp%2Fabs.txt" --data-binary 'PAYLOAD123' >/dev/null
check "arquivo ida e volta (files/%2F...)" test "$(api "$base/api/v1/machines/$name/files/%2Ftmp%2Fabs.txt")" = PAYLOAD123
api -X POST "$base/api/v1/machines/$name/stop" -H 'content-type: application/json' -d '{}' >/dev/null
api -X DELETE "$base/api/v1/machines/$name" >/dev/null
check "depois do delete: NOT_FOUND em .code" test "$(api "$base/api/v1/machines/$name" | jget code)" = NOT_FOUND
finish
