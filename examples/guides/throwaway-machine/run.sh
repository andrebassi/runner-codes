#!/usr/bin/env bash
# Guia throwaway-machine: código não confiável numa VM descartável, com o repo só de leitura,
# uma pasta de saída e a rede fechada (ou aberta um host por vez). A prova tem duas metades:
# de dentro (não escreve no repo, não sai para a rede) e de fora (o artefato saiu, o repo está
# intacto). "Rodou sem erro" não prova nenhuma das duas.
#
# A imagem é baixada no host e fica em cache, então a VM nunca tem rede para isso; o loopback
# do host abre com --allow-cidr 127.0.0.1/32.
source "$(dirname "$0")/../lib.sh"
work=$(mktemp -d)
trap 'rm -rf "$work"' EXIT
mkdir -p "$work/repo" "$work/out"
printf 'def soma(a, b):\n    return a + b\n' >"$work/repo/lib.py"

# probe roda o mesmo formato de um run de verdade e devolve as linhas chave=valor do guest.
probe() {
	"$RC" machine run --mem 2048 "$@" --stack python -v "$work/repo:/workspace:ro" -v "$work/out:/out" -- sh -c '
		touch /workspace/PROBE 2>/dev/null && echo workspace=WRITABLE || echo workspace=readonly
		touch /out/PROBE 2>/dev/null && echo out=writable || echo out=READONLY
		cd /workspace && python3 -c "import lib; open(\"/out/result.txt\",\"w\").write(str(lib.soma(2,3)))"
		for h in example.com www.example.com iana.org; do
			if timeout 8 wget -q -T5 -O- http://$h >/dev/null 2>&1; then echo net_$h=REACHED; else echo net_$h=blocked; fi
		done' 2>&1 | tr -d '\r'
}
# has confere uma linha chave=valor na saída do probe.
has() { grep -qx "$1" <<<"$out"; }

step "rota offline (padrão): sem --net"
out=$(probe)
printf '%s\n' "$out" | sed 's/^/  /'
check "repo só de leitura por dentro" has workspace=readonly
check "pasta de saída gravável" has out=writable
check "sem rede" has net_example.com=blocked
check "artefato saiu para o host (5)" test "$(cat "$work/out/result.txt" 2>/dev/null)" = 5
check "repo intacto por fora" test ! -e "$work/repo/PROBE"

step "rede um host por vez: --allow-host example.com (nome e subdomínios)"
out=$(probe --allow-host example.com)
printf '%s\n' "$out" | sed 's/^/  /'
check "example.com alcançado" has net_example.com=REACHED
check "www.example.com alcançado (subdomínio)" has net_www.example.com=REACHED
check "iana.org recusado" has net_iana.org=blocked

step "--allow-host-pattern example.com (só o nome exato)"
out=$(probe --allow-host-pattern example.com)
check "example.com alcançado" has net_example.com=REACHED
check "www.example.com recusado" has net_www.example.com=blocked

step "--block-private e o loopback do host"
port=$((19600 + RANDOM % 300))
python3 -m http.server "$port" --bind 127.0.0.1 --directory "$work/repo" >/dev/null 2>&1 &
srv=$!
sleep 1
# loop devolve "open" se o guest abre conexão TCP com o serviço no loopback do host.
loop() {
	"$RC" machine run "$@" --stack python -- python3 -c "
import socket
try:
    socket.create_connection(('100.96.0.1', $port), 5); print('open')
except OSError: print('closed')" 2>&1 | tail -1
}
check "com --net, o loopback do host continua fechado" test "$(loop --net)" = closed
check "--allow-cidr 127.0.0.1/32 abre o loopback" test "$(loop --net --allow-cidr 127.0.0.1/32)" = open
kill "$srv" 2>/dev/null
# Um servidor de verdade no IP de rede local deste host: com --net ele responde (a conferência
# do --block-private só vale se o mesmo destino abre sem ele).
lan=$(ipconfig getifaddr en0 2>/dev/null || hostname -I 2>/dev/null | awk '{print $1}')
python3 -m http.server "$port" --bind "$lan" --directory "$work/repo" >/dev/null 2>&1 &
srv=$!
sleep 1
lanprobe() {
	"$RC" machine run "$@" --stack python -- sh -c "timeout 8 wget -q -T5 -O- http://example.com >/dev/null 2>&1 && echo pub=REACHED; timeout 6 wget -q -T4 -O- http://$lan:$port/lib.py >/dev/null 2>&1 && echo lan=REACHED || echo lan=blocked" 2>&1
}
out=$(lanprobe --net)
check "com --net, a rede local ($lan) responde" has lan=REACHED
out=$(lanprobe --net --block-private)
check "--block-private: internet aberta" has pub=REACHED
check "--block-private: rede local fechada" has lan=blocked
out=$(lanprobe --net --block-private --allow-private "$lan/32")
check "--allow-private $lan/32 reabre só esse endereço" has lan=REACHED
kill "$srv" 2>/dev/null

step "cleanup"
check "nenhuma VM efêmera sobrou" test "$(vmm_count run-)" = 0
finish
