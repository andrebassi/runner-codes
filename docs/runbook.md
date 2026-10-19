# Runbook — testar o rc à mão, passo a passo

Sequência para conferir num terminal novo, um comando por vez, que o `rc` faz o que promete. Cada passo diz o que você
deve ver.

**Este runbook é executável.** `task runbook` (`scripts/runbook.sh`) roda exatamente estes passos e confere cada
resultado esperado. Ele reprova se algum sair diferente. Regra do projeto: **todo recurso novo ganha uma seção aqui e
o passo correspondente no `scripts/runbook.sh`, na mesma entrega.**

| Última execução | Host | Resultado |
|-----------------|------|-----------|
| 19/10/2026 | macOS 27, Apple M1 Pro | `task runbook`: 0 falhas em 36 conferências (com o `runner.code`, passo 8b). Prova de falha: com o init rodando a cada start, o 8b reprovou (`init não roda de novo`) |

Pré-requisitos: `task krun:install`, `task build` e as imagens (`./bin/rc image build --all`; o passo 9 precisa da
`llama`).

> Rode com o e2e parado: os testes conferem com `pgrep` que nenhuma VM sobrou, e uma máquina sua ligada faz eles
> reprovarem.

## 0. Preparar

```bash
cd ~/works/labs/github/runner.codes
./bin/rc doctor
```

Esperado: `ok` em `platform`, `libkrun`, `gpu` e `guest`, e uma linha `ok image <stack>` por imagem construída.

## 1. Job efêmero com saída em streaming

```bash
./bin/rc run --stack python --code $'import time\nprint("early")\ntime.sleep(1.5)\nprint("late")'
```

Esperado: `early` aparece logo (~0,3 s, o boot) e `late` 1,5 s depois, enquanto o programa ainda roda. No fim,
`[status=success exit=0 ...]` no stderr. A VM é destruída ao terminar.

## 2. Criar e ligar uma máquina

```bash
./bin/rc machine create --name teste --stack python
./bin/rc machine start --name teste
./bin/rc machine ls
```

Esperado:
- `created teste (stack=python cpus=2 memory=2048MiB disk=10GiB gpu=false)`;
- `started teste (pid N) in ~200ms`;
- `teste` com `running` no `ls`.

A máquina continua ligada depois que o comando sai.

## 3. Gravar coisas em três lugares

```bash
./bin/rc machine exec --name teste -- sh -c 'echo persistiu > /etc/minha-config; echo dados > /workspace/arquivo.txt; echo some > /tmp/temporario.txt'
```

Esperado: nada na saída, e rc 0.

## 4. Parar e ligar de novo

```bash
./bin/rc machine stop --name teste
./bin/rc machine start --name teste
```

Esperado: `stopped teste` e `started teste (pid N) in ~200ms`. O stop faz `sync` do disco antes de desligar.

## 5. Conferir o que ficou

```bash
./bin/rc machine exec --name teste -- sh -c 'cat /etc/minha-config /workspace/arquivo.txt; cat /tmp/temporario.txt'
```

Esperado:

```
persistiu
dados
cat: can't open '/tmp/temporario.txt': No such file or directory
```

`/etc` (camada gravável sobre a imagem) e `/workspace` (disco da máquina) persistem; `/tmp` é tmpfs e volta vazio.

## 6. Copiar um arquivo para a máquina e de volta

```bash
head -c 20000000 /dev/urandom > /tmp/rc-up.bin
./bin/rc machine cp /tmp/rc-up.bin teste:/workspace/up.bin
./bin/rc machine exec --name teste -- sha256sum /workspace/up.bin
shasum -a 256 /tmp/rc-up.bin
./bin/rc machine cp teste:/workspace/up.bin /tmp/rc-down.bin
cmp /tmp/rc-up.bin /tmp/rc-down.bin && echo "idênticos"
```

Esperado:
- `uploaded 20000000 bytes ... in ~230ms`;
- o mesmo sha256 dentro e fora da máquina;
- `downloaded ... in ~230ms`;
- `idênticos`.

## 7. Código de saída e streaming dentro da máquina

```bash
./bin/rc machine exec --name teste -- sh -c 'echo a; sleep 1; echo b; exit 7'; echo "rc=$?"
```

Esperado: `a`, `b` um segundo depois e `rc=7`. O rc do `rc` é o do comando.

## 7b. Terminal interativo na máquina

```bash
./bin/rc machine shell --name teste
```

Dentro do shell que abrir, digite:

```bash
test -t 0 && echo IS-TTY; stty size     # IS-TTY e o tamanho do seu terminal
sleep 30                                 # aperte Ctrl+C: interrompe só o sleep
echo ALIVE                               # o shell continua vivo
exit 4
```

Fora, `echo $?` mostra `4`. A máquina continua ligada: o shell é uma sessão, não a VM.

Esperado: terminal de verdade, com Ctrl+C, setas, histórico, `vi` e redimensionamento da janela. É `bash` se a imagem
tiver; na python (Alpine) é o `sh`.

## 8. Logs da máquina

```bash
./bin/rc machine logs --name teste --tail 10
```

Esperado: as últimas linhas do console (o guest registra cada operação). Funciona também com a máquina parada.

## 8b. Máquina declarada num `runner.code`

O [`runner.code`](docusaurus/docs/cli/runner-code.md) é o Smolfile do `rc`: stack ou imagem, recursos, env, init e
workload num TOML versionável.

```bash
mkdir -p /tmp/rc-demo && cd /tmp/rc-demo
cat > runner.code <<'TOML'
stack = "python"
cpus = 2
env = ["GREETING=hello"]
workdir = "/app"
init = ["echo $(( $(cat count 2>/dev/null || echo 0) + 1 )) > count"]
entrypoint = ["sh", "-c"]
cmd = ["trap 'echo saved > saved; exit 0' INT; echo \"up $GREETING in $(pwd)\"; while :; do sleep 0.2; done"]
stop_signal = "SIGINT"
stop_timeout = 5
TOML
~/works/labs/github/runner.codes/bin/rc machine create --name web --cpus 1
cd ~/works/labs/github/runner.codes
./bin/rc machine status --name web --json
./bin/rc machine start  --name web
./bin/rc machine logs   --name web --source workload
./bin/rc machine exec   --name web -- sh -c 'cat count; echo $GREETING; pwd'
./bin/rc machine stop   --name web
./bin/rc machine start  --name web
./bin/rc machine exec   --name web -- sh -c 'cat count saved'
./bin/rc machine stop   --name web
./bin/rc machine delete --name web
```

Esperado:
- o `create` sem `-f` diz `from runner.code`, e o status mostra `"cpus": 1`: a flag vence o arquivo;
- `logs --source workload` mostra `up hello in /app`: o workdir foi criado e env e workdir chegaram ao workload;
- o primeiro `exec` imprime `1`, `hello`, `/app`;
- o segundo imprime `1` e `saved`: o init **não** rodou de novo no segundo start, e o `SIGINT` do stop chegou ao
  workload, que gravou o arquivo antes do sync.

Chave que o `runner.code` ainda não implementa é recusada, nunca ignorada:

```bash
printf 'stack = "python"\nnet = true\n' > /tmp/rc-demo/bad.code
./bin/rc machine create --name x -f /tmp/rc-demo/bad.code
```

Esperado: `key "net" is not supported yet: networking (planned: phase C)`, e nada criado.

## 9. GPU

Só se a imagem `llama` estiver construída (o `rc doctor` mostra `ok image llama`).

```bash
./bin/rc run --stack llama --gpu --code 'vulkaninfo --summary | grep deviceName'
./bin/rc run --stack llama --gpu --timeout 120s --code 'llama-bench -m /opt/models/qwen2.5-0.5b-q4_k_m.gguf -p 128 -n 32 -ngl 99 -dev Vulkan0'
```

Esperado:
- `deviceName = Virtio-GPU Venus (Apple M1 Pro)`, e também uma linha do `llvmpipe` (Vulkan em software);
- a tabela do `llama-bench` com `Vulkan0`, `pp128` na faixa de 900–1.365 t/s e `tg32` de ~57 t/s.

Detalhes em [GPU](docusaurus/docs/advanced/gpu.md).

## 10. API HTTP

Num terminal:

```bash
./bin/rc serve --pool-size 0
```

Em outro:

```bash
curl -s -X POST localhost:8080/api/v1/machines/teste/exec -H 'content-type: application/json' -d '{"command":["python3","-c","print(6*7)"]}'
curl -s localhost:8080/api/v1/machines | head -c 300; echo
```

Esperado:
- `{"exitCode":0,"stdout":"42\n","stderr":"",...}`;
- a lista com `"name":"teste"` e `"state":"running"`.

`Ctrl+C` encerra o serve. A máquina continua ligada, porque é destacada.

## 11. Limpar

```bash
./bin/rc machine stop --name teste
./bin/rc machine delete --name teste
rm -rf /tmp/rc-up.bin /tmp/rc-down.bin /tmp/rc-demo
```

Esperado: `stopped teste` e `deleted teste`. `./bin/rc machine ls` volta vazio.

## Quando algo sair diferente

| Sintoma | Onde olhar |
|---------|------------|
| `image for X not built` | `./bin/rc image build --stack X` |
| `read stream: the guest in this image does not support streaming` | imagem de antes do streaming: reconstruir |
| `INVALID_STATE: machine X is not running` | `./bin/rc machine start --name X` |
| `CONFLICT: machine X already exists` | outro nome, ou `delete` antes |
| `start` falha com o fim do console | `./bin/rc machine logs --name X` |
| GPU: `ERROR_OUT_OF_HOST_MEMORY` | [GPU › Por que Fedora](docusaurus/docs/advanced/gpu.md) |
| máquina que não para | `./bin/rc machine stop --name X --timeout 0` mata na hora |
