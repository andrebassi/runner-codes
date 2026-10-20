---
title: 'Rede e volumes'
description: 'Rede de saída com política (allow_hosts, CIDRs, bloqueio de privados), portas publicadas no 127.0.0.1 e diretórios do host montados por virtiofs'
---

Uma máquina nasce **sem rede e sem nada do host**. Os dois se abrem por declaração, máquina a máquina, e cada
abertura é algo que o código dentro da VM passa a poder usar. Dê a um workload não confiável só o que ele precisa.

## Em uma tela

```bash
rc machine create --name web --stack python --net                       # internet aberta (com o piso)
rc machine create --name api --stack python --allow-host api.openai.com # só esse nome e subdomínios
rc machine create --name srv --stack python -p 8080:8000                # porta publicada, sem saída
rc machine create --name dev --stack python -v ./src:/app -v ~/models:/models:ro
rc machine update --name api --allow-host pypi.org                      # com a máquina ligada
```

## Como a rede funciona

```
guest (eth0 100.96.0.2/30)                         host (processo `rc vmm` da máquina)
  │ virtio-net                                     ┌──────────────────────────────────────┐
  └──── socketpair (4 bytes + frame Ethernet) ────▶│ gateway: netstack do gVisor          │
                                                   │  DNS  ─ filtro de nomes ─▶ resolvedor do host
                                                   │  TCP  ─ política no SYN  ─▶ conexão do host
                                                   │  UDP  ─ política         ─▶ socket do host
                                                   │  -p   ◀─ 127.0.0.1:HOST ─ conexão de entrada
                                                   └──────────────────────────────────────┘
```

- **Gateway em userspace, dentro do processo da VM.** O netstack do gVisor (a mesma pilha do gvproxy) fica do lado
  do host de um socketpair que a libkrun liga à virtio-net do guest (`krun_add_net_unixstream`). Ele vive e morre com
  a VM.
- **NAT:** cada conexão TCP ou fluxo UDP do guest vira uma conexão do host para o mesmo destino. Nenhum pacote do guest
  chega cru à rede do host.
- **O guest monta a própria rede no boot:** `eth0` `100.96.0.2/30`, rota padrão e DNS em `100.96.0.1` (o gateway), por
  netlink. A imagem não precisa ter `ip`.
- **Sem TSI.** Toda máquina com rede passa pelo gateway, mesmo com só `net = true`, para o piso
  abaixo valer sempre: o TSI não aplica filtro nenhum.

## Política de saída

| Declaração | Efeito |
|------------|--------|
| nada | sem placa de rede |
| `--net` / `net = true` | internet aberta, com o piso |
| `--allow-host example.com` | `example.com` e subdomínios; o resto não resolve |
| `--allow-host =example.com` | só o nome exato |
| `--allow-host '*.example.com'` | só subdomínios |
| `--allow-host-pattern registry.npmjs.org` | nome exato (ou `*.x` para subdomínios) |
| `--allow-cidr 1.1.1.1/32` | o IP ou faixa |
| `--deny-cidr 10.0.0.0/8` | negado antes de qualquer permissão |
| `--block-private` | fecha RFC 1918, CGNAT (100.64/10) e ULA; a internet continua aberta |
| `--allow-private 192.168.1.0/24` | reabre uma faixa privada (implica `--block-private`) |
| só `-p`, sem `--net` | o host alcança a porta; o guest **não** sai nem resolve nada |

Qualquer `allow` inverte o padrão: só sai o que estiver permitido. Toda regra de saída implica `--net`.

**Como um nome vira IP permitido.** O guest pergunta ao DNS do gateway, e:
- nome fora da lista recebe **NXDOMAIN**: o programa vê `bad address` na hora, sem esperar timeout;
- nome permitido vai ao resolvedor do host, e os IPs da resposta viram permitidos pelo TTL dela, limitado a 60 s–1 h;
- qualquer pergunta UDP/53, para qualquer IP (um `8.8.8.8` fixo no código, por exemplo), cai no mesmo filtro.

Conexão a um IP que não veio de um nome permitido é recusada.

**Só `--allow-cidr`, sem lista de hosts:** o DNS resolve qualquer nome (não há o que filtrar), mas a conexão só sai para
os CIDRs listados. Resolver não autoriza: até 19/10/2026 todo IP resolvido contava como "aprendido" e passava, e uma
máquina com `--allow-cidr 127.0.0.1/32` alcançava `example.com`. Esse furo está fechado, com teste de unidade e e2e
(`TestMachineEgressCIDROnly`).

**O piso, que nenhuma regra abre:**
- metadata de nuvem (`169.254.169.254`, `fd00:ec2::254`, `100.100.100.200`) e todo o link-local. É o ataque clássico
  de SSRF para roubar a credencial da conta.
- o **loopback do host** (`127.0.0.1`, que o guest alcança pelo IP do gateway, `100.96.0.1`) fica fechado por
  padrão. Só abre com `--allow-cidr 127.0.0.1`, e nunca por IP aprendido em DNS: um domínio permitido não consegue
  apontar para o loopback (DNS rebinding).

**Recusa é rápida.** O SYN negado recebe RST antes de existir socket no host, e o programa falha em milissegundos.
Toda recusa vai para `<cache>/machines/<nome>/egress-denials.log`:

```
2026-10-19T22:44:24Z deny dns google.com: name not in the allow list
2026-10-19T22:45:02Z deny tcp 1.2.3.4:443: not in the allow list
```

## Mudar a política com a máquina ligada

```bash
rc machine update --name api --allow-host pypi.org --allow-cidr 1.1.1.1
rc machine update --name api --remove-allow-host pypi.org
```

- A mudança é gravada no registro e entregue ao gateway pelo socket de controle (`egress.sock`, modo 0600).
- Vale na próxima conexão ou consulta. Os IPs aprendidos de um nome removido são esquecidos na hora.
- Máquina ligada **sem** placa de rede recusa o update com `INVALID_STATE`: pare, atualize e ligue de novo.
- Pela API: `PUT /api/v1/machines/:name/egress` com a política inteira no corpo.

## Portas publicadas

```bash
rc machine create --name srv --stack python -p 8080:8000 -p 5353:53/udp
```

- `PORTA`, `HOST:GUEST`, faixa de mesmo tamanho dos dois lados (`5173-5180:5173-5180`, ou só `5173-5180`) e o
  sufixo `/udp`. A faixa vira uma porta por número; até 64 por máquina, contando cada uma.
- Escutam **só no `127.0.0.1`** do host. Expor na rede do host é decisão de quem opera o host, não de quem escreve o
  `runner.code`.
- A política de saída não se aplica à porta (é tráfego de entrada). Porta sem `--net` deixa o guest servir sem poder
  discar para fora.
- UDP publicado ganha uma sessão por cliente, fechada após 60 s sem tráfego.

## Volumes

```bash
rc machine create --name dev --stack python -v ./src:/app -v ~/models:/models:ro
```

| Ponto | Como é |
|-------|--------|
| formato | `HOST:GUEST`, `HOST:GUEST:ro`, `HOST:GUEST:rw` (padrão rw) |
| transporte | virtio-fs (`krun_add_virtiofs3`), uma tag por volume (`vol0`, `vol1`...) |
| `:ro` | imposto pela libkrun no **device**: o root do guest não o desfaz com um remount |
| quando monta | a cada `start`, antes do workdir e do init (o workdir pode ser um volume) |
| caminho do host | precisa ser diretório existente; o caminho real (sem link simbólico) é gravado no `create` |
| relativo | na CLI, ao diretório atual; no `runner.code`, à pasta do arquivo |
| `~` | expandido para o HOME |
| destino proibido | `/`, `/proc`, `/sys`, `/dev`, `/tmp`, `/opt/runner`, `/persist` e o que estiver abaixo |
| limite | 16 por máquina |
| pela API | **recusado**: volume pela API seria leitura e escrita arbitrária no disco do servidor para quem alcança a porta |
| arquivo novo que o guest cria | no macOS fica `0600` no host (diretório `0700`): o virtio-fs da libkrun guarda o modo do guest no xattr `user.containers.override_stat` (medido). Arquivo existente sobrescrito mantém o modo |

### Volume de disco

`-v /var/lib/docker` (só o caminho do guest) é um diretório no ext4 da máquina, fora do overlay, ligado ao caminho a
cada start. É o que o overlay2 do Docker precisa, e persiste como o resto do disco.

### Volume staged: `-v HOST:GUEST:staged`

```bash
rc machine create --name dev --stack python -v ./repo:/work:staged
rc machine start --name dev        # 1º start: copia ./repo para o disco da máquina
rc machine sync --name dev         # devolve a cópia ao host
```

| Ponto | Como é |
|-------|--------|
| onde mora | no ext4 da máquina (o mesmo lugar de um volume de disco em `GUEST`): o workload não faz ida e volta de virtio-fs por arquivo |
| semente | só no primeiro start; um start seguinte não pisa no que o workload mudou |
| sync | espelho: o que sumiu da cópia some do host, e o que o host criou ou editou depois do primeiro start é sobrescrito. Edite só pela máquina |
| quando sincroniza | `rc machine sync`, `POST /api/v1/machines/{name}/sync`, o stop gracioso e o fim do `machine run`. `stop --timeout 0` não sincroniza |
| sync que falha | o stop não desliga a máquina: a cópia segue no disco dela |
| modos | o sync aplica no host o modo do guest (um script novo chega `755`, não `0600`) |
| branch | a filha herda a cópia como volume de disco comum e não devolve nada ao host |

## Volume S3 (AWS, R2, MinIO)

```bash
rc machine create --name etl --stack python --net \
  -e AWS_ACCESS_KEY_ID=... -e AWS_SECRET_ACCESS_KEY=... \
  -e AWS_ENDPOINT_URL=https://<conta>.r2.cloudflarestorage.com \
  -v s3://meu-bucket/datasets:/data:ro
```

| Ponto | Como é |
|-------|--------|
| formato | `s3://BUCKET[/PREFIX]:/GUEST[:ro]`, em `-v`, em `volumes` do `runner.code` e pela API (diretório do host pela API continua recusado; bucket não toca o disco do servidor) |
| quem monta | o **guest**, por FUSE (`go-fuse`, `mount(2)` direto): a imagem não precisa de ferramenta FUSE |
| credenciais | do `-e` da máquina, nunca do host: `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`, `AWS_SESSION_TOKEN`, `AWS_REGION` (padrão `us-east-1`) e `AWS_ENDPOINT_URL` (R2, MinIO...). Sem par de chaves, leitura anônima |
| rede | exige `--net` (ou uma política de saída): o tráfego sai do guest pelo gateway, sujeito à política. S3 no host: `AWS_ENDPOINT_URL=http://100.96.0.1:PORTA` e `--allow-cidr 127.0.0.1/32` |
| protocolo | path-style, assinatura SigV4 própria (o vetor oficial da AWS está nos testes) |
| start | o guest lista o prefixo antes de montar: bucket inexistente ou chave errada **falham o start** com o motivo do S3 |
| leitura | sob demanda, por faixa (`Range`); nada é baixado inteiro na montagem |
| escrita | numa cópia local no tmpfs do guest; sobe inteira no `close`/`fsync` (e no stop, que desmonta) |
| `mv` | `CopyObject` + `Delete`: não é atômico; cópia que falha deixa a origem intacta |
| `:ro` | `EROFS` em qualquer escrita, sem tocar no bucket |

Limites: sem link simbólico; dono, permissão e data não persistem; rename de
diretório não existe; `mkdir` vale na montagem e o diretório passa a existir no bucket com o primeiro arquivo dentro. A
gravação de marcador `x/` foi descartada porque um backend S3 compatível (o `rclone serve s3`) o grava como arquivo, e o
diretório quebra. Escrita perdida com a VM antes do `close` não chega ao bucket.

## Rede nomeada

```bash
rc machine run -d --name db --stack python --network lab -- python3 -m http.server 8080
rc machine create --name app --stack python --network lab && rc machine start --name app
rc machine exec --name app -- wget -qO- http://db.lab.internal:8080/
```

| Ponto | Como é |
|-------|--------|
| endereço | `100.98.0.10` em diante, dado no `create` e fixo enquanto a máquina existir |
| nome | `<máquina>.<rede>.internal` resolve no DNS da máquina |
| isolamento | redes de nomes diferentes não se alcançam nem se resolvem; cada rede conta os IPs à parte |
| `--net` | **não** é implícito: tráfego dentro da rede não é saída, e sem `--net` a máquina só fala com as da rede |
| protocolo | TCP; o servidor vê o IP de rede de quem conectou |
| máquina parada | some da rede na hora: a conexão é recusada e o nome dá NXDOMAIN; o stop e o delete limpam até depois de um `SIGKILL` |
| no `runner.code` | `name = "lab"` na tabela `[network]`; `--network` na CLI vence |
| pela API | campo `"network": "lab"` no `POST /api/v1/machines` |

Por dentro: cada gateway publica a máquina num socket Unix `<ip>.sock` e num arquivo `<nome>.ip`, no diretório da
rede (`~/Library/Caches/runner-codes/n/<hash do nome>`, no Linux `~/.cache/runner-codes/n/...`). Conexão para
`100.98.x.y` não passa pela política de saída: vai direto ao socket da máquina dona do IP, com 6 bytes de cabeçalho
(IP de origem e porta).

## No `runner.code`

```toml
stack = "python"
net = true
ports = ["8080:8000"]
volumes = ["./src:/app", "~/models:/models:ro"]

[network]
allow_hosts = ["pypi.org", "files.pythonhosted.org"]
block_private = true

[dev]
ports = ["3000"]          # substitui a lista de cima
volumes = [".:/workspace"] # substitui a lista de cima
```

`net_backend = "virtio-net"` é aceito; `"tsi"` é recusado com o motivo.

## Números medidos (macOS 27, M1 Pro)

| Medida | Valor |
|--------|-------|
| `start` de máquina com rede | 190 ms |
| nome bloqueado até o erro no guest | < 1 s (NXDOMAIN) |
| metadata recusada | imediato (RST) |

## Limites conhecidos

- **IPv4 só.** O guest não recebe endereço IPv6, e conexão IPv6 não sai.
- **ICMP:** o `ping` sai pelo host num socket ICMP de datagrama (sem root no macOS; no Linux, o usuário precisa estar
  em `net.ipv4.ping_group_range`), sob a mesma política do TCP, e a resposta só volta ao guest se o host recebeu a
  dele. Negado vai para o `egress-denials.log` como `deny icmp`. Até 32 pings no ar por máquina.
- Rede nomeada só com TCP: UDP entre máquinas da rede não passa. O `ping` a um IP da rede responde se a máquina
  dona dele está ligada nela (não há kernel do outro lado para repassar o echo; o tráfego da rede é por conexão).
- No Linux, máquina com rede roda sem Landlock/seccomp: o gateway no mesmo processo abre sockets.
