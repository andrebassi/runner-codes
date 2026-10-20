---
sidebar_position: 6
title: Credenciais sem ler
---

# Credenciais que a máquina usa sem ler

Uma API key ou um token que o programa da máquina **usa**, mas não consegue **ler**. O guest recebe só um placeholder
na variável que ele espera. O gateway troca o placeholder pelo valor real nas requisições HTTPS aos hosts permitidos,
e só dentro de um cabeçalho. O valor nunca entra na máquina, no registro dela nem em disco.

```bash
rc machine create --name notas --stack python --credential notion=NOTION_API_KEY@api.notion.com
NOTION_API_KEY=secret_... rc machine start --name notas

rc machine exec --name notas -- sh -c 'echo "$NOTION_API_KEY"'
# RC_PLACEHOLDER_NOTION_7F3A...        <- tudo o que o guest tem
rc machine exec --name notas -- python3 -c '
import os, urllib.request
r = urllib.request.Request("https://api.notion.com/v1/users/me",
    headers={"Authorization": "Bearer " + os.environ["NOTION_API_KEY"], "Notion-Version": "2022-06-28"})
print(urllib.request.urlopen(r).status)'
# 200                                  <- a Notion recebeu o valor real
```

## Declarar

```bash
--credential NOME=VARIAVEL@HOST[,HOST...][:body][:set-header=CABECALHO]   # repete; implica --net
```

```toml
[[network.credentials]]
name = "github"
environment_variable = "GITHUB_TOKEN"
allowed_hosts = ["api.github.com"]
methods = ["GET", "HEAD"]                            # opcional; padrão: todos
set_header = "authorization"                         # opcional (abaixo)
injection_location = { header = true, body = true }  # opcional: corpo de formulário
```

Pela API, o mesmo objeto em `credentials` no `POST /api/v1/machines`.

Regras conferidas no create:
- `allowed_hosts` são nomes DNS em minúsculas, ou `*.exemplo.com` (todo subdomínio, não o próprio `exemplo.com`). Sem
  IP, esquema, porta ou `*.com`, e a lista não pode ser vazia: credencial não vai a lugar nenhum por padrão.
- Com `allow_hosts` na máquina, todo host de credencial tem de estar dentro dele: credencial não alarga a rede, e rede
  não alarga credencial.
- Nome e variável únicos na máquina; dois `set_header` iguais para hosts que se cruzam são recusados.

## De onde vem o valor

Do **ambiente de quem dá o `start`** (CLI, `machine run` ou o `rc serve`). O processo da VM herda esse ambiente, e o
gateway lê a variável **a cada requisição**. O valor não vai para a spec, para o registro nem para o guest. Mudar a
variável depois do start não tem efeito até o próximo start.

## O que o guest vê

- `VARIAVEL=RC_PLACEHOLDER_<NOME>_<32 hexa>`, criado no create e estável enquanto a máquina existir (o `branch` herda).
- `/run/rc/credentials/ca.pem`: a CA da máquina. Ela é restrita por name constraints aos `allowed_hosts`, então o
  cliente recusa qualquer certificado que ela assine para outro domínio (testado).
- `/run/rc/ca-bundle.pem`: as raízes da imagem mais a CA da máquina.
- `SSL_CERT_FILE`, `CURL_CA_BUNDLE`, `REQUESTS_CA_BUNDLE` e `GIT_SSL_CAINFO` apontam para o pacote;
  `NODE_EXTRA_CA_CERTS` e `DENO_CERT`, para o `ca.pem`. curl, Python, Node, Deno, Git e OpenSSL funcionam sem mudança.
  O `-e` da máquina vence essas variáveis.

## O que acontece com uma requisição

Conexão à porta 443, já aprovada pela política de saída, passa pelo interceptador, que lê o nome pedido no TLS (SNI):
- **host de uma binding**: o TLS termina no gateway com certificado da CA da máquina. A requisição (HTTP/1.1) tem o
  placeholder trocado no lugar: o `Bearer ` que o guest escreveu fica, só o placeholder muda. Ela segue numa conexão
  TLS nova, verificada, **para o mesmo IP que o guest resolveu**. A resposta volta em streaming (server-sent events
  chegam ao vivo).
- **qualquer outro host**: os bytes passam intactos, o guest vê o certificado real, e um placeholder ali é só texto.

Recusas, com uma linha de motivo que nunca traz o valor:

| Status | Quando |
|--------|--------|
| 403 | placeholder na URL (caminho ou query), num cabeçalho de roteamento, enquadramento ou cookie (`Host`, `Cookie`, ...), no corpo sem `:body`, placeholder que a máquina não emitiu, mais de um placeholder, `Upgrade`/`CONNECT` |
| 405 | binding que não vale para o host ou o método |
| 415 | com `:body`, corpo que não é `application/x-www-form-urlencoded` |
| 413 | corpo acima de 8 MiB (o corpo é lido inteiro para procurar o placeholder) |
| 502 | `runner.code credentials: credential unavailable`: variável ausente, vazia ou com caractere de controle (um `\r\n` injetaria cabeçalho). O detalhe vai para o log do processo da VM |

Com `set_header`, o cabeçalho inteiro (`Bearer ...` incluído) é definido pelo host em toda requisição àqueles hosts e
métodos, por cima do que o guest mandou. Serve para cliente que não dá para apontar para um placeholder (um
`git clone`, por exemplo).

## Limites

- Só a porta 443 é interceptada. HTTP em claro e outras portas passam sem troca.
- Do lado do guest, HTTP/1.1. WebSocket e `CONNECT` são recusados nos hosts de credencial.
- O servidor ainda pode devolver a credencial no corpo da resposta: isto é substituição, não prevenção de vazamento.
- Fora: `--secret-file`/`[secrets]` como fonte do valor, `PUT .../credential-values` da API e
  o interceptador externo (`--egress-interceptor`). O valor vem só do ambiente do start.
