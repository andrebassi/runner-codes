---
sidebar_position: 6
title: 不可读取的凭据
---

# 机器可以使用但无法读取的凭据 {#credenciais-que-a-máquina-usa-sem-ler}

一个 API key 或 token，机器中的程序可以**使用**，却无法**读取**。guest 在它期望的变量中只拿到一个占位符。
网关在发往允许的 host 的 HTTPS 请求中把占位符替换为真实值，而且只在请求头中替换。
这个值永远不会进入机器、机器的注册信息或磁盘。

```bash
rc machine create --name notas --stack python --credential notion=NOTION_API_KEY@api.notion.com
NOTION_API_KEY=secret_... rc machine start --name notas

rc machine exec --name notas -- sh -c 'echo "$NOTION_API_KEY"'
# RC_PLACEHOLDER_NOTION_7F3A...        <- guest 拥有的只是这个
rc machine exec --name notas -- python3 -c '
import os, urllib.request
r = urllib.request.Request("https://api.notion.com/v1/users/me",
    headers={"Authorization": "Bearer " + os.environ["NOTION_API_KEY"], "Notion-Version": "2022-06-28"})
print(urllib.request.urlopen(r).status)'
# 200                                  <- Notion 收到的是真实值
```

## 声明 {#declarar}

```bash
--credential NOME=VARIAVEL@HOST[,HOST...][:body][:set-header=CABECALHO]   # 可重复；隐含 --net
```

```toml
[[network.credentials]]
name = "github"
environment_variable = "GITHUB_TOKEN"
allowed_hosts = ["api.github.com"]
methods = ["GET", "HEAD"]                            # 可选；默认：全部
set_header = "authorization"                         # 可选（见下文）
injection_location = { header = true, body = true }  # 可选：表单请求体
```

通过 API 时，在 `POST /api/v1/machines` 的 `credentials` 中传入同样的对象。

create 时检查的规则：
- `allowed_hosts` 是小写的 DNS 名称，或 `*.exemplo.com`（所有子域名，不含 `exemplo.com` 本身）。不允许
  IP、协议、端口或 `*.com`，列表也不能为空：凭据默认不会发往任何地方。
- 机器上有 `allow_hosts` 时，每个凭据 host 都必须包含在其中：凭据不会扩大网络范围，网络也不会
  扩大凭据范围。
- 名称和变量在机器内唯一；对相互重叠的 host 设置两个相同的 `set_header` 会被拒绝。

## 值从哪里来 {#de-onde-vem-o-valor}

来自**执行 `start` 的一方的环境**（CLI、`machine run` 或 `rc serve`）。VM 进程继承这个环境，
网关**在每次请求时**读取该变量。这个值不会进入 spec、注册信息或 guest。start 之后再修改
变量，要到下一次 start 才生效。

## guest 看到什么 {#o-que-o-guest-vê}

- `VARIAVEL=RC_PLACEHOLDER_<NOME>_<32 hexa>`，在 create 时生成，只要机器存在就保持不变（`branch` 会继承）。
- `/run/rc/credentials/ca.pem`：机器的 CA。它通过 name constraints 被限制在 `allowed_hosts` 范围内，因此
  客户端会拒绝它为其他域名签发的任何证书（已测试）。
- `/run/rc/ca-bundle.pem`：镜像的根证书加上机器的 CA。
- `SSL_CERT_FILE`、`CURL_CA_BUNDLE`、`REQUESTS_CA_BUNDLE` 和 `GIT_SSL_CAINFO` 指向该 bundle；
  `NODE_EXTRA_CA_CERTS` 和 `DENO_CERT` 指向 `ca.pem`。curl、Python、Node、Deno、Git 和 OpenSSL 无需改动即可工作。
  机器的 `-e` 优先于这些变量。

## 一个请求会经历什么 {#o-que-acontece-com-uma-requisição}

到 443 端口、且已被出站策略放行的连接会经过拦截器，拦截器读取 TLS 中请求的名称（SNI）：
- **属于某个 binding 的 host**：TLS 在网关处终止，使用机器 CA 签发的证书。请求（HTTP/1.1）中的
  占位符被原地替换：guest 写的 `Bearer ` 保持不变，只替换占位符。随后请求通过一条新的、经过验证的 TLS
  连接，**发往 guest 解析到的同一个 IP**。响应以 streaming 方式返回（server-sent events
  实时到达）。
- **其他任何 host**：字节原样通过，guest 看到真实证书，其中的占位符只是普通文本。

拒绝时会给出一行原因，且绝不包含真实值：

| 状态码 | 何时出现 |
|--------|--------|
| 403 | 占位符出现在 URL（路径或 query）、路由/分帧/cookie 相关的请求头（`Host`、`Cookie` 等）、未加 `:body` 时的请求体中；占位符不是本机器签发的；出现多个占位符；`Upgrade`/`CONNECT` |
| 405 | binding 不适用于该 host 或该方法 |
| 415 | 加了 `:body` 时，请求体不是 `application/x-www-form-urlencoded` |
| 413 | 请求体超过 8 MiB（需要完整读取请求体来查找占位符） |
| 502 | `runner.code credentials: credential unavailable`：变量缺失、为空或含有控制字符（`\r\n` 会注入请求头）。详细信息写入 VM 进程的日志 |

使用 `set_header` 时，对这些 host 和方法的每个请求，整个请求头（包括 `Bearer ...`）都由 host 设置，
覆盖 guest 发送的内容。适用于无法让其使用占位符的客户端（例如
`git clone`）。

## 限制 {#limites}

- 只拦截 443 端口。明文 HTTP 和其他端口不做替换，直接通过。
- guest 一侧只支持 HTTP/1.1。凭据 host 上的 WebSocket 和 `CONNECT` 会被拒绝。
- 服务器仍然可能在响应体中返回凭据：这是替换，而不是防泄漏。
- 不支持：用 `--secret-file`/`[secrets]` 作为值的来源、API 的 `PUT .../credential-values` 以及
  外部拦截器（`--egress-interceptor`）。值只来自 start 时的环境。
