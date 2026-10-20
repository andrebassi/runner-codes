---
sidebar_position: 6
title: Credentials without reading
---

# Credentials the machine uses without reading

An API key or token that the machine's program **uses** but cannot **read**. The guest gets only a placeholder
in the variable it expects. The gateway swaps the placeholder for the real value in HTTPS requests to the allowed hosts,
and only inside a header. The value never enters the machine, its registry or the disk.

```bash
rc machine create --name notas --stack python --credential notion=NOTION_API_KEY@api.notion.com
NOTION_API_KEY=secret_... rc machine start --name notas

rc machine exec --name notas -- sh -c 'echo "$NOTION_API_KEY"'
# RC_PLACEHOLDER_NOTION_7F3A...        <- all the guest has
rc machine exec --name notas -- python3 -c '
import os, urllib.request
r = urllib.request.Request("https://api.notion.com/v1/users/me",
    headers={"Authorization": "Bearer " + os.environ["NOTION_API_KEY"], "Notion-Version": "2022-06-28"})
print(urllib.request.urlopen(r).status)'
# 200                                  <- Notion received the real value
```

## Declaring

```bash
--credential NOME=VARIAVEL@HOST[,HOST...][:body][:set-header=CABECALHO]   # repeatable; implies --net
```

```toml
[[network.credentials]]
name = "github"
environment_variable = "GITHUB_TOKEN"
allowed_hosts = ["api.github.com"]
methods = ["GET", "HEAD"]                            # optional; default: all
set_header = "authorization"                         # optional (below)
injection_location = { header = true, body = true }  # optional: form body
```

Through the API, the same object goes in `credentials` on `POST /api/v1/machines`.

Rules checked at create:
- `allowed_hosts` are lowercase DNS names, or `*.exemplo.com` (every subdomain, not `exemplo.com` itself). No
  IP, scheme, port or `*.com`, and the list cannot be empty: a credential goes nowhere by default.
- With `allow_hosts` on the machine, every credential host must be inside it: a credential does not widen the network, and the network
  does not widen a credential.
- Name and variable are unique per machine; two identical `set_header` values for overlapping hosts are rejected.

## Where the value comes from

From the **environment of whoever runs `start`** (the CLI, `machine run` or `rc serve`). The VM process inherits that environment, and the
gateway reads the variable **on every request**. The value does not go into the spec, the registry or the guest. Changing the
variable after start has no effect until the next start.

## What the guest sees

- `VARIAVEL=RC_PLACEHOLDER_<NOME>_<32 hexa>`, created at create time and stable for as long as the machine exists (`branch` inherits it).
- `/run/rc/credentials/ca.pem`: the machine's CA. It is restricted by name constraints to the `allowed_hosts`, so the
  client rejects any certificate it signs for another domain (tested).
- `/run/rc/ca-bundle.pem`: the image's roots plus the machine's CA.
- `SSL_CERT_FILE`, `CURL_CA_BUNDLE`, `REQUESTS_CA_BUNDLE` and `GIT_SSL_CAINFO` point to the bundle;
  `NODE_EXTRA_CA_CERTS` and `DENO_CERT`, to `ca.pem`. curl, Python, Node, Deno, Git and OpenSSL work unchanged.
  The machine's `-e` overrides these variables.

## What happens to a request

A connection to port 443, already approved by the egress policy, goes through the interceptor, which reads the requested name from TLS (SNI):
- **a binding's host**: TLS terminates at the gateway with a certificate from the machine's CA. The request (HTTP/1.1) gets the
  placeholder swapped in place: the `Bearer ` the guest wrote stays, only the placeholder changes. It continues over a new, verified TLS
  connection **to the same IP the guest resolved**. The response comes back streamed (server-sent events
  arrive live).
- **any other host**: the bytes pass through untouched, the guest sees the real certificate, and a placeholder there is just text.

Rejections, with a one-line reason that never contains the value:

| Status | When |
|--------|--------|
| 403 | placeholder in the URL (path or query), in a routing, framing or cookie header (`Host`, `Cookie`, ...), in the body without `:body`, a placeholder the machine did not issue, more than one placeholder, `Upgrade`/`CONNECT` |
| 405 | a binding that does not apply to the host or the method |
| 415 | with `:body`, a body that is not `application/x-www-form-urlencoded` |
| 413 | body over 8 MiB (the body is read in full to look for the placeholder) |
| 502 | `runner.code credentials: credential unavailable`: variable missing, empty or with a control character (a `\r\n` would inject a header). The detail goes to the VM process log |

With `set_header`, the whole header (`Bearer ...` included) is set by the host on every request to those hosts and
methods, overriding what the guest sent. It is meant for clients you cannot point at a placeholder (a
`git clone`, for example).

## Limits

- Only port 443 is intercepted. Plain HTTP and other ports pass through with no swap.
- On the guest side, HTTP/1.1. WebSocket and `CONNECT` are rejected on credential hosts.
- The server can still return the credential in the response body: this is substitution, not leak prevention.
- Out of scope: `--secret-file`/`[secrets]` as the value source, the API's `PUT .../credential-values` and
  the external interceptor (`--egress-interceptor`). The value comes only from the start environment.
