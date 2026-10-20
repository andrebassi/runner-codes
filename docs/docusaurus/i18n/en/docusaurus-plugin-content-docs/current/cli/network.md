---
title: 'Network and volumes'
description: 'Egress network with a policy (allow_hosts, CIDRs, private-range blocking), ports published on 127.0.0.1 and host directories mounted over virtiofs'
---

A machine is born **with no network and nothing from the host**. Both are opened by declaration, machine by machine, and each
opening is something the code inside the VM can now use. Give an untrusted workload only what it needs.

## At a glance

```bash
rc machine create --name web --stack python --net                       # open internet (with the floor)
rc machine create --name api --stack python --allow-host api.openai.com # only that name and its subdomains
rc machine create --name srv --stack python -p 8080:8000                # published port, no egress
rc machine create --name dev --stack python -v ./src:/app -v ~/models:/models:ro
rc machine update --name api --allow-host pypi.org                      # while the machine is running
```

## How the network works

```
guest (eth0 100.96.0.2/30)                         host (`rc vmm` process of the machine)
  │ virtio-net                                     ┌──────────────────────────────────────┐
  └──── socketpair (4 bytes + Ethernet frame) ────▶│ gateway: gVisor netstack             │
                                                   │  DNS  ─ name filter    ─▶ host resolver
                                                   │  TCP  ─ policy on SYN    ─▶ host connection
                                                   │  UDP  ─ policy           ─▶ host socket
                                                   │  -p   ◀─ 127.0.0.1:HOST ─ inbound connection
                                                   └──────────────────────────────────────┘
```

- **Userspace gateway, inside the VM process.** The gVisor netstack (the same stack gvproxy uses) sits on the
  host side of a socketpair that libkrun connects to the guest's virtio-net (`krun_add_net_unixstream`). It lives and dies with
  the VM.
- **NAT:** each guest TCP connection or UDP flow becomes a host connection to the same destination. No raw guest packet
  reaches the host network.
- **The guest sets up its own network at boot:** `eth0` `100.96.0.2/30`, default route and DNS at `100.96.0.1` (the gateway), via
  netlink. The image does not need `ip`.
- **No TSI.** Every machine with a network goes through the gateway, even with just `net = true`, so the floor
  below always applies: TSI applies no filtering at all.

## Egress policy

| Declaration | Effect |
|------------|--------|
| nothing | no network card |
| `--net` / `net = true` | open internet, with the floor |
| `--allow-host example.com` | `example.com` and subdomains; everything else does not resolve |
| `--allow-host =example.com` | the exact name only |
| `--allow-host '*.example.com'` | subdomains only |
| `--allow-host-pattern registry.npmjs.org` | exact name (or `*.x` for subdomains) |
| `--allow-cidr 1.1.1.1/32` | the IP or range |
| `--deny-cidr 10.0.0.0/8` | denied before any allow |
| `--block-private` | closes RFC 1918, CGNAT (100.64/10) and ULA; the internet stays open |
| `--allow-private 192.168.1.0/24` | reopens a private range (implies `--block-private`) |
| only `-p`, without `--net` | the host reaches the port; the guest can **not** get out or resolve anything |

Any `allow` flips the default: only what is allowed gets out. Every egress rule implies `--net`.

**How a name becomes an allowed IP.** The guest asks the gateway's DNS, and:
- a name outside the list gets **NXDOMAIN**: the program sees `bad address` right away, without waiting for a timeout;
- an allowed name goes to the host resolver, and the IPs in the answer become allowed for its TTL, clamped to 60 s–1 h;
- any UDP/53 query, to any IP (a `8.8.8.8` hard-coded in the program, for example), hits the same filter.

A connection to an IP that did not come from an allowed name is refused.

**Only `--allow-cidr`, with no host list:** DNS resolves any name (there is nothing to filter), but the connection only goes out to
the listed CIDRs. Resolving does not authorize: until 2026-10-19 every resolved IP counted as "learned" and went through, and a
machine with `--allow-cidr 127.0.0.1/32` could reach `example.com`. That hole is closed, with a unit test and an e2e test
(`TestMachineEgressCIDROnly`).

**The floor, which no rule opens:**
- cloud metadata (`169.254.169.254`, `fd00:ec2::254`, `100.100.100.200`) and all of link-local. This is the classic
  SSRF attack to steal the account credentials.
- the **host loopback** (`127.0.0.1`, which the guest reaches through the gateway IP, `100.96.0.1`) is closed by
  default. It only opens with `--allow-cidr 127.0.0.1`, and never through an IP learned from DNS: an allowed domain cannot
  point to loopback (DNS rebinding).

**Refusals are fast.** A denied SYN gets an RST before any socket exists on the host, and the program fails within milliseconds.
Every refusal goes to `<cache>/machines/<nome>/egress-denials.log`:

```
2026-10-19T22:44:24Z deny dns google.com: name not in the allow list
2026-10-19T22:45:02Z deny tcp 1.2.3.4:443: not in the allow list
```

## Changing the policy while the machine is running

```bash
rc machine update --name api --allow-host pypi.org --allow-cidr 1.1.1.1
rc machine update --name api --remove-allow-host pypi.org
```

- The change is written to the registry and delivered to the gateway through the control socket (`egress.sock`, mode 0600).
- It applies to the next connection or query. IPs learned from a removed name are forgotten immediately.
- A running machine **without** a network card refuses the update with `INVALID_STATE`: stop it, update it and start it again.
- Through the API: `PUT /api/v1/machines/:name/egress` with the full policy in the body.

## Published ports

```bash
rc machine create --name srv --stack python -p 8080:8000 -p 5353:53/udp
```

- `PORT`, `HOST:GUEST`, a range of the same size on both sides (`5173-5180:5173-5180`, or just `5173-5180`) and the
  `/udp` suffix. A range becomes one port per number; up to 64 per machine, counting each one.
- They listen **only on the host's `127.0.0.1`**. Exposing on the host network is a decision for whoever operates the host, not for whoever writes the
  `runner.code`.
- The egress policy does not apply to the port (it is inbound traffic). A port without `--net` lets the guest serve without being able to
  dial out.
- Published UDP gets one session per client, closed after 60 s without traffic.

## Volumes

```bash
rc machine create --name dev --stack python -v ./src:/app -v ~/models:/models:ro
```

| Point | How it works |
|-------|--------|
| format | `HOST:GUEST`, `HOST:GUEST:ro`, `HOST:GUEST:rw` (default rw) |
| transport | virtio-fs (`krun_add_virtiofs3`), one tag per volume (`vol0`, `vol1`...) |
| `:ro` | enforced by libkrun on the **device**: guest root cannot undo it with a remount |
| when it mounts | on every `start`, before the workdir and init (the workdir can be a volume) |
| host path | must be an existing directory; the real path (with no symlink) is recorded at `create` |
| relative | in the CLI, to the current directory; in `runner.code`, to the file's folder |
| `~` | expanded to HOME |
| forbidden target | `/`, `/proc`, `/sys`, `/dev`, `/tmp`, `/opt/runner`, `/persist` and anything below them |
| limit | 16 per machine |
| through the API | **rejected**: a volume through the API would mean arbitrary reads and writes on the server's disk for anyone who reaches the port |
| new file the guest creates | on macOS it ends up `0600` on the host (directory `0700`): libkrun's virtio-fs stores the guest mode in the `user.containers.override_stat` xattr (measured). An existing file that is overwritten keeps its mode |

### Disk volume

`-v /var/lib/docker` (only the guest path) is a directory on the machine's ext4, outside the overlay, bound to the path on
every start. It is what Docker's overlay2 needs, and it persists like the rest of the disk.

### Staged volume: `-v HOST:GUEST:staged`

```bash
rc machine create --name dev --stack python -v ./repo:/work:staged
rc machine start --name dev        # 1st start: copies ./repo to the machine's disk
rc machine sync --name dev         # returns the copy to the host
```

| Point | How it works |
|-------|--------|
| where it lives | on the machine's ext4 (the same place as a disk volume at `GUEST`): the workload does no virtio-fs round trip per file |
| seed | only on the first start; a later start does not overwrite what the workload changed |
| sync | a mirror: what disappeared from the copy disappears from the host, and whatever the host created or edited after the first start is overwritten. Edit only through the machine |
| when it syncs | `rc machine sync`, `POST /api/v1/machines/{name}/sync`, a graceful stop and the end of `machine run`. `stop --timeout 0` does not sync |
| a failed sync | the stop does not power the machine off: the copy stays on its disk |
| modes | the sync applies the guest's mode on the host (a new script arrives as `755`, not `0600`) |
| branch | the child inherits the copy as a regular disk volume and returns nothing to the host |

## S3 volume (AWS, R2, MinIO)

```bash
rc machine create --name etl --stack python --net \
  -e AWS_ACCESS_KEY_ID=... -e AWS_SECRET_ACCESS_KEY=... \
  -e AWS_ENDPOINT_URL=https://<conta>.r2.cloudflarestorage.com \
  -v s3://meu-bucket/datasets:/data:ro
```

| Point | How it works |
|-------|--------|
| format | `s3://BUCKET[/PREFIX]:/GUEST[:ro]`, in `-v`, in `runner.code` `volumes` and through the API (a host directory through the API is still rejected; a bucket does not touch the server's disk) |
| who mounts it | the **guest**, via FUSE (`go-fuse`, direct `mount(2)`): the image needs no FUSE tooling |
| credentials | from the machine's `-e`, never from the host: `AWS_ACCESS_KEY_ID`, `AWS_SECRET_ACCESS_KEY`, `AWS_SESSION_TOKEN`, `AWS_REGION` (default `us-east-1`) and `AWS_ENDPOINT_URL` (R2, MinIO...). Without a key pair, anonymous reads |
| network | requires `--net` (or an egress policy): traffic leaves the guest through the gateway, subject to the policy. S3 on the host: `AWS_ENDPOINT_URL=http://100.96.0.1:PORTA` and `--allow-cidr 127.0.0.1/32` |
| protocol | path-style, our own SigV4 signing (the official AWS test vector is in the tests) |
| start | the guest lists the prefix before mounting: a missing bucket or wrong key **fails the start** with the S3 reason |
| reads | on demand, by range (`Range`); nothing is downloaded in full at mount time |
| writes | to a local copy on the guest's tmpfs; uploaded in full on `close`/`fsync` (and on stop, which unmounts) |
| `mv` | `CopyObject` + `Delete`: not atomic; a failed copy leaves the source intact |
| `:ro` | `EROFS` on any write, without touching the bucket |

Limits: no symlinks; owner, permissions and timestamps do not persist; directory
rename does not exist; `mkdir` works on the mount and the directory comes into existence in the bucket with the first file inside it. Writing an
`x/` marker was dropped because an S3-compatible backend (`rclone serve s3`) stores it as a file, and the
directory breaks. A write lost along with the VM before `close` never reaches the bucket.

## Named network

```bash
rc machine run -d --name db --stack python --network lab -- python3 -m http.server 8080
rc machine create --name app --stack python --network lab && rc machine start --name app
rc machine exec --name app -- wget -qO- http://db.lab.internal:8080/
```

| Point | How it works |
|-------|--------|
| address | `100.98.0.10` onward, assigned at `create` and fixed for as long as the machine exists |
| name | `<machine>.<network>.internal` resolves in the machine's DNS |
| isolation | networks with different names cannot reach or resolve each other; each network counts its IPs separately |
| `--net` | **not** implied: traffic inside the network is not egress, and without `--net` the machine only talks to those on the network |
| protocol | TCP; the server sees the network IP of whoever connected |
| stopped machine | disappears from the network immediately: the connection is refused and the name returns NXDOMAIN; stop and delete clean up even after a `SIGKILL` |
| in `runner.code` | `name = "lab"` in the `[network]` table; `--network` on the CLI wins |
| through the API | the `"network": "lab"` field on `POST /api/v1/machines` |

Under the hood: each gateway publishes the machine on a Unix socket `<ip>.sock` and a file `<nome>.ip`, in the network's
directory (`~/Library/Caches/runner-codes/n/<hash do nome>`, on Linux `~/.cache/runner-codes/n/...`). A connection to
`100.98.x.y` does not go through the egress policy: it goes straight to the socket of the machine that owns the IP, with a 6-byte header
(source IP and port).

## In `runner.code`

```toml
stack = "python"
net = true
ports = ["8080:8000"]
volumes = ["./src:/app", "~/models:/models:ro"]

[network]
allow_hosts = ["pypi.org", "files.pythonhosted.org"]
block_private = true

[dev]
ports = ["3000"]          # replaces the list above
volumes = [".:/workspace"] # replaces the list above
```

`net_backend = "virtio-net"` is accepted; `"tsi"` is rejected with the reason.

## Measured numbers (macOS 27, M1 Pro)

| Measurement | Value |
|--------|-------|
| `start` of a machine with a network | 190 ms |
| blocked name until the error in the guest | < 1 s (NXDOMAIN) |
| metadata refused | immediate (RST) |

## Known limits

- **IPv4 only.** The guest gets no IPv6 address, and IPv6 connections do not go out.
- **ICMP:** `ping` goes out through the host on an ICMP datagram socket (no root on macOS; on Linux, the user must be
  in `net.ipv4.ping_group_range`), under the same policy as TCP, and the reply only returns to the guest if the host got
  its own. Denials go to `egress-denials.log` as `deny icmp`. Up to 32 pings in flight per machine.
- Named networks are TCP only: UDP between machines on the network does not pass. A `ping` to a network IP answers if the machine
  that owns it is running on it (there is no kernel on the other side to relay the echo; network traffic is per connection).
- On Linux, a machine with a network runs without Landlock/seccomp: the gateway in the same process opens sockets.
