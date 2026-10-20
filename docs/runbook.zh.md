# Runbook — 手动逐步测试 rc

在一个新终端里逐条执行命令,确认 `rc` 做到了它承诺的事。每一步都写明你
应该看到什么。

**这个 runbook 是可执行的。** `task runbook`(`scripts/runbook.sh`)会原样执行这些步骤,并核对每一个预期
结果。只要有一项不同就判定失败。项目规则:**每个新功能都要在同一次交付中,在这里新增一节,并在
`scripts/runbook.sh` 中加入对应的步骤。**

| 最近一次执行 | Host | 结果 |
|-----------------|------|-----------|
| 20/10/2026 | macOS 27, Apple M1 Pro | `task runbook`:116 项核对 0 失败(到 8u 为止:用户、secret、socket 桥接、ping、staged、拒绝) |
| 19/10/2026 | macOS 27, Apple M1 Pro | `task runbook`:50 项核对 0 失败(含卷和网络,步骤 8c 和 8d)。失败验证:网关的 DNS 不做过滤时,8d 有 2 项核对失败 |

前置条件:`task krun:install`、`task build` 以及镜像(`./bin/rc image build --all`;第 9 步需要
`llama`)。

> 运行时请先停掉 e2e:测试会用 `pgrep` 确认没有残留的 VM,你自己开着的机器会导致它们
> 失败。

## 0. 准备

```bash
cd ~/works/labs/github/runner.codes
./bin/rc doctor
```

预期:`platform`、`libkrun`、`gpu` 和 `guest` 均为 `ok`,并且每个已构建的镜像有一行 `ok image <stack>`。

## 1. 带 streaming 输出的临时 job

```bash
./bin/rc run --stack python --code $'import time\nprint("early")\ntime.sleep(1.5)\nprint("late")'
```

预期:`early` 立即出现(约 0.3 秒,即 boot 时间),`late` 在 1.5 秒后出现,此时程序仍在运行。最后在
stderr 中输出 `[status=success exit=0 ...]`。结束时 VM 被销毁。

## 2. 创建并启动一台机器

```bash
./bin/rc machine create --name teste --stack python
./bin/rc machine start --name teste
./bin/rc machine ls
```

预期:
- `created teste (stack=python cpus=4 memory=8192MiB disk=10GiB gpu=false)`;
- `started teste (pid N) in ~200ms`;
- `ls` 中 `teste` 显示为 `running`。

命令退出后机器仍保持运行。

## 3. 在三个位置写入内容

```bash
./bin/rc machine exec --name teste -- sh -c 'echo persistiu > /etc/minha-config; echo dados > /workspace/arquivo.txt; echo some > /tmp/temporario.txt'
```

预期:没有任何输出,rc 为 0。

## 4. 停止后再次启动

```bash
./bin/rc machine stop --name teste
./bin/rc machine start --name teste
```

预期:`stopped teste` 和 `started teste (pid N) in ~200ms`。stop 在关机前会对磁盘执行 `sync`。

## 5. 检查保留下来的内容

```bash
./bin/rc machine exec --name teste -- sh -c 'cat /etc/minha-config /workspace/arquivo.txt; cat /tmp/temporario.txt'
```

预期:

```
persistiu
dados
cat: can't open '/tmp/temporario.txt': No such file or directory
```

`/etc`(镜像之上的可写层)和 `/workspace`(机器的磁盘)会持久保留;`/tmp` 是 tmpfs,重启后为空。

## 6. 把文件复制到机器里再复制回来

```bash
head -c 20000000 /dev/urandom > /tmp/rc-up.bin
./bin/rc machine cp /tmp/rc-up.bin teste:/workspace/up.bin
./bin/rc machine exec --name teste -- sha256sum /workspace/up.bin
shasum -a 256 /tmp/rc-up.bin
./bin/rc machine cp teste:/workspace/up.bin /tmp/rc-down.bin
cmp /tmp/rc-up.bin /tmp/rc-down.bin && echo "idênticos"
```

预期:
- `uploaded 20000000 bytes ... in ~230ms`;
- 机器内外的 sha256 相同;
- `downloaded ... in ~230ms`;
- `idênticos`。

## 7. 机器内的退出码与 streaming

```bash
./bin/rc machine exec --name teste -- sh -c 'echo a; sleep 1; echo b; exit 7'; echo "rc=$?"
```

预期:`a`,一秒后 `b`,以及 `rc=7`。`rc` 的退出码就是命令的退出码。

## 7b. 机器上的交互式终端

```bash
./bin/rc machine shell --name teste
```

在打开的 shell 中输入:

```bash
test -t 0 && echo IS-TTY; stty size     # IS-TTY e o tamanho do seu terminal
sleep 30                                 # aperte Ctrl+C: interrompe só o sleep
echo ALIVE                               # o shell continua vivo
exit 4
```

退出后,`echo $?` 显示 `4`。机器仍在运行:shell 是一个会话,而不是 VM 本身。

预期:真正的终端,支持 Ctrl+C、方向键、历史记录、`vi` 以及窗口大小调整。镜像中有 `bash` 时就用它;
python 镜像(Alpine)中是 `sh`。

## 8. 机器日志

```bash
./bin/rc machine logs --name teste --tail 10
```

预期:控制台的最后几行(guest 会记录每个操作)。机器停止时同样可用。

## 8b. 在 `runner.code` 中声明的机器

[`runner.code`](docusaurus/docs/cli/runner-code.md) 是用文件声明的机器:stack 或镜像、资源、env、init 和
workload,都写在一个可版本化的 TOML 中。

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

预期:
- 不带 `-f` 的 `create` 显示 `from runner.code`,status 显示 `"cpus": 1`:flag 优先于文件;
- `logs --source workload` 显示 `up hello in /app`:workdir 已被创建,env 和 workdir 都传到了 workload;
- 第一次 `exec` 输出 `1`、`hello`、`/app`;
- 第二次输出 `1` 和 `saved`:第二次 start 时 init **没有**再次运行,而 stop 发出的 `SIGINT` 到达了
  workload,它在 sync 之前写入了文件。

`runner.code` 尚未实现的键会被拒绝,绝不会被忽略:

```bash
printf 'stack = "python"\n[health]\nexec = ["true"]\n' > /tmp/rc-demo/bad.code
./bin/rc machine create --name x -f /tmp/rc-demo/bad.code
```

预期:`key "health" is not supported yet: health checks`,且不会创建任何东西。

## 8c. 卷:在机器内使用 host 目录

```bash
mkdir -p /tmp/rc-vol/ro && echo "do host" > /tmp/rc-vol/hello.txt
./bin/rc machine create --name vol --stack python -v /tmp/rc-vol:/data -v /tmp/rc-vol/ro:/ro:ro -w /data
./bin/rc machine start  --name vol
./bin/rc machine exec   --name vol -- sh -c 'cat hello.txt; echo do-guest > from-guest.txt; echo y > /ro/y; echo ro=$?'
cat /tmp/rc-vol/from-guest.txt
./bin/rc machine stop   --name vol
./bin/rc machine delete --name vol
```

预期:
- `do host`:guest 能读取 host 的目录;
- `Read-only file system` 和 `ro=1`:`:ro` 由 libkrun 在设备层强制执行,guest 的 root 也无法解除;
- 在 host 上,`cat` 显示 `do-guest`:guest 的写入立即出现在 host 上。

## 8d. 网络:出站、策略与端口

机器创建时**没有网络**。`--net` 开启出站;`--allow-host` 开启并加以限制;`-p` 把端口发布到 host 的
`127.0.0.1` 上。

```bash
./bin/rc machine create --name net --stack python --allow-host example.com -p 18080:8000
./bin/rc machine start  --name net
./bin/rc machine exec   --name net -- sh -c 'wget -qO /dev/null http://example.com; echo ok=$?'
./bin/rc machine exec   --name net -- sh -c 'wget -qO /dev/null http://iana.org'
./bin/rc machine exec   --name net -- sh -c 'wget -qO- -T 3 http://169.254.169.254; echo meta=$?'
./bin/rc machine update --name net --allow-host iana.org
./bin/rc machine exec   --name net -- sh -c 'wget -qO /dev/null http://iana.org; echo ok=$?'
./bin/rc machine exec   --name net -- sh -c 'cd /tmp && echo do-guest > i.html && (python3 -m http.server 8000 >/dev/null 2>&1 &)'
curl -s http://127.0.0.1:18080/i.html
cat ~/Library/Caches/runner-codes/machines/net/egress-denials.log
./bin/rc machine stop   --name net
./bin/rc machine delete --name net
```

预期:
- `example.com` 返回 `ok=0`;
- **立即**出现 `wget: bad address 'iana.org'`:网关的 DNS 返回 NXDOMAIN,无需等待超时;
- `meta=1`:即使网络已开放,云 metadata(`169.254.169.254`)也会被拒绝;
- `update` 之后,`iana.org` **无需重启**机器即可访问;
- host 上的 `curl` 通过发布的端口收到 `do-guest`;
- 拒绝日志中有 `deny dns iana.org: name not in the allow list`。

## 8e. 临时机器:`machine run`

```bash
./bin/rc machine run --stack python -- python3 -c 'print("ola"); raise SystemExit(3)'; echo "rc=$?"
echo dados | ./bin/rc machine run -i --stack python -- sh -c 'read x; echo leu:$x'
./bin/rc machine run --net --image alpine -- uname -a
./bin/rc machine run -it --image alpine -- /bin/sh        # terminal; `exit` sai e apaga a máquina
./bin/rc machine ls
```

预期:
- `ola` 和 `rc=3`:退出码就是命令的退出码;
- `leu:dados`:`-i` 会转发 stdin(不带 `-i` 时不读取 stdin);
- 在目录之外的镜像(首次使用时构建)中输出 libkrunfw 内核的 `uname -a`;
- `ls` 中没有任何 `run-…`:临时机器在结束时消失,按 Ctrl+C 也一样。

## 8f. 发布的端口能访问只监听 localhost 的服务

```bash
./bin/rc machine run -d --name web --stack python -p 18080:9000 -- python3 -m http.server 9000 --bind 127.0.0.1
curl -s -o /dev/null -w '%{http_code}\n' http://127.0.0.1:18080/
./bin/rc machine delete --name web -f
```

预期:`200`。发布的端口经由 guest 进入(vsock 上的 `dial` 操作),并在那里拨号 `127.0.0.1`,因此
只监听 localhost 的服务(例如 vite 或 openclaw)也能响应。

## 8g. 浏览器中的屏幕:显示、键盘和鼠标

```bash
./bin/rc machine run -d --name desktop -s examples/desktop/runner.code
open http://localhost:6080
./bin/rc machine stop --name desktop
```

预期:
- 页面上出现 Weston 桌面,带有 guest 的面板和时钟;
- 点击面板上的图标会打开终端;
- 在页面上输入的内容会到达终端,包括大写字母、符号和 Ctrl。

`task runbook` 无需人眼即可核对同一路径:
- 一台带 `--display 1024x768` 的 alpine 机器;
- `modetest` 点亮屏幕;
- 1024x768 的画面到达查看器;
- 通过 `POST /input` 发送的 `KEY_A` 出现在 guest 的 `evtest` 中。

## 8h. 示例

```bash
./bin/rc machine run -s examples/python-app/runner.code -- python3 --version
./bin/rc machine run -s examples/node-app/runner.code -- node -e 'console.log("ola", 1+1)'
./bin/rc machine run -d --name doom -s examples/doom-web/runner.code && open http://localhost:8080
```

预期:
- `Python 3.12.x`;
- `ola 2`;
- 浏览器中的 DOOM。

其他示例以及每个示例测量的内容见 [`examples/README.md`](../examples/README.md)。
`docker-in-vm` 有自己的核对脚本 `examples/docker-in-vm/check.sh`。

## 8i. Branch 与暂停

```bash
./bin/rc machine create --name mom --stack python && ./bin/rc machine start --name mom
./bin/rc machine exec   --name mom -- sh -c 'echo antes > /root/marca'
./bin/rc machine pause  --name mom && ./bin/rc machine status --name mom
./bin/rc machine branch --from mom --name kid
./bin/rc machine resume --name mom
./bin/rc machine start  --name kid && ./bin/rc machine exec --name kid -- cat /root/marca
./bin/rc machine delete --name kid -f && ./bin/rc machine delete --name mom -f
```

预期:
- `status` 显示 `paused`,在此状态下的 `exec` 会被拒绝并给出原因;
- 子机器显示 `antes`:即使母机器处于暂停状态,branch 也会带走它写入的内容(`rc` 会短暂恢复它
  以执行 `sync`);
- 此后母机器与子机器相互独立。

## 8j. 命名网络

```bash
./bin/rc machine run -d --name db --stack python --network lab -- python3 -m http.server 8080
./bin/rc machine create --name app --stack python --network lab && ./bin/rc machine start --name app
./bin/rc machine exec --name app -- wget -qO- -T 3 http://100.98.0.10:8080/
./bin/rc machine exec --name app -- wget -qO- -T 3 http://db.lab.internal:8080/
./bin/rc machine delete --name db -f
./bin/rc machine exec --name app -- nslookup db.lab.internal
./bin/rc machine delete --name app -f
```

预期:
- 两台机器获得该网络中连续的 IP(`db` 是 `100.98.0.10`,`app` 是 `100.98.0.11`),无需 `--net` 即可通过 IP
  和名称 `<máquina>.<rede>.internal` 互相访问:网络内部的流量不算出站;
- 其他网络中的机器无法访问它们,也无法解析它们的名称;
- 删除的 `db` 会从网络的 DNS 中消失:`nslookup` 不再返回它的 IP。

## 8k. S3 卷

一个本地 S3,服务于一个文件夹(MinIO 已于 11/10/2026 从 nixpkgs 中移除):

```bash
mkdir -p /tmp/s3/bkt/data && echo 'ola do bucket' > /tmp/s3/bkt/data/hello.txt
nix shell nixpkgs#rclone -c rclone serve s3 /tmp/s3 --addr 127.0.0.1:19000 --auth-key chave,segredo123 &
./bin/rc machine create --name s3 --stack python --net --allow-cidr 127.0.0.1/32 \
  -e AWS_ACCESS_KEY_ID=chave -e AWS_SECRET_ACCESS_KEY=segredo123 -e AWS_ENDPOINT_URL=http://100.96.0.1:19000 \
  -v s3://bkt/data:/data
./bin/rc machine start --name s3
./bin/rc machine exec --name s3 -- sh -c 'cat /data/hello.txt; echo do-guest > /data/novo.txt'
cat /tmp/s3/bkt/data/novo.txt
./bin/rc machine delete --name s3 -f
```

预期:
- `start` 挂载 bucket(密钥错误时会失败,输出 `s3: 403` 及原因);
- guest 读到 `ola do bucket`,`novo.txt` 出现在服务端的文件夹中,内容为 `do-guest`。

## 8l. 机器使用但无法读取的凭据

```bash
./bin/rc machine create --name cred --stack python --credential tok=RB_TOKEN@postman-echo.com
RB_TOKEN=valor-do-runbook ./bin/rc machine start --name cred
./bin/rc machine exec --name cred -- python3 -c '
import json, os, urllib.request
tok = os.environ["RB_TOKEN"]
print("guest:", tok)
r = urllib.request.Request("https://postman-echo.com/headers", headers={"Authorization": "Bearer " + tok, "User-Agent": "runbook/1"})
print("server:", json.load(urllib.request.urlopen(r, timeout=20))["headers"]["authorization"])'
./bin/rc machine delete --name cred -f
```

预期:
- `guest: RC_PLACEHOLDER_TOK_...`:guest 只拿到 placeholder;
- `server: Bearer valor-do-runbook`:服务端收到了真实值,它来自 `start` 的环境变量。

之所以使用自定义 User-Agent,是因为 postman-echo 的 Cloudflare 会拒绝 `Python-urllib`(403,error code 1010)。

## 8m. Pack:把机器装进一个文件

```bash
./bin/rc machine create --name pk --stack python && ./bin/rc machine start --name pk
./bin/rc machine exec --name pk -- sh -c 'echo marca-do-runbook > /root/marca'
./bin/rc machine stop --name pk
./bin/rc pack create --from-vm pk --output /tmp/one --single-file
./bin/rc machine delete --name pk
/tmp/one run -- cat /root/marca
/tmp/one
```

预期:
- `marca-do-runbook`:即使源机器已被删除,产物也带走了机器的磁盘(丢失磁盘的 pack
  仍然能启动并以 0 退出;所以要核对一个具体的值);
- 单独运行 `/tmp/one` 显示 `usage: one run ...`。

## 8n. 用户、磁盘卷与 `docker.sock`

```bash
./bin/rc machine create --name usr --stack python --user nobody
./bin/rc machine start --name usr
./bin/rc machine exec --name usr -- id -un
./bin/rc machine exec --name usr --user root -- id -un
./bin/rc machine create --name dk --stack python --docker-socket -v /var/lib/dados -- python3 -c '
import os, socket
s = socket.socket(socket.AF_UNIX); s.bind("/var/run/docker.sock"); s.listen()
while True:
    c, _ = s.accept(); c.sendall(b"do-guest:" + c.recv(64)); c.close()'
./bin/rc machine start --name dk
./bin/rc machine exec --name dk -- stat -f -c %T /var/lib/dados
python3 -c 'import socket,sys; s=socket.socket(socket.AF_UNIX); s.connect(sys.argv[1]); s.sendall(b"ping"); print(s.recv(64).decode())' "$(./bin/rc machine data-dir --name dk)/docker.sock"
```

预期:
- `nobody`,以及带 `--user root` 时的 `root`(exec 的 `--user` 优先于机器的设置);
- `ext2/ext3`:磁盘卷是真正的 ext4,位于 overlay 之外(这正是 Docker 的 overlay2 所需要的);
- `do-guest:ping`:host 通过机器目录中的 `docker.sock` 与 guest 中的服务通信。

## 8o. Secret 与 SSH agent

```bash
ssh-keygen -q -t ed25519 -N "" -f /tmp/rb-id && ssh-agent -D -a /tmp/rb-agent.sock & sleep 1
SSH_AUTH_SOCK=/tmp/rb-agent.sock ssh-add -q /tmp/rb-id
echo do-arquivo > /tmp/rb-sec.txt
./bin/rc machine create --name sec --image alpine --net --init 'apk add -q --no-cache openssh-client' \
  --secret-env API=RB_SECRET --secret-file ARQ=/tmp/rb-sec.txt --ssh-agent
RB_SECRET=valor-secreto SSH_AUTH_SOCK=/tmp/rb-agent.sock ./bin/rc machine start --name sec
./bin/rc machine exec --name sec -- sh -c 'echo $API $ARQ; ssh-add -l'
grep -rl valor-secreto --exclude='*.ext4' "$(./bin/rc machine data-dir --name sec)"
```

预期:
- `valor-secreto do-arquivo`:两个 secret 都在每条命令的环境中;
- `ssh-add -l` 列出 `/tmp/rb-id` 的密钥(私钥留在 host 的 agent 中);
- `grep` 什么也找不到:该值不会留在机器的记录中。

agent 和密钥都是这一步专用、用完即弃的:不会触碰运行者自己的 agent。

## 8p. 双向的 socket 桥接

```bash
python3 -c 'import socket; s=socket.socket(socket.AF_UNIX); s.bind("/tmp/rb-h.sock"); s.listen()
while True:
    c,_=s.accept(); c.sendall(b"do-host:"+c.recv(64)); c.close()' &
./bin/rc machine create --name skt --stack python --expose-socket /run/app.sock --mount-socket /tmp/rb-h.sock:/run/h/h.sock -- python3 -c '
import socket
s = socket.socket(socket.AF_UNIX); s.bind("/run/app.sock"); s.listen()
while True:
    c, _ = s.accept(); c.sendall(b"do-guest:" + c.recv(64)); c.close()'
./bin/rc machine start --name skt
python3 -c 'import socket,sys; s=socket.socket(socket.AF_UNIX); s.connect(sys.argv[1]); s.sendall(b"ping"); print(s.recv(64).decode())' "$(./bin/rc machine data-dir --name skt)/app.sock"
./bin/rc machine exec --name skt -- python3 -c 'import socket; s=socket.socket(socket.AF_UNIX); s.connect("/run/h/h.sock"); s.sendall(b"pong"); print(s.recv(64).decode())'
```

预期:`do-guest:ping`(host 能访问 workload 监听的 socket)和 `do-host:pong`(guest 能访问 host 的
socket,位于镜像中原本不存在的目录里)。

## 8q. Ping 与端口范围

```bash
./bin/rc machine create --name png --stack python --net -p 18301-18303:18301-18303 -- python3 -m http.server 18302
./bin/rc machine start --name png
curl -s -o /dev/null -w '%{http_code}\n' http://127.0.0.1:18302/
./bin/rc machine exec --name png -- ping -c1 -W3 1.1.1.1
./bin/rc machine create --name pngd --stack python --allow-cidr 198.51.100.0/24
./bin/rc machine start --name pngd
./bin/rc machine exec --name pngd -- ping -c1 -W2 1.1.1.1
grep 'deny icmp' "$(./bin/rc machine data-dir --name pngd)/egress-denials.log"
```

预期:
- `200`:端口范围按编号逐个发布端口(最多 64 个),中间那个有响应;
- 网络开放时,对 `1.1.1.1` 的 ping 会返回,且仅当 host 收到了回复时才返回(不会伪造回复);
- 在 allowlist 之外 ping 不会返回,拒绝记录 `deny icmp 1.1.1.1` 写入日志。

## 8r. Staged 卷与 sync

```bash
mkdir -p /tmp/rb-repo && echo original > /tmp/rb-repo/a.txt && echo x > /tmp/rb-repo/old.txt
./bin/rc machine create --name stg --stack python -v /tmp/rb-repo:/work:staged
./bin/rc machine start --name stg
./bin/rc machine exec --name stg -- sh -c 'stat -f -c %T /work; echo mudou > /work/a.txt; rm /work/old.txt; printf "#!/bin/sh\n" > /work/run.sh; chmod 755 /work/run.sh'
cat /tmp/rb-repo/a.txt
./bin/rc machine sync --name stg
cat /tmp/rb-repo/a.txt; ls /tmp/rb-repo; ls -l /tmp/rb-repo/run.sh
```

预期:
- `ext2/ext3`:副本位于机器的磁盘上,无需经过 virtio-fs 往返;
- sync 之前 host 上仍是 `original`;
- 之后:`mudou`,没有 `old.txt`(sync 是镜像式的:副本中消失的内容也会从 host 上消失),`run.sh` 为 `-rwxr-xr-x`。

优雅停止和 `machine run` 结束时也会同步;`--timeout 0` 不会。branch 出来的子机器不会向
host 回写任何内容。

## 8s. 说明原因的拒绝

```bash
./bin/rc doctor | grep -E 'cuda|nested'
./bin/rc machine create --name x --image alpine --cuda
time ./bin/rc machine run --nested --image alpine -- ls -l /dev/kvm
```

预期:
- `info  cuda: not supported (no CUDA remoting; --gpu is Vulkan)` 以及本 host 关于 nested 的那一行;
- `--cuda is not supported: rc has no CUDA remoting ...`,且不创建机器;
- 在不支持嵌套的 host(M1/M2)上,`failed to start: Error: nested virtualization is not supported on this host ...`
  在 1 秒内出现(以前是 30 秒后报 `did not become ready`);在支持嵌套的 host 上,VM 内有 `/dev/kvm`。

## 8t. 机器目录与缓存清理

```bash
ls "$(./bin/rc machine data-dir --name usr)"
./bin/rc image prune
./bin/rc pack prune
```

预期:机器目录中有 `machine.json`;两个 prune 都以 0 退出(不带 `--all` 时,`image prune` 只删除
不再被任何名称引用的 ext4,即某个镜像之前的构建,而 `pack prune` 保留最近使用的 5 个)。

## 8u. 指南

```bash
task guides DIRS=gpu-cuda      # uma pasta; sem DIRS, todas (cada uma liga as suas VMs)
```

预期:每个文件夹都是 `falhas: 0`。每个文件夹验证的内容见 `examples/guides/README.md`。

## 8v. Web 终端

```bash
./bin/rc machine web-terminal --name demo                 # 在浏览器中打开机器的 shell
./bin/rc machine web-terminal --name demo --no-open -- sh -c 'echo wt-ok:$((6*7))'
```

预期:
- `web terminal: http://127.0.0.1:端口/?t=<32 位十六进制>`:只在回环地址,带 token;浏览器会自动打开
  (`--no-open` 只打印 URL);
- 浏览器中是真正的终端(xterm.js):Ctrl+C、方向键、全屏编辑器和窗口大小都会传到 guest,刷新后会重新显示最近的屏幕;
- 带命令时,屏幕显示 `wt-ok:42`,页面提示程序已退出,`rc` 以该程序的退出码退出;
- 没有 token 的 `/stream` 返回 403,非回环的 `Host`(DNS rebinding)也返回 403。
- 标签页 `claude`(命令)、`shell`(第一次点击时打开的独立终端)、`eventos`,最后是每个端口一个标签页:程序在 VM 中启动的服务器(任意端口)会以 iframe 显示应用,并在 host 的 `127.0.0.1` 上响应。

`coding-agent` 指南就用它在浏览器中打开 Claude Code:
`rc machine web-terminal --name agent -- claude --dangerously-skip-permissions`。

## 8w. 公共 playground (claude-in-chrome.runner.codes)

每位访客都有一台自己的 Fly Machine(Fly 不提供 `/dev/kvm`,所以 microVM 就是 Fly 的),Web 终端里是 Claude Code,端口在
`https://<端口>-<id>.runner.codes/`。最多 3 台;空闲 30 分钟关机,24 小时未使用则删除。

```bash
scripts/playground-cloudflare.sh     # claude-in-chrome 隧道、ingress 和 CNAME(幂等)
scripts/playground-fly.sh setup      # runner-codes-playground 和 runner-codes-visitors 应用、独立网络、secrets
scripts/playground-image.sh all      # 在 Fly 构建机上用 Nix 构建 visitor 和 front 镜像
scripts/playground-fly.sh deploy     # 发布 front
scripts/playground-token-check.sh    # front 的令牌只能访问访客应用
curl -s https://claude-in-chrome.runner.codes/ | grep -c FingerprintJS.load
```

预期:
- 页面打开,计算浏览器的 visitorId,并带你进入带 Claude Code 的终端 `/v/<id>/?t=<令牌>`;
- 无效访客调用 `/api/session` 返回 400;3 台机器都在使用时返回 503 "playground is full";
- `https://3000-0000000000.runner.codes/`(不存在的机器)返回 502,`docs.runner.codes` 仍为 200;
- `scripts/playground-token-check.sh`:访客应用 200,网站和 front 本身 4xx;
- 在访客机器内,连接 Fly 私有网络(`_api.internal`)失败,DNS 和互联网正常(`scripts/fly-firewall-probe.sh`
  在一台一次性机器上测同一套防火墙)。

## 9. GPU

仅当 `llama` 镜像已构建时执行(`rc doctor` 显示 `ok image llama`)。

```bash
./bin/rc run --stack llama --gpu --code 'vulkaninfo --summary | grep deviceName'
./bin/rc run --stack llama --gpu --timeout 120s --code 'llama-bench -m /opt/models/qwen2.5-0.5b-q4_k_m.gguf -p 128 -n 32 -ngl 99 -dev Vulkan0'
```

预期:
- `deviceName = Virtio-GPU Venus (Apple M1 Pro)`,以及一行 `llvmpipe`(软件实现的 Vulkan);
- `llama-bench` 的表格中有 `Vulkan0`,`pp128` 在 900–1.365 t/s 范围内,`tg32` 约 57 t/s。

详情见 [GPU](docusaurus/docs/advanced/gpu.md)。

## 10. HTTP API

在一个终端中:

```bash
./bin/rc serve --pool-size 0
```

在另一个终端中:

```bash
curl -s -X POST localhost:8080/api/v1/machines/teste/exec -H 'content-type: application/json' -d '{"command":["python3","-c","print(6*7)"]}'
curl -s localhost:8080/api/v1/machines | head -c 300; echo
```

预期:
- `{"exitCode":0,"stdout":"42\n","stderr":"",...}`;
- 列表中有 `"name":"teste"` 和 `"state":"running"`。

替代请求体和按行 stream,在同一个服务上:

```bash
curl -s -o /dev/null -w '%{http_code}\n' -X POST localhost:8080/api/v1/machines -H 'content-type: application/json' -d '{"name":"t2","image":"alpine","allowedHost":["a.com"]}'
curl -s -X POST localhost:8080/api/v1/machines -H 'content-type: application/json' -d '{"name":"t3","image":"alpine","allowedHosts":["api.example.com"],"memoryMb":512}'
curl -s -o /dev/null -w '%{http_code}\n' -X POST localhost:8080/api/v1/machines -H 'content-type: application/json' -d '{"name":"t4","image":"alpine","nestedVirt":true}'
curl -sN -X POST localhost:8080/api/v1/machines/teste/exec/stream -H 'content-type: application/json' -d '{"command":["sh","-c","echo a; echo b"]}'
curl -s -o /dev/null -w '%{http_code}\n' -X POST localhost:8080/api/v1/machines/teste/sync
```

预期:
- `400`:未知字段(`allowedHost`,拼写错误)会被拒绝,而不是被忽略后让出站保持开放;
- `"allow_hosts":["api.example.com"]`、`"enabled":true` 和 `"memory_mib":512`:替代名称同样有效;
- `nestedVirt` 返回 `400`(只有 `rc serve --allow-nested-virt` 时才放行);
- `event: stdout` / `data: a`,然后 `data: b`:每个事件一行原始文本,最后是 `event: exit`;
- `sync` 返回 `204`(机器没有 staged 卷:无事可做)。

`Ctrl+C` 结束 serve。机器仍在运行,因为它是分离的。

## 10b. Go 与 Python SDK

`serve` 仍在运行时:

```bash
go run ./examples/sdk-go http://127.0.0.1:8080
PYTHONPATH=sdk/python python3 examples/sdk-python/main.py http://127.0.0.1:8080
```

预期(两者):
- `job: 42`(临时 job);
- `machine: sdk-... running` 和 `exec: ola do sdk go`(或 `python`):机器已创建,文件已写入并执行;
- `error: NOT_FOUND`:API 的拒绝以带类型的错误返回,带有稳定的错误码;
- `./bin/rc machine ls` 中没有残留的 `sdk-*` 机器。

## 11. 清理

```bash
./bin/rc machine stop --name teste
./bin/rc machine delete --name teste
rm -rf /tmp/rc-up.bin /tmp/rc-down.bin /tmp/rc-demo /tmp/rc-vol
```

预期:`stopped teste` 和 `deleted teste`。`./bin/rc machine ls` 恢复为空。

## 结果不一致时

| 症状 | 排查位置 |
|---------|------------|
| `image for X not built` | `./bin/rc image build --stack X` |
| `read stream: the guest in this image does not support streaming` | streaming 之前的镜像:重新构建 |
| `INVALID_STATE: machine X is not running` | `./bin/rc machine start --name X` |
| `CONFLICT: machine X already exists` | 换个名称,或先 `delete` |
| `start` 失败并显示控制台末尾 | `./bin/rc machine logs --name X` |
| GPU:`ERROR_OUT_OF_HOST_MEMORY` | [GPU › 为什么用 Fedora](docusaurus/docs/advanced/gpu.md) |
| 停不下来的机器 | `./bin/rc machine stop --name X --timeout 0` 立即杀掉 |
