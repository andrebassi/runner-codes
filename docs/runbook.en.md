# Runbook — testing rc by hand, step by step

A sequence to check, in a fresh terminal, one command at a time, that `rc` does what it promises. Each step says what you
should see.

**This runbook is executable.** `task runbook` (`scripts/runbook.sh`) runs exactly these steps and checks each
expected result. It fails if any of them comes out different. Project rule: **every new feature gets a section here and
the matching step in `scripts/runbook.sh`, in the same delivery.**

| Last run | Host | Result |
|-----------------|------|-----------|
| 20/10/2026 | macOS 27, Apple M1 Pro | `task runbook`: 0 failures in 116 checks (up to 8u: user, secrets, socket bridges, ping, staged, refusals) |
| 19/10/2026 | macOS 27, Apple M1 Pro | `task runbook`: 0 failures in 50 checks (with volumes and network, steps 8c and 8d). Failure proof: with the gateway DNS unfiltered, 8d failed 2 checks |

Prerequisites: `task krun:install`, `task build` and the images (`./bin/rc image build --all`; step 9 needs the
`llama` image).

> Run with the e2e stopped: the tests check with `pgrep` that no VM was left behind, and a machine of yours that is running
> makes them fail.

## 0. Prepare

```bash
cd ~/works/labs/github/runner.codes
./bin/rc doctor
```

Expected: `ok` on `platform`, `libkrun`, `gpu` and `guest`, and one `ok image <stack>` line per built image.

## 1. Ephemeral job with streaming output

```bash
./bin/rc run --stack python --code $'import time\nprint("early")\ntime.sleep(1.5)\nprint("late")'
```

Expected: `early` shows up right away (~0.3 s, the boot) and `late` 1.5 s later, while the program is still running. At the end,
`[status=success exit=0 ...]` on stderr. The VM is destroyed when it finishes.

## 2. Create and start a machine

```bash
./bin/rc machine create --name teste --stack python
./bin/rc machine start --name teste
./bin/rc machine ls
```

Expected:
- `created teste (stack=python cpus=4 memory=8192MiB disk=10GiB gpu=false)`;
- `started teste (pid N) in ~200ms`;
- `teste` with `running` in `ls`.

The machine stays running after the command exits.

## 3. Write things in three places

```bash
./bin/rc machine exec --name teste -- sh -c 'echo persistiu > /etc/minha-config; echo dados > /workspace/arquivo.txt; echo some > /tmp/temporario.txt'
```

Expected: no output, and rc 0.

## 4. Stop and start again

```bash
./bin/rc machine stop --name teste
./bin/rc machine start --name teste
```

Expected: `stopped teste` and `started teste (pid N) in ~200ms`. The stop does a `sync` of the disk before shutting down.

## 5. Check what was kept

```bash
./bin/rc machine exec --name teste -- sh -c 'cat /etc/minha-config /workspace/arquivo.txt; cat /tmp/temporario.txt'
```

Expected:

```
persistiu
dados
cat: can't open '/tmp/temporario.txt': No such file or directory
```

`/etc` (writable layer over the image) and `/workspace` (the machine's disk) persist; `/tmp` is tmpfs and comes back empty.

## 6. Copy a file to the machine and back

```bash
head -c 20000000 /dev/urandom > /tmp/rc-up.bin
./bin/rc machine cp /tmp/rc-up.bin teste:/workspace/up.bin
./bin/rc machine exec --name teste -- sha256sum /workspace/up.bin
shasum -a 256 /tmp/rc-up.bin
./bin/rc machine cp teste:/workspace/up.bin /tmp/rc-down.bin
cmp /tmp/rc-up.bin /tmp/rc-down.bin && echo "idênticos"
```

Expected:
- `uploaded 20000000 bytes ... in ~230ms`;
- the same sha256 inside and outside the machine;
- `downloaded ... in ~230ms`;
- `idênticos`.

## 7. Exit code and streaming inside the machine

```bash
./bin/rc machine exec --name teste -- sh -c 'echo a; sleep 1; echo b; exit 7'; echo "rc=$?"
```

Expected: `a`, `b` one second later and `rc=7`. The exit code of `rc` is the command's.

## 7b. Interactive terminal on the machine

```bash
./bin/rc machine shell --name teste
```

Inside the shell that opens, type:

```bash
test -t 0 && echo IS-TTY; stty size     # IS-TTY e o tamanho do seu terminal
sleep 30                                 # aperte Ctrl+C: interrompe só o sleep
echo ALIVE                               # o shell continua vivo
exit 4
```

Outside, `echo $?` shows `4`. The machine stays running: the shell is a session, not the VM.

Expected: a real terminal, with Ctrl+C, arrow keys, history, `vi` and window resizing. It is `bash` if the image
has it; on python (Alpine) it is `sh`.

## 8. Machine logs

```bash
./bin/rc machine logs --name teste --tail 10
```

Expected: the last lines of the console (the guest logs every operation). Also works with the machine stopped.

## 8b. Machine declared in a `runner.code`

The [`runner.code`](docusaurus/docs/cli/runner-code.md) is the machine declared in a file: stack or image, resources, env, init and
workload in a versionable TOML.

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

Expected:
- `create` without `-f` says `from runner.code`, and the status shows `"cpus": 1`: the flag wins over the file;
- `logs --source workload` shows `up hello in /app`: the workdir was created and env and workdir reached the workload;
- the first `exec` prints `1`, `hello`, `/app`;
- the second prints `1` and `saved`: init did **not** run again on the second start, and the stop's `SIGINT` reached the
  workload, which wrote the file before the sync.

A key that `runner.code` does not implement yet is refused, never ignored:

```bash
printf 'stack = "python"\n[health]\nexec = ["true"]\n' > /tmp/rc-demo/bad.code
./bin/rc machine create --name x -f /tmp/rc-demo/bad.code
```

Expected: `key "health" is not supported yet: health checks`, and nothing created.

## 8c. Volumes: a host directory inside the machine

```bash
mkdir -p /tmp/rc-vol/ro && echo "do host" > /tmp/rc-vol/hello.txt
./bin/rc machine create --name vol --stack python -v /tmp/rc-vol:/data -v /tmp/rc-vol/ro:/ro:ro -w /data
./bin/rc machine start  --name vol
./bin/rc machine exec   --name vol -- sh -c 'cat hello.txt; echo do-guest > from-guest.txt; echo y > /ro/y; echo ro=$?'
cat /tmp/rc-vol/from-guest.txt
./bin/rc machine stop   --name vol
./bin/rc machine delete --name vol
```

Expected:
- `do host`: the guest reads the host directory;
- `Read-only file system` and `ro=1`: `:ro` is enforced by libkrun on the device, and the guest's root cannot undo it;
- on the host, `cat` shows `do-guest`: the guest's write shows up on the host immediately.

## 8d. Network: egress, policy and port

A machine is born **without network**. `--net` turns egress on; `--allow-host` turns it on and restricts it; `-p` publishes a port on the
host's `127.0.0.1`.

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

Expected:
- `ok=0` for `example.com`;
- `wget: bad address 'iana.org'` **immediately**: the gateway DNS answers NXDOMAIN, with no waiting for a timeout;
- `meta=1`: cloud metadata (`169.254.169.254`) is refused even with the network open;
- after the `update`, `iana.org` goes through **without restarting** the machine;
- `curl` on the host receives `do-guest` through the published port;
- the denial log has `deny dns iana.org: name not in the allow list`.

## 8e. Ephemeral machine: `machine run`

```bash
./bin/rc machine run --stack python -- python3 -c 'print("ola"); raise SystemExit(3)'; echo "rc=$?"
echo dados | ./bin/rc machine run -i --stack python -- sh -c 'read x; echo leu:$x'
./bin/rc machine run --net --image alpine -- uname -a
./bin/rc machine run -it --image alpine -- /bin/sh        # terminal; `exit` sai e apaga a máquina
./bin/rc machine ls
```

Expected:
- `ola` and `rc=3`: the exit code is the command's;
- `leu:dados`: `-i` forwards stdin (without `-i` stdin is not read);
- the `uname -a` of the libkrunfw kernel on an image outside the catalog (built on first use);
- `ls` with no `run-…` at all: the ephemeral machine disappears at the end, including on Ctrl+C.

## 8f. A published port reaches a server that only listens on localhost

```bash
./bin/rc machine run -d --name web --stack python -p 18080:9000 -- python3 -m http.server 9000 --bind 127.0.0.1
curl -s -o /dev/null -w '%{http_code}\n' http://127.0.0.1:18080/
./bin/rc machine delete --name web -f
```

Expected: `200`. The published port enters through the guest (`dial` op over vsock) and dials `127.0.0.1` from there, so a
server that only listens on localhost, like vite or openclaw, answers.

## 8g. Screen in the browser: display, keyboard and mouse

```bash
./bin/rc machine run -d --name desktop -s examples/desktop/runner.code
open http://localhost:6080
./bin/rc machine stop --name desktop
```

Expected:
- the Weston desktop appears on the page, with the guest's panel and clock;
- clicking the panel icon opens the terminal;
- what you type on the page reaches the terminal, including uppercase letters, symbols and Ctrl.

`task runbook` checks the same path without needing a human eye:
- an alpine machine with `--display 1024x768`;
- `modetest` turns the screen on;
- the 1024x768 frame reaches the viewer;
- a `KEY_A` sent via `POST /input` shows up in the guest's `evtest`.

## 8h. Examples

```bash
./bin/rc machine run -s examples/python-app/runner.code -- python3 --version
./bin/rc machine run -s examples/node-app/runner.code -- node -e 'console.log("ola", 1+1)'
./bin/rc machine run -d --name doom -s examples/doom-web/runner.code && open http://localhost:8080
```

Expected:
- `Python 3.12.x`;
- `ola 2`;
- DOOM in the browser.

The other examples and what each one measures are in [`examples/README.md`](../examples/README.md). The
`docker-in-vm` example has its own check, `examples/docker-in-vm/check.sh`.

## 8i. Branch and pause

```bash
./bin/rc machine create --name mom --stack python && ./bin/rc machine start --name mom
./bin/rc machine exec   --name mom -- sh -c 'echo antes > /root/marca'
./bin/rc machine pause  --name mom && ./bin/rc machine status --name mom
./bin/rc machine branch --from mom --name kid
./bin/rc machine resume --name mom
./bin/rc machine start  --name kid && ./bin/rc machine exec --name kid -- cat /root/marca
./bin/rc machine delete --name kid -f && ./bin/rc machine delete --name mom -f
```

Expected:
- `status` shows `paused`, and an `exec` in that state is refused with the reason;
- the child shows `antes`: the branch carries what the parent wrote, even while it is paused (`rc` resumes it for an instant
  for the `sync`);
- after that, parent and child are independent.

## 8j. Named network

```bash
./bin/rc machine run -d --name db --stack python --network lab -- python3 -m http.server 8080
./bin/rc machine create --name app --stack python --network lab && ./bin/rc machine start --name app
./bin/rc machine exec --name app -- wget -qO- -T 3 http://100.98.0.10:8080/
./bin/rc machine exec --name app -- wget -qO- -T 3 http://db.lab.internal:8080/
./bin/rc machine delete --name db -f
./bin/rc machine exec --name app -- nslookup db.lab.internal
./bin/rc machine delete --name app -f
```

Expected:
- the two machines get consecutive IPs from the network (`db` is `100.98.0.10`, `app` is `100.98.0.11`) and reach each other by IP
  and by the name `<máquina>.<rede>.internal`, without `--net`: traffic inside the network is not egress;
- a machine on another network does not reach these, nor resolve their names;
- a deleted `db` disappears from the network's DNS: `nslookup` no longer returns its IP.

## 8k. S3 volume

A local S3, serving a folder (MinIO left nixpkgs on 11/10/2026):

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

Expected:
- `start` mounts the bucket (with a wrong key, it fails with `s3: 403` and the reason);
- the guest reads `ola do bucket`, and `novo.txt` appears in the server's folder with `do-guest`.

## 8l. A credential the machine uses without reading it

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

Expected:
- `guest: RC_PLACEHOLDER_TOK_...`: the guest only has the placeholder;
- `server: Bearer valor-do-runbook`: the server received the real value, which came from the `start` environment.

The User-Agent is custom because postman-echo's Cloudflare refuses `Python-urllib` (403, error code 1010).

## 8m. Pack: the machine in a file

```bash
./bin/rc machine create --name pk --stack python && ./bin/rc machine start --name pk
./bin/rc machine exec --name pk -- sh -c 'echo marca-do-runbook > /root/marca'
./bin/rc machine stop --name pk
./bin/rc pack create --from-vm pk --output /tmp/one --single-file
./bin/rc machine delete --name pk
/tmp/one run -- cat /root/marca
/tmp/one
```

Expected:
- `marca-do-runbook`: the artifact carried the machine's disk, even with the source deleted (a pack that lost the disk
  would still boot and exit 0; that is why a value is checked);
- `/tmp/one` alone shows `usage: one run ...`.

## 8n. User, disk volume and `docker.sock`

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

Expected:
- `nobody`, and `root` with `--user root` (the exec's `--user` wins over the machine's);
- `ext2/ext3`: the disk volume is real ext4, outside the overlay (which Docker's overlay2 needs);
- `do-guest:ping`: the host talks to the guest's server through the `docker.sock` in the machine directory.

## 8o. Secrets and SSH agent

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

Expected:
- `valor-secreto do-arquivo`: both secrets in the environment of every command;
- `ssh-add -l` lists the key from `/tmp/rb-id` (the private key stays in the host's agent);
- `grep` finds nothing: the value is not kept in the machine's record.

The agent and the key are disposable, specific to this step: nothing touches the agent of whoever runs it.

## 8p. Socket bridges in both directions

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

Expected: `do-guest:ping` (the host reaches the socket the workload listens on) and `do-host:pong` (the guest reaches the
host's socket, in a directory the image did not have).

## 8q. Ping and port ranges

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

Expected:
- `200`: the range publishes one port per number (up to 64), and the middle one answers;
- the ping to `1.1.1.1` comes back with the network open, and only if the host received the reply (no reply is made up);
- outside the allowlist the ping does not come back, and the denial `deny icmp 1.1.1.1` goes to the log.

## 8r. Staged volume and sync

```bash
mkdir -p /tmp/rb-repo && echo original > /tmp/rb-repo/a.txt && echo x > /tmp/rb-repo/old.txt
./bin/rc machine create --name stg --stack python -v /tmp/rb-repo:/work:staged
./bin/rc machine start --name stg
./bin/rc machine exec --name stg -- sh -c 'stat -f -c %T /work; echo mudou > /work/a.txt; rm /work/old.txt; printf "#!/bin/sh\n" > /work/run.sh; chmod 755 /work/run.sh'
cat /tmp/rb-repo/a.txt
./bin/rc machine sync --name stg
cat /tmp/rb-repo/a.txt; ls /tmp/rb-repo; ls -l /tmp/rb-repo/run.sh
```

Expected:
- `ext2/ext3`: the copy lives on the machine's disk, with no virtio-fs round trips;
- before the sync the host still has `original`;
- after: `mudou`, no `old.txt` (sync mirrors: what disappeared from the copy disappears from the host) and `run.sh` with `-rwxr-xr-x`.

A graceful stop and the end of `machine run` also sync; `--timeout 0` does not. The child of a branch returns nothing
to the host.

## 8s. Refusals that state the reason

```bash
./bin/rc doctor | grep -E 'cuda|nested'
./bin/rc machine create --name x --image alpine --cuda
time ./bin/rc machine run --nested --image alpine -- ls -l /dev/kvm
```

Expected:
- `info  cuda: not supported (no CUDA remoting; --gpu is Vulkan)` and this host's nested line;
- `--cuda is not supported: rc has no CUDA remoting ...`, without creating a machine;
- on a host that does not nest (M1/M2), `failed to start: Error: nested virtualization is not supported on this host ...`
  in under 1 s (before: 30 s and `did not become ready`); on a host that nests, `/dev/kvm` inside the VM.

## 8t. Machine directory and cache cleanup

```bash
ls "$(./bin/rc machine data-dir --name usr)"
./bin/rc image prune
./bin/rc pack prune
```

Expected: `machine.json` in the machine directory; both prunes exit 0 (without `--all`, `image prune` only deletes
the ext4 no name points to anymore, the previous build of an image, and `pack prune` keeps the 5 most recently used).

## 8u. The guides

```bash
task guides DIRS=gpu-cuda      # uma pasta; sem DIRS, todas (cada uma liga as suas VMs)
```

Expected: `falhas: 0` in each folder. What each folder proves is in `examples/guides/README.md`.

## 8v. Web terminal

```bash
./bin/rc machine web-terminal --name demo                 # opens the browser with the machine's shell
./bin/rc machine web-terminal --name demo --no-open -- sh -c 'echo wt-ok:$((6*7))'
```

Expected:
- `web terminal: http://127.0.0.1:PORT/?t=<32 hex>`: loopback only, with a token; the browser opens by itself
  (`--no-open` only prints);
- in the browser, a real terminal (xterm.js): Ctrl+C, arrow keys, full-screen editors and the window size reach the
  guest, and a reload shows the recent screen again;
- with the command, the screen shows `wt-ok:42`, the page says the program exited and `rc` exits with its code;
- `/stream` without the token answers 403, and so does a non-loopback `Host` (DNS rebinding).
- tabs `claude` (the command), `shell` (a separate terminal, on first click), `eventos` and, last, one tab per port: a server the program starts in the VM (any port) shows up with the app in an iframe and answers on the host's `127.0.0.1`.

The `coding-agent` guide uses this to open Claude Code in the browser:
`rc machine web-terminal --name agent -- claude --dangerously-skip-permissions`.

## 9. GPU

Only if the `llama` image is built (`rc doctor` shows `ok image llama`).

```bash
./bin/rc run --stack llama --gpu --code 'vulkaninfo --summary | grep deviceName'
./bin/rc run --stack llama --gpu --timeout 120s --code 'llama-bench -m /opt/models/qwen2.5-0.5b-q4_k_m.gguf -p 128 -n 32 -ngl 99 -dev Vulkan0'
```

Expected:
- `deviceName = Virtio-GPU Venus (Apple M1 Pro)`, and also an `llvmpipe` line (software Vulkan);
- the `llama-bench` table with `Vulkan0`, `pp128` in the 900–1,365 t/s range and `tg32` at ~57 t/s.

Details in [GPU](docusaurus/docs/advanced/gpu.md).

## 10. HTTP API

In one terminal:

```bash
./bin/rc serve --pool-size 0
```

In another:

```bash
curl -s -X POST localhost:8080/api/v1/machines/teste/exec -H 'content-type: application/json' -d '{"command":["python3","-c","print(6*7)"]}'
curl -s localhost:8080/api/v1/machines | head -c 300; echo
```

Expected:
- `{"exitCode":0,"stdout":"42\n","stderr":"",...}`;
- the list with `"name":"teste"` and `"state":"running"`.

The alternative bodies and the per-line stream, on the same server:

```bash
curl -s -o /dev/null -w '%{http_code}\n' -X POST localhost:8080/api/v1/machines -H 'content-type: application/json' -d '{"name":"t2","image":"alpine","allowedHost":["a.com"]}'
curl -s -X POST localhost:8080/api/v1/machines -H 'content-type: application/json' -d '{"name":"t3","image":"alpine","allowedHosts":["api.example.com"],"memoryMb":512}'
curl -s -o /dev/null -w '%{http_code}\n' -X POST localhost:8080/api/v1/machines -H 'content-type: application/json' -d '{"name":"t4","image":"alpine","nestedVirt":true}'
curl -sN -X POST localhost:8080/api/v1/machines/teste/exec/stream -H 'content-type: application/json' -d '{"command":["sh","-c","echo a; echo b"]}'
curl -s -o /dev/null -w '%{http_code}\n' -X POST localhost:8080/api/v1/machines/teste/sync
```

Expected:
- `400`: an unknown field (`allowedHost`, with a typo) is refused, not ignored with egress left open;
- `"allow_hosts":["api.example.com"]`, `"enabled":true` and `"memory_mib":512`: the alternative names are accepted;
- `400` on `nestedVirt` (only allowed with `rc serve --allow-nested-virt`);
- `event: stdout` / `data: a`, then `data: b`: one line per event, raw text, and `event: exit` at the end;
- `204` on `sync` (machine without a staged volume: nothing to do).

`Ctrl+C` stops serve. The machine stays running, because it is detached.

## 10b. Go and Python SDKs

With `serve` still up:

```bash
go run ./examples/sdk-go http://127.0.0.1:8080
PYTHONPATH=sdk/python python3 examples/sdk-python/main.py http://127.0.0.1:8080
```

Expected, in both:
- `job: 42` (ephemeral job);
- `machine: sdk-... running` and `exec: ola do sdk go` (or `python`): machine created, file written and executed;
- `error: NOT_FOUND`: the API refusal arrives as a typed error, with the stable code;
- no `sdk-*` machine is left in `./bin/rc machine ls`.

## 11. Clean up

```bash
./bin/rc machine stop --name teste
./bin/rc machine delete --name teste
rm -rf /tmp/rc-up.bin /tmp/rc-down.bin /tmp/rc-demo /tmp/rc-vol
```

Expected: `stopped teste` and `deleted teste`. `./bin/rc machine ls` comes back empty.

## When something comes out different

| Symptom | Where to look |
|---------|------------|
| `image for X not built` | `./bin/rc image build --stack X` |
| `read stream: the guest in this image does not support streaming` | image from before streaming: rebuild |
| `INVALID_STATE: machine X is not running` | `./bin/rc machine start --name X` |
| `CONFLICT: machine X already exists` | another name, or `delete` first |
| `start` fails with the end of the console | `./bin/rc machine logs --name X` |
| GPU: `ERROR_OUT_OF_HOST_MEMORY` | [GPU › Why Fedora](docusaurus/docs/advanced/gpu.md) |
| machine that does not stop | `./bin/rc machine stop --name X --timeout 0` kills it immediately |
