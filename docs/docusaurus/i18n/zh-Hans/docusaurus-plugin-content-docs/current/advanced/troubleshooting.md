---
title: '故障排查'
description: '已知错误、原因与修复方法'
---

## 第一步 {#primeiro-passo}

```bash
rc doctor
```

检查平台、libkrun、GPU、CUDA、嵌套虚拟化、guest、进程约束和镜像。每行以 `ok`、`info`、`warn` 或 `FAIL` 开头，
失败时会给出原因。

## Couldn't find or load libkrunfw.5.dylib {#couldnt-find-or-load-libkrunfw5dylib}

**原因：** 上游 libkrun 只按名字打开 libkrunfw（`dlopen("libkrunfw.5.dylib")`），而动态加载器在标准路径之外找不到它。

**修复：** `.#krun`（macOS）提供的 libkrun 已经按 Nix store 中的绝对路径打开 libkrunfw，所以这个错误只会出现在其他来源的 libkrun 上。这种情况下 launcher 仍会把 `DYLD_LIBRARY_PATH` 设为 `RUNNER_KRUN_LIB` 所在目录：确认它是完整路径，并且 libkrunfw 位于同一目录。

## krun_start_enter rc=-22 {#krun_start_enter-rc-22}

**原因：** 在 macOS 上，二进制没有 entitlement `com.apple.security.hypervisor`。

**修复：** 用 `task build` 编译，它会为 host 签名。直接 `go build` 生成的二进制没有签名。

## /opt/runner/guest: not found {#optrunnerguest-not-found}

**原因：** guest 被编译成了动态链接。镜像是 Alpine（musl），没有 glibc 的加载器；内核找到了文件，却找不到解释器，于是报告 `not found`。

**修复：** 用 `task build` 编译，它会生成静态 guest，并在其为动态链接时报错。然后重新构建镜像。

## 修改 guest 后仍是旧行为 {#comportamento-antigo-depois-de-mudar-o-guest}

**原因：** 正在使用的镜像是用之前的 guest 构建的。

**修复：** 重新运行 `rc image build --stack <l>`。缓存键包含 guest，所以会自动生成新镜像；无需删除任何东西。

## 带空格的参数到 guest 中被拆开 {#argumento-com-espaço-chega-partido-no-guest}

**原因：** libkrun 通过内核 cmdline 传递第一个进程的 argv，而 cmdline 按空格分隔。

**修复：** 不要把复合命令作为参数传递。写入文件并执行该文件——`start.sh` 和构建时的 warmup 就是这么做的。

## VM for X did not become ready ... console: ... {#vm-for-x-did-not-become-ready--console-}

**原因：** guest 没有响应就绪等待发出的 `sh true` 作业。消息中带有 guest 控制台的末尾内容，通常能说明原因（动态链接的 guest、`start.sh` 失败、镜像损坏）。

**修复：** 阅读消息中的控制台片段。最常见的情况就是本页列出的：动态链接的 guest 或过期的镜像。

## machine X did not become ready / failed to start {#machine-x-did-not-become-ready--failed-to-start}

**原因：** machine 的 `vmm` 进程在启动时退出，或者 guest 没有响应。消息会引用**本次** start 写入 `vmm.log` 的最后
一行（日志是追加写入的），例如 `nested virtualization is not supported on this host`。

**修复：** 阅读引用的那一行。完整日志是 `rc machine data-dir --name X` 输出的目录下的 `vmm.log`；控制台输出可通过
`rc machine logs --name X` 查看。

## INVALID_STATE: machine X is running; stop it first or use force {#invalid_state-machine-x-is-running-stop-it-first-or-use-force}

**原因：** 对运行中的 machine 执行 `delete`（以及其他要求 machine 处于停止状态的操作）。

**修复：** 先执行 `rc machine stop --name X`，或用 `--force` 强制杀掉。

## machine X is paused; resume it (rc machine resume) or stop it first {#machine-x-is-paused-resume-it-rc-machine-resume-or-stop-it-first}

**原因：** 对已暂停的 machine 执行 `start` 或 `exec`。

**修复：** `rc machine resume --name X`，或用 `stop` 关机。

## resolve secret 'NOME': env_unset {#resolve-secret-nome-env_unset}

**原因：** 某个 `--secret-env`（或带 `from_env` 的 `[secrets]`）指向的变量在执行 `start` 的环境中不存在。

**修复：** 在 `start` 之前 export 该变量；每次 start 时都会读取该值，且从不落盘。

## machine 中的程序无法解析域名（`bad address`） {#o-programa-na-máquina-não-resolve-um-nome-bad-address}

**原因：** 出站策略不允许该域名；网关的 DNS 有意返回 NXDOMAIN。

**修复：** 查看 machine 目录（`rc machine data-dir`）中的 `egress-denials.log`，并在 machine 运行时用
`rc machine update --name X --allow-host <nome>` 放行。见[网络与卷](/cli/network)。

## E007 {#e007}

**原因：** 在目录中没有 `gpu` 的 stack 上使用了 `--gpu`（或 `"gpu": true`）。

**修复：** 使用带 GPU 的 stack，例如 `llama`。不存在 CUDA：GPU 走的是 Vulkan（Venus）。见 [GPU](/advanced/gpu)。

## E006: unsupported language {#e006-unsupported-language}

**原因：** 该语言不在目录中，**或者**它的镜像尚未在这台 host 上构建。

**修复：** `rc languages` 显示目录和 `ready` 状态。用 `rc image build --stack <l>` 构建。

## E005: stdin too long {#e005-stdin-too-long}

**原因：** stdin 超过 1.000.000 字节。

**修复：** 减小输入，或把数据作为文件放在 `files` 中发送（所有文件合计最多 4 MiB）。

## 无法访问 /dev/kvm（Linux） {#sem-acesso-a-devkvm-linux}

**原因：** 用户对 `/dev/kvm` 没有读写权限，或者 KVM 未启用。

**修复：** 在 host 上启用 KVM，并授予用户访问该设备的权限（通常是加入 `kvm` 组）。

## Windows {#windows}

不支持：上游 libkrun 没有 WHP 后端。为 Windows 构建会有意失败。

## API 返回了 "status": "success"，但程序失败了 {#a-api-respondeu-status-success-mas-o-programa-falhou}

这不是错误。`status` 表示 API 执行了该作业。程序的结果在 `stderr` 和 `exception`（`"Timeout"`、`"Compilation failed"`）中。
