---
title: '屏幕控制'
description: '像人一样操作机器的屏幕：通过 CLI、API、SDK 以及面向 agent 的 MCP 服务器进行截图、点击、键盘输入和滚动'
---

# 屏幕控制

带[屏幕](/cli/display)的机器可以像人一样操作：截图、点击、输入文字、按快捷键和滚动。可以通过 CLI、HTTP API、Go 和
Python SDK 以及一个 MCP 服务器来操作。借助 MCP，像 Claude Code 这样的 agent 可以用操作浏览器的方式来操作 VM。

一切都经过屏幕查看器，也就是浏览器页面背后的同一个查看器：图像从它输出，鼠标和键盘则到达 VM 的 virtio-input 设备。
**guest 中不需要安装任何东西。**

## Agent 的箭头 {#a-seta-do-agente}

在浏览器里观看的人能看到自动化在哪里操作：一个带橙色描边和光晕的浅色箭头平滑移动到每个动作的位置（1.1 秒），然后才点击，就像人一样；点击时有一个圆圈脉动，按键时出现“digitando…”标签。最后一个动作后箭头保留 30 秒，然后消失。只有屏幕控制的动作（CLI、API、SDK 和 MCP）会画出箭头；页面上那个人的鼠标仍只是他自己的光标。

页面标题栏和标签页上的名称是 `runner.codes`；`runner.code` 里的 `display_title`（或 `--display-title`）可以更改。

## 通过 CLI {#pela-cli}

```bash
rc machine screen --name desk screenshot -o tela.jpg     # screen 1512x794 -> tela.jpg
rc machine screen --name desk click 640 400              # --button right, --double
rc machine screen --name desk type "ls -la"
rc machine screen --name desk key Return                 # ctrl+c, ctrl+alt+t, alt+F4, Up...
rc machine screen --name desk scroll 640 400 --up --amount 5
rc machine screen --name desk drag 100 100 400 300
rc machine screen --name desk move 640 400
echo '[{"action":"click","x":640,"y":400},{"action":"type","text":"ls\n"}]' | rc machine screen --name desk do
```

坐标是当前屏幕的像素，原点在左上角。屏幕可以[随浏览器窗口调整](/cli/display#tela-que-acompanha-a-janela)并改变尺寸；
因此每条命令都会在执行时读取当前尺寸。

## 动作 {#ações}

| 动作 | 字段 | 作用 |
|------|--------|-----|
| `click` | `x`、`y`、`button`（`left`、`right`、`middle`）、`double` | 移动到该点并点击 |
| `move` | `x`、`y` | 只移动指针（hover） |
| `drag` | `x`、`y`、`to_x`、`to_y` | 按下，经过中间点拖动，然后松开 |
| `type` | `text` | 输入文字，需要时自动按 Shift；`\n` 为 Enter |
| `key` | `keys` | 一个键或快捷键：`Return`、`Escape`、`Tab`、`ctrl+c`、`ctrl+shift+t`、`alt+F4`、`F5`、`Page_Down` |
| `scroll` | `x`、`y`、`direction`（`up`、`down`）、`amount`（默认 3） | 在该点滚动滚轮 |
| `wait` | `ms`（最多 60000） | 在动作之间等待 |

只要列表中有任何一个无效动作，整个列表都会在发出任何事件之前被拒绝：执行到一半的 `type`，或缺了最后一个键的快捷键，
都会让机器处于异常状态。

**`type` 按美式键盘输入**，只支持 ASCII。带重音的字符和 emoji 会被拒绝并给出提示。其他文本请在机器内用 `exec` 运行
`xdotool type`（[示例桌面](#exemplo-desktop-com-claude-code)已自带）。

## 通过 API {#pela-api}

```bash
curl -s -o tela.jpg -D - localhost:8080/api/v1/machines/desk/screen | grep -i x-screen
# X-Screen-Width: 1512
# X-Screen-Height: 794
curl -s -X POST localhost:8080/api/v1/machines/desk/screen/actions \
  -d '{"actions":[{"action":"click","x":640,"y":400},{"action":"type","text":"ls"},{"action":"key","keys":"Return"}]}'
# 204
```

| 路由 | 响应 |
|------|----------|
| `GET /api/v1/machines/:name/screen` | `200`，屏幕为 JPEG，尺寸在 `X-Screen-Width` 和 `X-Screen-Height` 中 |
| `POST /api/v1/machines/:name/screen/actions` | 所有动作都已发出时返回 `204`；每次调用最多 500 个动作 |

错误：无效动作或机器没有屏幕时返回 `400 CONFIG_ERROR`；机器已关闭或已暂停时返回 `409 INVALID_STATE`；机器的查看器
无响应时返回 `502 SCREEN_UNAVAILABLE`。

## 通过 SDK {#pelos-sdks}

```go title="Go"
shot, _ := c.Screenshot(ctx, "desk")                  // shot.JPEG, shot.Width, shot.Height
c.Click(ctx, "desk", 640, 400)
c.Type(ctx, "desk", "ls -la")
c.Key(ctx, "desk", "Return")
c.ScreenActions(ctx, "desk",
	client.ScreenAction{Action: "scroll", X: 640, Y: 400, Direction: "up"},
	client.ScreenAction{Action: "wait", Ms: 300},
	client.ScreenAction{Action: "key", Keys: "ctrl+c"})
```

```python title="Python"
shot = c.screenshot("desk")                           # shot.jpeg, shot.width, shot.height
c.click("desk", 640, 400)
c.type("desk", "ls -la")
c.key("desk", "Return")
c.screen_actions("desk", {"action": "scroll", "x": 640, "y": 400, "direction": "up"},
                 {"action": "wait", "ms": 300}, {"action": "key", "keys": "ctrl+c"})
```

## 面向 agent：`rc mcp` {#para-um-agente-rc-mcp}

`rc mcp` 是一个通过 stdio 通信的 [MCP](https://modelcontextprotocol.io) 服务器，绑定到一台机器。在 Claude Code 中注册后，
它为 agent 提供操作 VM 的工具：

```bash
claude mcp add rc -- rc mcp --name desk
```

| 工具 | 作用 |
|------------|-----|
| `screenshot` | 以图像返回屏幕，并返回以像素计的尺寸 |
| `click`、`move`、`drag`、`type`、`key`、`scroll` | 上表中的动作，每次调用一个 |
| `actions` | 在一次调用中按顺序执行多个动作 |
| `exec` | 用 `sh -c` 运行命令，返回退出码、stdout 和 stderr |

工具失败（机器已关闭、动作被拒绝、命令退出码非 0）会作为带 `isError` 的结果返回，让 agent 读取消息并尝试其他途径。

用 Claude Code 实测：我们要求它“截一张图，点击任务栏上的 `bash · claude` 标签，并确认窗口已切换”。它截了图，找到
标签并点击，看到屏幕还没变化，便等待，然后在第二张截图中确认了切换。

## 示例：带 Claude Code 的桌面 {#exemplo-desktop-com-claude-code}

[`examples/desktop-claude`](https://github.com/andrebassi/runner-codes/tree/main/examples/desktop-claude) 是一个
`runner.code` 中的 XFCE 桌面：黑色背景，Arquivos、Claude、Navegador 和 Terminal 四个快捷方式，Firefox 为默认浏览器，
并已安装 Claude Code。Claude 快捷方式会打开 `claude --dangerously-skip-permissions`；VM 就是 sandbox。Claude 的登录
信息通过[凭据](/cli/credentials)进入：guest 只能看到 placeholder。

[`examples/desktop-claude-agent`](https://github.com/andrebassi/runner-codes/tree/main/examples/desktop-claude-agent)
用三种方式向这个 Claude 派发任务，都通过 Go SDK：

| 模式 | 任务如何到达 | 屏幕上可见？ |
|------|---------------------|-------------|
| `headless` | 通过 exec 运行 `claude -p`；结果以 JSON 返回 | 否 |
| `gui` | 用 `xdotool` 在 Claude 窗口中输入（任意文本） | 是 |
| `keys` | 通过屏幕 API（`ScreenActions`）输入，VM 中无需安装任何东西 | 是 |

每种模式都让 Claude 生成一个内容已知的文件，并通过 API 核对该文件：“成功”指的是 VM 磁盘上有正确的文件，而不只是
一条回复。
