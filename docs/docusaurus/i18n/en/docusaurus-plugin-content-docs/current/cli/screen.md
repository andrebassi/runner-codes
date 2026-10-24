---
title: 'Screen control'
description: 'Operate a machine screen like a person: capture, click, keyboard and scroll through the CLI, the API, the SDKs and an MCP server for agents'
---

# Screen control

A machine with a [screen](/cli/display) can be operated the way a person would: take a screenshot, click, type, press
shortcuts and scroll. It works through the CLI, the HTTP API, the Go and Python SDKs and an MCP server. With MCP, an agent
like Claude Code operates the VM the same way it operates a browser.

Everything goes through the screen viewer, the same one behind the browser page: the image comes out of it, and mouse
and keyboard reach the VM's virtio-input devices. **Nothing is installed in the guest.**

## The agent's arrow

Whoever watches in the browser sees where the automation acts: a light arrow with an orange outline and glow glides to the point of
each action (1.1 s) and only then clicks, as a person would; a
circle pulses on a click, and a "digitando…" badge shows up on keystrokes. It stays on screen for 30 s after the last action. Only
the screen-control actions (CLI, API, SDKs and MCP) draw the arrow; the mouse of the person on the page stays just their
own cursor.

The name on the page bar and tab is `runner.codes`; `display_title` in the `runner.code` (or `--display-title`) changes it.

## Through the CLI

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

Coordinates are pixels of the current screen, with the origin at the top-left corner. The screen can
[follow the browser window](/cli/display#screen-that-follows-the-window) and change size; that is why every command reads
the size at that moment.

## Actions

| Action | Fields | Does |
|------|--------|-----|
| `click` | `x`, `y`, `button` (`left`, `right`, `middle`), `double` | moves to the point and clicks |
| `move` | `x`, `y` | only moves the pointer (hover) |
| `drag` | `x`, `y`, `to_x`, `to_y` | presses, drags through intermediate points and releases |
| `type` | `text` | types, with Shift when needed; `\n` is Enter |
| `key` | `keys` | one key or shortcut: `Return`, `Escape`, `Tab`, `ctrl+c`, `ctrl+shift+t`, `alt+F4`, `F5`, `Page_Down` |
| `scroll` | `x`, `y`, `direction` (`up`, `down`), `amount` (default 3) | turns the wheel at the point |
| `wait` | `ms` (up to 60000) | waits between actions |

A list with any invalid action is rejected as a whole, before any event goes out: a half-done `type` or a shortcut
missing its last key would leave the machine in a strange state.

**`type` types like a US keyboard** and covers ASCII only. Accents and emoji are rejected with a message. For other text,
use `exec` with `xdotool type` inside the machine (the [example desktop](#example-desktop-with-claude-code) already ships
it).

## Through the API

```bash
curl -s -o tela.jpg -D - localhost:8080/api/v1/machines/desk/screen | grep -i x-screen
# X-Screen-Width: 1512
# X-Screen-Height: 794
curl -s -X POST localhost:8080/api/v1/machines/desk/screen/actions \
  -d '{"actions":[{"action":"click","x":640,"y":400},{"action":"type","text":"ls"},{"action":"key","keys":"Return"}]}'
# 204
```

| Route | Response |
|------|----------|
| `GET /api/v1/machines/:name/screen` | `200` with the screen as JPEG and the size in `X-Screen-Width` and `X-Screen-Height` |
| `POST /api/v1/machines/:name/screen/actions` | `204` when every action went out; up to 500 actions per call |

Errors: `400 CONFIG_ERROR` for an invalid action or a machine without a screen; `409 INVALID_STATE` for a stopped or
paused machine; `502 SCREEN_UNAVAILABLE` when the machine's viewer does not respond.

## Through the SDKs

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

## For an agent: `rc mcp` {#para-um-agente-rc-mcp}

`rc mcp` is an [MCP](https://modelcontextprotocol.io) server over stdio, bound to one machine. Registered in Claude Code,
it gives the agent the tools to operate the VM:

```bash
claude mcp add rc -- rc mcp --name desk
```

| Tool | Does |
|------------|-----|
| `screenshot` | returns the screen as an image and its size in pixels |
| `click`, `move`, `drag`, `type`, `key`, `scroll` | the actions from the table above, one per call |
| `actions` | several actions in order, in one call |
| `exec` | runs a command with `sh -c` and returns the exit code, stdout and stderr |

A tool failure (machine stopped, action rejected, command exiting non-zero) comes back as a result with `isError`, so the
agent can read the message and try another path.

Measured with Claude Code: we asked "take a screenshot, click the `bash · claude` tab on the taskbar and confirm that the
window changed". It took the screenshot, found the tab, clicked, saw the screen not yet changed, waited and confirmed the
switch in the second screenshot.

## Example: desktop with Claude Code

[`examples/desktop-claude`](https://github.com/andrebassi/runner-codes/tree/main/examples/desktop-claude) is an XFCE
desktop in a `runner.code`: black background, the Arquivos, Claude, Navegador and Terminal shortcuts, Firefox as the
default browser and Claude Code installed. The Claude shortcut opens `claude --dangerously-skip-permissions`; the VM is
the sandbox. The Claude login enters as a [credential](/cli/credentials): the guest only sees a placeholder.

[`examples/desktop-claude-agent`](https://github.com/andrebassi/runner-codes/tree/main/examples/desktop-claude-agent)
sends tasks to that Claude in three ways, all through the Go SDK:

| Mode | How the task arrives | Visible on screen? |
|------|---------------------|-------------|
| `headless` | `claude -p` via exec; the answer comes back as JSON | no |
| `gui` | typed into the Claude window with `xdotool` (any text) | yes |
| `keys` | typed through the screen API (`ScreenActions`), with nothing installed in the VM | yes |

Each mode asks Claude for a file with known content and checks the file through the API: "it worked" means the right
file on the VM's disk, not just an answer.
