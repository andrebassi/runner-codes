---
title: 'Tela no navegador'
description: 'Display de uma máquina servido no navegador: framebuffer virtio-gpu, teclado e mouse por virtio-input, sem cliente VNC'
---

Uma máquina com `--display` ganha uma tela. O `rc` a serve numa página no `127.0.0.1` do host, e o teclado e o mouse da
página voltam para a VM. Não precisa de cliente VNC nem de servidor gráfico no host.

## Em uma tela

```bash
rc machine create --name tela --image alpine:latest --gpu --net --display 1024x768
rc machine start  --name tela
open http://localhost:6080
rc machine create --name ui --image alpine:latest --gpu --display 1920x1080 --display-port 7000
```

No `runner.code`:

```toml
gpu = true
display = "1280x800"
display_port = 6080      # padrão
```

## Como funciona

```
guest                                    processo `rc vmm` da máquina (host)
  KMS / Wayland / X  ─▶ virtio-gpu 2D ─▶ backend de display (callbacks C, purego)
                                           └─ tela compartilhada (RGBA, versão)
                                                └─ GET /frame?after=N  (JPEG, long-poll) ─▶ página
  evdev (libinput) ◀─ virtio-input ◀─── filas de teclado e tablet ◀─ POST /input (eventos evdev) ◀─ página
```

- **Saída:**
  - a libkrun chama o nosso backend (`krun_set_display_backend`): `configure_scanout`, `alloc_frame` e
    `present_frame`;
  - os buffers são memória `mmap`, fora do GC;
  - cada quadro apresentado vira RGBA, com a conversão de formato (BGRX, XRGB...), e acorda quem espera;
  - o visualizador responde ao `GET /frame` com JPEG só quando há quadro novo.
- **Entrada:**
  - são dois dispositivos `virtio-input` (`krun_add_input_device`): `runner.code keyboard` e `runner.code
    tablet`, este com mouse absoluto, três botões e roda;
  - a página converte `KeyboardEvent.code` em keycode do evdev e sincroniza Ctrl, Shift, Alt e Meta pelo estado
    do evento;
  - o host entrega um pacote por vez (até o `SYN_REPORT`), com 3 ms entre pacotes.
- **HTTP simples em vez de VNC/WebSocket:** no loopback, o long-poll passa de 30 quadros por segundo, e a página
  não depende de biblioteca.

## A troca: tela 2D, sem Vulkan

No macOS, o virtio-gpu com Venus (Vulkan) não atende recurso 2D.

- **O que acontece sem o patch:** o recurso que o guest cria para a tela falha (medido:
  `TRANSFER_TO_HOST_2D → ERR_UNSPEC`, `SET_SCANOUT → INVALID_RESOURCE_ID`), e nenhum quadro sai.
- **O patch:** a libkrun do `rc` (`nix/libkrun-darwin.nix`) aceita `KRUN_GPU_BACKEND=2d`, que troca o componente
  do rutabaga pelo `Rutabaga2D`, um framebuffer por CPU. O processo de uma VM com tela liga essa variável.
- **O preço:** a VM com tela não tem Vulkan.
  - Compositor que desenha por CPU funciona (Weston com `--renderer=pixman`, o `modetest`, o X com framebuffer).
  - Compositor que exige GLES (Hyprland, GNOME Shell) cai no `llvmpipe`, lento.
- **VM sem tela:** continua com o Venus, para o `llama.cpp` e o resto.

## Requisitos no guest

| Para | Precisa |
|------|---------|
| a tela | `gpu = true`; o kernel da libkrunfw já traz o `virtio_gpu` (DRM/KMS, sem fbdev: não há `/dev/fb0`) |
| teclado e mouse num compositor | `udev` rodando (o libinput enumera por ele); no Alpine, `eudev` + `udevadm trigger` |
| compositor sem logind | `seatd` |

## Segurança

- O visualizador escuta **só no `127.0.0.1`** do host. Quem alcança a porta vê a tela e comanda teclado e mouse da VM,
  então não exponha a porta.
- Não há senha: a proteção é escutar só no loopback.

## Números medidos (macOS 27, M1 Pro)

| Medida | Valor |
|--------|-------|
| `machine run -d` do exemplo desktop (init já feito) | 5 s até o Weston |
| primeira vez (init instala Weston, eudev, seatd) | 12 s |
| texto de 50 caracteres com Shift e símbolos pelo navegador | todos os caracteres chegam |

## Limites

- Uma tela por máquina; o tamanho é fixo na criação.
- Sem áudio.
- O cursor é desenhado pelo guest, dentro do quadro: sem plano de cursor separado no navegador.
- Testado só no macOS. No Linux o Venus atende o 2D pelo virgl, mas não foi medido.
