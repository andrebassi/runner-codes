# Exemplos de `runner.code`

Um exemplo por pasta, cada um testado numa VM real no macOS 27 (Apple M1 Pro). O `rc` aceita `-s` como sinônimo de
`-f` para apontar o arquivo da máquina.

| Exemplo | O que mostra | Estado |
|---------|--------------|--------|
| [`python-app`](python-app/runner.code) | imagem pública, `init` com `pip` pela rede | funciona |
| [`node-app`](node-app/runner.code) | `init` com `npm -g`, app numa porta publicada | funciona |
| [`openclaw-app`](openclaw-app/runner.code) | gateway de LLM como workload, `/health` pela porta | funciona |
| [`doom-web`](doom-web/runner.code) | servidor estático de um volume, jogo no navegador | funciona |
| [`docker-in-vm`](docker-in-vm/runner.code) | Docker completo dentro da VM | funciona |
| [`headless-browser`](headless-browser/runner.code) | Chromium headless e screenshot | funciona, **sem GPU** (veja abaixo) |
| [`gpu-chrome`](gpu-chrome/runner.code) | Fedora com Vulkan/Venus e Chromium | Vulkan funciona; Chromium sem GPU |
| [`local-llm`](local-llm/runner.code) | llama.cpp com Vulkan na GPU do host | funciona: 56,7 tokens/s na GPU contra 10,3 na CPU (Qwen2.5 0.5B Q4_K_M, 5,5×) |
| [`desktop`](desktop/runner.code) | desktop Wayland (Weston) na tela do navegador, com teclado e mouse | funciona (Weston, veja abaixo) |

```bash
rc machine run -s examples/python-app/runner.code -- python3 --version
rc machine run -d --name doom -s examples/doom-web/runner.code && open http://localhost:8080
rc machine run -d --name openclaw -s examples/openclaw-app/runner.code && curl localhost:18789/health
```

## Notas de cada exemplo

- **openclaw-app**: a versão atual do openclaw escuta só em `127.0.0.1`; a porta publicada do `rc`
  entra pelo guest e disca o `127.0.0.1` de lá, então funciona sem `--bind lan` (que exigiria token).
- **doom-web**:
  - sem `net`: a VM só serve o arquivo; quem baixa o jogo é o navegador.
  - volume `.:/var/www:ro`, relativo à pasta do arquivo.
  - três armadilhas do js-dos, todas fora da VM:
    - o script `js-dos.com/v8/latest` responde 404;
    - a API `.run()` não existe na v8;
    - o pacote do jogo no `cdn.dos.zone` responde 403.
    Agora usa js-dos 8.3.20 pelo jsDelivr e o pacote oficial `v8.js-dos.com/bundles/doom.jsdos`.
- **docker-in-vm**:
  - `image = "alpine:latest"`: toda máquina tem imagem base.
  - `/var/lib/docker` no `/workspace` (ext4 da máquina): o overlay2 não aninha sobre o overlay da raiz.
  - backend nftables: o kernel não tem o iptables legado.
  - `DOCKER_RAMDISK=true`: o runc não faz `pivot_root` a partir do chroot da máquina.
  - API na porta 2375, publicada só no `127.0.0.1` do host (sem TLS).
  - Conferência completa em [`docker-in-vm/check.sh`](docker-in-vm/check.sh).
- **headless-browser / gpu-chrome**:
  - O Mesa do Alpine (latest e edge) falha no `vkCreateInstance` no Apple Silicon, por falta do patch de página de
    16 KiB.
  - Com o Mesa do COPR `slp/mesa-libkrun-vulkan` (Fedora) o Venus aparece, mas o ANGLE do Chromium exige
    `VK_KHR_external_memory_fd` e `VK_KHR_external_semaphore_fd`, que o Venus do host (virglrenderer + MoltenVK
    upstream) não expõe.
  - Resultado: o Chromium renderiza em software. O screenshot sai, sem aceleração.
- **desktop**:
  - Usa Weston (pixman, CPU) porque o Hyprland exige GLES, e sem GPU no modo de tela cairia no llvmpipe.
  - O caminho de tela e entrada foi medido no navegador: lançador, terminal, texto com Shift e símbolos.
    `echo "Desktop OK: $(cat /etc/alpine-release)" | tr a-z A-Z` devolveu `DESKTOP OK: 3.24.2`.
  - A tela é um virtio-gpu **2D** (a libkrun do rc tem um patch, `KRUN_GPU_BACKEND=2d`): no macOS a GPU Venus não
    atende recurso 2D. Por isso a VM com tela não tem Vulkan.
- **local-llm**: `~` no volume é expandido pelo `rc`. O 1º start levou 488 s (COPR Mesa + build
  do llama.cpp); o `llama-completion --list-devices` mostra `Vulkan0: Virtio-GPU Venus (Apple M1 Pro)`.

## Tempo do primeiro start (medido)

| Exemplo | 1º start (init) | Seguintes |
|---------|-----------------|-----------|
| python-app | 6 s | < 1 s |
| node-app | 2 s | < 1 s |
| openclaw-app | ~73 s (inclui converter a imagem) | < 1 s |
| docker-in-vm | 4 s | 4 s (o dockerd sobe a cada start) |
| headless-browser | 15 s | < 1 s |
| gpu-chrome | 581 s (metadados e pacotes do Fedora) | < 1 s |
| local-llm | 488 s (Fedora + build do llama.cpp) | < 1 s |
| desktop | 12 s | 5 s (udev, seatd e Weston sobem a cada start) |
