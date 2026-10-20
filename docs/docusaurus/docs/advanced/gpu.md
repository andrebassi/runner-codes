---
title: 'GPU (Vulkan)'
description: 'Como a microVM usa a GPU do host por virtio-gpu/Venus: arquitetura, build pelo Nix, a stack llama, medições e diagnóstico'
---

Esta página explica, de ponta a ponta, como um programa dentro da microVM usa a GPU do Mac. Ela serve para quem só
quer usar (seções 1 e 2), para quem precisa entender (3 a 6) e para quem vai depurar (7 e 8).

## 1. Em uma frase

Com `--gpu`, a VM ganha uma placa de vídeo virtual (**virtio-gpu**) que fala **Vulkan**. Cada chamada Vulkan do guest
atravessa a fronteira da VM e é executada pela GPU real do host. No macOS isso passa pelo **Metal**.

## 2. Uso

```bash
# uma vez: a imagem com o driver Vulkan do guest, o llama.cpp e o modelo (~6 min, 1,4 GiB)
./bin/rc image build --stack llama

# a VM enxerga a GPU?
./bin/rc run --stack llama --gpu --code 'vulkaninfo --summary | grep deviceName'
# deviceName = Virtio-GPU Venus (Apple M1 Pro)

# um modelo de linguagem rodando na GPU
./bin/rc run --stack llama --gpu --timeout 120s --code \
  'llama-cli -m /opt/models/qwen2.5-0.5b-q4_k_m.gguf -ngl 99 -dev Vulkan0 -no-cnv -n 64 -p "O que é uma microVM?"'

# shell com GPU, para explorar
./bin/rc shell --stack llama --gpu
```

Pela API, com o campo `gpu` no corpo de `/api/v1/execute` ou `/api/v1/execute/stream`:

```bash
curl -N -X POST http://127.0.0.1:8080/api/v1/execute/stream -H 'content-type: application/json' -d '{
  "language": "llama", "gpu": true, "timeout": 120,
  "code": "llama-bench -m /opt/models/qwen2.5-0.5b-q4_k_m.gguf -p 128 -n 32 -ngl 99 -dev Vulkan0"
}'
```

| Pedido | Resultado |
|--------|-----------|
| `--gpu` numa stack com `gpu: true` no catálogo | VM com virtio-gpu; `/dev/dri/card0` e `/dev/dri/renderD128` no guest |
| `--gpu` numa stack sem `gpu` | recusado antes de subir VM: `E007: stack <nome> has no GPU support` (HTTP 400) |
| sem `--gpu` | VM igual à de sempre; nem `/dev/dri` existe |

## 3. O caminho de uma chamada Vulkan

```
 guest (Linux 6.12, aarch64)                    │ host (macOS, Apple Silicon)
                                                │
 programa (llama.cpp, vulkaninfo)               │
   └─ libvulkan (loader)                        │
       └─ libvulkan_virtio (Mesa "Venus")       │
           │  serializa os comandos Vulkan      │
           ▼                                    │
   /dev/dri/renderD128  (driver virtio-gpu) ────┼──▶ libkrun: dispositivo virtio-gpu (processo `rc vmm`)
                                                │      └─ virglrenderer (Venus): desserializa e chama Vulkan
                                                │           └─ MoltenVK: traduz Vulkan para Metal
                                                │                └─ Metal ─▶ GPU do Apple Silicon
```

Dois canais atravessam a fronteira:

- **comandos**: uma fila (*ring*) em memória compartilhada, onde o Mesa escreve os comandos Vulkan e o virglrenderer
  os lê;
- **memória**: buffers e texturas que a GPU usa são *blobs*. O host os aloca, e a libkrun os mapeia dentro da janela
  de VRAM do guest com `hv_vm_map`, do Hypervisor.framework. O guest lê e escreve neles sem cópia.

**Por que só Vulkan:** o MoltenVK traduz Vulkan, não OpenGL. Por isso a VM liga o Venus e desliga o *virgl* (o
caminho de OpenGL), com os flags `VIRGLRENDERER_VENUS | VIRGLRENDERER_NO_VIRGL`. O krunkit usa a mesma
combinação.

## 4. As peças do host (Nix)

Tudo vem do `flake.nix` do repositório, pelo `task krun:install` (`nix profile add .#krun`). Nada sai do Homebrew.

| Peça | De onde vem | Por quê |
|------|-------------|---------|
| **virglrenderer** `0.10.4e-krunkit` | `nix/virglrenderer-darwin.nix`, a partir do fork do autor da libkrun (`gitlab.freedesktop.org/slp/virglrenderer`), com sha256 conferido | o do nixpkgs sai com `-Dvenus=false` no macOS, e é justamente o Venus que dá Vulkan ao guest. O fork traz os ajustes para o Venus rodar sobre o MoltenVK |
| **MoltenVK** 1.4.2 | nixpkgs | Vulkan → Metal. O virglrenderer liga direto na `libMoltenVK.dylib`, sem loader nem ICD no host |
| **libepoxy** | nixpkgs | dependência do virglrenderer |
| **libkrun** 1.19.6 com `GPU=1` | `nix/libkrun-darwin.nix` | sem `GPU=1` não existe o dispositivo virtio-gpu nem `krun_set_gpu_options2` |

Duas armadilhas resolvidas no build, para quem for mexer:

- o hook de meson do Nix liga toda *feature* `auto` (`-Dauto_features=enabled`). O `drm` do virglrenderer é `auto` e
  exigiria a libdrm, que só existe no Linux, então a derivação passa `-Ddrm=disabled`;
- o virglrenderer roda **dentro do processo `rc vmm`** (`-Drender-server=false`). O *render server* separado é coisa
  do Linux.

Conferir o que está instalado:

```bash
./bin/rc doctor
# ok    gpu: libkrun built with virtio-gpu (Venus); use --gpu on a stack with gpu
```

## 5. O que o `rc` faz quando você pede `--gpu`

1. **Validação** (`internal/domain`): `Job.CheckGPU` confere se a stack tem `gpu: true` no catálogo. Se não tiver,
   devolve `E007`, que vira HTTP 400. A recusa é de propósito: quem pede GPU está medindo tempo, e rodar na CPU em
   silêncio daria um número errado.
2. **VM a frio, fora do pool** (`internal/app/pool.go`, `TakeGPU`): VM com GPU não fica parada no pool, porque segura
   uma janela de VRAM e uma vaga da GPU do host. No máximo **2** VMs com GPU vivas ao mesmo tempo
   (`DefaultMaxGPUVMs`); um terceiro pedido espera vaga até o prazo do job. A vaga volta no `Destroy`.
3. **Spec da VM** (`ports.VMSpec`): `GPU: true` e `GPUVRAMMiB` vindo do catálogo (`gpu_vram_mib`, 4096 na stack
   llama).
4. **libkrun** (`internal/adapters/krun/config.go`): antes do `krun_set_exec`, o `Configure` chama
   `krun_set_gpu_options2(ctx, VENUS|NO_VIRGL, vram_em_bytes)`.
5. **libkrun sem GPU** (a do nixpkgs no Linux): o símbolo `krun_set_gpu_options2` nem existe. O binding confere com
   `Dlsym` antes de registrar, então a biblioteca carrega normalmente. Só a VM que pedir GPU falha, com `ENOTSUP`.

A **VRAM** é uma janela de endereço do host, não RAM reservada: o host só aloca o que o guest de fato usa. Cada VM
recebe a sua janela.

## 6. O que o guest precisa: a stack `llama`

A imagem é **Fedora 43**, não Alpine. A seção 7 conta o porquê. O warmup do catálogo
(`internal/adapters/catalog/languages.yaml`) roda uma vez, na VM de build, com rede, e faz:

1. **Mesa com o patch de 16 KiB**: `dnf copr enable slp/mesa-libkrun-vulkan` e `dnf install mesa-vulkan-drivers`.
   O COPR é do autor da libkrun.
2. **llama.cpp com backend Vulkan**, compilado ali mesmo: tag `v0.7.0`, tarball conferido por sha256, com
   `-DGGML_VULKAN=ON -DBUILD_SHARED_LIBS=OFF`. O pacote `llama-cpp` do Fedora não tem backend Vulkan (só CPU e HIP).
   Só `llama-cli` e `llama-bench` vão para `/usr/local/bin`.
3. **Modelo** Qwen2.5 0.5B Instruct Q4_K_M (491 MB) em `/opt/models`, com o sha256 conferido. Ele entra na imagem
   porque a VM do job não tem rede.

O job é um script `sh`. As flags que importam no llama.cpp:

| Flag | Efeito |
|------|--------|
| `-ngl 99` | todas as camadas do modelo na GPU (`-ngl 0` = CPU) |
| `-dev Vulkan0` | usa o Venus. O Mesa também expõe o `llvmpipe` (Vulkan em software, na CPU), e sem esta flag o llama.cpp pode dividir o trabalho entre os dois |

## 7. Por que Fedora: o defeito dos 16 KiB

Esta é a parte que mais custou e que mais morde quem tenta reproduzir.

**Sintoma**, com o Mesa padrão (medido no Alpine edge, `mesa-vulkan-virtio`):

```
/dev/dri/renderD128 existe          ← o dispositivo virtual subiu
krun_vmm::macos::vstate] Error removing memory map: MemoryUnmap
krun_vmm::macos::vstate] Error adding memory map: MemoryMap
vkCreateInstance failed with ERROR_OUT_OF_HOST_MEMORY
```

**Causa**: a página de memória do Apple Silicon tem **16 KiB**, e o `hv_vm_map` só mapeia regiões alinhadas a 16 KiB.
O kernel do guest usa página de 4 KiB (`getconf PAGESIZE` = 4096). O Mesa padrão pede blobs, inclusive o *ring* de
comandos criado no `vkCreateInstance`, alinhados a 4 KiB. O mapeamento falha, e o Vulkan não chega a abrir.

**O que não resolve**: aumentar a VRAM. Medido com 2, 8 e 16 GiB, e o erro é idêntico.

**Correção**: o patch `0001-virtio-vulkan-force-16k-alignment-for-allocations` do COPR arredonda para 16 KiB:

- o tamanho de toda alocação de memória (`vn_device_memory.c`);
- os blobs (`vn_renderer_virtgpu.c`);
- o *ring* (`vn_ring.c`).

Ele só existe pronto em pacote Fedora, e o binário é glibc, então não serve numa imagem Alpine (musl).

## 8. Diagnóstico

| Sintoma | Causa provável | O que fazer |
|---------|----------------|-------------|
| `E007: stack X has no GPU support` | a stack não tem `gpu: true` | usar a stack `llama`, ou declarar `gpu` numa stack cuja imagem tenha o Mesa com o patch |
| `krun_set_gpu_options2 failed: errno 45` (ENOTSUP) | libkrun compilada sem `GPU=1` | `task krun:install` (macOS); no Linux a do nixpkgs vem sem GPU |
| `/dev/dri` não existe com `--gpu` | `Configure` não ligou o dispositivo | `rc doctor` deve dizer `ok gpu`; o e2e `TestGPUVulkan` pega exatamente isso |
| `vkCreateInstance ... ERROR_OUT_OF_HOST_MEMORY` + `Error adding memory map` | Mesa sem o patch de 16 KiB | imagem com o Mesa do COPR `slp/mesa-libkrun-vulkan` (seção 7) |
| `vulkaninfo` lista só `llvmpipe` | o Venus não carregou e só sobrou o Vulkan em software | conferir a mensagem acima; `VK_LOADER_DEBUG=all vulkaninfo` mostra qual ICD falhou |
| GPU lenta ou travando | outra VM com GPU ou outro app disputando a GPU | no máximo 2 VMs com GPU; ver Monitor de Atividade › GPU |

Mensagem inofensiva: `Error opening virtio-gpu device for Asahi native context`. O Mesa tenta primeiro o driver nativo
do Asahi, não acha e cai no Venus.

## 9. Números medidos

macOS 27, Apple M1 Pro (16 GB), 19/10/2026. `llama-bench` dentro da VM, Qwen2.5 0.5B Q4_K_M, 4 vCPU, 4 rodadas:

| Teste | GPU (Venus, `-ngl 99 -dev Vulkan0`) | CPU (`-ngl 0`) |
|-------|-------------------------------------|----------------|
| processar o prompt (pp128) | **893–1.365 t/s** | 193–264 t/s |
| gerar texto (tg32/tg64) | 47–58 t/s | 46–123 t/s |

Como ler:

- **prompt**: a GPU é 3,5 a 7 vezes mais rápida, porque é trabalho em lote (128 tokens de uma vez), exatamente o que
  a GPU paraleliza;
- **geração**: um token por vez, e cada token paga a ida e volta guest → virtio → virglrenderer → MoltenVK → Metal.
  Num modelo de 0,5B essa latência pesa mais que a conta, e a CPU empata ou ganha. Modelo maior tende a inverter isso,
  mas não foi medido;
- a CPU varia muito (46 a 123 t/s) com a carga do host. A GPU é mais estável.

O e2e `TestGPULlama` cobra que o device usado é o Venus e que o prompt na GPU sai pelo menos 2 vezes mais rápido que
na CPU. Ele não cobra a geração, pelo motivo acima.

## 10. Limites e o que não foi testado

| Ponto | Situação |
|-------|----------|
| macOS Apple Silicon | **testado** de ponta a ponta (`TestGPUVulkan`, `TestGPULlama`, duas rodadas seguidas) |
| Linux (KVM) | **não testado**: a libkrun do nixpkgs sai sem `GPU=1`, e não há host Linux com KVM e GPU disponível. No Linux o virglrenderer usa o Mesa do host e o *render server* |
| Linux: confinamento | VM com GPU roda **sem Landlock/seccomp**: o virglrenderer abre `/dev/dri` e sobe o *render server* com `execve`, que a política nega. Liberar isso exige medir num host Linux com GPU |
| CUDA | **não suportado**. CUDA na VM exigiria *remoting* da API CUDA para o driver NVIDIA do host, com host Linux e placa NVIDIA |
| Windows | fora (a libkrun upstream não tem WHP) |
| Isolamento | CPU, memória e disco continuam isolados pela VM, mas a **GPU é compartilhada** por processo do host, não por fronteira de VM. Não use GPU para código de terceiros mutuamente hostis |

## 11. Onde está cada coisa no código

| Arquivo | O que tem |
|---------|-----------|
| `nix/virglrenderer-darwin.nix`, `nix/libkrun-darwin.nix`, `flake.nix` | o build do host |
| `internal/adapters/krun/config.go` | `gpuVenusFlags`, `DefaultGPUVRAMMiB`, a chamada no `Configure` |
| `internal/adapters/krun/binding.go` | símbolo opcional, `HasGPU()` |
| `internal/adapters/krun/sandbox.go` | VM com GPU fora do Landlock/seccomp |
| `internal/domain/domain.go` | `Language.GPU`, `Job.GPU`, `CheckGPU`, `ErrNoGPU` (E007) |
| `internal/app/pool.go` | `TakeGPU`, `DefaultMaxGPUVMs` |
| `internal/adapters/catalog/languages.yaml` | a stack `llama` |
| `e2e/gpu_test.go` | `TestGPUVulkan`, `TestGPULlama` |
| `docs/stories/6.1.gpu-venus.md` | a story, com o log de cada defeito encontrado |
