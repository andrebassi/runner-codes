---
title: 'GPU (Vulkan)'
description: 'Cómo usa la microVM la GPU del host mediante virtio-gpu/Venus: arquitectura, build con Nix, el stack llama, mediciones y diagnóstico'
---

Esta página explica, de principio a fin, cómo un programa dentro de la microVM usa la GPU del Mac. Sirve para quien solo
quiere usarla (secciones 1 y 2), para quien necesita entenderla (3 a 6) y para quien va a depurarla (7 y 8).

## 1. En una frase

Con `--gpu`, la VM obtiene una tarjeta de video virtual (**virtio-gpu**) que habla **Vulkan**. Cada llamada Vulkan del guest
cruza la frontera de la VM y la ejecuta la GPU real del host. En macOS esto pasa por **Metal**.

## 2. Uso

```bash
# una vez: la imagen con el driver Vulkan del guest, llama.cpp y el modelo (~6 min, 1,4 GiB)
./bin/rc image build --stack llama

# ¿la VM ve la GPU?
./bin/rc run --stack llama --gpu --code 'vulkaninfo --summary | grep deviceName'
# deviceName = Virtio-GPU Venus (Apple M1 Pro)

# un modelo de lenguaje ejecutándose en la GPU
./bin/rc run --stack llama --gpu --timeout 120s --code \
  'llama-cli -m /opt/models/qwen2.5-0.5b-q4_k_m.gguf -ngl 99 -dev Vulkan0 -no-cnv -n 64 -p "O que é uma microVM?"'

# shell con GPU, para explorar
./bin/rc shell --stack llama --gpu
```

Por la API, con el campo `gpu` en el cuerpo de `/api/v1/execute` o `/api/v1/execute/stream`:

```bash
curl -N -X POST http://127.0.0.1:8080/api/v1/execute/stream -H 'content-type: application/json' -d '{
  "language": "llama", "gpu": true, "timeout": 120,
  "code": "llama-bench -m /opt/models/qwen2.5-0.5b-q4_k_m.gguf -p 128 -n 32 -ngl 99 -dev Vulkan0"
}'
```

| Solicitud | Resultado |
|--------|-----------|
| `--gpu` en un stack con `gpu: true` en el catálogo | VM con virtio-gpu; `/dev/dri/card0` y `/dev/dri/renderD128` en el guest |
| `--gpu` en un stack sin `gpu` | rechazado antes de levantar la VM: `E007: stack <nome> has no GPU support` (HTTP 400) |
| sin `--gpu` | VM igual que siempre; ni siquiera existe `/dev/dri` |

## 3. El camino de una llamada Vulkan

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

Dos canales cruzan la frontera:

- **comandos**: una cola (*ring*) en memoria compartida, donde Mesa escribe los comandos Vulkan y virglrenderer
  los lee;
- **memoria**: los buffers y texturas que usa la GPU son *blobs*. El host los asigna, y la libkrun los mapea dentro de la ventana
  de VRAM del guest con `hv_vm_map`, de Hypervisor.framework. El guest lee y escribe en ellos sin copia.

**Por qué solo Vulkan:** MoltenVK traduce Vulkan, no OpenGL. Por eso la VM activa Venus y desactiva *virgl* (el
camino de OpenGL), con las flags `VIRGLRENDERER_VENUS | VIRGLRENDERER_NO_VIRGL`.

## 4. Las piezas del host (Nix)

Todo viene del `flake.nix` del repositorio, mediante `task krun:install` (`nix profile add .#krun`). Nada sale de Homebrew.

| Pieza | De dónde viene | Por qué |
|------|-------------|---------|
| **virglrenderer** `0.10.4e-krunkit` | `nix/virglrenderer-darwin.nix`, a partir del fork del autor de la libkrun (`gitlab.freedesktop.org/slp/virglrenderer`), con sha256 verificado | el de nixpkgs sale con `-Dvenus=false` en macOS, y es justamente Venus lo que da Vulkan al guest. El fork trae los ajustes para que Venus funcione sobre MoltenVK |
| **MoltenVK** 1.4.2 | nixpkgs | Vulkan → Metal. virglrenderer enlaza directamente con `libMoltenVK.dylib`, sin loader ni ICD en el host |
| **libepoxy** | nixpkgs | dependencia de virglrenderer |
| **libkrun** 1.19.6 con `GPU=1` | `nix/libkrun-darwin.nix` | sin `GPU=1` no existe el dispositivo virtio-gpu ni `krun_set_gpu_options2` |

Dos trampas resueltas en el build, para quien vaya a modificarlo:

- el hook de meson de Nix activa toda *feature* `auto` (`-Dauto_features=enabled`). El `drm` de virglrenderer es `auto` y
  exigiría la libdrm, que solo existe en Linux, así que la derivación pasa `-Ddrm=disabled`;
- virglrenderer se ejecuta **dentro del proceso `rc vmm`** (`-Drender-server=false`). El *render server* separado es cosa
  de Linux.

Verificar lo que está instalado:

```bash
./bin/rc doctor
# ok    gpu: libkrun built with virtio-gpu (Venus); use --gpu on a stack with gpu
```

## 5. Qué hace `rc` cuando usted pide `--gpu`

1. **Validación** (`internal/domain`): `Job.CheckGPU` verifica si el stack tiene `gpu: true` en el catálogo. Si no lo tiene,
   devuelve `E007`, que se convierte en HTTP 400. El rechazo es intencional: quien pide GPU está midiendo tiempo, y ejecutar en la CPU en
   silencio daría un número equivocado.
2. **VM en frío, fuera del pool** (`internal/app/pool.go`, `TakeGPU`): una VM con GPU no queda en espera en el pool, porque retiene
   una ventana de VRAM y un cupo de la GPU del host. Como máximo **2** VMs con GPU vivas al mismo tiempo
   (`DefaultMaxGPUVMs`); una tercera solicitud espera cupo hasta el plazo del job. El cupo se libera en `Destroy`.
3. **Spec de la VM** (`ports.VMSpec`): `GPU: true` y `GPUVRAMMiB` proveniente del catálogo (`gpu_vram_mib`, 4096 en el stack
   llama).
4. **libkrun** (`internal/adapters/krun/config.go`): antes de `krun_set_exec`, `Configure` llama a
   `krun_set_gpu_options2(ctx, VENUS|NO_VIRGL, vram_em_bytes)`.
5. **libkrun sin GPU** (la de nixpkgs en Linux): el símbolo `krun_set_gpu_options2` ni siquiera existe. El binding lo verifica con
   `Dlsym` antes de registrarlo, así que la biblioteca carga con normalidad. Solo falla la VM que pida GPU, con `ENOTSUP`.

La **VRAM** es una ventana de direcciones del host, no RAM reservada: el host solo asigna lo que el guest realmente usa. Cada VM
recibe su propia ventana.

## 6. Lo que necesita el guest: el stack `llama`

La imagen es **Fedora 43**, no Alpine. La sección 7 explica por qué. El warmup del catálogo
(`internal/adapters/catalog/languages.yaml`) se ejecuta una vez, en la VM de build, con red, y hace lo siguiente:

1. **Mesa con el parche de 16 KiB**: `dnf copr enable slp/mesa-libkrun-vulkan` y `dnf install mesa-vulkan-drivers`.
   El COPR es del autor de la libkrun.
2. **llama.cpp con backend Vulkan**, compilado allí mismo: tag `v0.7.0`, tarball verificado por sha256, con
   `-DGGML_VULKAN=ON -DBUILD_SHARED_LIBS=OFF`. El paquete `llama-cpp` de Fedora no tiene backend Vulkan (solo CPU e HIP).
   Solo `llama-cli` y `llama-bench` van a `/usr/local/bin`.
3. **Modelo** Qwen2.5 0.5B Instruct Q4_K_M (491 MB) en `/opt/models`, con el sha256 verificado. Entra en la imagen
   porque la VM del job no tiene red.

El job es un script `sh`. Las flags que importan en llama.cpp:

| Flag | Efecto |
|------|--------|
| `-ngl 99` | todas las capas del modelo en la GPU (`-ngl 0` = CPU) |
| `-dev Vulkan0` | usa Venus. Mesa también expone `llvmpipe` (Vulkan por software, en la CPU), y sin esta flag llama.cpp puede repartir el trabajo entre los dos |

## 7. Por qué Fedora: el defecto de los 16 KiB

Esta es la parte que más costó y la que más afecta a quien intenta reproducirlo.

**Síntoma**, con el Mesa estándar (medido en Alpine edge, `mesa-vulkan-virtio`):

```
/dev/dri/renderD128 existe          ← o dispositivo virtual subiu
krun_vmm::macos::vstate] Error removing memory map: MemoryUnmap
krun_vmm::macos::vstate] Error adding memory map: MemoryMap
vkCreateInstance failed with ERROR_OUT_OF_HOST_MEMORY
```

**Causa**: la página de memoria de Apple Silicon tiene **16 KiB**, y `hv_vm_map` solo mapea regiones alineadas a 16 KiB.
El kernel del guest usa páginas de 4 KiB (`getconf PAGESIZE` = 4096). El Mesa estándar pide blobs, incluido el *ring* de
comandos creado en `vkCreateInstance`, alineados a 4 KiB. El mapeo falla y Vulkan no llega a abrirse.

**Lo que no lo resuelve**: aumentar la VRAM. Medido con 2, 8 y 16 GiB, y el error es idéntico.

**Corrección**: el parche `0001-virtio-vulkan-force-16k-alignment-for-allocations` del COPR redondea a 16 KiB:

- el tamaño de toda asignación de memoria (`vn_device_memory.c`);
- los blobs (`vn_renderer_virtgpu.c`);
- el *ring* (`vn_ring.c`).

Solo existe listo en un paquete de Fedora, y el binario es glibc, así que no sirve en una imagen Alpine (musl).

## 8. Diagnóstico

| Síntoma | Causa probable | Qué hacer |
|---------|----------------|-------------|
| `E007: stack X has no GPU support` | el stack no tiene `gpu: true` | usar el stack `llama`, o declarar `gpu` en un stack cuya imagen tenga el Mesa con el parche |
| `krun_set_gpu_options2 failed: errno 45` (ENOTSUP) | libkrun compilada sin `GPU=1` | `task krun:install` (macOS); en Linux la de nixpkgs viene sin GPU |
| `/dev/dri` no existe con `--gpu` | `Configure` no activó el dispositivo | `rc doctor` debe decir `ok gpu`; el e2e `TestGPUVulkan` detecta exactamente esto |
| `vkCreateInstance ... ERROR_OUT_OF_HOST_MEMORY` + `Error adding memory map` | Mesa sin el parche de 16 KiB | imagen con el Mesa del COPR `slp/mesa-libkrun-vulkan` (sección 7) |
| `vulkaninfo` lista solo `llvmpipe` | Venus no cargó y solo quedó el Vulkan por software | revisar el mensaje anterior; `VK_LOADER_DEBUG=all vulkaninfo` muestra qué ICD falló |
| GPU lenta o que se bloquea | otra VM con GPU u otra app compitiendo por la GPU | como máximo 2 VMs con GPU; ver Monitor de Actividad › GPU |

Mensaje inofensivo: `Error opening virtio-gpu device for Asahi native context`. Mesa intenta primero el driver nativo
de Asahi, no lo encuentra y recurre a Venus.

## 9. Números medidos

macOS 27, Apple M1 Pro (16 GB), 19/10/2026. `llama-bench` dentro de la VM, Qwen2.5 0.5B Q4_K_M, 4 vCPU, 4 ejecuciones:

| Prueba | GPU (Venus, `-ngl 99 -dev Vulkan0`) | CPU (`-ngl 0`) |
|-------|-------------------------------------|----------------|
| procesar el prompt (pp128) | **893–1.365 t/s** | 193–264 t/s |
| generar texto (tg32/tg64) | 47–58 t/s | 46–123 t/s |

Cómo leerlo:

- **prompt**: la GPU es de 3,5 a 7 veces más rápida, porque es trabajo por lotes (128 tokens a la vez), exactamente lo que
  la GPU paraleliza;
- **generación**: un token a la vez, y cada token paga el viaje de ida y vuelta guest → virtio → virglrenderer → MoltenVK → Metal.
  En un modelo de 0,5B esa latencia pesa más que el cálculo, y la CPU empata o gana. Un modelo mayor tiende a invertirlo,
  pero no se midió;
- la CPU varía mucho (46 a 123 t/s) con la carga del host. La GPU es más estable.

El e2e `TestGPULlama` exige que el dispositivo usado sea Venus y que el prompt en la GPU sea al menos 2 veces más rápido que
en la CPU. No exige nada sobre la generación, por el motivo anterior.

## 10. Límites y lo que no se probó

| Punto | Situación |
|-------|----------|
| macOS Apple Silicon | **probado** de extremo a extremo (`TestGPUVulkan`, `TestGPULlama`, dos ejecuciones seguidas) |
| Linux (KVM) | **no probado**: la libkrun de nixpkgs sale sin `GPU=1`, y no hay un host Linux con KVM y GPU disponible. En Linux virglrenderer usa el Mesa del host y el *render server* |
| Linux: confinamiento | una VM con GPU se ejecuta **sin Landlock/seccomp**: virglrenderer abre `/dev/dri` y levanta el *render server* con `execve`, que la política deniega. Permitirlo exige medir en un host Linux con GPU |
| CUDA | **no soportado**. CUDA en la VM exigiría *remoting* de la API de CUDA hacia el driver NVIDIA del host, con host Linux y tarjeta NVIDIA |
| Windows | fuera (la libkrun upstream no tiene WHP) |
| Aislamiento | CPU, memoria y disco siguen aislados por la VM, pero la **GPU es compartida** a nivel de proceso del host, no por frontera de VM. No use la GPU para código de terceros mutuamente hostiles |

## 11. Dónde está cada cosa en el código

| Archivo | Qué contiene |
|---------|-----------|
| `nix/virglrenderer-darwin.nix`, `nix/libkrun-darwin.nix`, `flake.nix` | el build del host |
| `internal/adapters/krun/config.go` | `gpuVenusFlags`, `DefaultGPUVRAMMiB`, la llamada en `Configure` |
| `internal/adapters/krun/binding.go` | símbolo opcional, `HasGPU()` |
| `internal/adapters/krun/sandbox.go` | VM con GPU fuera de Landlock/seccomp |
| `internal/domain/domain.go` | `Language.GPU`, `Job.GPU`, `CheckGPU`, `ErrNoGPU` (E007) |
| `internal/app/pool.go` | `TakeGPU`, `DefaultMaxGPUVMs` |
| `internal/adapters/catalog/languages.yaml` | el stack `llama` |
| `e2e/gpu_test.go` | `TestGPUVulkan`, `TestGPULlama` |
| `docs/stories/6.1.gpu-venus.md` | la story, con el registro de cada defecto encontrado |
