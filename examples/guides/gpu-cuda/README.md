# gpu-cuda

**O rc não tem CUDA.** CUDA numa VM exige remoting da API de CUDA para um daemon no host, e isso só tem sentido com
GPU NVIDIA no host. O rc não implementa isso: a GPU do rc é o `--gpu`, que entrega Vulkan (virtio-gpu Venus) e foi
testado só no macOS.

Este guia é a resposta **não**, dada alto. A armadilha central de CUDA em VM é a sonda passar num host sem GPU
NVIDIA, contra um dispositivo de CPU emulado, e o resultado ser lido como um sim. No rc não existe esse caminho
meio-ligado:

| Tema | No rc |
|---|---|
| preflight | `rc doctor` imprime `info  cuda: not supported (no CUDA remoting; --gpu is Vulkan)`, sem ligar VM |
| sonda de CUDA | não se aplica: não há o que sondar |
| limpeza | nada a limpar: as recusas não criam máquina (o `run.sh` confere) |
| armadilhas | o "exit 0 sem GPU" não existe aqui: o pedido é recusado antes |
| Windows | fora do escopo: o rc não roda no Windows |

As três formas de pedir CUDA são recusadas com o motivo:

| Via | Resposta |
|---|---|
| `rc machine create --cuda` | `--cuda is not supported: rc has no CUDA remoting (...); --gpu gives the guest Vulkan, not CUDA` (a flag existe só para isso, fora do `--help`) |
| `cuda = true` no `runner.code` | `key "cuda" is not supported yet: CUDA (needs a Linux host with an NVIDIA GPU)` |
| `"cuda": true` na API | 400, `unknown field "cuda"` |

```bash
bash examples/guides/gpu-cuda/run.sh   # 13 conferências, nenhuma VM
```
