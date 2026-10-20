---
sidebar_position: 8
title: Pack (artefacto portátil)
---

# Pack: una máquina en un archivo

`rc pack create` convierte una imagen, o una máquina con lo que se instaló en ella, en un artefacto que se copia a
otro host y se ejecuta sin pull, sin build y sin descargas.

```bash
rc pack create --stack python --output ./py312         # stub ./py312 + payload ./py312.rcmachine
./py312 run -- python3 --version

rc pack create --from-vm dev --output ./dev --single-file   # un solo ejecutable
./dev run -- python3 -c 'import meu_pacote'
```

| Salida | Qué es |
|-------|---------|
| `py312` | el stub: este `rc`, firmado con el entitlement de hypervisor en macOS |
| `py312.rcmachine` | el payload: la imagen ext4 (y, si viene de una máquina, su disco) y el manifiesto |
| `--single-file` | el payload anexado al stub, con un trailer de 16 bytes al final |

Los dos archivos viajan juntos. El stub solo muestra el uso y termina.

## Orígenes

| Flag | Qué va en el payload |
|------|----------------------|
| `--stack python` | la imagen de la stack (construida antes, si aún no existe) |
| `--image alpine:3.20` | la imagen OCI construida |
| `--from-vm NOME` | la imagen de la máquina **y su disco**: lo que se instaló y grabó viaja con ella. La máquina debe estar detenida (un disco en uso no es consistente) |

Las flags de máquina (`--cpus`, `--mem`, `-e`, `--workdir`, `--net`, `--allow-host`, `-p`, `--` workload...) entran
en el manifiesto y se convierten en los valores por defecto del artefacto. No viajan:
- el volumen de directorio del host (la ruta es de otra máquina), que se rechaza con el motivo; el volumen `s3://` sí
  viaja;
- las credenciales, porque la CA y los placeholders son de la máquina de origen;
- la red con nombre.

## Subcomandos del artefacto

```bash
./dev run [-- COMMAND...]    # máquina efímera, borrada al final (se devuelve el código de salida)
./dev start                  # máquina persistente con el nombre del archivo (creada la 1.ª vez)
./dev exec -- pip install x  # en ella
./dev shell | logs | status | stop
./dev inspect                # el manifiesto, sin extraer
```

También: `rc machine create --name x --from dev.rcmachine` (o `--from ./dev`), `rc machine run --from ...` y
`rc pack inspect ARQUIVO`.

## Cómo es por dentro

- **Formato propio, disperso.** Un disco de máquina tiene 10 GiB lógicos y casi todo es hueco, y el `archive/tar` de
  Go no graba de forma dispersa. El payload guarda solo los tramos con datos (encontrados con `SEEK_DATA`/`SEEK_HOLE`)
  en un flujo gzip; la extracción recrea los huecos. Medido: la imagen python (134 MiB) queda en 31 MiB; una máquina
  python con un paquete instalado, en archivo único con el stub, queda en 46 MiB.
- **Se extrae una sola vez** en `<cache>/packs/<chave>`, donde la clave es la huella del manifiesto, y se registra como
  imagen `pack:<chave>`. Un segundo `run` no vuelve a extraer.
- **El payload viene de fuera**: se rechazan (probado) nombres de archivo con `/` o `..`, tramos fuera del archivo,
  datos truncados, un manifiesto de más de 1 MiB y una versión de formato futura.

## Prueba: verificar un valor, no un arranque

Un pack que perdió el disco aún enciende y termina con 0. El e2e (`TestPack`) graba un marcador y un módulo Python en
una máquina, empaqueta, **borra la máquina de origen** y lee el marcador a través del artefacto. El defecto plantado (un
pack sin el disco) se detecta.

## Lo que el artefacto no lleva

El stub no incrusta el runtime de la VM: usa la **libkrun del host de destino** (`task krun:install`; `rc doctor` lo
verifica): la libkrun de Nix en macOS enlaza virglrenderer y MoltenVK por ruta absoluta de `/nix/store`, y llevar ese
cierre exigiría reescribir las rutas de las bibliotecas. Lo que viaja es la imagen y el estado de la máquina, que es lo
que no se puede reproducir en el destino. El stub es del sistema y de la arquitectura donde se creó (`host_platform` en
el manifiesto).
