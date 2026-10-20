---
title: 'Visión general de las tasks'
description: 'Las tasks del Taskfile.yaml de Runner Codes'
---

## Lista

El proyecto usa [Task](https://taskfile.dev). `task` sin argumentos lo lista todo.

| Task | Qué hace |
|------|-----------|
| `task krun:install` | instala libkrun + libkrunfw mediante Nix (`.#krun`) |
| `task build` | compila host (`bin/rc`) y guest en `bin/` y firma el host en macOS |
| `task image:build -- --stack <l>` | genera el ext4 del stack (`-- --all` para todos); compila antes |
| `task run -- <flags>` | ejecuta código en una VM nueva |
| `task shell -- --stack <l>` | shell interactivo en una VM nueva |
| `task serve -- <flags>` | inicia la API en `127.0.0.1:8080` con pool; compila antes |
| `task doctor` | verifica si el host puede ejecutar microVMs |
| `task test` | pruebas unitarias con race detector |
| `task test:cov` | gate de cobertura: falla por debajo del 90% total, del 100% en el dominio o de la catraca del guest; incluye el SDK de Python |
| `task sdk:py:cov` | solo el SDK de Python: línea y branch, cada uno ≥ 90% |
| `task test:targets` | gate multiplataforma: darwin/arm64, linux/amd64 y linux/arm64 compilan; windows se rechaza |
| `task test:e2e` | de extremo a extremo contra la libkrun real; necesita las imágenes construidas |
| `task runbook` | ejecuta el `docs/runbook.md` y verifica cada resultado esperado |
| `task guides` | ejecuta las guías de `examples/guides`, cada una en una VM real (`DIRS="pack teardown"` para algunas) |
| `task site:demos` | las ejecuciones reales detrás de las demos del sitio (`SCENES="network pack"` para algunas) |
| `task gates` | `test:targets`, `test:cov`, `test:e2e` y `runbook`, en ese orden |
| `task ship -- "<título>" <arquivo>` | publica la branch actual, abre el PR y hace el merge |
| `task clean` | borra `bin/`; la caché de imágenes se queda donde está |

## Flujo típico

```bash
task krun:install
task build
task doctor
task image:build -- --stack python
task run -- --stack python --code 'print(1)'
./bin/rc machine run --stack python -- python3 -V
task serve
task runbook
```

Detalles en [Build](/tasks/build) y [Pruebas](/tasks/testing).
