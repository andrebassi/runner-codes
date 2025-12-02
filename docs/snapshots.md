# Firecracker Snapshots - Boot Instantaneo

> **Status**: Completo - 45 linguagens com snapshots no S3
> **Data**: 2025-11-29
> **Resultado**: Boot reduzido de ~2600ms para ~300ms (8.4x mais rapido)

## O que sao Snapshots?

Firecracker suporta snapshots de microVM, permitindo salvar o estado completo de uma VM em execucao e restaura-lo instantaneamente.

```
Cold Boot:      Kernel Load → Init → Guest-Runner → Ready  (~2600ms)
Warm Boot:      Restore State → Ready                       (~300ms)
Speedup:        8.4x mais rapido
```

## Arquitetura de Snapshots

### Armazenamento no S3

```
s3://llm-firesandbox-rootfs/
└── snapshots/
    ├── python/
    │   ├── mem.snapshot      # ~512 MB (memoria da VM)
    │   └── vmstate.snapshot  # ~13 KB (estado do vCPU)
    ├── nodejs/
    │   ├── mem.snapshot
    │   └── vmstate.snapshot
    ├── rust/
    │   ├── mem.snapshot
    │   └── vmstate.snapshot
    ... (45 linguagens)
```

**Armazenamento Total:** ~22.5 GB (90 objetos)

### Estrutura Local (apos download)

```
/srv/firecracker/snapshots/
├── python/
│   ├── vmstate.snapshot      # Estado da VM (CPU, registradores)
│   └── mem.snapshot          # Memoria completa (512MB)
├── nodejs/
│   ├── vmstate.snapshot
│   └── mem.snapshot
└── ... (uma pasta por linguagem)
```

## Implementacao

### 1. Criar Snapshot (Uma vez por linguagem)

```bash
# 1. Boot normal da VM
firecracker --api-sock /tmp/fc-python.sock --config-file /etc/fc-python.json

# 2. Aguardar guest-runner ficar pronto
curl --unix-socket /tmp/fc-python.sock \
  -X GET http://localhost/vm/ready

# 3. Pausar a VM
curl --unix-socket /tmp/fc-python.sock \
  -X PATCH -H "Content-Type: application/json" \
  -d '{"state": "Paused"}' \
  http://localhost/vm

# 4. Criar snapshot
curl --unix-socket /tmp/fc-python.sock \
  -X PUT -H "Content-Type: application/json" \
  -d '{
    "snapshot_type": "Full",
    "snapshot_path": "/srv/firecracker/snapshots/python/vmstate.snapshot",
    "mem_file_path": "/srv/firecracker/snapshots/python/mem.snapshot"
  }' \
  http://localhost/snapshot/create
```

### 2. Restaurar Snapshot (Por execucao)

```bash
# Restore instantaneo
firecracker --api-sock /tmp/fc-exec-123.sock

curl --unix-socket /tmp/fc-exec-123.sock \
  -X PUT -H "Content-Type: application/json" \
  -d '{
    "snapshot_path": "/srv/firecracker/snapshots/python/vmstate.snapshot",
    "mem_backend": {
      "backend_type": "File",
      "backend_path": "/srv/firecracker/snapshots/python/mem.snapshot"
    },
    "enable_diff_snapshots": false,
    "resume_vm": true
  }' \
  http://localhost/snapshot/load
```

## Modificacoes no Host-Agent

### snapshot_manager.go (novo)

```go
package main

import (
    "fmt"
    "os"
    "path/filepath"
)

type SnapshotManager struct {
    snapshotsDir string
    languages    map[string]bool
}

func NewSnapshotManager(dir string) *SnapshotManager {
    return &SnapshotManager{
        snapshotsDir: dir,
        languages:    make(map[string]bool),
    }
}

// HasSnapshot verifica se existe snapshot para a linguagem
func (sm *SnapshotManager) HasSnapshot(lang string) bool {
    vmstate := filepath.Join(sm.snapshotsDir, lang, "vmstate.snapshot")
    mem := filepath.Join(sm.snapshotsDir, lang, "mem.snapshot")

    _, err1 := os.Stat(vmstate)
    _, err2 := os.Stat(mem)

    return err1 == nil && err2 == nil
}

// GetSnapshotPaths retorna os caminhos dos arquivos de snapshot
func (sm *SnapshotManager) GetSnapshotPaths(lang string) (vmstate, mem string) {
    vmstate = filepath.Join(sm.snapshotsDir, lang, "vmstate.snapshot")
    mem = filepath.Join(sm.snapshotsDir, lang, "mem.snapshot")
    return
}

// CreateSnapshot cria um snapshot de uma VM em execucao
func (sm *SnapshotManager) CreateSnapshot(lang string, fc *FirecrackerClient) error {
    dir := filepath.Join(sm.snapshotsDir, lang)
    if err := os.MkdirAll(dir, 0755); err != nil {
        return fmt.Errorf("create snapshot dir: %w", err)
    }

    vmstate, mem := sm.GetSnapshotPaths(lang)

    // 1. Pausar VM
    if err := fc.PauseVM(); err != nil {
        return fmt.Errorf("pause VM: %w", err)
    }

    // 2. Criar snapshot
    if err := fc.CreateSnapshot(vmstate, mem); err != nil {
        return fmt.Errorf("create snapshot: %w", err)
    }

    sm.languages[lang] = true
    return nil
}
```

### firecracker.go (adicoes)

```go
// PauseVM pausa a microVM
func (fc *FirecrackerClient) PauseVM() error {
    return fc.httpPatch("/vm", map[string]string{"state": "Paused"})
}

// ResumeVM retoma a microVM
func (fc *FirecrackerClient) ResumeVM() error {
    return fc.httpPatch("/vm", map[string]string{"state": "Resumed"})
}

// CreateSnapshot cria um snapshot full da VM
func (fc *FirecrackerClient) CreateSnapshot(vmstatePath, memPath string) error {
    return fc.httpPut("/snapshot/create", map[string]interface{}{
        "snapshot_type": "Full",
        "snapshot_path": vmstatePath,
        "mem_file_path": memPath,
    })
}

// LoadSnapshot restaura uma VM a partir de snapshot
func (fc *FirecrackerClient) LoadSnapshot(vmstatePath, memPath string) error {
    return fc.httpPut("/snapshot/load", map[string]interface{}{
        "snapshot_path": vmstatePath,
        "mem_backend": map[string]string{
            "backend_type": "File",
            "backend_path": memPath,
        },
        "enable_diff_snapshots": false,
        "resume_vm":             true,
    })
}
```

## Warm Pool com Snapshots

Para performance maxima, manter um pool de VMs pre-restauradas:

```go
type WarmPool struct {
    mu       sync.Mutex
    vms      map[string]chan *FirecrackerVM // por linguagem
    poolSize int
}

func (wp *WarmPool) GetVM(lang string) (*FirecrackerVM, error) {
    wp.mu.Lock()
    ch, ok := wp.vms[lang]
    wp.mu.Unlock()

    if !ok {
        return nil, fmt.Errorf("no pool for language: %s", lang)
    }

    select {
    case vm := <-ch:
        // Repor o pool em background
        go wp.refillPool(lang)
        return vm, nil
    default:
        // Pool vazio, criar nova VM
        return wp.createVM(lang)
    }
}
```

## Metricas Reais (Testadas em 2025-11-29)

| Metrica | Cold Boot | Warm Boot (Snapshot) | Speedup |
|---------|-----------|---------------------|---------|
| **Tempo de Boot** | ~2600ms | ~300ms | **8.4x** |
| **Primeiro Request** | ~3000ms | ~500ms | **6x** |
| **Memoria por snapshot** | N/A | ~512MB | - |
| **vmstate por snapshot** | N/A | ~13KB | - |
| **Total 45 langs** | N/A | ~22.5GB | - |

## Linguagens Suportadas (45 total)

### Por Categoria

| Categoria | Linguagens |
|-----------|------------|
| **Populares** | python, nodejs, typescript, go, rust, java, dotnet |
| **Web** | php, ruby, perl |
| **Compiladas** | c, cpp, fortran, pascal, cobol, nasm |
| **Funcionais** | haskell, ocaml, elixir, erlang, clojure, lisp, scheme |
| **JVM** | java, kotlin, scala, groovy, clojure |
| **Modernas** | zig, nim, crystal, d, dart, bun, deno |
| **Cientificas** | julia, r, octave |
| **Scripting** | lua, tcl, awk, jq, bash |
| **Outras** | prolog, sqlite, swift, base |

### Lista Completa (Alfabetica)

```
awk, base, bash, bun, c, clojure, cobol, cpp, crystal, d, dart, deno,
dotnet, elixir, erlang, fortran, go, groovy, haskell, java, jq, julia,
kotlin, lisp, lua, nasm, nim, nodejs, ocaml, octave, pascal, perl, php,
prolog, python, r, ruby, rust, scala, scheme, sqlite, swift, tcl,
typescript, zig
```

## Script de Geracao em Massa

O script abaixo foi usado para gerar todos os 45 snapshots:

```bash
#!/bin/bash
# generate-all-snapshots.sh
set -e

WORKDIR="/srv/firecracker"
KERNEL="$WORKDIR/vmlinux"
IMAGES_DIR="$WORKDIR/images"
SNAPSHOT_DIR="$WORKDIR/snapshots"
S3_BUCKET="s3://llm-firesandbox-rootfs/snapshots"

# Listar todas as linguagens
LANGUAGES=$(ls $IMAGES_DIR/rootfs-*.ext4 | sed 's/.*rootfs-\(.*\)\.ext4/\1/' | sort)

for LANG in $LANGUAGES; do
    ROOTFS="$IMAGES_DIR/rootfs-${LANG}.ext4"
    LANG_SNAPSHOT_DIR="$SNAPSHOT_DIR/$LANG"
    API_SOCKET="/tmp/fc-snap-${LANG}.sock"

    # Cleanup
    rm -f $API_SOCKET
    pkill -f "firecracker.*fc-snap-${LANG}" 2>/dev/null || true
    mkdir -p $LANG_SNAPSHOT_DIR

    # Copiar rootfs para RAM (evita I/O lento)
    TMP_ROOTFS="/dev/shm/rootfs-${LANG}.ext4"
    cp $ROOTFS $TMP_ROOTFS

    # Iniciar Firecracker
    firecracker --api-sock $API_SOCKET &
    FC_PID=$!
    sleep 0.5

    # Configurar VM
    curl -s --unix-socket $API_SOCKET -X PUT \
        -H "Content-Type: application/json" \
        -d "{\"kernel_image_path\": \"$KERNEL\", \"boot_args\": \"console=ttyS0 reboot=k panic=1 pci=off\"}" \
        http://localhost/boot-source

    curl -s --unix-socket $API_SOCKET -X PUT \
        -H "Content-Type: application/json" \
        -d "{\"drive_id\": \"rootfs\", \"path_on_host\": \"$TMP_ROOTFS\", \"is_root_device\": true, \"is_read_only\": false}" \
        http://localhost/drives/rootfs

    curl -s --unix-socket $API_SOCKET -X PUT \
        -H "Content-Type: application/json" \
        -d "{\"vcpu_count\": 1, \"mem_size_mib\": 512}" \
        http://localhost/machine-config

    # Iniciar VM
    curl -s --unix-socket $API_SOCKET -X PUT \
        -H "Content-Type: application/json" \
        -d '{"action_type": "InstanceStart"}' \
        http://localhost/actions

    # Aguardar boot
    sleep 3

    # Pausar VM
    curl -s --unix-socket $API_SOCKET -X PATCH \
        -H "Content-Type: application/json" \
        -d '{"state": "Paused"}' \
        http://localhost/vm

    sleep 0.3

    # Criar snapshot
    curl -s --unix-socket $API_SOCKET -X PUT \
        -H "Content-Type: application/json" \
        -d "{\"snapshot_type\": \"Full\", \"snapshot_path\": \"$LANG_SNAPSHOT_DIR/vmstate.snapshot\", \"mem_file_path\": \"$LANG_SNAPSHOT_DIR/mem.snapshot\"}" \
        http://localhost/snapshot/create

    # Cleanup
    kill $FC_PID 2>/dev/null || true
    rm -f $API_SOCKET $TMP_ROOTFS

    # Upload para S3
    aws s3 cp "$LANG_SNAPSHOT_DIR/mem.snapshot" "$S3_BUCKET/$LANG/mem.snapshot" --only-show-errors
    aws s3 cp "$LANG_SNAPSHOT_DIR/vmstate.snapshot" "$S3_BUCKET/$LANG/vmstate.snapshot" --only-show-errors

    # Remover snapshot local para liberar espaco
    rm -rf "$LANG_SNAPSHOT_DIR"

    echo "[$LANG] OK"
done
```

## Comandos Uteis

### Listar todos os snapshots no S3

```bash
aws s3 ls s3://llm-firesandbox-rootfs/snapshots/ --recursive
```

### Verificar total de linguagens

```bash
aws s3 ls s3://llm-firesandbox-rootfs/snapshots/ | grep -c "PRE"
# Esperado: 45
```

### Download de snapshot especifico

```bash
LANG="python"
mkdir -p /srv/firecracker/snapshots/$LANG
aws s3 cp s3://llm-firesandbox-rootfs/snapshots/$LANG/ /srv/firecracker/snapshots/$LANG/ --recursive
```

### Verificar tamanho total

```bash
aws s3 ls s3://llm-firesandbox-rootfs/snapshots/ --recursive --summarize | tail -2
# Total Objects: 90
# Total Size: ~22.5 GB
```

## Troubleshooting

### Problema: "No working init found"

**Causa:** Rootfs corrompido ou incompleto.

**Solucao:** Recriar rootfs com debootstrap. Exemplo para dotnet:

```bash
dd if=/dev/zero of=rootfs-dotnet.ext4 bs=1M count=4000
mkfs.ext4 -F rootfs-dotnet.ext4
mount -o loop rootfs-dotnet.ext4 /mnt/dotnet
debootstrap --variant=minbase --include=ca-certificates,curl bookworm /mnt/dotnet
chroot /mnt/dotnet /bin/bash -c "curl -L https://dot.net/v1/dotnet-install.sh -o install.sh && bash install.sh --channel 8.0 --install-dir /usr/share/dotnet"
ln -sf /usr/share/dotnet/dotnet /mnt/dotnet/usr/bin/dotnet
umount /mnt/dotnet
```

### Problema: Disco cheio em /tmp

**Causa:** Copiar rootfs para /tmp enche o disco.

**Solucao:** Usar /dev/shm (memoria RAM):

```bash
TMP_ROOTFS="/dev/shm/rootfs-${LANG}.ext4"
cp $ROOTFS $TMP_ROOTFS
```

### Problema: Snapshot nao restaura

**Causa:** Versao do Firecracker diferente.

**Solucao:** Usar mesma versao para criar e restaurar.

## Historico de Alteracoes

| Data | Alteracao |
|------|-----------|
| 2025-11-29 | Criacao de 45 snapshots e upload para S3 |
| 2025-11-29 | Recriacao do dotnet (rootfs corrompido) |
| 2025-11-29 | Limpeza do S3 - removidos rootfs, mantidos apenas snapshots |
| 2025-11-29 | Armazenamento reduzido de ~69GB para ~22.5GB |

## Uso do Host-Agent com Snapshots

### Modo Cold Boot (original)

```bash
./host-agent -lang python -code 'print("hello")'
```

### Modo Warm Boot (com snapshots) - 8.4x mais rapido

```bash
./host-agent -snapshot -lang python -code 'print("hello")'
```

### Flags de Snapshot

| Flag | Descricao | Default |
|------|-----------|---------|
| `-snapshot` | Ativar warm boot via snapshot | `false` |
| `-cache-dir` | Diretorio para cache local | `/srv/firecracker/snapshots` |
| `-s3-bucket` | Bucket S3 com snapshots | `llm-firesandbox-rootfs` |
| `-s3-region` | Regiao do S3 | `us-east-1` |

### Exemplo de Execucao

```bash
# Primeira execucao - baixa snapshot do S3 (alguns segundos)
./host-agent -snapshot -lang python -code 'print("hello world")'

# Execucoes subsequentes - usa cache local (~300ms)
./host-agent -snapshot -lang nodejs -code 'console.log("fast!")'
```

## Status Final

- [x] 45 linguagens com snapshots no S3
- [x] Script de geracao em massa
- [x] Documentacao completa
- [x] Cleanup do S3 (apenas snapshots)
- [x] Implementar warm pool no host-agent (`warm_pool.go`)
- [x] Cache local de snapshots frequentes (`snapshot_cache.go`)
- [x] Integracao com main.go (flag `-snapshot`)

## Referencias

- [Firecracker Snapshot Documentation](https://github.com/firecracker-microvm/firecracker/blob/main/docs/snapshotting/snapshot-support.md)
- [Firecracker API - Snapshots](https://github.com/firecracker-microvm/firecracker/blob/main/src/api_server/swagger/firecracker.yaml#L1000)
- [AWS S3 CLI](https://docs.aws.amazon.com/cli/latest/reference/s3/)
