"""Planta, um por vez, os defeitos que o suporte a Android corrigiu, roda o teste que deveria pegá-lo e restaura.

Prova de falha dos testes do Android (máquina Android, ADB, rootfs, limite de arquivos): um teste que nunca reprovou
não prova nada. Cada linha da tabela é um defeito medido na prova de 05-06/11/2026.

    python3 -I scripts/android_planted_defects.py      # da raiz do clone; sai com quantos passaram batido
"""
import pathlib
import subprocess
import sys

ROOT = pathlib.Path(__file__).resolve().parent.parent

# (descrição, arquivo, trecho certo, trecho com defeito, pacote, filtro de teste)
DEFECTS = [
    ("porta do ADB não publicada", "internal/app/android.go",
     "netw.Ports = append(append([]domain.PortMap{}, netw.Ports...), domain.PortMap{Host: port, Guest: domain.ADBPort})",
     "netw.Ports = append([]domain.PortMap{}, netw.Ports...)", "./internal/app/", "TestAndroidStart$"),
    ("entrada sem validação da tela", "internal/app/android.go",
     "cmd, err := in.Command(*st.Android)",
     "cmd, err := in.Command(domain.Android{Width: 99999, Height: 99999})", "./internal/app/", "TestAndroidScreenOps"),
    ("--android perdido na junção com o runner.code", "internal/adapters/runfile/runfile.go",
     "base.Android = &a", "_ = a", "./internal/adapters/runfile/", "TestOverrideKeepsAndroid"),
    ("limite de arquivos não sobe", "internal/adapters/krun/rlimit.go",
     "lim.Cur = lim.Max", "lim.Cur = lim.Cur", "./internal/adapters/krun/", "TestRaiseNofile"),
    ("hard link zera o modo do alvo", "internal/adapters/androidfs/androidfs.go",
     "\t\t\tcontinue\n\t\tdefault:", "\t\tdefault:", "./internal/adapters/androidfs/", "TestUntar$"),
    ("WaitBoot perde o motivo no prazo", "internal/adapters/adb/adb.go",
     "case timedOut(err) && last != nil:", "case false:", "./internal/adapters/adb/", "TestWaitBoot"),
    ("ADB só em vsock (sem fixar o TCP)", "internal/adapters/androidfs/tweak.go",
     "    setprop service.adb.listen_addrs tcp:%d", "    # tcp:%d", "./internal/adapters/androidfs/", "TestTweak"),
    ("sem a rota de volta pela tabela main", "internal/adapters/androidfs/tweak.go",
     "/system/bin/ip rule add pref 31000 from all lookup main", "true", "./internal/adapters/androidfs/", "TestTweak"),
    ("kernel próprio atrás do do servidor", "internal/adapters/krun/launcher.go",
     "return s.KrunfwDir + string(os.PathListSeparator) + l.LibDir",
     "return l.LibDir + string(os.PathListSeparator) + s.KrunfwDir", "./internal/adapters/krun/", "TestLibDirFor"),
]


def main() -> int:
    """Roda cada defeito plantado e conta os que nenhum teste pegou (o arquivo volta ao original sempre)."""
    missed = 0
    for i, (desc, rel, good, bad, pkg, run) in enumerate(DEFECTS, 1):
        path = ROOT / rel
        original = path.read_text()
        if good not in original:
            print(f"[{i}/{len(DEFECTS)}] ⚠️  {desc}: trecho não encontrado em {rel} (o código mudou; atualize a tabela)")
            missed += 1
            continue
        path.write_text(original.replace(good, bad, 1))
        try:
            r = subprocess.run(["go", "test", pkg, "-run", run, "-count=1"], cwd=ROOT,
                               capture_output=True, text=True, timeout=300)
        finally:
            path.write_text(original)
        caught = r.returncode != 0
        missed += not caught
        print(f"[{i}/{len(DEFECTS)}] {'✅ pego' if caught else '🛑 passou batido'}: {desc}")
    return missed


if __name__ == "__main__":
    sys.exit(main())
