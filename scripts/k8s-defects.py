#!/usr/bin/env python3
"""Planta defeitos de propósito no código do runtime de Kubernetes e confere se os testes pegam (rule 13).

Cada defeito troca um trecho de um arquivo, roda os testes do pacote dele e restaura o original, mesmo em
erro. Sai com o número de defeitos que passaram batido (0 = todos pegos).

Uso: scripts/k8s-defects.py
"""

import pathlib
import subprocess
import sys

ROOT = pathlib.Path(__file__).resolve().parent.parent

# (arquivo, trecho original, trecho com defeito, o que o defeito quebra)
DEFECTS = [
    ("internal/adapters/k8sshim/service.go", "s.exited(t, 128+r.Signal)", "s.exited(t, r.Signal)", "código 128+sinal do kill sem processo"),
    ("internal/adapters/k8sshim/service.go", "task.Status_STOPPED, code, time.Now()", "task.Status_STOPPED, 0, time.Now()", "código de saída perdido"),
    ("internal/adapters/k8sshim/service.go", "if p.status == task.Status_RUNNING {", "if false {", "delete de processo rodando"),
    ("internal/adapters/k8sshim/service.go", "owner == r.ID", "owner != r.ID", "uso da VM no container errado"),
    ("internal/adapters/k8sshim/bundle.go", "!annotated || t == containerSandbox", "t == containerSandbox", "ctr run sem sandbox"),
    ("internal/adapters/k8sshim/bundle.go", "n >= 1 && n <= 16", "n >= 1 && n <= 160", "teto de vCPU"),
    ("internal/adapters/k8sshim/mounts.go", "\t\t\tif ro {", "\t\t\tif !ro {", "bind somente leitura"),
    ("internal/adapters/k8sshim/mounts.go", "next := len(known)", "next := 0", "índice de cópia repetido no pod"),
    ("internal/adapters/k8sshim/legacy.go", "return p.InstanceID != id || p.Namespace != namespace", "return false", "start antigo visto como novo"),
    ("internal/adapters/k8sshim/podvm.go", "g.code = ev.Code", "g.code = 0", "código de saída do processo"),
    ("internal/adapters/k8sshim/podvm.go", "if errors.Is(err, io.EOF) {", "if !errors.Is(err, io.EOF) {", "EOF do stdin não repassado"),
    ("internal/adapters/k8sshim/usage.go", "(ut + st) * (1_000_000 / clockTicks)", "ut * (1_000_000 / clockTicks)", "tempo de sistema fora da CPU"),
    ("internal/adapters/podnet/podnet.go", "binary.BigEndian.PutUint32(out, uint32(n))", "binary.BigEndian.PutUint32(out, uint32(n+1))", "prefixo do quadro"),
    ("internal/adapters/podnet/podnet.go", "if n == 0 || n > maxFrame {", "if n > maxFrame {", "quadro vazio aceito"),
    ("internal/guest/proc.go", "return 128 + int(st.Signal())", "return int(st.Signal())", "código de morte por sinal"),
    ("internal/guest/proc.go", "cmd.Process.Signal(syscall.Signal(ev.Code))", "cmd.Process.Signal(syscall.SIGKILL)", "sinal trocado"),
    ("internal/guest/proc.go", "Env: job.Env, Dir: job.Cwd}", "Env: append(os.Environ(), job.Env...), Dir: job.Cwd}", "ambiente do guest vaza"),
    ("internal/adapters/krun/sandbox_rules.go", "if abi >= 5 {\n\t\tall |= ll.AccessFSIoctlDev", "if abi >= 4 {\n\t\tall |= ll.AccessFSIoctlDev", "direito que a ABI não conhece"),
    ("internal/adapters/krun/sandbox_rules.go", "if abi >= 4 {\n\t\t// Nenhuma regra de porta", "if abi >= 5 {\n\t\t// Nenhuma regra de porta", "rede liberada na ABI 4"),
    ("internal/guest/netcfg.go", "!c.Addr.Contains(c.Gateway)", "c.Addr.Contains(c.Gateway)", "rota de link do Calico"),
]


def run(path, old, new):
    """Aplica um defeito, roda os testes do pacote e devolve True se algum teste reprovou."""
    f = ROOT / path
    orig = f.read_text()
    if orig.count(old) != 1:
        raise SystemExit(f"trecho não é único em {path}: {old!r}")
    try:
        f.write_text(orig.replace(old, new))
        pkg = "./" + str(pathlib.Path(path).parent)
        r = subprocess.run(["go", "test", "-count=1", "-timeout", "120s", pkg], cwd=ROOT, capture_output=True, text=True)
        return r.returncode != 0
    finally:
        f.write_text(orig)


def main():
    """Planta cada defeito e imprime o resultado; o código de saída é o número que passou batido."""
    missed = 0
    for i, (path, old, new, what) in enumerate(DEFECTS, 1):
        caught = run(path, old, new)
        missed += not caught
        print(f"[{i}/{len(DEFECTS)}] {'pego' if caught else 'PASSOU BATIDO'}: {what} ({path})")
    print(f"plantados: {len(DEFECTS)}; pegos: {len(DEFECTS) - missed}; passaram batido: {missed}")
    return missed


if __name__ == "__main__":
    sys.exit(main())
