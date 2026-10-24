"""Receitas do SDK Python contra um `rc serve`: as mesmas do exemplo Go, uma linha conferível por receita.

Uso, com o SDK instalado (pip) ou pelo repositório:
    PYTHONPATH=sdk/python python3 examples/sdk-python-recipes/main.py [http://127.0.0.1:8080]
"""

import os
import sys

from runner_code import Client, Job, RunnerCodeError


def run_untrusted(c: Client) -> None:
    """Roda código de fora numa VM descartável; quem diz se o programa deu certo é `exception`."""
    code = "import sys\nprint(sum(range(10)))\nprint('aviso', file=sys.stderr)"
    r = c.execute(Job(language="python", code=code, timeout=10))
    if r.exception:
        raise RuntimeError(f"o programa falhou: {r.exception}")
    print(f"untrusted: stdout={r.stdout.strip()} stderr={r.stderr.strip()} tempo={r.execution_time}ms")


def files_and_stdin(c: Client) -> None:
    """Projeto de vários arquivos e entrada padrão; o primeiro arquivo é o ponto de entrada."""
    r = c.execute(Job(
        language="python",
        files=[
            {"name": "main.py", "content": "import util, sys\nprint(util.dobro(int(sys.stdin.read())))"},
            {"name": "util.py", "content": "def dobro(n):\n    return 2 * n"},
        ],
        stdin="21",
    ))
    print(f"files: {r.stdout.strip()}")


def timeout(c: Client) -> None:
    """Prazo estourado: a VM é morta e o resultado vem com exception "Timeout", sem exceção Python."""
    r = c.execute(Job(language="python", code="while True: pass", timeout=2))
    print(f"timeout: exception={r.exception}")


def compile_error(c: Client) -> None:
    """Falha de compilação (C, C++, Rust, Java...): exception "Compilation failed" e o compilador no stderr.

    Go é exceção: roda por `go run`, e o erro de compilação chega só no stderr, com exception vazio.
    """
    r = c.execute(Job(language="c", code="int main(void) { return x; }"))
    print(f"compile: exception={r.exception} stderr_tem_erro={str(bool(r.stderr)).lower()}")


# Programa que roda DENTRO da máquina: tenta dois sites e grava o resultado num arquivo. O
# `get` devolve o status HTTP, ou "bloqueado" quando a política de saída recusa a conexão.
APP = """import os, urllib.request
# status HTTP do endereço, ou "bloqueado" quando a política de saída recusa
def get(u):
    try:
        return urllib.request.urlopen(u, timeout=5).status
    except Exception:
        return "bloqueado"
open("saida.txt", "w").write(f"{os.environ['QUEM']} pypi={get('https://pypi.org/simple/')} example={get('https://example.com')}")
"""


def machine_workflow(c: Client) -> None:
    """Máquina persistente com saída só para pypi.org: grava, roda com env e workdir, lê de volta."""
    name = f"recipe-py-{os.getpid()}"
    c.create_machine(name, stack="python", net=True, egress={"allow_hosts": ["pypi.org"]})
    try:
        c.start(name)
        c.write_file(name, "/work/app.py", APP.encode())
        out = c.exec(name, ["python3", "app.py"], env=["QUEM=sdk-python"], workdir="/work", timeout=30)
        if out.exit_code != 0:
            raise RuntimeError(f"exit {out.exit_code}: {out.stderr}")
        print(f"machine: {c.read_file(name, '/work/saida.txt').decode()}")
    finally:
        # force=True apaga mesmo ligada: a máquina não sobra se uma etapa falhar no meio.
        c.delete_machine(name, force=True)


def branch(c: Client) -> None:
    """Copia uma máquina: a mãe pausada, a filha nasce com o que a mãe tinha gravado."""
    mom = f"recipe-py-mom-{os.getpid()}"
    kid = mom + "-kid"
    c.create_machine(mom, stack="python")
    try:
        c.start(mom)
        c.write_file(mom, "/root/estado.txt", b"feito antes do branch")
        c.pause(mom)
        c.branch(mom, kid)
        try:
            c.start(kid)
            print(f"branch: {c.read_file(kid, '/root/estado.txt').decode()}")
            c.stop(kid, timeout=10)
        finally:
            c.delete_machine(kid, force=True)
    finally:
        c.delete_machine(mom, force=True)


def typed_error(c: Client) -> None:
    """Trata a recusa da API pelo código estável, sem comparar texto de mensagem."""
    try:
        c.machine("nao-existe")
    except RunnerCodeError as e:
        if e.code != "NOT_FOUND":
            raise
        print(f"errors: {e.status} {e.code}")
        return
    raise RuntimeError("esperava RunnerCodeError")


def main() -> None:
    """Roda as receitas em ordem; a primeira que falhar propaga a exceção (rc 1)."""
    c = Client(sys.argv[1] if len(sys.argv) > 1 else "http://127.0.0.1:8080")
    for recipe in (run_untrusted, files_and_stdin, timeout, compile_error, machine_workflow, branch, typed_error):
        recipe(c)


if __name__ == "__main__":
    main()
