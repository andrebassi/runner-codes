"""Exemplo do SDK Python contra um `rc serve`: um job efêmero e uma máquina do começo ao fim.

Uso: PYTHONPATH=sdk/python python3 examples/sdk-python/main.py [http://127.0.0.1:8080]
"""

import os
import sys

from runner_code import Client, Job, RunnerCodeError


def main() -> None:
    """Imprime uma linha por etapa; a máquina é apagada mesmo se algo falhar no meio."""
    c = Client(sys.argv[1] if len(sys.argv) > 1 else "http://127.0.0.1:8080")
    r = c.execute(Job(language="python", code="print(6*7)"))
    print("job:", r.stdout, end="")

    name = f"sdk-py-{os.getpid()}"
    c.create_machine(name, stack="python")
    try:
        m = c.start(name)
        print("machine:", m["name"], m["state"])
        c.write_file(name, "/root/hello.py", b"print('ola do sdk python')\n")
        print("exec:", c.exec(name, ["python3", "/root/hello.py"]).stdout, end="")
        try:
            c.machine("nao-existe")
        except RunnerCodeError as e:
            print("error:", e.code)
    finally:
        c.delete_machine(name, force=True)


if __name__ == "__main__":
    main()
