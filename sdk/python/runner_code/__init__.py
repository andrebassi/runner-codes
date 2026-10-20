"""SDK Python da API HTTP do runner.code (`rc serve`).

Só biblioteca padrão: o SDK roda em qualquer Python 3.9+ sem instalar dependência, que é o
caso típico de quem só quer mandar código para uma microVM. Os métodos espelham o SDK Go
(`runner.code/pkg/client`) e as rotas da API.
"""

from __future__ import annotations

import json
import urllib.error
import urllib.parse
import urllib.request
from dataclasses import dataclass, field
from typing import Any, Optional

__all__ = ["Client", "RunnerCodeError", "Job", "JobResult", "ExecResult"]

# Teto do cliente HTTP: acima dos 300 s que um job pode durar, para o servidor responder
# o timeout do job antes de o cliente desistir.
DEFAULT_TIMEOUT = 600.0


class RunnerCodeError(Exception):
    """Recusa da API: status HTTP, código estável (só nas rotas de máquina) e mensagem."""

    def __init__(self, status: int, code: str, message: str):
        self.status, self.code, self.message = status, code, message
        prefix = f"runner.code: {status} {code}" if code else f"runner.code: {status}"
        super().__init__(f"{prefix}: {message}")


@dataclass
class Job:
    """Execução efêmera (POST /api/v1/execute): VM nova, usada uma vez."""

    language: str
    code: str = ""
    files: list = field(default_factory=list)  # [{"name": ..., "content": ...}]
    stdin: str = ""
    timeout: int = 0  # segundos; 0 = padrão do servidor (10 s)
    gpu: bool = False


@dataclass
class JobResult:
    """Resultado de um job. status "success" diz que a API executou, não que o programa
    passou: o desfecho do programa está em exception e stderr."""

    status: str
    stdout: str
    stderr: str
    exception: str
    execution_time: int
    error: str = ""


@dataclass
class ExecResult:
    """Resultado de um comando numa máquina; exit_code é o do processo."""

    exit_code: int
    stdout: str
    stderr: str
    timed_out: bool
    execution_time: int


def _decode_error(status: int, body: bytes) -> RunnerCodeError:
    """Lê os dois formatos de erro da API ({"error":{"code","message"}} das máquinas e
    {"error":"..."} dos jobs); corpo que não é nenhum dos dois vira a mensagem crua."""
    text = body.decode("utf-8", "replace").strip()
    try:
        err = json.loads(text).get("error")
    except (ValueError, AttributeError):
        return RunnerCodeError(status, "", text)
    if isinstance(err, dict):
        return RunnerCodeError(status, err.get("code", ""), err.get("message", ""))
    if isinstance(err, str):
        return RunnerCodeError(status, "", err)
    return RunnerCodeError(status, "", text)


def _compact(d: dict) -> dict:
    """Tira os campos vazios: o servidor lê campo presente como override do runner.code."""
    return {k: v for k, v in d.items() if v not in (None, "", 0, False, [], {})}


class Client:
    """Cliente de um `rc serve`; base_url é a raiz (http://127.0.0.1:8080)."""

    def __init__(self, base_url: str, timeout: float = DEFAULT_TIMEOUT):
        self.base_url = base_url.rstrip("/")
        self.timeout = timeout

    def _do(self, method: str, path: str, body: Optional[bytes] = None,
            content_type: str = "", raw: bool = False) -> Any:
        """Manda a requisição; fora de 2xx levanta RunnerCodeError. raw devolve os bytes."""
        req = urllib.request.Request(self.base_url + path, data=body, method=method)
        if content_type:
            req.add_header("Content-Type", content_type)
        try:
            with urllib.request.urlopen(req, timeout=self.timeout) as resp:
                data = resp.read()
        except urllib.error.HTTPError as e:
            raise _decode_error(e.code, e.read()) from None
        if raw:
            return data
        return json.loads(data) if data else None

    def _json(self, method: str, path: str, payload: Any = None) -> Any:
        """Manda payload como JSON (None manda sem corpo) e decodifica a resposta."""
        if payload is None:
            return self._do(method, path)
        return self._do(method, path, json.dumps(payload).encode(), "application/json")

    @staticmethod
    def _machine(name: str, suffix: str = "") -> str:
        """Caminho de uma máquina, com o nome escapado."""
        return "/api/v1/machines/" + urllib.parse.quote(name, safe="") + suffix

    def execute(self, job: Job) -> JobResult:
        """Roda um job numa VM efêmera."""
        r = self._json("POST", "/api/v1/execute", _compact(job.__dict__))
        return JobResult(status=r.get("status", ""), stdout=r.get("stdout", ""),
                         stderr=r.get("stderr") or "", exception=r.get("exception") or "",
                         execution_time=r.get("executionTime", 0), error=r.get("error", ""))

    def create_machine(self, name: str, **spec: Any) -> dict:
        """Cria a máquina parada. spec usa as chaves da API: stack, image, cpus, memory_mib,
        net, ports, egress, network, runner_code..."""
        return self._json("POST", "/api/v1/machines", _compact({"name": name, **spec}))

    def machines(self) -> list:
        """Lista as máquinas com o estado de cada uma."""
        return self._json("GET", "/api/v1/machines")

    def machine(self, name: str) -> dict:
        """Uma máquina."""
        return self._json("GET", self._machine(name))

    def delete_machine(self, name: str, force: bool = False) -> None:
        """Apaga a máquina; force mata se estiver ligada."""
        self._json("DELETE", self._machine(name, "?force=true" if force else ""))

    def start(self, name: str) -> dict:
        """Liga a máquina."""
        return self._json("POST", self._machine(name, "/start"))

    def stop(self, name: str, timeout: Optional[int] = None) -> None:
        """Desliga; timeout None usa o stop_timeout da máquina, 0 mata na hora."""
        q = "" if timeout is None else f"?timeout={int(timeout)}"
        self._json("POST", self._machine(name, "/stop" + q))

    def exec(self, name: str, argv: list, env: Optional[list] = None, workdir: str = "",
             stdin: str = "", timeout: int = 0) -> ExecResult:
        """Roda argv na máquina, sem shell implícito (para shell: ["sh", "-c", ...])."""
        body = {"command": argv, **_compact({"env": env, "workdir": workdir, "stdin": stdin, "timeout": timeout})}
        r = self._json("POST", self._machine(name, "/exec"), body)
        return ExecResult(exit_code=r["exitCode"], stdout=r["stdout"], stderr=r["stderr"],
                          timed_out=r.get("timedOut", False), execution_time=r.get("executionTime", 0))

    def write_file(self, name: str, path: str, data: bytes, mode: int = 0) -> None:
        """Grava data em path (absoluto) dentro da máquina; mode 0 usa o padrão (0644)."""
        q = {"path": path}
        if mode:
            q["mode"] = format(mode, "o")
        self._do("PUT", self._machine(name, "/files?" + urllib.parse.urlencode(q)), data,
                 "application/octet-stream", raw=True)

    def read_file(self, name: str, path: str) -> bytes:
        """Lê path (absoluto) de dentro da máquina."""
        return self._do("GET", self._machine(name, "/files?" + urllib.parse.urlencode({"path": path})), raw=True)

    def logs(self, name: str, tail: int = 0, workload_only: bool = False) -> str:
        """Console da máquina; tail 0 traz tudo, workload_only filtra a saída do workload."""
        q = _compact({"tail": tail, "source": "workload" if workload_only else ""})
        return self._do("GET", self._machine(name, "/logs?" + urllib.parse.urlencode(q)), raw=True).decode()

    def update_egress(self, name: str, **egress: Any) -> dict:
        """Troca a política de saída inteira (allow_hosts, allow_cidrs, block_private...)."""
        return self._json("PUT", self._machine(name, "/egress"), _compact(egress))

    def pause(self, name: str) -> None:
        """Congela a máquina ligada."""
        self._json("POST", self._machine(name, "/pause"))

    def resume(self, name: str) -> None:
        """Continua a máquina pausada."""
        self._json("POST", self._machine(name, "/resume"))

    def branch(self, source: str, name: str) -> dict:
        """Cria name como cópia copy-on-write do disco de source."""
        return self._json("POST", self._machine(source, "/branch"), {"name": name})
