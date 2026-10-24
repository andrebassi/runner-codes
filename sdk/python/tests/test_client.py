"""Testes do SDK Python contra um servidor HTTP falso que grava cada chamada."""

import json
import threading
import unittest
from http.server import BaseHTTPRequestHandler, ThreadingHTTPServer
from urllib.parse import urlsplit

from runner_code import Client, Job, RunnerCodeError, _jpeg_size

MACHINE = {"name": "dev", "stack": "python", "state": "running", "pid": 42,
           "network": {"name": "lab", "address": "100.98.0.10"}}

# Rotas do servidor falso: "MÉTODO caminho" -> (status, corpo). Cada teste ajusta a sua.
ROUTES: dict = {}
CALLS: list = []


class FakeAPI(BaseHTTPRequestHandler):
    """Grava método, caminho, corpo e Content-Type, e responde pela tabela ROUTES."""

    def _handle(self):
        n = int(self.headers.get("Content-Length") or 0)
        body = self.rfile.read(n) if n else b""
        CALLS.append((self.command, self.path, body, self.headers.get("Content-Type")))
        status, out = ROUTES.get(f"{self.command} {urlsplit(self.path).path}", (418, b"teapot"))
        if isinstance(out, (dict, list)):
            out = json.dumps(out).encode()
        self.send_response(status)
        self.send_header("Content-Length", str(len(out)))
        self.end_headers()
        self.wfile.write(out)

    do_GET = do_POST = do_PUT = do_DELETE = _handle

    def log_message(self, *args):
        """Silencia o log de acesso do http.server na saída dos testes."""


def jpeg(width: int, height: int, marker: int = 0xC0) -> bytes:
    """Cabeçalho mínimo de um JPEG: SOI, um APP0 (para o SOF não ser o primeiro segmento), uma DHT e o SOF."""
    app0 = b"\xff\xe0" + (16).to_bytes(2, "big") + b"JFIF\x00" + bytes(9)
    dht = b"\xff\xc4" + (3).to_bytes(2, "big") + b"\x00"
    sof = bytes([0xFF, marker]) + (17).to_bytes(2, "big") + b"\x08" + height.to_bytes(2, "big") + width.to_bytes(2, "big") + bytes(10)
    return b"\xff\xd8" + app0 + dht + sof


class SDKTest(unittest.TestCase):
    """Um servidor por classe; ROUTES e CALLS zerados a cada teste."""

    @classmethod
    def setUpClass(cls):
        cls.srv = ThreadingHTTPServer(("127.0.0.1", 0), FakeAPI)
        threading.Thread(target=cls.srv.serve_forever, daemon=True).start()
        cls.client = Client(f"http://127.0.0.1:{cls.srv.server_port}/")

    @classmethod
    def tearDownClass(cls):
        cls.srv.shutdown()

    def setUp(self):
        ROUTES.clear()
        CALLS.clear()

    def last(self):
        """Última chamada recebida pelo servidor falso."""
        return CALLS[-1]

    def test_lifecycle(self):
        """Cada método chama a rota certa, com corpo e query certos, e decodifica a resposta."""
        ROUTES.update({
            "POST /api/v1/machines": (201, MACHINE), "GET /api/v1/machines": (200, [MACHINE]),
            "GET /api/v1/machines/a%2Fb": (200, MACHINE), "POST /api/v1/machines/dev/start": (200, MACHINE),
            "POST /api/v1/machines/dev/exec": (200, {"exitCode": 3, "stdout": "o", "stderr": "e", "timedOut": True, "executionTime": 7}),
            "PUT /api/v1/machines/dev/files": (200, {"path": "/a", "bytes": 2}),
            "GET /api/v1/machines/dev/files": (200, b"hi"), "GET /api/v1/machines/dev/logs": (200, b"boot\n"),
            "PUT /api/v1/machines/dev/egress": (200, MACHINE), "POST /api/v1/machines/dev/pause": (204, b""),
            "POST /api/v1/machines/dev/resume": (204, b""), "POST /api/v1/machines/dev/branch": (201, {**MACHINE, "name": "kid"}),
            "POST /api/v1/machines/dev/stop": (204, b""), "DELETE /api/v1/machines/dev": (204, b""),
        })
        c = self.client
        m = c.create_machine("dev", stack="python", network="lab", cpus=0, egress={"allow_hosts": ["pypi.org"]})
        self.assertEqual(m["network"]["address"], "100.98.0.10")
        sent = json.loads(self.last()[2])
        self.assertEqual(sent, {"name": "dev", "stack": "python", "network": "lab", "egress": {"allow_hosts": ["pypi.org"]}},
                         "campo vazio não vai: o servidor o leria como override")
        self.assertEqual(self.last()[3], "application/json")
        self.assertEqual(c.machines()[0]["pid"], 42)
        c.machine("a/b")
        self.assertEqual(self.last()[1], "/api/v1/machines/a%2Fb")
        c.start("dev")
        r = c.exec("dev", ["sh", "-c", "x"], env=["A=1"], stdin="in", timeout=5)
        self.assertEqual((r.exit_code, r.stdout, r.stderr, r.timed_out, r.execution_time), (3, "o", "e", True, 7))
        self.assertEqual(json.loads(self.last()[2]), {"command": ["sh", "-c", "x"], "env": ["A=1"], "stdin": "in", "timeout": 5})
        c.exec("dev", ["true"])
        self.assertEqual(json.loads(self.last()[2]), {"command": ["true"]})
        c.write_file("dev", "/a b", b"hi", mode=0o600)
        self.assertEqual(self.last()[1:], ("/api/v1/machines/dev/files?path=%2Fa+b&mode=600", b"hi", "application/octet-stream"))
        c.write_file("dev", "/a", b"")
        self.assertNotIn("mode", self.last()[1])
        self.assertEqual(c.read_file("dev", "/a"), b"hi")
        self.assertEqual(self.last()[1], "/api/v1/machines/dev/files?path=%2Fa")
        self.assertEqual(c.logs("dev", tail=5, workload_only=True), "boot\n")
        self.assertEqual(self.last()[1], "/api/v1/machines/dev/logs?tail=5&source=workload")
        c.logs("dev")
        self.assertEqual(self.last()[1], "/api/v1/machines/dev/logs?")
        c.update_egress("dev", block_private=True, allow_hosts=[])
        self.assertEqual(json.loads(self.last()[2]), {"block_private": True})
        c.pause("dev")
        c.resume("dev")
        self.assertEqual(c.branch("dev", "kid")["name"], "kid")
        self.assertEqual(json.loads(self.last()[2]), {"name": "kid"})
        for timeout, uri in [(None, "/api/v1/machines/dev/stop"), (0, "/api/v1/machines/dev/stop?timeout=0"),
                             (30, "/api/v1/machines/dev/stop?timeout=30")]:
            c.stop("dev", timeout)
            self.assertEqual(self.last()[1], uri)
        for force, uri in [(True, "/api/v1/machines/dev?force=true"), (False, "/api/v1/machines/dev")]:
            c.delete_machine("dev", force)
            self.assertEqual(self.last()[1], uri)

    def test_execute(self):
        """stderr e exception nulos viram texto vazio; o corpo do job leva só o preenchido."""
        ROUTES["POST /api/v1/execute"] = (200, {"status": "success", "stdout": "42\n", "stderr": None,
                                                "exception": None, "executionTime": 31})
        r = self.client.execute(Job(language="python", code="print(42)"))
        self.assertEqual((r.status, r.stdout, r.stderr, r.exception, r.execution_time), ("success", "42\n", "", "", 31))
        self.assertEqual(json.loads(self.last()[2]), {"language": "python", "code": "print(42)"})
        ROUTES["POST /api/v1/execute"] = (200, {"status": "success", "stderr": "boom", "exception": "Timeout"})
        r = self.client.execute(Job(language="python"))
        self.assertEqual((r.stderr, r.exception), ("boom", "Timeout"))

    def test_errors(self):
        """Os dois formatos de erro, corpo cru e formato desconhecido viram RunnerCodeError."""
        ROUTES.update({
            "GET /api/v1/machines/ghost": (404, {"error": {"code": "NOT_FOUND", "message": "machine ghost not found"}}),
            "POST /api/v1/execute": (400, {"status": "failed", "error": "invalid request: language"}),
            "GET /api/v1/machines": (502, b"bad gateway"),
            "POST /api/v1/machines": (500, {"error": 42}),
        })
        with self.assertRaises(RunnerCodeError) as e:
            self.client.machine("ghost")
        self.assertEqual((e.exception.status, e.exception.code, str(e.exception)),
                         (404, "NOT_FOUND", "runner.code: 404 NOT_FOUND: machine ghost not found"))
        with self.assertRaises(RunnerCodeError) as e:
            self.client.execute(Job(language=""))
        self.assertEqual((e.exception.code, str(e.exception)), ("", "runner.code: 400: invalid request: language"))
        with self.assertRaises(RunnerCodeError) as e:
            self.client.machines()
        self.assertEqual(e.exception.message, "bad gateway")
        with self.assertRaises(RunnerCodeError) as e:
            self.client.create_machine("x")
        self.assertEqual(e.exception.message, '{"error": 42}')
        with self.assertRaises(RunnerCodeError) as e:
            self.client.pause("nope")
        self.assertEqual(e.exception.status, 418)

    def test_screen(self):
        """screenshot lê a imagem e o tamanho; click, type e key mandam a ação certa; erro da API e corpo que não é
        imagem viram RunnerCodeError."""
        ROUTES.update({
            "GET /api/v1/machines/desk/screen": (200, jpeg(1512, 794)),
            "POST /api/v1/machines/desk/screen/actions": (204, b""),
            "GET /api/v1/machines/broken/screen": (200, b"not an image"),
            "POST /api/v1/machines/a%2Fb/screen/actions": (400, {"code": "CONFIG_ERROR", "error": {"code": "CONFIG_ERROR", "message": "bad"}}),
        })
        c = self.client
        shot = c.screenshot("desk")
        self.assertEqual((shot.width, shot.height), (1512, 794))
        self.assertTrue(shot.jpeg.startswith(b"\xff\xd8"))
        c.click("desk", 10, 20)
        c.click("desk", 5, 6, button="right", double=True)
        c.type("desk", "ls")
        c.key("desk", "Return")
        bodies = [json.loads(call[2])["actions"] for call in CALLS[1:5]]
        self.assertEqual(bodies, [
            [{"action": "click", "x": 10, "y": 20, "button": "left"}],
            [{"action": "click", "x": 5, "y": 6, "button": "right", "double": True}],
            [{"action": "type", "text": "ls"}], [{"action": "key", "keys": "Return"}],
        ])
        c.screen_actions("desk", {"action": "wait", "ms": 10}, {"action": "key", "keys": "a"})
        self.assertEqual(len(json.loads(self.last()[2])["actions"]), 2)
        with self.assertRaises(RunnerCodeError) as e:
            c.screen_actions("a/b", {"action": "fly"})
        self.assertEqual((e.exception.status, e.exception.code), (400, "CONFIG_ERROR"))
        with self.assertRaises(RunnerCodeError) as e:
            c.screenshot("broken")
        self.assertIn("not an image", e.exception.message)

    def test_jpeg_size(self):
        """O tamanho sai do SOF depois de pular segmentos; cabeçalho torto, sem SOF ou cortado no SOF é ValueError."""
        self.assertEqual(_jpeg_size(jpeg(640, 480)), (640, 480))
        self.assertEqual(_jpeg_size(jpeg(7, 9, marker=0xC2)), (7, 9))  # JPEG progressivo (SOF2)
        for bad in (b"PNG...", b"\xff\xd8\x00\x00\x00\x00", b"\xff\xd8\xff\xc4\x00\x02",
                    b"\xff\xd8\xff\xc0\x00\x11\x08\x00"):
            with self.assertRaises(ValueError):
                _jpeg_size(bad)

    def test_transport_failure(self):
        """Servidor fora do ar não é erro da API: chega como OSError (URLError)."""
        with self.assertRaises(OSError) as e:
            Client("http://127.0.0.1:1", timeout=2).machines()
        self.assertNotIsInstance(e.exception, RunnerCodeError)


if __name__ == "__main__":
    unittest.main()
