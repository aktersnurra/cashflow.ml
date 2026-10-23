"""Synthetic real-route Bonsai smoke. Build native fixture + web fixture before running."""
import json
import os
import pathlib
import shutil
import subprocess
import tempfile
import time
import unittest
import urllib.request

ROOT = pathlib.Path(__file__).resolve().parents[1]

class AdvisorBrowser(unittest.TestCase):
    def run_smoke(self, mode=None):
        with tempfile.TemporaryDirectory(prefix="advisor-browser-") as directory:
            path = pathlib.Path(directory)
            (path / "index.html").write_text('<!doctype html><html><body><div id="app"></div><script src="/main.bc.js"></script></body></html>')
            shutil.copyfile(ROOT / "_build/web/test/advisor_browser/fixture.bc.js", path / "main.bc.js")
            server = subprocess.Popen([str(ROOT / "_build/default/test/advisor_fixture_server.exe"), directory, "--browser-text"], stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
            try:
                for _ in range(100):
                    try:
                        with urllib.request.urlopen("http://127.0.0.1:18197/fixture/session", timeout=1) as response:
                            json.load(response)
                        break
                    except OSError:
                        time.sleep(.02)
                else:
                    self.fail("fixture did not start")
                env = dict(os.environ, NODE_PATH="/tmp/cashflow-browser-tools/node_modules")
                if mode:
                    env[mode] = "1"
                result = subprocess.run(["node", "web/e2e/advisor_controller_smoke.js"], cwd=ROOT, env=env, text=True, capture_output=True, timeout=65)
                self.assertEqual(result.returncode, 0, result.stdout + result.stderr)
                print(result.stdout.strip())
            finally:
                server.terminate()
                _, error = server.communicate(timeout=5)
                self.assertEqual(error, b"")

    def test_stream_reconnect_scroll_access(self):
        self.run_smoke()

    def test_shell_deactivation(self):
        self.run_smoke("ADVISOR_DEACTIVATE")

    def test_stop(self):
        self.run_smoke("ADVISOR_STOP_ONLY")

    def test_http_error_distinctions(self):
        self.run_smoke("ADVISOR_API_ERRORS")

    def test_ambiguous_post_timeout(self):
        self.run_smoke("ADVISOR_POST_TIMEOUT")

if __name__ == "__main__":
    unittest.main()
