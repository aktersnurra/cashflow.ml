"""Synthetic actual-Advisor HTTP/SSE loopback regression; no live calls."""
import contextlib
import http.client
import json
import pathlib
import socket
import subprocess
import time
import unittest
from typing import Any

ROOT = pathlib.Path(__file__).resolve().parents[1]
ORIGIN = "http://127.0.0.1:18197"


class AdvisorSocket(unittest.TestCase):
    def setUp(self):
        args = [str(ROOT / "_build/default/test/advisor_fixture_server.exe")]
        if self._testMethodName.startswith("test_stalled"):
            args.append("--small-socket-buffers")
        self.server = subprocess.Popen(args, stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
        self.addCleanup(self.stop_server)
        for _ in range(100):
            try:
                self.sessions = self.request("GET", "/fixture/session")[1]
                return
            except (ConnectionError, OSError):
                time.sleep(.02)
        self.fail("fixture did not start")

    def stop_server(self):
        if self.server.poll() is None:
            self.server.terminate()
        _, err = self.server.communicate(timeout=5)
        self.assertEqual(err, b"")

    def headers(self, who="a"):
        s = self.sessions[who]
        return {"Cookie": "cashflow_session=" + s["token"], "X-CSRF-Token": s["csrf"], "Origin": ORIGIN, "Content-Type": "application/json"}

    def request(self, method, path, body=None, who="a") -> tuple[int, Any]:
        c = http.client.HTTPConnection("127.0.0.1", 18197, timeout=3)
        c.request(method, path, None if body is None else json.dumps(body), self.headers(who) if hasattr(self, "sessions") else {})
        r = c.getresponse()
        status, data = r.status, r.read()
        c.close()
        return status, json.loads(data) if data else None

    def start(self):
        status, a = self.request("POST", "/api/advisor/turns", {"version": 1, "data": {"conversation_id": None, "scope": {"type": "Personal"}, "currency": {"code": "SEK", "scale": 2}, "locale": "en", "user_text": "Synthetic fixture", "idempotency_key": "socket", "expected_version": "0", "source_finding_key": None}})
        self.assertEqual(status, 202)
        return a["data"]["ids"]

    @contextlib.contextmanager
    def events(self, ids, who="a", suffix="", last=None):
        c = http.client.HTTPConnection("127.0.0.1", 18197, timeout=3)
        h = self.headers(who)
        if last is not None:
            h["Last-Event-ID"] = last
        c.request("GET", "/api/advisor/runs/" + ids["run_id"] + "/events" + suffix, headers=h)
        r = c.getresponse()
        self.assertEqual(r.status, 200)
        self.assertIsNone(r.getheader("content-length"))
        self.assertIn("no-transform", r.getheader("cache-control") or "")
        self.assertEqual(r.getheader("x-content-type-options"), "nosniff")
        try:
            yield r
        finally:
            r.close()
            c.close()

    def frame(self, r):
        lines = []
        while True:
            line = r.readline()
            if not line:
                return None
            if line in (b"\n", b"\r\n"):
                if lines:
                    return b"".join(lines).decode()
            else:
                lines.append(line)

    def until_text(self, r):
        while True:
            frame = self.frame(r)
            assert frame is not None, "stream ended before text"
            if 'Text_appended' in frame:
                return frame

    def wait_state(self, key, value, timeout=2):
        state = {}
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            state = self.request("GET", "/state")[1]
            if state[key] == value:
                return state
            time.sleep(.02)
        self.fail(f"state {key} != {value}: {state}")

    def test_first_frame_before_release_reconnect_and_delete(self):
        ids = self.start()
        with self.events(ids) as r:
            first = self.until_text(r)
            self.assertIn('first', first)
            self.assertFalse(self.request("GET", "/state")[1]["released"])
            sequence = first.split("id: ")[1].splitlines()[0]
            disconnect_started = time.monotonic()
        self.wait_state("connections", 1)  # only the state probe remains
        self.assertLess(time.monotonic() - disconnect_started, .5)
        self.assertFalse(self.request("GET", "/state")[1]["producer_closed"])
        with self.events(ids, suffix="?after=" + sequence) as r:
            self.request("GET", "/release")
            second = self.until_text(r)
            self.assertIn(' second', second)
            self.assertNotEqual(first, second)
            terminal = []
            while (frame := self.frame(r)) is not None:
                terminal.append(frame)
            self.assertTrue(any("Run_completed" in frame for frame in terminal))
            self.assertEqual(r.read(), b"")  # valid chunked terminator, not truncated close
        self.wait_state("connections", 1)
        self.assertEqual(self.request("GET", "/state")[1]["drain_timeouts"], 0)
        with self.events(ids, last=sequence) as r:
            self.assertIn(' second', self.until_text(r))
        self.wait_state("producer_closed", True)
        self.request("GET", "/fixture/prune")
        with self.events(ids) as r:
            self.assertIn("event: resync_required", self.frame(r) or "")
            self.assertEqual(r.read(), b"")
        self.wait_state("connections", 1)
        self.assertEqual(self.request("GET", "/state")[1]["drain_timeouts"], 0)
        self.assertEqual(self.request("GET", "/health"), (200, {"status": "ok"}))
        self.assertEqual(self.request("POST", "/api/advisor/conversations/" + ids["conversation_id"] + "/delete", {})[0], 200)
        self.assertEqual(self.request("GET", "/api/advisor/conversations/" + ids["conversation_id"])[0], 404)

    def test_subscriber_b_expiry_closes_quiet_stream_not_origin_a(self):
        ids = self.start()
        with self.events(ids, who="b") as r:
            self.until_text(r)
            self.request("GET", "/fixture/expire-b")
            self.assertIsNone(self.frame(r))
        self.assertFalse(self.request("GET", "/state")[1]["producer_closed"])
        with self.events(ids) as r:
            self.until_text(r)
            self.request("GET", "/release")
            self.assertIn(' second', self.until_text(r))

    def test_origin_logout_cancels_while_b_subscribed(self):
        ids = self.start()
        with self.events(ids, who="b") as r:
            self.until_text(r)
            self.assertEqual(self.request("POST", "/auth/logout", {})[0], 200)
            while self.frame(r) is not None:
                pass
        self.wait_state("producer_closed", True)
        status, snapshot = self.request("GET", "/api/advisor/conversations/" + ids["conversation_id"], who="b")
        self.assertEqual(status, 200)
        self.assertEqual(snapshot["data"]["turns"][0]["attempts"][0]["state"]["type"], "Cancelled")

    def test_origin_expiry_with_b_subscribed(self):
        ids = self.start()
        with self.events(ids, who="b") as r:
            self.until_text(r)
            self.request("GET", "/fixture/expire-a")
            while self.frame(r) is not None:
                pass
        self.wait_state("producer_closed", True)
        self.assertEqual(self.request("GET", "/api/advisor/status")[0], 401)
        self.assertEqual(self.request("GET", "/api/advisor/conversations/" + ids["conversation_id"], who="b")[0], 200)

    def test_b_logout_does_not_rebind_or_cancel_a(self):
        ids = self.start()
        with self.events(ids, who="b") as r:
            self.until_text(r)
            self.assertEqual(self.request("POST", "/auth/logout", {}, who="b")[0], 200)
            self.assertIsNone(self.frame(r))
        self.assertFalse(self.request("GET", "/state")[1]["producer_closed"])
        with self.events(ids) as r:
            self.until_text(r)
            self.request("GET", "/release")
            self.assertIn(' second', self.until_text(r))

    def stalled_reader(self, revoke_origin):
        ids = self.start()
        client = socket.socket()
        try:
            client.setsockopt(socket.SOL_SOCKET, socket.SO_RCVBUF, 1024)
            client.settimeout(3)
            client.connect(("127.0.0.1", 18197))
            headers = self.headers("a" if revoke_origin else "b")
            wire = "GET /api/advisor/runs/" + ids["run_id"] + "/events HTTP/1.1\r\nHost: 127.0.0.1:18197\r\n"
            wire += "".join(k + ": " + v + "\r\n" for k, v in headers.items()) + "\r\n"
            client.sendall(wire.encode())
            state = self.request("GET", "/state")[1]
            self.assertIn("blocked_writes", state, "fixture must observe actual socket write suspension")
            self.wait_state("blocked_writes", 1)
            started = time.monotonic()
            who = "a" if revoke_origin else "b"
            self.assertEqual(self.request("POST", "/auth/logout", {}, who=who)[0], 200)
            self.wait_state("connections", 1)
            self.assertLess(time.monotonic() - started, .5, "cleanup must not await heartbeat/client draining")
            self.assertEqual(self.request("GET", "/state")[1]["producer_closed"], revoke_origin)
            if not revoke_origin:
                self.request("POST", "/api/advisor/runs/" + ids["run_id"] + "/cancel", {})
                self.wait_state("producer_closed", True)
        finally:
            client.close()

    def test_stalled_reader_authorized_terminal_timeout(self):
        ids = self.start()
        client = socket.socket()
        try:
            client.setsockopt(socket.SOL_SOCKET, socket.SO_RCVBUF, 1024)
            client.connect(("127.0.0.1", 18197))
            wire = "GET /api/advisor/runs/" + ids["run_id"] + "/events HTTP/1.1\r\nHost: 127.0.0.1:18197\r\n"
            wire += "".join(k + ": " + v + "\r\n" for k, v in self.headers("b").items()) + "\r\n"
            client.sendall(wire.encode())
            self.wait_state("blocked_writes", 1)
            started = time.monotonic()
            self.request("POST", "/auth/logout", {})
            self.wait_state("producer_closed", True)
            status, snapshot = self.request("GET", "/api/advisor/conversations/" + ids["conversation_id"], who="b")
            self.assertEqual(status, 200)
            self.assertEqual(snapshot["data"]["turns"][0]["attempts"][0]["state"]["type"], "Cancelled")
            self.assertEqual(self.request("GET", "/state")[1]["connections"], 2)
            self.wait_state("connections", 1, timeout=32)
            self.assertGreater(time.monotonic() - started, 29, "authorized history must not be classified as access loss")
            self.assertEqual(self.request("GET", "/api/advisor/status", who="b")[0], 200)
        finally:
            client.close()

    def test_stalled_reader_origin_revocation(self):
        self.stalled_reader(revoke_origin=True)

    def test_stalled_reader_subscriber_revocation(self):
        self.stalled_reader(revoke_origin=False)

    def test_delete_cancels_late_provider_and_delivery(self):
        ids = self.start()
        with self.events(ids) as r:
            self.until_text(r)
            self.assertEqual(self.request("POST", "/api/advisor/conversations/" + ids["conversation_id"] + "/delete", {})[0], 200)
            self.assertIsNone(self.frame(r))
        self.wait_state("producer_closed", True)
        self.request("GET", "/release")
        self.assertEqual(self.request("GET", "/api/advisor/conversations/" + ids["conversation_id"])[0], 404)


if __name__ == "__main__":
    unittest.main()
