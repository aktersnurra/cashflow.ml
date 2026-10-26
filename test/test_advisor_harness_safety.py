"""Fixture-only filesystem and actual runner-function cleanup regressions."""
import json
import os
import pathlib
import signal
import socket
import sqlite3
import subprocess
import tempfile
import time
import unittest
import urllib.request

ROOT = pathlib.Path(__file__).resolve().parents[1]
BIN = ROOT / '_build/default/test/advisor_fixture_server.exe'
RUNNER = ROOT / 'web/e2e/run_advisor.sh'


class AdvisorHarnessSafety(unittest.TestCase):
    def refused(self, path):
        result = subprocess.run([str(BIN), str(ROOT / '_build/web/web'), '--product',
                                 '--privacy-mode', 'restart', '--database', str(path)],
                                capture_output=True, timeout=10)
        self.assertNotEqual(result.returncode, 0, result.stderr.decode())

    def test_dangling_database_symlink(self):
        with tempfile.TemporaryDirectory(prefix='advisor-path-safety-') as directory:
            root = pathlib.Path(directory)
            target = root / 'outside' / 'target.sqlite3'
            target.parent.mkdir()
            path = root / 'fixture.sqlite3'
            path.symlink_to(target)
            self.refused(path)
            self.assertTrue(path.is_symlink())
            self.assertFalse(os.path.lexists(target), 'dangling symlink target must not be created')
            self.assertFalse(os.path.lexists(str(path) + '.advisor-fixture'), 'marker must not be created')

    def test_existing_unowned_paths_unchanged(self):
        for kind in ('unmarked', 'mismarked', 'database-directory', 'marker-directory',
                     'marker-symlink', 'dangling-marker-symlink', 'orphan-marker',
                     'orphan-marker-symlink', 'orphan-dangling-marker-symlink'):
            with self.subTest(kind=kind), tempfile.TemporaryDirectory(prefix='advisor-path-safety-') as directory:
                root = pathlib.Path(directory)
                path = root / 'fixture.sqlite3'
                marker = pathlib.Path(str(path) + '.advisor-fixture')
                target = root / 'marker-target'
                if kind == 'database-directory':
                    path.mkdir()
                elif not kind.startswith('orphan-'):
                    if kind == 'marker-symlink':
                        with sqlite3.connect(path) as db:
                            db.execute('CREATE TABLE sentinel (value TEXT)')
                    else:
                        path.write_bytes(b'not a fixture database')
                if kind in ('mismarked', 'orphan-marker'):
                    marker.write_text('not this fixture')
                elif kind == 'marker-directory':
                    marker.mkdir()
                elif kind in ('marker-symlink', 'dangling-marker-symlink',
                              'orphan-marker-symlink', 'orphan-dangling-marker-symlink'):
                    if kind in ('marker-symlink', 'orphan-marker-symlink'):
                        target.write_text('cashflow synthetic advisor fixture\n' + str(path))
                    marker.symlink_to(target)
                before = {p.name: (p.lstat().st_mode, os.readlink(p) if p.is_symlink() else
                           p.read_bytes() if p.is_file() else None) for p in root.iterdir()}
                self.refused(path)
                after = {p.name: (p.lstat().st_mode, os.readlink(p) if p.is_symlink() else
                          p.read_bytes() if p.is_file() else None) for p in root.iterdir()}
                self.assertEqual(after, before)

    def test_new_database_and_marked_restart(self):
        with tempfile.TemporaryDirectory(prefix='advisor-path-safety-') as directory:
            path = pathlib.Path(directory) / 'fixture.sqlite3'
            digests = []
            for _ in range(2):
                server = subprocess.Popen([str(BIN), str(ROOT / '_build/web/web'), '--product',
                                           '--privacy-mode', 'restart', '--database', str(path)],
                                          stdout=subprocess.DEVNULL, stderr=subprocess.PIPE)
                try:
                    for _ in range(100):
                        if server.poll() is not None:
                            self.fail('fixture exited before readiness')
                        try:
                            with urllib.request.urlopen('http://127.0.0.1:18197/state', timeout=1) as response:
                                state = json.load(response)
                            break
                        except OSError:
                            time.sleep(.02)
                    else:
                        self.fail('fixture did not become ready')
                    self.assertEqual(state['provider_starts'], 0)
                    digests.append(state['financial_digest'])
                finally:
                    server.terminate()
                    _, error = server.communicate(timeout=5)
                    self.assertEqual(error, b'')
            self.assertEqual(digests[0], digests[1], 'restart must not reseed financial state')
            marker = pathlib.Path(str(path) + '.advisor-fixture')
            self.assertEqual(marker.read_text(), 'cashflow synthetic advisor fixture\n' + str(path))
            self.assertEqual(path.stat().st_mode & 0o777, 0o600)
            self.assertEqual(marker.stat().st_mode & 0o777, 0o600)

    @staticmethod
    def live(pid):
        try:
            # Zombies are dead, but may await adoption/reaping by this host's init.
            return pathlib.Path(f'/proc/{pid}/stat').read_text().split(') ', 1)[1][0] != 'Z'
        except FileNotFoundError:
            return False

    def test_exited_leader_descendants(self):
        # Exercise the exact run()/cleanup definitions, without introducing a second
        # implementation or a production runner command interface.
        source = RUNNER.read_text()
        seam = 'run native-fixture-build '
        self.assertEqual(source.count(seam), 1)
        prefix = source.split(seam, 1)[0]
        for status in (0, 7):
            with self.subTest(status=status), tempfile.TemporaryDirectory(prefix='advisor-group-safety-') as directory:
                root = pathlib.Path(directory)
                pidfile = root / 'descendant.pid'
                workfile = root / 'work.path'
                program = root / 'leader.py'
                program.write_text('''import json, os, signal, socket, sys, time
ready_r, ready_w = os.pipe()
pid = os.fork()
if pid == 0:
    os.close(ready_r)
    signal.signal(signal.SIGTERM, signal.SIG_IGN)
    listener = socket.socket()
    listener.bind(('127.0.0.1', 0))
    listener.listen()
    open(sys.argv[1], 'w').write(json.dumps({'pid': os.getpid(), 'port': listener.getsockname()[1]}))
    os.write(ready_w, b'1')
    os.close(ready_w)
    while True: time.sleep(1)
os.close(ready_w)
os.read(ready_r, 1)
os.close(ready_r)
sys.exit(int(sys.argv[2]))
''')
                script = prefix + '\nprintf "%s" "$WORK" > "$WORK_RECORD"\nrun descendant python3 "$LEADER" "$PIDFILE" "$LEADER_STATUS"\n'
                env = dict(os.environ, ADVISOR_EVIDENCE_DIR=str(root / 'evidence'),
                           WORK_RECORD=str(workfile), LEADER=str(program), PIDFILE=str(pidfile),
                           LEADER_STATUS=str(status))
                pid = None
                try:
                    result = subprocess.run(['bash', '-c', script, str(RUNNER)], cwd=ROOT, env=env,
                                            capture_output=True, timeout=18)
                    self.assertEqual(result.returncode, status, result.stderr.decode())
                    descendant = json.loads(pidfile.read_text())
                    pid = descendant['pid']
                    self.assertFalse(self.live(pid), 'TERM-ignoring descendant survived leader exit')
                    with socket.socket() as probe:
                        probe.settimeout(1)
                        self.assertNotEqual(probe.connect_ex(('127.0.0.1', descendant['port'])), 0,
                                            'descendant listener survived leader exit')
                    self.assertFalse(pathlib.Path(workfile.read_text()).exists(), 'runner temp directory survived')
                    self.assertIn(f'descendant exit={status}', (root / 'evidence/exits.txt').read_text())
                finally:
                    if pid is None and pidfile.exists():
                        pid = json.loads(pidfile.read_text())['pid']
                    if pid is not None and self.live(pid):
                        os.kill(pid, signal.SIGKILL)


if __name__ == '__main__':
    unittest.main()
