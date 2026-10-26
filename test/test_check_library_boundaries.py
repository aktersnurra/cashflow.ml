"""Owned negative projects exercise the checker without changing application sources."""
import contextlib
import io
import pathlib
import tempfile
import unittest

from check_library_boundaries import check


class LibraryBoundaries(unittest.TestCase):
    def setUp(self):
        self.temp = tempfile.TemporaryDirectory(prefix='cashflow-boundaries-')
        self.addCleanup(self.temp.cleanup)
        self.root = pathlib.Path(self.temp.name)
        for owner, deps in [('core', 'base ptime'), ('workflow', 'cashflow_core base ptime'),
                            ('io', 'cashflow_core cashflow_workflow sqlite3 eio')]:
            self.put(f'lib/{owner}/dune',
                     f'(library (name cashflow_{owner}) (wrapped true) (libraries {deps}))')
        self.put('lib/core/value.ml', 'let value = 1\n')
        self.put('lib/workflow/report.ml', 'let value = Cashflow_core.Value.value\n')
        self.put('lib/io/adapter.ml', 'let value = Cashflow_workflow.Report.value\n')

    def put(self, name, text):
        path = self.root / name
        path.parent.mkdir(parents=True, exist_ok=True)
        path.write_text(text)

    def result(self):
        output = io.StringIO()
        with contextlib.redirect_stderr(output), contextlib.redirect_stdout(output):
            status = check(self.root)
        return status, output.getvalue()

    def rejects(self, diagnostic):
        status, output = self.result()
        self.assertEqual(status, 1, output)
        self.assertIn(diagnostic, output)
        print(diagnostic)

    def test_valid_graph(self):
        status, output = self.result()
        self.assertEqual(status, 0, output)

    def test_declared_edge(self):
        self.put('lib/core/dune', '(library (name cashflow_core) (libraries cashflow_io))')
        self.rejects('forbidden declared edge cashflow_core -> cashflow_io')

    def test_indirect_source_closure(self):
        self.put('lib/core/value.ml', 'let value = Helper.value\n')
        self.put('lib/core/helper.ml', 'let value = Adapter.value\n')
        self.rejects('forbidden source closure Value -> Helper -> Adapter')

    def test_interface_edge(self):
        self.put('lib/core/value.mli', 'val db : Sqlite3.db\n')
        self.rejects('forbidden source closure Value -> Sqlite3')

    def test_core_workflow_edge(self):
        self.put('lib/core/value.ml', 'let value = Cashflow_workflow.Report.value\n')
        self.rejects('forbidden source closure Value -> Cashflow_workflow')

    def test_core_clock_edge(self):
        self.put('lib/core/value.ml', 'let now = Ptime_clock.now\n')
        self.rejects('forbidden source closure Value -> Ptime_clock')

    def test_core_json_edge(self):
        self.put('lib/core/value.ml', 'let value = Yojson.Safe.from_string "null"\n')
        self.rejects('forbidden source closure Value -> Yojson')

    def test_workflow_io_edge(self):
        self.put('lib/workflow/report.ml', 'let value = Cashflow_io.Adapter.value\n')
        self.rejects('forbidden source closure Report -> Cashflow_io')

    def test_workflow_eio_edge(self):
        self.put('lib/workflow/report.ml', 'let value = Eio.Fiber.yield\n')
        self.rejects('forbidden source closure Report -> Eio')

    def test_workflow_declared_sqlite(self):
        self.put('lib/workflow/dune', '(library (name cashflow_workflow) (libraries sqlite3))')
        self.rejects('forbidden declared edge cashflow_workflow -> sqlite3')

    def test_browser_transitive_native_edge(self):
        self.put('web/dune', '(executable (name main) (libraries helper))')
        self.put('shared/dune', '(library (name helper) (libraries cashflow_core))')
        self.rejects('browser/MF2 native dependency main -> helper -> cashflow_core')

    def test_protocol_declared_native_edge(self):
        self.put('shared/advisor/dune', '(library (name cashflow_advisor_protocol) (libraries cashflow_io))')
        self.rejects('browser/MF2 native dependency cashflow_advisor_protocol -> cashflow_io')

    def test_protocol_transitive_declared_native_edge(self):
        self.put('shared/advisor/dune', '(library (name cashflow_advisor_protocol) (libraries helper))')
        self.put('test/helper/dune', '(library (name helper) (libraries cashflow_workflow))')
        self.rejects('browser/MF2 native dependency cashflow_advisor_protocol -> helper -> cashflow_workflow')

    def test_protocol_direct_native_source_edge(self):
        self.put('shared/advisor/wire.ml', 'let value = Cashflow_core.Value.value\n')
        self.rejects('forbidden source closure Wire -> Cashflow_core')

    def test_protocol_transitive_native_source_edge(self):
        self.put('shared/advisor/wire.ml', 'let value = Protocol_helper.value\n')
        self.put('shared/advisor/protocol_helper.ml', 'let value = Cashflow_io.Adapter.value\n')
        self.rejects('forbidden source closure Wire -> Protocol_helper -> Cashflow_io')

    def test_protocol_interface_io_edge(self):
        self.put('shared/advisor/wire.mli', 'val db : Sqlite3.db\n')
        self.rejects('forbidden source closure Wire -> Sqlite3')

    def test_core_protocol_source_edge(self):
        self.put('lib/core/value.ml', 'let value = Cashflow_advisor_protocol.Wire.version\n')
        self.rejects('forbidden source closure Value -> Cashflow_advisor_protocol')

    def test_old_consumer(self):
        self.put('bin/dune', '(executable (name cashflow) (libraries cashflow))')
        self.rejects('old flat library dependency: bin/dune')

    def test_unallocated_flat_implementation(self):
        self.put('lib/escaped.ml', 'let value = 1\n')
        self.rejects('unallocated source lib/escaped.ml')


if __name__ == '__main__':
    unittest.main(verbosity=2)
