"""Exercise the real architecture rule's sandbox inputs and invalidation.

Each invocation owns a separate temporary Dune project/build directory; it never
builds the active application recursively or uses its build lock.
"""
import os
import pathlib
import shutil
import subprocess
import tempfile
import unittest


class LibraryBoundaryDuneInputs(unittest.TestCase):
    def test_new_shared_helper_and_edits_are_visible(self):
        source = pathlib.Path(__file__).resolve().parent
        rule_start = '(rule\n (alias check-library-boundaries)'
        rule_end = '\n(alias\n (name runtest)'
        rule = rule_start + (source / 'dune').read_text().split(rule_start, 1)[1].split(rule_end, 1)[0]
        with tempfile.TemporaryDirectory(prefix='cashflow-boundary-dune-') as directory:
            root = pathlib.Path(directory)
            def put(name, text):
                path = root / name
                path.parent.mkdir(parents=True, exist_ok=True)
                path.write_text(text)

            put('dune-project', '(lang dune 3.24)\n(name boundary_fixture)\n')
            put('test/dune', rule)
            for name in ('check_library_boundaries.py', 'test_check_library_boundaries.py'):
                shutil.copyfile(source / name, root / 'test' / name)
            for owner, deps in [('core', ''), ('workflow', 'cashflow_core'),
                                ('io', 'cashflow_core cashflow_workflow')]:
                put(f'lib/{owner}/dune',
                    f'(library (name cashflow_{owner}) (libraries {deps}))\n')
            for name in ('bin/dune', 'shared/mf2/dune', 'tools/mf2c/dune', 'locales/dune'):
                put(name, '')
            put('web/dune', '(executable (name main) (libraries boundary_helper))\n')
            put('web/main.ml', 'let () = ()\n')
            command = ['dune', 'build', '--root', str(root), '--build-dir', str(root / '_build'),
                       '--sandbox=copy', '@test/check-library-boundaries']
            environment = os.environ.copy()
            # The root and build directory above, not an enclosing Dune action's
            # project, own every fixture invocation.
            environment.pop('DUNE_SOURCEROOT', None)

            def build(phase, forbidden):
                result = subprocess.run(command, cwd=root, env=environment,
                                        text=True, stdout=subprocess.PIPE, stderr=subprocess.STDOUT)
                print(f'{phase}: {command!r}\nexit={result.returncode}\n{result.stdout}', flush=True)
                if forbidden:
                    self.assertNotEqual(result.returncode, 0, f'{phase}: new input was invisible')
                    self.assertIn('browser/MF2 native dependency main -> boundary_helper -> cashflow_core',
                                  result.stdout)
                else:
                    self.assertEqual(result.returncode, 0, result.stdout)
                    self.assertIn('Library boundaries OK', result.stdout)

            build('initial graph', forbidden=False)
            put('shared/helper/dune', '(library (name boundary_helper) (libraries cashflow_core))\n')
            put('shared/helper/boundary_helper.ml', 'let value = 1\n')
            build('new nested helper', forbidden=True)
            put('shared/helper/dune', '(library (name boundary_helper))\n')
            build('helper repaired', forbidden=False)
            put('shared/helper/dune', '(library (name boundary_helper) (libraries cashflow_core))\n')
            build('helper edited without force', forbidden=True)


if __name__ == '__main__':
    unittest.main(verbosity=2)
