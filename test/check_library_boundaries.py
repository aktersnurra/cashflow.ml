#!/usr/bin/env python3
"""Check declared native boundaries and ocamldep's real source dependency closure.

This checks module dependencies, not purity of caller-injected callbacks.
Run from the project root (or pass an owned fixture root).
"""
import pathlib
import re
import subprocess
import sys


def sexps(text):
    tokens = iter(re.findall(r'"(?:\\.|[^"\\])*"|[()]|[^\s();]+', re.sub(r';[^\n]*', '', text)))
    def forms(nested=False):
        result = []
        for lexeme in tokens:
            if lexeme == '(':
                result.append(forms(True))
            elif lexeme == ')':
                return result
            else:
                result.append(lexeme.strip('"'))
        if nested:
            raise ValueError('unterminated Dune form')
        return result
    return forms()


def check(root):
    errors = []
    expected = {'core': 'cashflow_core', 'workflow': 'cashflow_workflow', 'io': 'cashflow_io'}
    allowed = {'core': {'base', 'ptime', 'uuidm', 'zipc', 'xmlm', 'sexplib0'},
               'workflow': {'cashflow_core', 'base', 'ptime'}}
    declarations = {}
    project_edges = {}
    independent = set()
    dune_files = sorted((root / 'lib').rglob('dune'))
    for directory in ('bin', 'test', 'shared', 'tools', 'locales', 'web'):
        dune_files.extend(sorted((root / directory).rglob('dune')))
    for dune in dune_files:
        for form in sexps(dune.read_text()):
            if not form or form[0] not in ('library', 'executable', 'test', 'tests'):
                continue
            fields = {f[0]: f[1:] for f in form[1:] if isinstance(f, list)}
            names = fields.get('name', fields.get('names', []))
            deps = fields.get('libraries', [])
            for name in names:
                project_edges.setdefault(name, set()).update(deps)
                if dune.relative_to(root).parts[0] in {'shared', 'tools', 'locales', 'web'}:
                    independent.add(name)
            if 'cashflow' in deps:
                errors.append(f'old flat library dependency: {dune.relative_to(root)}')
    for start in sorted(independent):
        pending = [(start, [start])]
        visited = set()
        while pending:
            name, path = pending.pop()
            if name in visited:
                continue
            visited.add(name)
            for dep in sorted(project_edges.get(name, ())):
                if dep in expected.values():
                    errors.append('browser/MF2 native dependency ' + ' -> '.join(path + [dep]))
                elif dep in project_edges:
                    pending.append((dep, path + [dep]))
    for dune in sorted((root / 'lib').rglob('dune')):
        for form in sexps(dune.read_text()):
            if not form or form[0] != 'library':
                continue
            fields = {f[0]: f[1:] for f in form[1:] if isinstance(f, list)}
            name = fields.get('name', [''])[0]
            declarations[name] = (dune, fields)
            owner = dune.parent.name
            if expected.get(owner) != name:
                errors.append(f'unallocated library {name}: {dune.relative_to(root)}')
            if owner in allowed:
                deps = set(fields.get('libraries', []))
                for dep in sorted(deps - allowed[owner]):
                    errors.append(f'forbidden declared edge {name} -> {dep}')
            if fields.get('wrapped', ['true']) != ['true']:
                errors.append(f'{name} must be wrapped')
    for name in expected.values():
        if name not in declarations:
            errors.append(f'missing wrapped library {name}')
    if 'cashflow_io' in declarations:
        deps = declarations['cashflow_io'][1].get('libraries', [])
        for name in ('cashflow_core', 'cashflow_workflow'):
            if name not in deps:
                errors.append(f'cashflow_io missing dependency {name}')

    owners = {}
    sources = {}
    for source in sorted((root / 'lib').rglob('*')):
        if source.suffix not in ('.ml', '.mli'):
            continue
        relative = source.relative_to(root / 'lib')
        if len(relative.parts) != 2 or relative.parts[0] not in expected:
            errors.append(f'unallocated source {source.relative_to(root)}')
            continue
        module = source.stem[0].upper() + source.stem[1:]
        owners[module] = relative.parts[0]
        sources.setdefault(module, []).append(source)
    # The portable protocol has its own source closure, including interfaces.
    for source in sorted((root / 'shared' / 'advisor').rglob('*')):
        if source.suffix not in ('.ml', '.mli'):
            continue
        module = source.stem[0].upper() + source.stem[1:]
        if module in owners and owners[module] != 'protocol':
            errors.append(f'ambiguous protocol module {module}')
            continue
        owners[module] = 'protocol'
        sources.setdefault(module, []).append(source)
    edges = {}
    for module, files in sources.items():
        output = subprocess.check_output(['ocamldep', '-modules', *map(str, files)], text=True)
        edges[module] = set()
        for line in output.splitlines():
            edges[module].update(line.split(':', 1)[1].split())
    namespaces = {'Cashflow_core': 'core', 'Cashflow_workflow': 'workflow', 'Cashflow_io': 'io',
                  'Cashflow_advisor_protocol': 'protocol'}
    concrete = {'Sqlite3', 'Eio', 'Eio_main', 'Eio_posix'}
    clock_json = {'Ptime_clock', 'Unix', 'Yojson', 'Json'}
    for start, owner in sorted(owners.items()):
        if owner == 'io':
            continue
        pending = [(start, [start])]
        visited = set()
        while pending:
            module, path = pending.pop()
            if module in visited:
                continue
            visited.add(module)
            for dep in sorted(edges.get(module, ())):
                target = namespaces.get(dep, owners.get(dep))
                forbidden = (dep in concrete or dep.startswith('Eio_') or
                             (owner == 'core' and (dep in clock_json or target in {'workflow', 'io', 'protocol'})) or
                             (owner == 'workflow' and target == 'io') or
                             (owner == 'protocol' and
                              (target in {'core', 'workflow', 'io'} or dep in {'Unix', 'Ptime_clock', 'Async', 'Async_js'})))
                if forbidden:
                    errors.append('forbidden source closure ' + ' -> '.join(path + [dep]))
                elif dep in edges:
                    pending.append((dep, path + [dep]))
    if errors:
        print('\n'.join(errors), file=sys.stderr)
        return 1
    print('Library boundaries OK (declared graph and ocamldep source closure; callback purity not proven)')
    return 0


if __name__ == '__main__':
    sys.exit(check(pathlib.Path(sys.argv[1] if len(sys.argv) > 1 else '.').resolve()))
