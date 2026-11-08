#!/usr/bin/env python3
"""Assemble an immutable native runtime and record build-input provenance."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import shutil
import subprocess


def digest(path):
    with path.open('rb') as stream:
        result = hashlib.sha256()
        for chunk in iter(lambda: stream.read(1024 * 1024), b''):
            result.update(chunk)
        return result.hexdigest()


def canonical(value):
    return json.dumps(value, sort_keys=True, separators=(',', ':')).encode()


def command(root, *args):
    return subprocess.check_output(args, cwd=root, text=True).strip()


def source_dirty(root):
    return bool(command(root, 'git', 'status', '--porcelain', '--untracked-files=normal',
                        '--ignore-submodules=untracked', '--', 'app', 'build', 'corpora', 'remote',
                        'environments', 'license', 'docs', 'spec', '.github', 'submodules',
                        'README.md', '.gitmodules', '.gitignore'))


def inventory(root, excluded):
    entries = sorted(root.rglob('*'))
    if any(p.is_symlink() for p in entries):
        raise ValueError('Package payload may not contain symbolic links')
    return [{'path': p.relative_to(root).as_posix(), 'size': p.stat().st_size, 'sha256': digest(p)}
            for p in entries if p.is_file() and p.relative_to(root).as_posix() not in excluded]


def verify_inventory(root, items, excluded):
    paths = [item['path'] for item in items]
    if len(paths) != len(set(paths)):
        raise ValueError('Duplicate inventory paths')
    for relative in paths:
        path = Path(relative)
        if path.is_absolute() or '..' in path.parts:
            raise ValueError('Inventory path escapes payload')
    if inventory(root, excluded) != sorted(items, key=lambda item: item['path']):
        raise ValueError('Inventory differs from final package bytes')


def finalize(args):
    runtime = Path(args.runtime).resolve()
    manifest_path = runtime / 'package-manifest.json'
    manifest = json.loads(manifest_path.read_text())
    provider = runtime / 'codex'
    if manifest['provider']:
        path = provider / 'sapiens-provider.json'
        upstream = provider / 'sapiens-provider-upstream.json'
        if upstream.exists():
            raise ValueError('Already finalized; assemble a fresh package before signing again')
        shutil.copy2(path, upstream)
        info = json.loads(path.read_text())
        info['upstreamInventory'] = upstream.name
        info['inventoryStage'] = 'packaged' if args.signing == 'none' else 'signed-package'
        info['files'] = inventory(provider, {'sapiens-provider.json'})
        path.write_text(json.dumps(info, indent=2) + '\n')
        manifest['providerInventorySha256'] = digest(path)
    manifest['signing'] = args.signing
    manifest_path.write_text(json.dumps(manifest, indent=2) + '\n')
    (runtime / 'package-inventory.json').write_text(json.dumps(
        {'schemaVersion': 1, 'files': inventory(runtime, {'package-inventory.json'})}, indent=2) + '\n')
    verify(args)


def verify(args):
    runtime = Path(args.runtime).resolve()
    manifest = json.loads((runtime / 'package-manifest.json').read_text())
    record = json.loads((runtime / 'package-inventory.json').read_text())
    if record['schemaVersion'] != 1:
        raise ValueError('Unsupported package inventory')
    verify_inventory(runtime, record['files'], {'package-inventory.json'})
    if manifest['provider']:
        provider = runtime / 'codex'
        path = provider / 'sapiens-provider.json'
        info = json.loads(path.read_text())
        if digest(path) != manifest['providerInventorySha256'] or info['version'] != manifest['provider']['version']:
            raise ValueError('Provider inventory differs from package manifest')
        verify_inventory(provider, info['files'], {'sapiens-provider.json'})
    print(json.dumps({'runtime': str(runtime), 'inventory': 'verified', 'files': len(record['files'])}))


def stage(args):
    root = Path(args.root).resolve()
    destination = Path(args.destination).resolve()
    if destination.exists():
        raise ValueError('Runtime destination must be new; refusing to mix old and new payloads')
    if root == destination or root.is_relative_to(destination):
        raise ValueError('Runtime destination must not contain the source checkout')
    destination.mkdir(parents=True)
    expected_arch = 'x64' if args.platform == 'windows' else 'arm64'
    if args.architecture != expected_arch:
        raise ValueError('Supported primary targets: macOS arm64 and Windows x64')
    suffix = '.exe' if args.platform == 'windows' else ''
    files = ['.build/bin/sapiens4' + suffix, '.build/web/app.js', '.build/web/guest.js', '.build/web/sw.js',
             'submodules/libs4/web/remote-vendor.js']
    files += [str(p.relative_to(root)) for folder in ('app/theme', 'app/shell', 'corpora/prompts')
              for p in (root / folder).rglob('*') if p.is_file() and p.suffix not in ('.purs',)]
    files += [str(p.relative_to(root)) for folder in ('license', 'submodules/libs4/license', 'submodules/blindly4/license')
              for p in (root / folder).rglob('*') if p.is_file()]
    blindly = 'submodules/blindly4/.build/' + ('windows/blindly4.exe' if suffix else 'release/blindly4')
    files += [blindly]
    for relative in sorted(set(files)):
        source = root / relative
        target = destination / relative
        target.parent.mkdir(parents=True, exist_ok=True)
        shutil.copy2(source, target)
    provider = Path(args.provider).resolve() if args.provider else root / 'runtime/codex'
    provider_info = None
    if provider.exists():
        info = json.loads((provider / 'sapiens-provider.json').read_text())
        expected = 'windows-x64' if suffix else 'darwin-arm64'
        if info['schema'] != 1 or info['platform'] != expected:
            raise ValueError('Provider metadata schema/platform does not match package')
        entry = Path(info['entrypoint'])
        if entry.is_absolute() or '..' in entry.parts:
            raise ValueError('Provider entrypoint must stay inside codex/')
        for item in info['files']:
            path = Path(item['path'])
            if path.is_absolute() or '..' in path.parts:
                raise ValueError('Provider file path escapes payload')
            source = provider / path
            if source.is_symlink() or not source.is_file() or source.stat().st_size != item['size'] or digest(source) != item['sha256']:
                raise ValueError('Provider content does not match acquisition inventory: ' + str(path))
        entries = list(provider.rglob('*'))
        if any(p.is_symlink() for p in entries):
            raise ValueError('Provider payload may not contain symbolic links')
        actual = {p.relative_to(provider).as_posix() for p in entries if p.is_file()}
        expected_files = {item['path'] for item in info['files']} | {'sapiens-provider.json'}
        if actual != expected_files:
            raise ValueError('Provider inventory does not cover the complete payload')
        shutil.copytree(provider, destination / 'codex', symlinks=False)
        if not (destination / 'codex' / entry).is_file():
            raise ValueError('Provider entrypoint is missing')
        provider_info = {'name': 'codex', 'version': info['version'], 'entrypoint': 'codex/' + entry.as_posix(),
                         'archiveSha256': info['archiveSha256']}
    elif not args.allow_missing_provider:
        raise ValueError('Complete pinned Codex payload required; run acquire-provider.py first')
    # No mutable shared caches or absolute source paths are part of the payload.
    if args.platform == 'macos':
        library_root = root / 'submodules/libs4/haskell/.runtime'
        if library_root.exists():
            shutil.copytree(library_root, destination / 'submodules/libs4/haskell/.runtime')
    else:
        for icon in ('AppIcon.ico', 'AppIconDark.ico'):
            target = destination / 'environments/windows' / icon
            target.parent.mkdir(parents=True, exist_ok=True)
            shutil.copy2(root / 'environments/windows' / icon, target)
    if (root / '.git').exists():
        provenance = {'sourceCommit': command(root, 'git', 'rev-parse', 'HEAD'),
                      'submodules': {name: command(root, 'git', '-C', 'submodules/' + name, 'rev-parse', 'HEAD') for name in ('libs4', 'blindly4')},
                      'sourceDirty': source_dirty(root)}
        tracked = command(root, 'git', 'ls-files', '--cached', '--others', '--exclude-standard', '-z').split('\0')
    else:
        provenance = json.loads((root / 'build/source-provenance.json').read_text())
        tracked = [p.relative_to(root).as_posix() for name in ('app', 'build', 'corpora', 'remote', 'environments', 'license', 'docs') for p in (root / name).rglob('*') if p.is_file()]
    tree = hashlib.sha256()
    source_paths = [p for p in filter(None, tracked) if p.startswith(('app/', 'build/', 'corpora/', 'remote/', 'environments/', 'license/', 'docs/', '.github/')) or p in ('README.md', '.gitmodules', '.gitignore')]
    for relative in sorted(source_paths):
        if relative.endswith(('.pyc', '.pyo')):
            continue
        path = root / relative
        if path.is_file():
            tree.update(relative.encode() + b'\0' + bytes.fromhex(digest(path)))
    inputs = {**provenance, 'sourceTreeSha256': tree.hexdigest(),
              'platform': args.platform, 'architecture': args.architecture, 'version': args.version,
              'repository': 'PoulWalcker/sapiens4', 'channel': 'development', 'provider': provider_info,
              'minimumOS': args.minimum_os, 'supportStatus': args.support_status,
              'toolchain': {name: command(root, *cmd).splitlines()[0] for name, cmd in
                            {'ghc': ['ghc', '--numeric-version'], 'cabal': ['cabal', '--numeric-version'],
                             'rustc': ['rustc', '--version'], 'node': ['node', '--version']}.items()}}
    manifest = {'schemaVersion': 1, 'appId': 'com.poulwalcker.sapiens4.development', 'version': args.version,
                'buildId': hashlib.sha256(canonical(inputs)).hexdigest(), **inputs,
                'packageStatus': 'provider-complete' if provider_info else 'validation-provider-incomplete',
                'dataEpoch': {'minReadable': None, 'maxReadable': None, 'target': None},
                'migrationSetId': None, 'legacyAdoption': 'not-implemented', 'updateDiscovery': False}
    Path(args.manifest).write_text(json.dumps(manifest, indent=2) + '\n')
    print(json.dumps({'runtime': str(destination), 'manifest': str(args.manifest), 'buildId': manifest['buildId'], 'status': manifest['packageStatus']}))


def receipt(args):
    artifact, manifest = Path(args.artifact), Path(args.manifest)
    package = json.loads(manifest.read_text())
    record = {'schemaVersion': 1, 'appId': package['appId'], 'version': package['version'],
              'repository': package['repository'], 'channel': package['channel'],
              'sourceCommit': package['sourceCommit'], 'sourceDirty': package['sourceDirty'],
              'packageStatus': package['packageStatus'], 'artifacts': [{
                  'platform': package['platform'], 'architecture': package['architecture'], 'buildId': package['buildId'],
                  'minimumOS': package['minimumOS'], 'supportStatus': package['supportStatus'], 'signing': package['signing'],
                  'assetName': artifact.name, 'size': artifact.stat().st_size, 'sha256': digest(artifact),
                  'packageManifestSha256': digest(manifest)}]}
    Path(args.output).write_text(json.dumps(record, indent=2) + '\n')


def provenance(args):
    root = Path(__file__).resolve().parents[2]
    record = {'sourceCommit': command(root, 'git', 'rev-parse', 'HEAD'),
              'sourceDirty': source_dirty(root),
              'submodules': {name: command(root, 'git', '-C', 'submodules/' + name, 'rev-parse', 'HEAD') for name in ('libs4', 'blindly4')}}
    Path(args.output).write_text(json.dumps(record, indent=2) + '\n')


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    commands = parser.add_subparsers(dest='command', required=True)
    archive = commands.add_parser('provenance')
    archive.add_argument('--output', required=True)
    archive.set_defaults(run=provenance)
    assemble = commands.add_parser('stage')
    for name in ('root', 'destination', 'manifest', 'version', 'architecture'):
        assemble.add_argument('--' + name, required=True)
    assemble.add_argument('--platform', choices=('macos', 'windows'), required=True)
    assemble.add_argument('--provider')
    assemble.add_argument('--minimum-os')
    assemble.add_argument('--support-status', choices=('declared-target', 'experimental-minimum-os'), default='declared-target')
    assemble.add_argument('--allow-missing-provider', action='store_true', help='Native validation only; cannot create user-ready release')
    assemble.set_defaults(run=stage)
    record = commands.add_parser('receipt')
    for name in ('artifact', 'manifest', 'output'):
        record.add_argument('--' + name, required=True)
    record.set_defaults(run=receipt)
    for name, action in [('finalize', finalize), ('verify', verify)]:
        operation = commands.add_parser(name)
        operation.add_argument('--runtime', required=True)
        if name == 'finalize':
            operation.add_argument('--signing', choices=('none', 'ad-hoc', 'developer-id', 'authenticode'), required=True)
        operation.set_defaults(run=action)
    options = parser.parse_args()
    options.run(options)
