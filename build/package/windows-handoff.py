#!/usr/bin/env python3
"""Export an exact frozen source/submodule snapshot into a Windows build kit."""
import argparse
import hashlib
import io
import json
from pathlib import Path, PurePosixPath
import shutil
import subprocess
import tarfile
import zipfile


def digest(path):
    result = hashlib.sha256()
    with path.open('rb') as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b''):
            result.update(chunk)
    return result.hexdigest()


def git(root, *args, binary=False):
    return subprocess.check_output(['git', '-C', str(root), *args], text=not binary)


def export(root, commit, destination):
    archive = git(root, 'archive', '--format=tar', commit, binary=True)
    with tarfile.open(fileobj=io.BytesIO(archive)) as tar:
        for member in tar.getmembers():
            path = PurePosixPath(member.name)
            if path.is_absolute() or '..' in path.parts or '\\' in member.name or ':' in member.name or not (member.isfile() or member.isdir()):
                raise ValueError('Unsafe source archive entry: ' + member.name)
        tar.extractall(destination)


def build(args):
    source_root = Path(args.source_root).resolve()
    commit = git(source_root, 'rev-parse', args.commit + '^{commit}').strip()
    if commit != args.commit:
        raise ValueError('Use the exact full frozen source commit')
    destination = Path(args.output).resolve()
    if destination.exists() or destination.with_suffix('.zip').exists():
        raise ValueError('Handoff outputs must be new; preserved outputs are never replaced')
    destination.mkdir(parents=True)
    source = destination / 'source'
    source.mkdir()
    export(source_root, commit, source)
    pins = {}
    for name in ('libs4', 'blindly4'):
        line = git(source_root, 'ls-tree', commit, 'submodules/' + name).strip().split()
        if len(line) != 4 or line[:2] != ['160000', 'commit']:
            raise ValueError('Expected exact submodule gitlink: ' + name)
        pins[name] = line[2]
        target = source / 'submodules' / name
        target.mkdir(parents=True, exist_ok=True)
        # Read immutable local objects; never fetch, checkout or mutate the source tree.
        export(source_root / 'submodules' / name, pins[name], target)
    for name in ('build/Build.hs', 'build/package/package.py', 'build/package/acquire-provider.py',
                 'build/package/codex-macos-compatibility.json', '.rspec', 'spec/onboarding_spec.rb',
                 'spec/support/WindowsProviderMain.hs', 'submodules/libs4/tools/codex-provider-lock.json'):
        if not (source / name).is_file():
            raise ValueError('Frozen source missing required validation input: ' + name)
    provenance = {'sourceCommit': commit, 'sourceDirty': False, 'submodules': pins}
    (source / 'build/source-provenance.json').write_text(json.dumps(provenance, indent=2) + '\n')
    provider = Path(args.provider).resolve()
    info = json.loads((provider / 'sapiens-provider.json').read_text())
    if info['platform'] != 'windows-x64' or info['version'] != '0.167.0' or len(info['files']) != 48:
        raise ValueError('Expected complete acquired pinned Windows provider')
    # Reuse the frozen production inventory verifier, not an alternate payload checker.
    verification = ('import importlib.util,json,pathlib,sys; '
                    'p=pathlib.Path(sys.argv[1]); s=importlib.util.spec_from_file_location("pkg",sys.argv[2]); '
                    'm=importlib.util.module_from_spec(s); s.loader.exec_module(m); '
                    'd=json.loads((p/"sapiens-provider.json").read_text()); '
                    'm.verify_inventory(p,d["files"],{"sapiens-provider.json"})')
    import sys
    subprocess.run([sys.executable, '-c', verification, str(provider), str(source / 'build/package/package.py')], check=True)
    target = destination / 'inputs/provider-package/runtime/codex'
    target.parent.mkdir(parents=True)
    shutil.copytree(provider, target)
    checks = destination / 'checks'
    checks.mkdir()
    probe = Path(args.lifecycle_probe).resolve()
    probe_manifest = json.loads((probe / 'MANIFEST.json').read_text())
    for relative, expected in probe_manifest['files_sha256'].items():
        if digest(probe / relative) != expected:
            raise ValueError('Existing lifecycle probe changed: ' + relative)
    shutil.copytree(probe, checks / 'lifecycle')
    shutil.copy2(args.d3_acceptance, checks / 'D3_WINDOWS_ACCEPTANCE.ps1')
    own = Path(__file__).with_name('windows-handoff')
    for name in ('Run-Windows-Verification.ps1', 'README.md'):
        shutil.copy2(own / name, destination / name)
    if args.readiness:
        shutil.copy2(args.readiness, destination / 'CI_SIGNING_READINESS.json')
    files = [{'path': p.relative_to(destination).as_posix(), 'size': p.stat().st_size, 'sha256': digest(p)}
             for p in sorted(destination.rglob('*')) if p.is_file()]
    manifest = {'schema': 1, 'kind': 'windows-source-build-verification', 'prebuiltSapiensBinary': False,
                'sourceCommit': commit, 'submodules': pins, 'applicationVersion': args.version,
                'provider': {'version': info['version'], 'upstreamCommit': info['upstreamCommit'],
                             'archiveSha256': info['archiveSha256'], 'acquiredFiles': 48, 'finalizedFiles': 49},
                'nativeWindowsExecution': 'NOT_RUN', 'installer': 'NOT_IMPLEMENTED/NOT_RUN',
                'd3AcceptanceSha256': digest(checks / 'D3_WINDOWS_ACCEPTANCE.ps1'), 'files': files}
    (destination / 'handoff-manifest.json').write_text(json.dumps(manifest, indent=2) + '\n')
    archive = destination.with_suffix('.zip')
    with zipfile.ZipFile(archive, 'x', compression=zipfile.ZIP_DEFLATED, compresslevel=6) as output:
        for path in sorted(destination.rglob('*')):
            if path.is_file():
                output.write(path, destination.name + '/' + path.relative_to(destination).as_posix())
    receipt = {'kind': manifest['kind'], 'sourceCommit': commit, 'submodules': pins,
               'prebuiltSapiensBinary': False, 'nativeWindowsExecution': 'NOT_RUN',
               'assetName': archive.name, 'size': archive.stat().st_size, 'sha256': digest(archive),
               'handoffManifestSha256': digest(destination / 'handoff-manifest.json')}
    archive.with_suffix('.receipt.json').write_text(json.dumps(receipt, indent=2) + '\n')
    print(json.dumps({'directory': str(destination), 'archive': str(archive), 'receipt': receipt}, indent=2))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('source-root', 'commit', 'provider', 'lifecycle-probe', 'd3-acceptance', 'output', 'version'):
        parser.add_argument('--' + name, required=True)
    parser.add_argument('--readiness')
    build(parser.parse_args())
