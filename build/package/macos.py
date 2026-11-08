#!/usr/bin/env python3
"""Build a relocatable development app, then sign its complete payload."""
import argparse
import json
import os
from pathlib import Path
import plistlib
import re
import shutil
import subprocess
import sys
import tempfile

MAGIC = (b'\xfe\xed\xfa\xce', b'\xce\xfa\xed\xfe', b'\xfe\xed\xfa\xcf', b'\xcf\xfa\xed\xfe', b'\xca\xfe\xba\xbe')


def run(*args):
    subprocess.run([str(a) for a in args], check=True)


def macho(path):
    if not path.is_file() or path.is_symlink():
        return False
    with path.open('rb') as stream:
        return stream.read(4) in MAGIC


def relocate(app, root):
    frameworks = app / 'Contents/Frameworks'
    pending = [p for p in app.rglob('*') if macho(p)]
    seen = set()
    while pending:
        binary = pending.pop()
        if binary in seen:
            continue
        seen.add(binary)
        output = subprocess.check_output(['otool', '-L', str(binary)], text=True)
        for line in output.splitlines()[1:]:
            dependency = line.strip().split(' (')[0]
            if dependency.startswith(('/usr/lib/', '/System/Library/')):
                continue
            name = Path(dependency).name
            if name == binary.name:
                continue  # dylib's own install identity
            if dependency.startswith('@rpath/'):
                commands = subprocess.check_output(['otool', '-l', str(binary)], text=True)
                rpaths = re.findall(r'cmd LC_RPATH\s+cmdsize \d+\s+path (.*?) \(offset', commands)
                candidates = [Path(r.replace('@loader_path', str(binary.parent))) / dependency.removeprefix('@rpath/') for r in rpaths]
                source = next((p for p in candidates if p.is_file()), Path('/nonexistent/unresolved-rpath'))
                if source.is_file() and source.resolve().is_relative_to(app.resolve()):
                    continue
            elif dependency.startswith('@loader_path/'):
                source = binary.parent / dependency.removeprefix('@loader_path/')
            elif dependency.startswith('@executable_path/'):
                source = app / 'Contents/MacOS' / dependency.removeprefix('@executable_path/')
            else:
                source = Path(dependency)
            if dependency.startswith('@loader_path/') and source.is_file():
                if not source.resolve().is_relative_to(app.resolve()):
                    raise ValueError('Dependency escapes application: ' + dependency)
                continue
            if not source.is_file():
                source = root / 'submodules/libs4/haskell/.runtime' / name
            if not source.is_file():
                raise ValueError(f'Unresolved non-system dependency {dependency} in {binary}')
            frameworks.mkdir(exist_ok=True)
            target = frameworks / name
            if not target.exists():
                shutil.copy2(source, target)
                pending.append(target)
            replacement = '@loader_path/' + os.path.relpath(target, binary.parent)
            run('install_name_tool', '-change', dependency, replacement, binary)
        if binary.parent == frameworks:
            run('install_name_tool', '-id', '@rpath/' + binary.name, binary)


def sign(app, identity):
    flags = ['--force', '--sign', identity]
    if identity != '-':
        flags += ['--options', 'runtime', '--timestamp']
    for binary in sorted((p for p in app.rglob('*') if macho(p)), key=lambda p: len(p.parts), reverse=True):
        run('codesign', *flags, binary)
    run(sys.executable, Path(__file__).with_name('package.py'), 'finalize',
        '--runtime', app / 'Contents/Resources/runtime', '--signing', 'ad-hoc' if identity == '-' else 'developer-id')
    run('codesign', *flags, app)
    run('codesign', '--verify', '--deep', '--strict', app)


def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument('--version', default='0.1.0-dev')
    parser.add_argument('--output-directory')
    parser.add_argument('--provider')
    parser.add_argument('--skip-build', action='store_true', help='Use existing outputs; their provenance remains the caller responsibility')
    parser.add_argument('--allow-missing-provider', action='store_true')
    parser.add_argument('--sign-identity', default=os.environ.get('SAPIENS_MAC_SIGN_IDENTITY', '-'))
    parser.add_argument('--notary-profile', default=os.environ.get('SAPIENS_MAC_NOTARY_PROFILE'))
    parser.add_argument('--experimental-minimum-os', help='Explicit local experiment only; does not change supported macOS 13 floor')
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[2]
    os.chdir(root)
    if not re.fullmatch(r'\d+\.\d+\.\d+(?:-[0-9A-Za-z.-]+)?(?:\+[0-9A-Za-z.-]+)?', args.version):
        parser.error('--version must be SemVer')
    if args.notary_profile and args.sign_identity == '-':
        parser.error('Notarization requires Developer ID signing')
    if args.experimental_minimum_os and not re.fullmatch(r'\d+\.\d+(?:\.\d+)?', args.experimental_minimum_os):
        parser.error('--experimental-minimum-os must be a numeric OS version')
    os.environ['MACOSX_DEPLOYMENT_TARGET'] = '13.0'
    if not args.skip_build:
        run('runghc', root / 'build/Build.hs')
        run(root / 'submodules/blindly4/macos/build.sh')
    output = Path(args.output_directory or root / '.build/macos').resolve()
    output.mkdir(parents=True, exist_ok=True)
    with tempfile.TemporaryDirectory(prefix='sapiens-package-', dir=output) as temporary:
        app = Path(temporary) / 'Sapiens4 Development.app'
        resources = app / 'Contents/Resources'
        macos = app / 'Contents/MacOS'
        resources.mkdir(parents=True)
        macos.mkdir()
        shutil.copy2(root / '.build/bin/sapiens-macos', macos / 'Sapiens4')
        stage = [sys.executable, root / 'build/package/package.py', 'stage', '--root', root, '--destination', resources / 'runtime',
                 '--manifest', resources / 'runtime/package-manifest.json', '--version', args.version, '--platform', 'macos', '--architecture', 'arm64',
                 '--minimum-os', args.experimental_minimum_os or '13.0', '--support-status',
                 'experimental-minimum-os' if args.experimental_minimum_os else 'declared-target']
        if args.provider:
            stage += ['--provider', args.provider]
        if args.allow_missing_provider:
            stage += ['--allow-missing-provider']
        run(*stage)
        # Icon construction has no external SVG/rendering dependency on the end user's machine.
        iconset = Path(temporary) / 'AppIcon.iconset'
        iconset.mkdir()
        run(root / '.build/bin/sapiens-macos', '--icon', iconset, root / 'app/shell/favicon.svg')
        run('iconutil', '-c', 'icns', iconset, '-o', resources / 'AppIcon.icns')
        version = args.version.split('-')[0].split('+')[0]
        info = {'CFBundleIdentifier': 'com.poulwalcker.sapiens4.development', 'CFBundleName': 'Sapiens4 Development',
                'CFBundleDisplayName': 'Sapiens4 Development', 'CFBundleExecutable': 'Sapiens4', 'CFBundlePackageType': 'APPL',
                'CFBundleVersion': version, 'CFBundleShortVersionString': version, 'CFBundleIconFile': 'AppIcon',
                'NSHighResolutionCapable': True, 'LSMinimumSystemVersion': '13.0',
                'NSAppTransportSecurity': {'NSAllowsLocalNetworking': True, 'NSAllowsArbitraryLoadsInWebContent': True}}
        with (app / 'Contents/Info.plist').open('wb') as stream:
            plistlib.dump(info, stream)
        # Refuse an accidental support-floor change; experiments require an explicit flag.
        relocate(app, root)
        declared_minimum = tuple(map(int, (args.experimental_minimum_os or '13.0').split('.')))
        for binary in (p for p in app.rglob('*') if macho(p)):
            architectures = subprocess.check_output(['lipo', '-archs', str(binary)], text=True).split()
            if 'arm64' not in architectures:
                raise ValueError(f'Payload has no arm64 executable slice: {binary}')
            load_commands = subprocess.check_output(['otool', '-l', str(binary)], text=True)
            minimums = re.findall(r'\bminos ([0-9.]+)', load_commands)
            for version in minimums:
                if tuple(map(int, version.split('.'))) > declared_minimum:
                    raise ValueError(f'{binary.relative_to(app)} requires macOS {version}; declared floor is {declared_minimum}. '
                                     'Resolve provider compatibility or use --experimental-minimum-os for a local experiment.')
        info['LSMinimumSystemVersion'] = '.'.join(map(str, declared_minimum))
        with (app / 'Contents/Info.plist').open('wb') as stream:
            plistlib.dump(info, stream)
        sign(app, args.sign_identity)
        if args.notary_profile:
            submitted = Path(temporary) / 'notarization.zip'
            run('ditto', '-c', '-k', '--keepParent', app, submitted)
            run('xcrun', 'notarytool', 'submit', submitted, '--keychain-profile', args.notary_profile, '--wait')
            run('xcrun', 'stapler', 'staple', app)
            run('xcrun', 'stapler', 'validate', app)
            run('spctl', '--assess', '--type', 'execute', '--verbose=2', app)
        destination = output / app.name
        if destination.exists():
            raise ValueError('Output app already exists; choose a new output directory')
        shutil.move(str(app), destination)
        artifact = output / f'sapiens4-development-{args.version}-macos-arm64.zip'
        if artifact.exists():
            raise ValueError('Versioned artifact already exists')
        run('ditto', '-c', '-k', '--keepParent', destination, artifact)
        run(sys.executable, root / 'build/package/package.py', 'receipt', '--artifact', artifact,
            '--manifest', destination / 'Contents/Resources/runtime/package-manifest.json', '--output', artifact.with_suffix('.release.json'))
        print(f'Built {destination}\nArtifact {artifact}')


if __name__ == '__main__':
    main()
