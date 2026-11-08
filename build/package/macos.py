#!/usr/bin/env python3
"""Build a relocatable development app, then sign its complete payload."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import plistlib
import re
import shutil
import subprocess
import sys
import tempfile
from package import canonical, inventory

MAGIC = (b'\xfe\xed\xfa\xce', b'\xce\xfa\xed\xfe', b'\xfe\xed\xfa\xcf', b'\xcf\xfa\xed\xfe',
         b'\xca\xfe\xba\xbe', b'\xbe\xba\xfe\xca', b'\xca\xfe\xba\xbf', b'\xbf\xba\xfe\xca')


def run(*args):
    subprocess.run([str(a) for a in args], check=True)


def macho(path):
    if not path.is_file() or path.is_symlink():
        return False
    with path.open('rb') as stream:
        return stream.read(4) in MAGIC


def optional_voice(app):
    """The exception applies only before mutation to this exact released payload."""
    rule = json.loads(Path(__file__).with_name('codex-macos-compatibility.json').read_text())
    provider = app / 'Contents/Resources/runtime/codex'
    metadata = provider / 'sapiens-provider.json'
    if not metadata.exists():
        return {}, None
    info = json.loads(metadata.read_text())
    if any(info.get(key) != value for key, value in rule['providerIdentity'].items()):
        return {}, None  # Unknown provider: every binary remains required.
    observed = inventory(provider, {'sapiens-provider.json'})
    if hashlib.sha256(canonical(observed)).hexdigest() != rule['providerFilesSha256']:
        raise ValueError('Provider bytes differ from the exact audited optional-voice release')
    voice = provider / rule['optionalRoot']
    return {p.resolve(): rule for p in voice.rglob('*') if macho(p)}, rule


def os_version(value):
    numbers = tuple(map(int, value.split('.')))
    return numbers + (0,) * (3 - len(numbers))


def audit_macos(app, optional, rule, declared):
    """Audit all load-command edges; neither side can cross the optional boundary."""
    app = app.resolve()
    required_minimum = '13.0'
    required, voice = [], []
    for binary in sorted(p for p in app.rglob('*') if macho(p)):
        binary = binary.resolve()
        architectures = subprocess.check_output(['lipo', '-archs', str(binary)], text=True).split()
        if architectures != ['arm64']:
            raise ValueError(f'Expected an audited arm64-only payload: {binary}')
        commands = subprocess.check_output(['otool', '-l', str(binary)], text=True)
        platforms = re.findall(r'cmd LC_BUILD_VERSION\s+cmdsize \d+\s+platform (\S+)', commands)
        if any(platform != '1' for platform in platforms):
            raise ValueError(f'Non-macOS Mach-O platform in {binary}')
        minimums = re.findall(r'\bminos ([0-9.]+)', commands)
        minimums += re.findall(r'cmd LC_VERSION_MIN_MACOSX\s+cmdsize \d+\s+version ([0-9.]+)', commands)
        if not minimums:
            raise ValueError(f'No macOS minimum found for {binary}')
        minimum = max(minimums, key=os_version)
        allowed = rule['maximumMinimumOS'] if binary in optional else declared
        if os_version(minimum) > os_version(allowed):
            raise ValueError(f'{binary.relative_to(app)} requires macOS {minimum}; allowed floor is {allowed}')
        dependencies = []
        identities = re.findall(r'cmd LC_ID_DYLIB\s+cmdsize \d+\s+name (.*?) \(offset', commands)
        rpaths = re.findall(r'cmd LC_RPATH\s+cmdsize \d+\s+path (.*?) \(offset', commands)
        for line in subprocess.check_output(['otool', '-L', str(binary)], text=True).splitlines()[1:]:
            dependency = line.strip().split(' (')[0]
            if dependency in identities or dependency.startswith(('/usr/lib/', '/System/Library/')):
                continue
            def expand(path):
                return path.replace('@loader_path', str(binary.parent)).replace('@executable_path', str(app / 'Contents/MacOS'))
            candidates = ([Path(expand(r)) / dependency.removeprefix('@rpath/') for r in rpaths]
                          if dependency.startswith('@rpath/') else [Path(expand(dependency))])
            target = next((p.resolve() for p in candidates if p.is_file()), None)
            if target is None or not target.is_relative_to(app):
                raise ValueError(f'Unresolved or external packaged dependency {dependency} in {binary}')
            if not macho(target):
                raise ValueError(f'Unrecognized packaged Mach-O dependency: {target}')
            if (binary in optional) != (target in optional):
                raise ValueError(f'Dependency crosses audited optional voice boundary: {binary} -> {target}')
            dependencies.append(target.relative_to(app).as_posix())
        record = {'path': binary.relative_to(app).as_posix(), 'minimumOS': minimum, 'dependencies': sorted(dependencies)}
        if binary in optional:
            voice.append(record)
        else:
            required_minimum = max(required_minimum, minimum, key=os_version)
            required.append(record)
    return {'declaredMinimumOS': declared, 'requiredMinimumOS': required_minimum,
            'requiredBinaries': required, 'optionalComponents': ([{
                'name': 'codex-voice', 'ruleId': rule['ruleId'],
                'minimumOS': max((item['minimumOS'] for item in voice), key=os_version),
                'notUsedBySelectedPaths': rule['selectedPaths'], 'binaries': voice,
                'evidenceCommit': rule['providerIdentity']['upstreamCommit']}] if voice else []),
            'targetOSAcceptance': 'not-run'}


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
    args = parser.parse_args()
    root = Path(__file__).resolve().parents[2]
    os.chdir(root)
    if not re.fullmatch(r'\d+\.\d+\.\d+(?:-[0-9A-Za-z.-]+)?(?:\+[0-9A-Za-z.-]+)?', args.version):
        parser.error('--version must be SemVer')
    if args.notary_profile and args.sign_identity == '-':
        parser.error('Notarization requires Developer ID signing')
    os.environ['MACOSX_DEPLOYMENT_TARGET'] = '13.0'
    if not args.skip_build:
        run('runghc', root / 'build/Build.hs', '--no-provider')
        run(root / 'submodules/blindly4/macos/build.sh')
    if not args.provider and not args.allow_missing_provider:
        args.provider = root / '.build/codex'
        run(sys.executable, root / 'build/package/acquire-provider.py', '--platform', 'darwin-arm64',
            '--destination', args.provider, '--ensure')
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
                 '--minimum-os', '13.0']
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
        # Fingerprint the complete upstream tree before relocation/signing changes bytes.
        optional, rule = optional_voice(app)
        relocate(app, root)
        audit = audit_macos(app, optional, rule, '13.0')
        manifest_path = resources / 'runtime/package-manifest.json'
        manifest = json.loads(manifest_path.read_text())
        manifest['macOSCompatibility'] = audit
        manifest_path.write_text(json.dumps(manifest, indent=2) + '\n')
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
