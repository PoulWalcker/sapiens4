#!/usr/bin/env python3
"""Key the isolated dependency cache and admit only checked Cabal stores."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import subprocess
from checksums import digest

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('mode', choices=('key', 'check'))
args = parser.parse_args()
root = Path(__file__).resolve().parents[2]
project = root / 'submodules/libs4/haskell'
cache = project / '.cabal'


def output(name, value):
    with open(os.environ['GITHUB_OUTPUT'], 'a') as stream:
        stream.write(f'{name}={value}\n')


if args.mode == 'key':
    inputs = {name: os.environ.get(name, '') for name in
              ('RUNNER_OS', 'RUNNER_ARCH', 'ImageOS', 'ImageVersion', 'MACOSX_DEPLOYMENT_TARGET')}
    # Never share across runs when the runner image cannot be identified.
    inputs['image'] = inputs['ImageVersion'] or os.environ['GITHUB_RUN_ID']
    inputs['workspace'] = str(root)
    inputs['ghc'] = subprocess.check_output(['ghc', '--info'], text=True)
    inputs['cabal'] = subprocess.check_output(['cabal', '--numeric-version'], text=True).strip()
    inputs['dependencies'] = {name: digest(project / name)
                              for name in ('cabal.project', 'cabal.project.freeze', 'sapiens-domain.cabal')}
    fingerprint = hashlib.sha256(json.dumps(inputs, sort_keys=True).encode()).hexdigest()
    output('key', f'cabal-deps-v1-{inputs["RUNNER_OS"]}-{inputs["RUNNER_ARCH"]}-{fingerprint}')
else:
    databases = list((cache / 'store').glob('*/package.db'))
    # Build.hs copies these only after Cabal compilation, before build-ui.
    # They are witnesses, never cache payloads: a partial dependency build must
    # not occupy an immutable cache key with an incomplete useful package set.
    binaries = ('sapiens4.exe', 'sapiens-windows.exe') if os.name == 'nt' else ('sapiens4', 'sapiens-macos')
    ready = (bool(databases) and (cache / 'packages/hackage.haskell.org/01-index.tar').is_file()
             and all((root / '.build/bin' / name).is_file() for name in binaries))
    log = []
    for database in databases:
        # Cabal retains empty .lock files normally; unfinished incoming entries
        # are not reusable, and we never delete them to make a cache look valid.
        incoming = database.parent / 'incoming'
        if incoming.exists() and any(p.is_dir() or p.suffix != '.lock' for p in incoming.iterdir()):
            ready = False
        checked = subprocess.run(['ghc-pkg', '--no-user-package-db', '--package-db', str(database), 'check'],
                                 capture_output=True, text=True)
        log.append(checked.stdout + checked.stderr)
        ready = ready and checked.returncode == 0
    (root / '.build').mkdir(exist_ok=True)
    (root / '.build/cabal-cache-check.log').write_text('\n'.join(log))
    output('ready', str(ready).lower())
    print('Cabal dependency cache verified' if ready else 'Dependency cache not saved: incomplete or invalid store')
