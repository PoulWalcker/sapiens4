#!/usr/bin/env python3
"""Windows/legacy CLI compatibility; packaging is implemented in Haskell.

The small file helpers remain for the existing Python provider/cache/release
callers. They do not implement a second manifest or receipt policy.
"""
import hashlib
from pathlib import Path
import subprocess
import sys


def digest(path):
    with path.open('rb') as stream:
        result = hashlib.sha256()
        for chunk in iter(lambda: stream.read(1024 * 1024), b''):
            result.update(chunk)
        return result.hexdigest()


def inventory(root, excluded):
    entries = sorted(root.rglob('*'), key=lambda path: path.relative_to(root).as_posix())
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



if __name__ == '__main__':
    # Compatibility CLI for existing Windows and verification callers. Runtime
    # assembly/integrity/receipts have one implementation in Package.hs.
    root = Path(__file__).resolve().parents[2]
    raise SystemExit(subprocess.call(['runghc', '-ibuild/package', 'build/Package.hs', *sys.argv[1:]], cwd=root))
