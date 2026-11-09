#!/usr/bin/env python3
"""Verify the complete signed artifact set before the tag workflow publishes it."""
import argparse
import json
from pathlib import Path
import re
from package import digest


def verify(args):
    if args.repository != 'PoulWalcker/sapiens4' or not re.fullmatch(r'[a-f0-9]{40}', args.source_commit):
        raise ValueError('Expected development fork and exact tagged source commit')
    test_release = getattr(args, 'test_release', False)
    if test_release and not re.fullmatch(r'(?:0|[1-9][0-9]*)\.(?:0|[1-9][0-9]*)\.(?:0|[1-9][0-9]*)-dev\.release-check\.[1-9][0-9]*', args.version):
        raise ValueError('Test signing is restricted to dev.release-check prereleases')
    receipts = sorted(Path(args.directory).resolve().rglob('*.release.json'))
    if len(receipts) != 2:
        raise ValueError('Exactly the Mac arm64 and Windows x64 receipts are required')
    assets, targets = [], set()
    for receipt in receipts:
        record = json.loads(receipt.read_text())
        if record['schemaVersion'] != 1 or record['appId'] != 'com.poulwalcker.sapiens4.development' or record['repository'] != args.repository or record['channel'] != 'development':
            raise ValueError('Unexpected release identity')
        if record['packageStatus'] != 'provider-complete' or record['sourceCommit'] != args.source_commit or record['sourceDirty'] or record['version'] != args.version:
            raise ValueError('Incomplete provider, wrong version/source or dirty build cannot be released')
        items = record['artifacts']
        if len(items) != 2 or {item.get('kind') for item in items} != {'installer', 'portable-zip'}:
            raise ValueError('Each platform requires its primary installer and portable ZIP')
        target = (items[0]['platform'], items[0]['architecture'])
        if target in targets:
            raise ValueError('Duplicate target')
        targets.add(target)
        expected = {('macos', 'arm64'): ('developer-id', 'Sapiens.dmg'),
                    ('windows', 'x64'): ('authenticode', 'Sapiens-Setup.exe')}
        if target not in expected:
            raise ValueError('Unsupported release target')
        signing, installer = expected[target]
        if test_release:
            signing = 'ad-hoc' if target[0] == 'macos' else 'none'
        names = set()
        for item in items:
            if (item['platform'], item['architecture']) != target or item.get('signing') != signing or item.get('supportStatus') != 'declared-target':
                raise ValueError('Publisher-signed supported-target artifacts are required')
            if target[0] == 'macos' and item.get('minimumOS') != '13.0':
                raise ValueError('Experimental support-floor artifacts cannot be released')
            name = item['assetName']
            if Path(name).name != name or '\n' in name or '\r' in name or name in names:
                raise ValueError('Invalid or duplicate asset name')
            names.add(name)
            wanted = installer if item['kind'] == 'installer' else f'sapiens4-development-{args.version}-{target[0]}-{target[1]}.zip'
            if name != wanted:
                raise ValueError('Unexpected installer/portable artifact name')
            artifact = receipt.parent / name
            if artifact.stat().st_size != item['size'] or digest(artifact) != item['sha256']:
                raise ValueError('Final artifact does not match receipt')
            assets.append(artifact)
        assets.append(receipt)
    if targets != {('macos', 'arm64'), ('windows', 'x64')}:
        raise ValueError('Both supported targets must have the exact tag version')
    Path(args.output).write_text(''.join(str(path) + '\n' for path in assets))
    print(json.dumps({'sourceCommit': args.source_commit, 'version': args.version, 'verifiedAssets': [p.name for p in assets]}, indent=2))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('repository', 'directory', 'source-commit', 'version', 'output'):
        parser.add_argument('--' + name, required=True)
    parser.add_argument('--test-release', action='store_true')
    verify(parser.parse_args())
