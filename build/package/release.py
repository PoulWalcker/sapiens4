#!/usr/bin/env python3
"""Verify the complete signed artifact set before the tag workflow publishes it."""
import argparse
import hashlib
import json
from pathlib import Path
import re


def verify(args):
    if args.repository != 'PoulWalcker/sapiens4' or not re.fullmatch(r'[a-f0-9]{40}', args.source_commit):
        raise ValueError('Expected development fork and exact tagged source commit')
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
        if len(record['artifacts']) != 1:
            raise ValueError('Each platform receipt must bind one exact artifact')
        item = record['artifacts'][0]
        target = (item['platform'], item['architecture'])
        if target in targets:
            raise ValueError('Duplicate target')
        targets.add(target)
        signing = {('macos', 'arm64'): 'developer-id', ('windows', 'x64'): 'authenticode'}
        if item.get('signing') != signing.get(target) or item.get('supportStatus') != 'declared-target':
            raise ValueError('Publisher-signed supported-target artifacts are required')
        if target[0] == 'macos' and item.get('minimumOS') != '13.0':
            raise ValueError('Experimental support-floor artifacts cannot be released')
        if Path(item['assetName']).name != item['assetName'] or '\n' in item['assetName'] or '\r' in item['assetName']:
            raise ValueError('Invalid asset name')
        artifact = receipt.parent / item['assetName']
        digest = hashlib.sha256()
        with artifact.open('rb') as stream:
            for chunk in iter(lambda: stream.read(1024 * 1024), b''):
                digest.update(chunk)
        if artifact.stat().st_size != item['size'] or digest.hexdigest() != item['sha256']:
            raise ValueError('Final artifact does not match receipt')
        assets += [artifact, receipt]
    if targets != {('macos', 'arm64'), ('windows', 'x64')}:
        raise ValueError('Both supported targets must have the exact tag version')
    Path(args.output).write_text(''.join(str(path) + '\n' for path in assets))
    print(json.dumps({'sourceCommit': args.source_commit, 'version': args.version, 'verifiedAssets': [p.name for p in assets]}, indent=2))


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description=__doc__)
    for name in ('repository', 'directory', 'source-commit', 'version', 'output'):
        parser.add_argument('--' + name, required=True)
    verify(parser.parse_args())
