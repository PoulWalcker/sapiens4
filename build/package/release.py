#!/usr/bin/env python3
"""Verify both development assets before an explicitly requested draft upload."""
import argparse
import hashlib
import json
from pathlib import Path
import re
import subprocess


def gh(*args):
    return subprocess.check_output(['gh', *args], text=True)


parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--repository', required=True)
parser.add_argument('--run', required=True)
parser.add_argument('--tag', required=True)
parser.add_argument('--download-directory', required=True)
parser.add_argument('--create-draft', action='store_true')
args = parser.parse_args()
if args.repository != 'PoulWalcker/sapiens4' or not args.run.isdecimal():
    parser.error('Only an explicit development-fork run is accepted')
if not re.fullmatch(r'dev-v\d+\.\d+\.\d+(?:-[A-Za-z0-9.-]+)?', args.tag):
    parser.error('A dev-vSemVer tag is required')
run = json.loads(gh('api', f'repos/{args.repository}/actions/runs/{args.run}'))
if run['conclusion'] != 'success' or run['event'] not in ('push', 'workflow_dispatch') or run['path'] != '.github/workflows/native-build.yml':
    raise ValueError('Select a successful trusted native build, not a pull request run')
folder = Path(args.download_directory).resolve()
if folder.exists():
    raise ValueError('Download directory must be new')
subprocess.run(['gh', 'run', 'download', args.run, '--repo', args.repository, '--pattern', 'sapiens4-development-*', '--dir', str(folder)], check=True)
receipts = sorted(folder.rglob('*.release.json'))
if len(receipts) != 2:
    raise ValueError('Exactly the Mac arm64 and Windows x64 receipts are required')
assets, targets, versions = [], set(), set()
for receipt in receipts:
    record = json.loads(receipt.read_text())
    if record['schemaVersion'] != 1 or record['appId'] != 'com.poulwalcker.sapiens4.development' or record['repository'] != args.repository or record['channel'] != 'development':
        raise ValueError('Unexpected release identity')
    if record['packageStatus'] != 'provider-complete' or record['sourceCommit'] != run['head_sha'] or record['sourceDirty']:
        raise ValueError('Incomplete provider, mismatched source, or dirty build cannot be released')
    versions.add(record['version'])
    if len(record['artifacts']) != 1:
        raise ValueError('Each platform receipt must bind one exact artifact')
    item = record['artifacts'][0]
    if item.get('supportStatus') != 'declared-target' or (item['platform'] == 'macos' and item.get('minimumOS') != '13.0'):
        raise ValueError('Experimental support-floor artifacts cannot be released')
    target = (item['platform'], item['architecture'])
    if target in targets:
        raise ValueError('Duplicate target')
    targets.add(target)
    if Path(item['assetName']).name != item['assetName']:
        raise ValueError('Invalid asset name')
    artifact = receipt.parent / item['assetName']
    digest = hashlib.sha256()
    with artifact.open('rb') as stream:
        for chunk in iter(lambda: stream.read(1024 * 1024), b''):
            digest.update(chunk)
    if artifact.stat().st_size != item['size'] or digest.hexdigest() != item['sha256']:
        raise ValueError('Final artifact does not match receipt')
    assets += [artifact, receipt]
if targets != {('macos', 'arm64'), ('windows', 'x64')} or len(versions) != 1 or args.tag != 'dev-v' + next(iter(versions)):
    raise ValueError('Both supported targets must have the exact tag version')
print(json.dumps({'sourceCommit': run['head_sha'], 'tag': args.tag, 'verifiedAssets': [p.name for p in assets]}, indent=2))
if args.create_draft:
    notes = folder / 'release-notes.txt'
    notes.write_text('Sapiens4 Development validation build.\n\nThese workflow artifacts are unsigned/ad-hoc signed and are not production releases.\nThe complete pinned provider is included. No automatic update discovery is enabled.\nNative Windows desktop acceptance and publisher signing remain required before production promotion.\n\nSource: ' + run['head_sha'] + '\nBuild run: ' + run['html_url'] + '\n')
    subprocess.run(['gh', 'release', 'create', args.tag, *map(str, assets), '--repo', args.repository,
                    '--target', run['head_sha'], '--title', 'Sapiens4 Development ' + next(iter(versions)),
                    '--draft', '--prerelease', '--notes-file', str(notes)], check=True)
