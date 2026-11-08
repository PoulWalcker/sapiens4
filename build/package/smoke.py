#!/usr/bin/env python3
"""Collect installed-runtime observations for the RSpec package scenario."""
import argparse
import hashlib
import json
import os
from pathlib import Path
import queue
import subprocess
import tempfile
import threading
import time
import urllib.request

parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument('--runtime', required=True)
args = parser.parse_args()
runtime = Path(args.runtime).resolve()
binary = runtime / ('.build/bin/sapiens4.exe' if os.name == 'nt' else '.build/bin/sapiens4')
observations = []
with tempfile.TemporaryDirectory(prefix='sapiens-package-smoke-') as temporary:
    root = Path(temporary)
    data = root / 'synthetic-data'
    environment = {k: v for k, v in os.environ.items() if not k.startswith(('SAPIENS_', 'CODEX_'))}
    environment.update(SAPIENS_REPOSITORY=str(runtime), SAPIENS_CONFIG_DIR=str(root / 'config'),
                       SAPIENS_DATA_DIR=str(data), SAPIENS_CODEX_BINARY=str(root / 'provider-disabled'))
    environment['PATH'] = os.environ.get('SystemRoot', 'C:\\Windows') + '\\System32' if os.name == 'nt' else '/usr/bin:/bin:/usr/sbin:/sbin'
    for iteration in (1, 2):
        process = subprocess.Popen([str(binary), '--port', '0', '--data-dir', str(data), '--desktop'],
                                   cwd=root, env=environment, stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
        lines = queue.Queue()
        errors = []
        def read_stdout(stream):
            for line in stream:
                lines.put(line)
        threading.Thread(target=read_stdout, args=(process.stdout,), daemon=True).start()
        error_reader = threading.Thread(target=lambda: errors.append(process.stderr.read()), daemon=True)
        error_reader.start()
        try:
            end = time.monotonic() + 40
            while time.monotonic() < end:
                try:
                    line = lines.get(timeout=.2)
                except queue.Empty:
                    if process.poll() is not None:
                        error_reader.join()
                        raise RuntimeError('Installed runtime exited before readiness: ' + ''.join(errors))
                    continue
                if line.startswith('Sapiens4: http://127.0.0.1:'):
                    address = line.removeprefix('Sapiens4: ').strip().split('/workspace/')[0]
                    break
            else:
                raise RuntimeError('Installed runtime did not report readiness')
            routes = []
            for route in ('/api/health', '/workspace/', '/workspace/app.js', '/workspace/guest.js', '/remote-vendor.js', '/workspace/styles.css'):
                with urllib.request.urlopen(address + route, timeout=5) as response:
                    routes.append({'path': route, 'status': response.status, 'bytes': len(response.read())})
            current = hashlib.sha256((data / 'corpora-identity.json').read_bytes()).hexdigest()
        finally:
            if process.poll() is None:
                process.stdin.write('stop\n')
                process.stdin.flush()
                try:
                    process.wait(timeout=15)
                except subprocess.TimeoutExpired:
                    process.kill()
                    process.wait()
                    raise RuntimeError('Installed runtime did not stop gracefully')
        error_reader.join()
        observations.append({'iteration': iteration, 'routes': routes, 'exitCode': process.returncode,
                             'identitySha256': current, 'stderr': ''.join(errors)})
print(json.dumps(observations))
