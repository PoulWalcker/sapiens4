#!/bin/sh
set -eu
root=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
if [ "$#" -ne 1 ]; then echo 'Usage: qualify.sh NEW_REPORT_DIRECTORY' >&2; exit 64; fi
report=$(realpath -m -- "$1")
mkdir -p -- "$(dirname -- "$report")"
if ! mkdir -- "$report"; then echo 'Choose a new report directory to preserve earlier evidence' >&2; exit 64; fi
cd "$root"
{
    date -u
    uname -a
    git rev-parse HEAD
    git status --short
    git submodule status --recursive
    sha256sum .build/bin/sapiens-linux "${SAPIENS_TEST_BLINDLY4:-submodules/blindly4/.build/release/blindly4}"
} > "$report/environment.txt"
export SAPIENS_TEST_LINUX_DESKTOP=1 SAPIENS_TEST_LINUX_AUTOMATION=1 SAPIENS_TEST_LINUX_VIRTUAL=1
status=0
sh environments/linux/virtual.sh ruby submodules/libs4/ruby/bundle.rb exec rspec \
    --format documentation --format json --out "$report/results.json" > "$report/run.log" 2>&1 || status=$?
printf 'Qualification exit status: %s; evidence: %s\n' "$status" "$report"
exit "$status"
