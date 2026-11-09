#!/bin/sh
set -eu
cd "$(dirname "$0")/.."
git submodule update --init --recursive
if [ "$(uname -s)" = Darwin ]; then
  ./environments/macos/build.sh --build-only
else
  runghc build/Build.hs
fi
exec .build/bin/sapiens4 "$@"
