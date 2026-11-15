#!/bin/sh
set -eu
cd "$(dirname "$0")/../.."
version=${1:-0.1.0}
output=${2:-.build/macos}
export MACOSX_DEPLOYMENT_TARGET=13.0
if [ "${3:-}" = --release ]; then
  npm ci --include=dev --prefix submodules/libs4
  rm -rf .build/web
fi
runghc build/Build.hs
./submodules/blindly4/macos/build.sh
shift "$(( $# < 2 ? $# : 2 ))"
./.build/bin/sapiens-package macos "$version" "$output" "$@"
