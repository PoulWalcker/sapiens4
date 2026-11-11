#!/bin/sh
set -eu
cd "$(dirname "$0")/../.."
version=${1:-0.1.0}
output=${2:-.build/macos}
export MACOSX_DEPLOYMENT_TARGET=13.0
runghc build/Build.hs
./submodules/blindly4/macos/build.sh
shift "$(( $# < 2 ? $# : 2 ))"
runghc -XOverloadedStrings -XTupleSections -icorpora/host build/Package.hs macos "$version" "$output" "$@"
