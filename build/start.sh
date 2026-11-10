#!/bin/sh
set -eu
cd "$(dirname "$0")/.."
git submodule update --init --recursive
platform=$(uname -s)
if [ "$platform" = Darwin ]; then
  ./submodules/blindly4/macos/build.sh
elif [ "$platform" != Linux ]; then
  echo "Blindly4 computer tasks require macOS. Chat is available on this platform."
fi
runghc build/Build.hs
if [ "$platform" = Linux ]; then exec ./environments/linux/start.sh "$@"; fi
exec .build/bin/sapiens4 "$@"
