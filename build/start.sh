#!/bin/sh
set -eu
cd "$(dirname "$0")/.."
git submodule update --init --recursive
if [ "$(uname -s)" = Darwin ]; then
  ./submodules/blindly4/macos/build.sh
else
  echo "Blindly4 computer tasks require macOS. Chat is available on this platform."
fi
runghc build/Build.hs
if [ -z "${SAPIENS_CODEX_BINARY:-}" ]; then
case "$(uname -s):$(uname -m)" in
  Darwin:arm64) python3 build/acquire-codex.py --platform darwin-arm64 --destination .build/codex --ensure ;;
  *) echo 'Bundled Codex supports macOS arm64 and Windows x64; set SAPIENS_CODEX_BINARY for other source targets.' >&2 ;;
esac
fi
exec .build/bin/sapiens4 "$@"
