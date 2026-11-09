#!/bin/sh
set -eu
cd "$(dirname "$0")/.."
git submodule update --init --recursive
if [ "$(uname -s)" = Darwin ]; then
  ./environments/macos/build.sh --build-only
else
  runghc build/Build.hs
fi
if [ -z "${SAPIENS_CODEX_BINARY:-}" ]; then
case "$(uname -s):$(uname -m)" in
  Darwin:arm64) runghc -ibuild/package build/Package.hs provider --platform darwin-arm64 --destination .build/codex --ensure ;;
  *) echo 'Bundled Codex requires macOS arm64; other source targets need SAPIENS_CODEX_BINARY.' >&2 ;;
esac
fi
exec .build/bin/sapiens4 "$@"
