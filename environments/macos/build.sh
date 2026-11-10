#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
cd "$ROOT"
# Optional local release preparation; ordinary build/skip-build callers are unchanged.
if [ "${1:-}" = --local ]; then
    [ "$#" -ge 4 ] && [ "$#" -le 5 ] || { echo 'Usage: build.sh --local dev|prod COMMIT VERSION [OUTPUT_DIRECTORY]' >&2; exit 64; }
    mode=$2
    commit=$3
    version=$4
    output=${5:-.build/local-$version-$mode}
    case "$mode" in dev|prod) ;; *) echo 'Expected dev or prod' >&2; exit 64 ;; esac
    [ "$(uname -s)/$(uname -m)" = Darwin/arm64 ] || { echo 'Local packages require macOS ARM64' >&2; exit 1; }
    # Prefer the pinned GHC without changing the user's global ghcup selection.
    export PATH="$HOME/.ghcup/ghc/9.14.1/bin:$HOME/.ghcup/bin:$HOME/.cargo/bin:$PATH"
    for tool in git runghc ghc cabal node cargo rustc curl tar make xcrun codesign hdiutil; do
        command -v "$tool" >/dev/null || { echo "Missing prerequisite: $tool (see environments/macos/README.md)" >&2; exit 1; }
    done
    [ "$(ghc --numeric-version)" = 9.14.1 ] && [ "$(cabal --numeric-version)" = 3.18.2.0 ] &&
        [ "$(node --version)" = v22.23.2 ] && case "$(rustc --version)" in 'rustc 1.99.0 '*) true ;; *) false ;; esac ||
        { echo 'Use GHC9.14.1, Cabal3.18.2.0, Node22.23.2 and Rust1.99.0' >&2; exit 1; }
    xcrun --find clang >/dev/null
    [ "$(git rev-parse HEAD)" = "$commit" ] || { echo 'Checkout HEAD must equal the full shared build commit SHA' >&2; exit 1; }
    [ -z "$(git status --porcelain --untracked-files=normal --ignore-submodules=untracked)" ] ||
        { echo 'Local release preparation requires a clean checkout' >&2; exit 1; }
    case "$mode" in
        dev) export SAPIENS_MAC_SIGN_IDENTITY=- SAPIENS_MAC_NOTARY_PROFILE= ;;
        prod)
            : "${SAPIENS_MAC_SIGN_IDENTITY:?Set a Developer ID Application identity}"
            : "${SAPIENS_MAC_NOTARY_PROFILE:?Set an existing notarytool keychain profile}"
            case "$SAPIENS_MAC_SIGN_IDENTITY" in 'Developer ID Application: '*) ;; *) echo 'Production requires Developer ID Application signing' >&2; exit 1 ;; esac
            ;;
    esac
    git submodule sync --recursive
    git submodule update --init --recursive
    cache=submodules/libs4/haskell/.cabal
    mkdir -p "$cache"
    if [ ! -f "$cache/config" ]; then
        printf 'repository hackage.haskell.org\n  url: https://hackage.haskell.org/\n  secure: True\n' > "$cache/config"
    fi
    # No clean/removal: Cabal, UI, Rust, provider and packaging caches stay in place.
    channel=development
    [ "$mode" != prod ] || channel=stable
    set -- --channel "$channel" --version "$version" --output-directory "$output"
fi
exec runghc -ibuild/package build/Package.hs macos "$@"
