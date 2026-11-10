#!/bin/sh
set -eu
ROOT=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
cd "$ROOT"
if [ ! -f "$ROOT/submodules/libs4/web/remote-vendor.js" ]; then
  git -C "$ROOT" submodule update --init --recursive -- submodules/libs4
  test -f "$ROOT/submodules/libs4/web/remote-vendor.js"
fi
runghc build/Build.hs
BUILD="$ROOT/.build/macos"
APP="$BUILD/Sapiens4.app"
rm -rf "$APP"
mkdir -p "$APP/Contents/MacOS" "$APP/Contents/Resources" "$BUILD/AppIcon.iconset"
cp "$ROOT/.build/bin/sapiens-macos" "$APP/Contents/MacOS/Sapiens4"
RUNTIME="$ROOT/submodules/libs4/haskell/.runtime"
if [ ! -d "$RUNTIME" ]; then RUNTIME="$ROOT/.build/bin/.libs"; fi
if [ -d "$RUNTIME" ]; then
  mkdir -p "$APP/Contents/Frameworks"
  for library in "$RUNTIME"/*.dylib; do
    [ -f "$library" ] || continue
    cp "$library" "$APP/Contents/Frameworks/"
  done
  for binary in "$APP/Contents/MacOS/Sapiens4" "$APP/Contents/Frameworks/"*.dylib; do
    [ -f "$binary" ] || continue
    otool -L "$binary" | awk 'NR > 1 { print $1 }' | while IFS= read -r dependency; do
      name=$(basename "$dependency")
      if [ -f "$APP/Contents/Frameworks/$name" ]; then
        install_name_tool -change "$dependency" "@executable_path/../Frameworks/$name" "$binary"
      fi
    done
    codesign --force --sign - "$binary"
  done
fi
"$ROOT/.build/bin/sapiens-macos" --icon "$BUILD/AppIcon.iconset" "$ROOT/app/shell/favicon.svg"
iconutil -c icns "$BUILD/AppIcon.iconset" -o "$APP/Contents/Resources/AppIcon.icns"
INFO="$APP/Contents/Info.plist"
plutil -create xml1 "$INFO"
plutil -insert CFBundleIdentifier -string com.sapiens4.desktop "$INFO"
plutil -insert CFBundleName -string Sapiens4 "$INFO"
plutil -insert CFBundleDisplayName -string Sapiens4 "$INFO"
plutil -insert CFBundleExecutable -string Sapiens4 "$INFO"
plutil -insert CFBundlePackageType -string APPL "$INFO"
plutil -insert CFBundleVersion -string 1 "$INFO"
plutil -insert CFBundleShortVersionString -string 1.0 "$INFO"
plutil -insert CFBundleIconFile -string AppIcon "$INFO"
plutil -insert NSHighResolutionCapable -bool YES "$INFO"
plutil -insert LSMinimumSystemVersion -string 13.0 "$INFO"
plutil -insert NSAppTransportSecurity -xml '<dict><key>NSAllowsLocalNetworking</key><true/><key>NSAllowsArbitraryLoadsInWebContent</key><true/></dict>' "$INFO"
plutil -insert NSHumanReadableCopyright -string 'Sapiens4 local desktop launcher' "$INFO"
plutil -insert NSMicrophoneUsageDescription -string 'Sapiens4 uses the microphone to record voice messages.' "$INFO"
CONFIG="$APP/Contents/Resources/Launcher.plist"
plutil -create xml1 "$CONFIG"
plutil -insert Repository -string "$ROOT" "$CONFIG"
plutil -insert Executable -string "$ROOT/.build/bin/sapiens4" "$CONFIG"
plutil -insert Path -string "$PATH" "$CONFIG"
DATA=${SAPIENS_DATA_DIR:-$ROOT/.sapiens4}
case "$DATA" in '~/'*) DATA="$HOME/${DATA#\~/}" ;; esac
case "$DATA" in /*) ;; *) DATA="$ROOT/$DATA" ;; esac
plutil -insert DataDirectory -string "$DATA" "$CONFIG"
mkdir -p "$APP/Contents/Resources/Licenses/Blindly4" "$APP/Contents/Resources/Licenses/Libs4"
cp "$ROOT/license/LICENSE.md" "$ROOT/license/LICENSING.md" "$APP/Contents/Resources/Licenses/"
cp "$ROOT/README.md" "$APP/Contents/Resources/README.md"
cp "$ROOT/submodules/blindly4/license/LICENSE.md" "$APP/Contents/Resources/Licenses/Blindly4/"
cp "$ROOT/submodules/blindly4/README.md" "$APP/Contents/Resources/Licenses/Blindly4/README.md"
cp "$ROOT/submodules/libs4/license/"* "$APP/Contents/Resources/Licenses/Libs4/"
codesign --force --sign - "$APP"
printf 'Built: %s\n' "$APP"
