#!/bin/sh
set -eu
root=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
if [ "$#" -ne 1 ]; then echo 'Usage: package.sh OUTPUT.deb (build desktop and release Blindly4 first)' >&2; exit 64; fi
if [ "$(uname -m)" != x86_64 ]; then echo 'Only amd64 is qualified for this package.' >&2; exit 64; fi
build=${SAPIENS_LINUX_BUILD_DIR:-$root/.build}
driver=${SAPIENS_LINUX_DRIVER_DIR:-$root/submodules/blindly4/.build/release}
for binary in "$build/bin/sapiens4" "$build/bin/sapiens-linux" "$build/domain/sapiens-domain" "$driver/blindly4"; do
    required=$(objdump -T "$binary" | sed -n 's/.*(GLIBC_\([0-9.]*\)).*/\1/p' | sort -V | tail -1)
    newest=$(printf '%s\n2.35\n' "$required" | sort -V | tail -1)
    if [ -z "$required" ] || [ "$newest" != 2.35 ]; then
        echo "Build $binary against Ubuntu 22.04 libraries before packaging (requires glibc $required)." >&2; exit 65
    fi
done
output=$(realpath -m -- "$1")
temporary=$(mktemp -d)
trap 'rm -rf "$temporary"' EXIT
chmod 755 "$temporary"
payload=$temporary/opt/sapiens4
mkdir -p "$payload/.build/bin" "$payload/.build/domain" "$payload/.build/web" "$payload/corpora" \
    "$payload/submodules/blindly4/.build/release" "$payload/submodules/blindly4/linux" \
    "$payload/submodules/libs4" "$temporary/DEBIAN" "$temporary/usr/bin" \
    "$temporary/usr/share/applications" "$temporary/usr/share/icons/hicolor/scalable/apps"
for binary in sapiens4 sapiens-linux; do
    install -m 755 "$build/bin/$binary" "$payload/.build/bin/"
    strip "$payload/.build/bin/$binary"
done
install -m 755 "$build/domain/sapiens-domain" "$payload/.build/domain/"
strip "$payload/.build/domain/sapiens-domain"
install -m 755 "$driver/blindly4" "$payload/submodules/blindly4/.build/release/"
for asset in app.js guest.js sw.js; do install -m 644 "$root/.build/web/$asset" "$payload/.build/web/"; done
cp -R "$root/app" "$root/license" "$payload/"
cp -R "$root/corpora/prompts" "$payload/corpora/"
cp -R "$root/submodules/libs4/web" "$root/submodules/libs4/license" "$payload/submodules/libs4/"
cp -R "$root/submodules/blindly4/license" "$payload/submodules/blindly4/"
install -m 644 "$root/submodules/blindly4/linux/README.md" "$root/submodules/blindly4/linux/Cargo.lock" "$payload/submodules/blindly4/linux/"
cargo metadata --locked --offline --format-version 1 --filter-platform x86_64-unknown-linux-gnu \
    --manifest-path "$root/submodules/blindly4/linux/Cargo.toml" > "$temporary/cargo.json"
python3 - "$temporary/cargo.json" "$payload/submodules/blindly4/license/dependencies" <<'PY'
import json
import shutil
import sys
from pathlib import Path

destination = Path(sys.argv[2])
destination.mkdir()
packages = [p for p in json.loads(Path(sys.argv[1]).read_text())['packages'] if p.get('source')]
for package in packages:
    target = destination / (package['name'] + '-' + package['version'])
    target.mkdir()
    for source in Path(package['manifest_path']).parent.iterdir():
        if source.name.lower().startswith(('license', 'copying', 'notice')):
            if source.is_dir():
                shutil.copytree(source, target / source.name)
            else:
                shutil.copyfile(source, target / source.name)
    if not any(target.iterdir()):
        raise SystemExit('Dependency notice missing: ' + package['name'])
inventory = [{key: package.get(key) for key in ('name', 'version', 'license', 'repository')} for package in packages]
(destination / 'inventory.json').write_text(json.dumps(inventory, indent=2) + '\n')
PY
rm "$temporary/cargo.json"
install -m 644 "$root/environments/linux/README.md" "$payload/README.md"
cat > "$temporary/usr/bin/sapiens4" <<'EOF'
#!/bin/sh
exec /opt/sapiens4/.build/bin/sapiens-linux "$@"
EOF
chmod 755 "$temporary/usr/bin/sapiens4"
cat > "$temporary/usr/share/applications/org.sapiens.Sapiens4.desktop" <<'EOF'
[Desktop Entry]
Type=Application
Name=Sapiens4 (Development)
Exec=sapiens4
Icon=org.sapiens.Sapiens4
Terminal=false
Categories=Office;
EOF
install -m 644 "$root/app/shell/favicon.svg" "$temporary/usr/share/icons/hicolor/scalable/apps/org.sapiens.Sapiens4.svg"
cat > "$temporary/DEBIAN/control" <<'EOF'
Package: sapiens4
Version: 0.2.0~linux-dev3
Section: utils
Priority: optional
Architecture: amd64
Maintainer: Sapiens4 <license@sapiens4.ai>
Depends: libc6 (>= 2.35), libgtk-3-0 (>= 3.24) | libgtk-3-0t64 (>= 3.24), libwebkit2gtk-4.1-0 (>= 2.40), libjson-glib-1.0-0, libgmp10, libffi8, libnuma1, zlib1g, dbus-user-session, at-spi2-core, xdg-utils, libglib2.0-bin
Description: Sapiens4 Linux development preview
 Native GTK/WebKit desktop and Blindly4 accessibility CLI.
 Desktop control requires an X11 session. Production qualification is pending.
EOF
mkdir -p "$(dirname -- "$output")"
dpkg-deb --root-owner-group --build "$temporary" "$output"
(cd "$(dirname -- "$output")" && sha256sum -- "$(basename -- "$output")") > "$output.sha256"
