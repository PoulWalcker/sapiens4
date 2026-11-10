#!/bin/sh
set -eu
if [ "$#" -eq 0 ]; then echo 'Usage: virtual.sh COMMAND [ARGUMENTS...]' >&2; exit 64; fi
for tool in xvfb-run xauth dbus-run-session openbox xprop; do
    command -v "$tool" >/dev/null || { echo "Missing $tool; install xvfb xauth dbus-x11 openbox x11-utils." >&2; exit 69; }
done
export LIBGL_ALWAYS_SOFTWARE=1 GDK_BACKEND=x11 XDG_SESSION_TYPE=x11
unset WAYLAND_DISPLAY AT_SPI_BUS_ADDRESS
runtime=$(mktemp -d)
trap 'rm -rf "$runtime"' EXIT
export XDG_RUNTIME_DIR=$runtime
xvfb-run -a -s '-screen 0 1440x960x24 -nolisten tcp' dbus-run-session -- sh -eu -c '
    openbox --sm-disable >/dev/null &
    manager=$!
    trap '\''kill "$manager" 2>/dev/null || :; wait "$manager" 2>/dev/null || :'\'' EXIT
    attempt=0
    until xprop -root _NET_SUPPORTING_WM_CHECK | grep -q "window id #"; do
        kill -0 "$manager"
        attempt=$((attempt + 1))
        [ "$attempt" -lt 50 ] || exit 1
        sleep 0.1
    done
    "$@"
' sapiens-virtual "$@"
