#!/bin/sh
set -eu
root=$(CDPATH= cd -- "$(dirname -- "$0")/../.." && pwd)
case ${XDG_DATA_HOME:-} in
  /*) data=$XDG_DATA_HOME/sapiens4 ;;
  *) data=$HOME/.local/share/sapiens4 ;;
esac
data=${SAPIENS_DATA_DIR:-$data}
export SAPIENS_REPOSITORY="$root"
case ${SAPIENS_LINUX_BACKEND:-native} in
  desktop)
    export SAPIENS_DATA_DIR="$data"
    if [ ! -x "$root/.build/bin/sapiens-linux" ]; then
      echo 'Desktop backend is not built; run runghc build/Build.hs --desktop.' >&2; exit 69
    fi
    exec "$root/.build/bin/sapiens-linux" "$@" ;;
  native) exec "$root/.build/bin/sapiens4" "$@" --data-dir "$data" ;;
  *) echo 'SAPIENS_LINUX_BACKEND must be desktop or native' >&2; exit 64 ;;
esac
