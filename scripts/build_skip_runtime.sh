#!/usr/bin/env bash
set -euo pipefail

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"
DEST_DIR="$PROJECT_ROOT/analysis/bin/skip_runtime"

SRC_DIR="${SKIP_OCAML_SRC:-$HOME/GitHub/skip-ocaml}"

if [ ! -d "$SRC_DIR" ]; then
  echo "ERROR: SKIP_OCAML_SRC directory '$SRC_DIR' does not exist." >&2
  echo "Set SKIP_OCAML_SRC to the path of the skip-ocaml checkout before running this script." >&2
  exit 1
fi

echo "Building skip-ocaml from $SRC_DIR"
(
  cd "$SRC_DIR"
  make clean >/dev/null 2>&1 || true
  make
)

mkdir -p "$DEST_DIR"

copy_artifact() {
  local filename="$1"
  if [ ! -f "$SRC_DIR/build/$filename" ]; then
    echo "ERROR: Expected artifact build/$filename not found in skip-ocaml build output." >&2
    exit 1
  fi
  cp "$SRC_DIR/build/$filename" "$DEST_DIR/$filename"
}

copy_artifact "libskip_reactive.a"

echo "Skip runtime artifacts copied to $DEST_DIR"

