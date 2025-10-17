#!/bin/bash
set -euo pipefail

cd "$(dirname "$0")"
source ./utils.sh

bold "Embeds: diagnostics mapping"

FIXDIR="./_tmp_embeds/rewatch_diags_proj"
REWATCH_BIN=$(cd "$(dirname "$REWATCH_EXECUTABLE")" >/dev/null 2>&1 && pwd)/$(basename "$REWATCH_EXECUTABLE")
rm -rf "$FIXDIR"
mkdir -p "$FIXDIR"
cp -R ./fixtures/embeds_diags/* "$FIXDIR"/

pushd "$FIXDIR" >/dev/null
# Capture console output including embed diagnostics
OUTFILE="console.txt"
"$REWATCH_BIN" build --snapshot-output > "$OUTFILE" 2>&1 || true
popd >/dev/null

SNAPSHOT_DIR="../tests/snapshots-extra"
mkdir -p "$SNAPSHOT_DIR"
SNAPSHOT="$SNAPSHOT_DIR/embeds-diags.txt"
{
  echo '=== Console ==='
  cat "$FIXDIR/console.txt"
} > "$SNAPSHOT"

normalize_paths "$SNAPSHOT"
success "Embeds diagnostics mapping OK"
