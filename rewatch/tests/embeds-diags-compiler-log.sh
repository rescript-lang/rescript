#!/bin/bash
set -euo pipefail

cd "$(dirname "$0")"
source ./utils.sh

bold "Embeds: diagnostics to .compiler.log"

FIXDIR="./_tmp_embeds/rewatch_diags_proj"
REWATCH_BIN=$(cd "$(dirname "$REWATCH_EXECUTABLE")" >/dev/null 2>&1 && pwd)/$(basename "$REWATCH_EXECUTABLE")
rm -rf "$FIXDIR"
mkdir -p "$FIXDIR"
cp -R ./fixtures/embeds_diags/* "$FIXDIR"/

pushd "$FIXDIR" >/dev/null
"$REWATCH_BIN" build --snapshot-output >/dev/null 2>&1 || true
popd >/dev/null

SNAPSHOT_DIR="../tests/snapshots-extra"
mkdir -p "$SNAPSHOT_DIR"
SNAPSHOT="$SNAPSHOT_DIR/embeds-diags-compiler-log.txt"
{
  echo '=== .compiler.log (filtered) ==='
  # Filter out volatile #Start/#Done timestamps
  grep -v '^#Start(' "$FIXDIR/lib/bs/.compiler.log" | grep -v '^#Done(' || true
} > "$SNAPSHOT"

normalize_paths "$SNAPSHOT"
success "Embeds diagnostics logged to .compiler.log"

