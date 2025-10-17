#!/bin/bash
set -euo pipefail

cd "$(dirname "$0")"
source ./utils.sh

bold "Schema: embeds JSON/OpenAPI"

OUTDIR="./_tmp_schema"
REWATCH_BIN=$(cd "$(dirname "$REWATCH_EXECUTABLE")" >/dev/null 2>&1 && pwd)/$(basename "$REWATCH_EXECUTABLE")
rm -rf "$OUTDIR"
mkdir -p "$OUTDIR"

"$REWATCH_BIN" schema embeds --output-dir "$OUTDIR" --openapi >/dev/null

SNAPSHOT_DIR="../tests/snapshots-extra"
mkdir -p "$SNAPSHOT_DIR"
SNAPSHOT="$SNAPSHOT_DIR/schema-embeds.txt"
{
  echo '=== embedlang.input.schema.json ==='
  cat "$OUTDIR/embedlang.input.schema.json" || true
  echo
  echo '=== embedlang.output.schema.json ==='
  cat "$OUTDIR/embedlang.output.schema.json" || true
  echo
  echo '=== embedlang.openapi.json ==='
  cat "$OUTDIR/embedlang.openapi.json" || true
} > "$SNAPSHOT"

normalize_paths "$SNAPSHOT"
success "Schema embeds OK"

